(defpackage #:pine/edit/key
  (:use #:cl)
  (:shadow #:last)
  (:local-nicknames (#:d #:pine/data) (#:cmd #:pine/repl/command)
                    (#:mode #:pine/repl/mode) (#:buffer #:pine/edit/buffer)
                    (#:fault #:pine/run/fault) (#:prompt #:pine/edit/prompt))
  (:export #:key #:make-key #:named #:key-sym #:key-ctrl #:key-meta #:key-shift
           #:key-super #:key= #:parse-key #:parse-chord #:chord-text
           #:self-insert-p #:dispatch #:pending #:last #:prefix #:*on-insert*
           #:take-next #:taking
           #:bindings-of #:where))
(in-package #:pine/edit/key)

(defvar *keys* (d:table))

(defparameter +names+
  '(("space" . "SPC") ("Space" . "SPC")
    ("Return" . "RET") ("Enter" . "RET") ("KP_Enter" . "RET")
    ("Tab" . "TAB") ("ISO_Left_Tab" . "TAB")
    ("BackSpace" . "DEL")
    ("Esc" . "Escape")
    ("Prior" . "PageUp") ("Next" . "PageDown"))
  "What a key is called here, whatever it was called where it came from.")
(defvar *pending* (d:box nil))
(defvar *last* (d:box nil))
(defvar *prefix* (d:box nil))
(defvar *on-insert* nil)
(defvar *taking* (d:box nil))

(defstruct (key (:constructor %make-key) (:copier nil))
  (sym "" :type string :read-only t)
  (ctrl nil :read-only t)
  (meta nil :read-only t)
  (shift nil :read-only t)
  (super nil :read-only t))

(defun named (sym)
  "One name for a key however it was spelled. A frontend says what xkb calls
it, a config says what emacs calls it, and C-SPC in a keymap has to be the key
xkb hands over as `space'."
  (if (< (length sym) 2)
      sym
      (or (cdr (assoc sym +names+ :test #'string-equal)) sym)))

(defun make-key (sym &key ctrl meta shift super)
  "The one key object for this chord. Two threads parsing the same chord get
the same object, which is what lets KEY= be EQ."
  (let* ((sym (named sym))
         (id (list sym ctrl meta shift super)))
    (d:claim *keys* id (%make-key :sym sym :ctrl ctrl :meta meta
                                  :shift shift :super super))))

(defun key= (a b) (eq a b))

(defun parse-key (spec)
  (let ((ctrl nil) (meta nil) (shift nil) (super nil)
        (i 0) (n (length spec)))
    (loop :while (and (< (1+ i) n) (char= (char spec (1+ i)) #\-))
          :do (case (char spec i)
                (#\C (setf ctrl t))
                (#\M (setf meta t))
                (#\S (setf shift t))
                (#\s (setf super t))
                (t (return)))
              (incf i 2))
    (make-key (subseq spec i) :ctrl ctrl :meta meta :shift shift :super super)))

(defun chord-text (keys)
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (k)
                    (with-output-to-string (s)
                      (when (key-ctrl k) (write-string "C-" s))
                      (when (key-meta k) (write-string "M-" s))
                      (when (key-shift k) (write-string "S-" s))
                      (when (key-super k) (write-string "s-" s))
                      (write-string (key-sym k) s)))
                  (alexandria:ensure-list keys))))

(defun parse-chord (spec)
  (mapcar #'parse-key
          (remove "" (uiop:split-string spec :separator '(#\Space))
                  :test #'string=)))

(defun self-insert-p (k)
  (and (= 1 (length (key-sym k)))
       (not (key-ctrl k)) (not (key-meta k)) (not (key-super k))
       (graphic-char-p (char (key-sym k) 0))))

(defun pending () (d:held *pending*))
(defun last () (d:held *last*))
(defun prefix () (d:held *prefix*))

(defun where (session)
  (let ((b (or session (buffer:current))))
    (when (and b (prompt:asking-p) (typep b 'buffer:buffer)
               (not (member "prompt" (buffer:minors-of b) :test #'equal)))
      (setf (buffer:minors-of b) (cons "prompt" (buffer:minors-of b))))
    (when (and b (not (prompt:asking-p)) (typep b 'buffer:buffer))
      (setf (buffer:minors-of b)
            (remove "prompt" (buffer:minors-of b) :test #'equal)))
    b))

(defun bindings-of (session)
  (loop :for m :in (mode:in-force session)
        :append (d:pairs (d:all (mode:keys m)))))

(defun %prefixp (session text)
  (loop :for (chord . nil) :in (bindings-of session)
        :thereis (and (> (length chord) (length text))
                      (string= text chord :end2 (length text))
                      (char= #\Space (char chord (length text))))))

(defun take-next (fn)
  "Hand the next key to FN instead of the keymap. FN answers :again to keep
taking them. This is what an incremental search is: a reader that re-installs
itself until something ends it."
  (d:put! *taking* fn))

(defun taking () (d:held *taking*))

(defun dispatch (session k)
  (let ((take (taking)))
    (when take
      (d:put! *taking* nil)
      (let ((said (fault:attempt (lambda () (funcall take k)) "a key")))
        (when (eq said :again) (d:put! *taking* take))
        (return-from dispatch (or said :taken)))))
  (%dispatch session k))

(defun %dispatch (session k)
  (let* ((session (where session))
         (so-far (append (pending) (list k)))
         (text (chord-text so-far))
         (found (mode:binding session text)))
    (cond ((mode:claimed session :key k)
           (d:put! *pending* nil)
           (d:put! *last* "the mode took it")
           :taken)
          (found
           (d:put! *pending* nil)
           (d:put! *last* (cmd:name found))
           (fault:attempt (lambda () (cmd:run found)) (cmd:name found)))
          ((%prefixp session text)
           (d:put! *pending* so-far)
           :pending)
          ((and (null (pending)) (self-insert-p k))
           (d:put! *last* "self-insert")
           (let ((b (buffer:current)))
             (unless (mode:claimed session :insert (key-sym k))
               (when b (buffer:insert! b (key-sym k))))
             (when *on-insert* (funcall *on-insert* k))
             :inserted))
          (t
           (d:put! *pending* nil)
           :unbound))))
