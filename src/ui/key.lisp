(defpackage #:pine/ui/key
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:meter #:pine/run/meter))
  (:shadow #:last)
  (:export #:key #:make-key #:parse #:chord #:text #:key= #:selfp #:typed
           #:sym #:ctrl #:meta #:shift #:super #:keysym-name
           #:pending #:last #:taking #:take-next #:reading))
(in-package #:pine/ui/key)

(defvar *keys* (d:table))
(defvar *pending* nil)
(defvar *last* nil)
(defvar *taking* nil)

(defparameter +named+
  '(("space" . "SPC") ("Space" . "SPC")
    ("Return" . "RET") ("Enter" . "RET") ("KP_Enter" . "RET")
    ("Tab" . "TAB") ("ISO_Left_Tab" . "TAB")
    ("BackSpace" . "DEL")
    ("Esc" . "Escape")
    ("Prior" . "PageUp") ("Next" . "PageDown"))
  "What a key is called here, whatever it was called where it came from.")

(defparameter +as-keysym+
  '(("SPC" . "space") ("RET" . "Return") ("TAB" . "Tab")
    ("DEL" . "BackSpace") ("PageUp" . "Prior") ("PageDown" . "Next"))
  "And back again. A compositor asked for a chord has to be asked in xkb's
spelling, not this one.")

(defstruct (key (:constructor %key) (:copier nil))
  (sym "" :type string :read-only t)
  (ctrl nil :read-only t)
  (meta nil :read-only t)
  (shift nil :read-only t)
  (super nil :read-only t))

(defun %named (sym)
  "One name for a key however it was spelled. A keyboard says what xkb calls it, a
config says what emacs calls it, and C-SPC in a keymap has to be the key that
arrives as `space'."
  (if (< (length sym) 2)
      sym
      (or (cdr (assoc sym +named+ :test #'string-equal)) sym)))

(defun keysym-name (sym)
  "What xkb calls this key. %NAMED took xkb's spelling and made it pine's; this
takes it back, so a chord can be asked of a compositor in the spelling it knows."
  (or (cdr (assoc sym +as-keysym+ :test #'string=)) sym))

(defun make-key (sym &key ctrl meta shift super)
  "The one key for this chord. Two threads parsing the same chord get the same
object, which is what lets KEY= be EQ."
  (let* ((sym (%named sym))
         (id (list sym ctrl meta shift super)))
    (d:claim *keys* id (%key :sym sym :ctrl ctrl :meta meta :shift shift
                             :super super))))

(defun key= (a b) (eq a b))

(defun parse (spec)
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

(defun text (keys)
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (k)
                    (with-output-to-string (s)
                      (when (key-ctrl k) (write-string "C-" s))
                      (when (key-meta k) (write-string "M-" s))
                      (when (key-shift k) (write-string "S-" s))
                      (when (key-super k) (write-string "s-" s))
                      (write-string (key-sym k) s)))
                  (alexandria:ensure-list keys))))

(defun chord (spec)
  "The keys a chord names. Written down, a space is what separates one key from the
next, so a space on its own is the space key rather than nothing at all: /key is the
one door everything types through, and a door that swallows what it is given is
worse than one that asks for a name."
  (let ((keys (remove "" (uiop:split-string spec :separator '(#\Space))
                      :test #'string=)))
    (if (and (null keys) (plusp (length spec)))
        (list (parse "SPC"))
        (mapcar #'parse keys))))

(defun typed (k)
  "What this key types, or nothing where it types nothing. SPC is a key with a
name and a space is what it puts in: a chord written down has to mean the same
thing as the key that arrived."
  (unless (or (key-ctrl k) (key-meta k) (key-super k))
    (let ((sym (key-sym k)))
      (cond ((equal sym "SPC") " ")
            ((and (= 1 (length sym)) (graphic-char-p (char sym 0)))
             (if (key-shift k) (string-upcase sym) sym))))))

(defun selfp (k) (and (typed k) t))

(defun sym (k) (key-sym k))
(defun ctrl (k) (key-ctrl k))
(defun meta (k) (key-meta k))
(defun shift (k) (key-shift k))
(defun super (k) (key-super k))

(defun pending () *pending*)
(defun last () *last*)
(defun taking () *taking*)

(defun (setf pending) (value) (setf *pending* value) value)
(defun (setf last) (value) (setf *last* value) value)

(defun take-next (fn)
  "Hand the next key to FN instead of the keymap. FN answers :again to keep taking
them. An incremental search is exactly this: a reader that re-installs itself until
something ends it."
  (setf *taking* fn))

(defun reading (k)
  "Give K to whoever asked to read the next one, if anybody did."
  (let ((take (taking)))
    (when take
      (setf *taking* nil)
      (let ((said (funcall take k)))
        (when (eq said :again) (setf *taking* take))
        (or said :taken)))))
