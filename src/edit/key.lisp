(defpackage #:pine.edit.key
  (:use #:cl)
  (:shadow #:last)
  (:local-nicknames (#:c #:pine.run.cell) (#:cmd #:pine.repl.command)
                    (#:mode #:pine.repl.mode) (#:buffer #:pine.edit.buffer)
                    (#:fault #:pine.run.fault) (#:prompt #:pine.edit.prompt))
  (:export #:key #:make-key #:key-sym #:key-ctrl #:key-meta #:key-shift
           #:key-super #:key= #:parse-key #:parse-chord #:chord-text
           #:self-insert-p #:dispatch #:pending #:last #:prefix #:*on-insert*
           #:bindings-of #:where))

(in-package #:pine.edit.key)

(defvar *keys* (make-hash-table :test 'equal))
(defvar *pending* (c:cell nil))
(defvar *last* (c:cell nil))
(defvar *prefix* (c:cell nil))
(defvar *on-insert* nil)

(defstruct (key (:constructor %make-key) (:copier nil))
  (sym "" :type string :read-only t)
  (ctrl nil :read-only t)
  (meta nil :read-only t)
  (shift nil :read-only t)
  (super nil :read-only t))

(defun make-key (sym &key ctrl meta shift super)
  (let ((id (list sym ctrl meta shift super)))
    (or (gethash id *keys*)
        (setf (gethash id *keys*)
              (%make-key :sym sym :ctrl ctrl :meta meta
                         :shift shift :super super)))))

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

(defun pending () (c:held *pending*))
(defun last () (c:held *last*))
(defun prefix () (c:held *prefix*))

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
  (let (acc)
    (dolist (m (mode:in-force session) (nreverse acc))
      (maphash (lambda (chord name) (push (cons chord name) acc))
               (mode:keys m)))))

(defun %prefixp (session text)
  (loop :for (chord . nil) :in (bindings-of session)
        :thereis (and (> (length chord) (length text))
                      (string= text chord :end2 (length text))
                      (char= #\Space (char chord (length text))))))

(defun dispatch (session k)
  (let* ((session (where session))
         (so-far (append (pending) (list k)))
         (text (chord-text so-far))
         (found (mode:binding session text)))
    (cond (found
           (c:put *pending* nil)
           (c:put *last* (cmd:name found))
           (fault:attempt (lambda () (cmd:run found)) (cmd:name found)))
          ((%prefixp session text)
           (c:put *pending* so-far)
           :pending)
          ((and (null (pending)) (self-insert-p k))
           (c:put *last* "self-insert")
           (let ((b (buffer:current)))
             (unless (mode:claimed session :insert (key-sym k))
               (when b (buffer:insert! b (key-sym k))))
             (when *on-insert* (funcall *on-insert* k))
             :inserted))
          (t
           (c:put *pending* nil)
           :unbound))))
