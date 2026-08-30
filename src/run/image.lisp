(defpackage #:pine/run/image
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:job #:pine/run/job) (#:node #:pine/fs/node)
                    (#:fault #:pine/run/fault) (#:actors #:pine/run/actors))
  (:export
   #:image #:child #:evaluate #:borrowing))
(in-package #:pine/run/image)

(defvar *sbcl* (namestring sb-ext:*runtime-pathname*))
(defvar *load-form* "(require :asdf)")
(defvar *ready* "pine-image-ready")
(defvar *said* "pine-image-said ")

(defparameter +loop+
  "(let ((*print-pretty* nil) (*print-circle* nil) (*print-readably* nil))
   (loop
     (let ((form (read *standard-input* nil :eof)))
       (when (eq form :eof) (sb-ext:exit))
       (format t \"~a~~s~~%\"
               (with-simple-restart (abort \"give up on this form\")
                 (handler-bind
                     ((error
                        (lambda (e)
                          (format t \"~a(:fault ~~s ~~s)~~%\"
                                  (princ-to-string e)
                                  (mapcar (lambda (r)
                                            (princ-to-string (restart-name r)))
                                          (remove nil (compute-restarts e)
                                                  :key (function restart-name))))
                          (force-output)
                          (let ((said (read *standard-input* nil :eof)))
                            (when (and (consp said) (eq :take (first said)))
                              (let ((r (find (second said) (compute-restarts e)
                                             :key (lambda (each)
                                                    (princ-to-string
                                                     (restart-name each)))
                                             :test (function equal))))
                                (when r (invoke-restart r))))))))
                   (eval form))))
       (force-output))))"
  "The child's read and eval loop. It stays inside HANDLER-BIND while it blocks on
READ, so the restarts it offers are the ones still there. Every line it means is
marked, because the systems it loads say things on the same stream.")

(defclass image (job:job) ()
  (:documentation "A lisp we can evaluate in. Where it is and how a form gets there
is the transport's business; that a fault in it stands rather than unwinding is
not."))

(defclass child (image job:program)
  ((systems :initarg :systems :accessor systems :initform '(:pine))
   (readyp  :initform nil :accessor readyp)
   (held    :initform nil :accessor held
            :documentation "The fault this child is standing in, or nothing. The
fault itself and not a flag, because whoever wants the pipe next has to wait for
it to be answered, and a fault is a thing you can wait on.")
   (turn    :initform (bordeaux-threads:make-lock "pine-image") :reader turn))
  (:documentation "An image started here, over pipes, that dies with pine.

One caller at a time holds the pipe: two threads writing forms down it interleave
and steal each other's replies. While the child stands in a fault the pipe belongs
to whoever will take a restart, because the next thing the child reads is which one
and a form written then would be swallowed as the answer."))

(job:kind :image
          (lambda (name said)
            (make-instance 'child :name name
                                  :on-fault (getf said :on-fault)
                                  :systems (or (getf said :systems) '(:pine)))))

(defgeneric evaluate (image form &key timeout)
  (:documentation "Do work in IMAGE and answer what it said:

  (values ANSWERED FAULT OFFERS SAID)

ANSWERED is what the form answered, as a list. FAULT is what broke, as text, and
OFFERS the restarts that image is still standing in."))

(defun borrowing (image said offers &key token)
  "Stand here in a fault that is standing there. Nobody has to be told: taking one
of its offers is TAKE, and TAKE on a borrowed fault is RESUME on the image.

The only thread this needs is one that gives up: a child blocked on READ waits for
an answer that may never come, so unattended the fault takes ABORT for it."
  (let ((f (fault:borrow image
                         (make-condition 'simple-error
                                         :format-control "~a: ~a"
                                         :format-arguments (list (job:name image) said))
                         offers
                         :token token
                         :label (format nil "in ~a" (job:name image)))))
    (actors:blocking
     (format nil "~a fault" (job:name image))
     (lambda ()
       (unless (fault:await f)
         (fault:or-nothing "the image it was standing in has gone"
           (fault:take f "ABORT")))))
    f))

(defun %argv (j)
  (list *sbcl* "--noinform" "--no-userinit" "--disable-debugger"
        "--eval" *load-form*
        "--eval" (format nil "(progn ~{(asdf:load-system ~s)~})" (systems j))
        "--eval" (format nil "(progn (princ ~s) (terpri) (force-output))" *ready*)
        "--eval" (format nil +loop+ *said* *said*)))

(defmethod initialize-instance :after ((j child) &key)
  (setf (job:argv j) (%argv j)))

(defmethod job:start ((j child))
  (let ((it (uiop:launch-program (job:argv j)
                                 :input :stream :output :stream
                                 :error-output :output)))
    (setf (job:took j) it)
    (wait-ready j)
    j))

(defun %in (j) (uiop:process-info-input (job:took j)))
(defun %out (j) (uiop:process-info-output (job:took j)))

(defun %line (j seconds)
  "The next line the child wrote, or nothing if it writes none in that long.

The descriptor is waited on, not the clock looked at. READ-LINE on a pipe blocks,
so a caller that wants a deadline cannot have one by reading and looking: what it
counted before was lines, not seconds, and a child that said nothing at all was
waited on for ever."
  (let ((stream (%out j)))
    (cond ((listen stream) (read-line stream nil nil))
          ((typep stream 'sb-sys:fd-stream)
           (when (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd stream)
                                              :input (max 0 seconds))
             (read-line stream nil nil)))
          (t (read-line stream nil nil)))))

(defun %until (j seconds sees)
  "Read what the child says until SEES answers something, or SECONDS run out. A
line it has nothing to say about is the child's own output and is emitted."
  (loop :with due := (+ (get-universal-time) seconds)
        :for line := (%line j (- due (get-universal-time)))
        :while line
        :do (let ((said (funcall sees line)))
              (when said (return said))
              (job:emit j line))
            (when (>= (get-universal-time) due) (return nil))))

(defun wait-ready (j &key (timeout 60))
  (and (%until j timeout (lambda (line) (search *ready* line)))
       (setf (readyp j) t)))

(defun saidp (line)
  (and line (>= (length line) (length *said*))
       (string= *said* line :end2 (length *said*))))

(defun answered (line)
  "What a marked line from the child says: a value, or a fault with the restarts
that image is still offering."
  (let* ((*read-eval* nil)
         (text (if (saidp line) (subseq line (length *said*)) line))
         (value (handler-case (read-from-string text) (error () text))))
    (if (and (consp value) (eq :fault (first value)))
        (values nil (second value) (third value))
        (values value nil nil))))

(defun %hear (j seconds)
  (%until j seconds (lambda (line) (and (saidp line) line))))

(defun %say (j form)
  (let ((in (%in j)))
    (write-string (prin1-to-string form) in)
    (terpri in)
    (force-output in)))

(defun %settle (j &optional (seconds fault:*waiting*))
  "Wait for whoever is standing in this child's fault to answer it. HELD is that
fault, so this waits on the fault rather than looking at a flag over and over."
  (let ((f (held j)))
    (when f (fault:await f seconds))))

(defmethod evaluate ((j child) form &key (timeout fault:*waiting*))
  (%settle j)
  (bordeaux-threads:with-lock-held ((turn j))
    (%say j form)
    (let ((line (%hear j timeout)))
      (when line
        (multiple-value-bind (value said offers) (answered line)
          (cond (said
                 (setf (held j) (borrowing j said offers))
                 (values nil said offers ""))
                (t (values (list value) nil nil ""))))))))

(defmethod fault:resume ((j child) f restart)
  (declare (ignore f))
  (bordeaux-threads:with-lock-held ((turn j))
    (unwind-protect
         (progn (%say j (list :take restart))
                (let ((line (%hear j fault:*waiting*)))
                  (when line (answered line))))
      (setf (held j) nil))))


(setf node:*elsewhere*
      (lambda (where form)
        "Work a node out in another image. A fault there is already standing here
with its restarts, because EVALUATE borrowed it; this signals so the node keeps what
it last worked out to rather than being written an answer nobody worked out."
        (let ((i (if (typep where 'image) where (job:named (princ-to-string where)))))
          (unless (typep i 'image)
            (error "~a is not an image to work anything out in." where))
          (multiple-value-bind (answered broke) (evaluate i form)
            (if broke
                (error "~a: ~a" (job:name i) broke)
                (first answered))))))
