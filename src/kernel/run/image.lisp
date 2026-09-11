(defpackage #:pine/run/image
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:job #:pine/run/job) (#:fs #:pine/fs)
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
       (force-output))))")

(defclass image (job:job) ())

(defclass child (image job:program)
  ((systems :initarg :systems :accessor systems :initform '(:pine))
   (readyp  :initform nil :accessor readyp)
   (held    :initform nil :accessor held)
   (turn    :initform (bordeaux-threads:make-lock "pine-image") :reader turn)))

(defmethod job:make-job ((kind (eql :image)) name said)
  (make-instance 'child :name name
                        :on-fault (getf said :on-fault :restart)
                        :systems (or (getf said :systems) '(:pine))))

(defgeneric evaluate (image form &key timeout))

(defun borrowing (image said offers &key token)
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
         (fault:or-nothing "the image it was suspended in has gone"
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
    (setf (job:handle j) it)
    (wait-ready j)
    j))

(defun %in (j) (uiop:process-info-input (job:handle j)))
(defun %out (j) (uiop:process-info-output (job:handle j)))

(defun %line (j seconds)
  (let ((stream (%out j)))
    (cond ((listen stream) (read-line stream nil nil))
          ((typep stream 'sb-sys:fd-stream)
           (when (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd stream)
                                              :input (max 0 seconds))
             (read-line stream nil nil)))
          (t (read-line stream nil nil)))))

(defun %until (j seconds sees)
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
  (let* ((*read-eval* nil)
         (text (if (saidp line) (subseq line (length *said*)) line))
         (value (handler-case (read-from-string text) (error () text))))
    (if (and (consp value) (eq :fault (first value)))
        (values nil (second value) (third value))
        (values value nil nil))))

(defun %hear (j seconds)
  (%until j seconds (lambda (line) (and (saidp line) line))))

(defun %drained (j)
  (loop :for line := (%line j 0)
        :while line
        :do (job:emit j line))
  j)

(defun %say (j form)
  (let ((in (%in j)))
    (write-string (prin1-to-string form) in)
    (terpri in)
    (force-output in)))

(defun %settle (j &optional (seconds fault:*unattended-seconds*))
  (let ((f (held j)))
    (when f (fault:await f seconds))))

(defmethod evaluate ((j child) form &key (timeout fault:*unattended-seconds*))
  (%settle j)
  (bordeaux-threads:with-lock-held ((turn j))
    (%say j form)
    (let ((line (%hear j timeout)))
      (multiple-value-prog1
          (when line
            (multiple-value-bind (value said offers) (answered line)
              (cond (said
                     (setf (held j) (borrowing j said offers))
                     (values nil said offers ""))
                    (t (values (list value) nil nil "")))))
        (unless (held j) (%drained j))))))

(defmethod fault:resume ((j child) f restart)
  (declare (ignore f))
  (bordeaux-threads:with-lock-held ((turn j))
    (unwind-protect
         (progn (%say j (list :take restart))
                (let ((line (%hear j fault:*unattended-seconds*)))
                  (when line (answered line))))
      (setf (held j) nil)
      (%drained j))))

(setf fs:*elsewhere*
      (lambda (where form)
        (let ((i (if (typep where 'image) where (job:named (princ-to-string where)))))
          (unless (typep i 'image)
            (error "~a is not an image to work anything out in." where))
          (multiple-value-bind (answered broke) (evaluate i form)
            (if broke
                (error "~a: ~a" (job:name i) broke)
                (first answered))))))
