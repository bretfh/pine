(defpackage #:pine/proc/elsewhere
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fault #:pine/run/fault)
                    (#:task #:pine/run/task))
  (:export #:evaluate #:resume-there #:came-home #:there #:standing #:*elsewhere*))
(in-package #:pine/proc/elsewhere)

(defvar *elsewhere* (d:box nil))

(defgeneric evaluate (image form &key timeout)
  (:documentation "Do work in IMAGE and answer what it said:

  (values ANSWERED FAULT OFFERS SAID)

ANSWERED is what the form answered, as a list. FAULT is what broke, as text,
and OFFERS the restarts that image is still standing in. SAID is what it
printed. How the form gets there is the transport's: a pipe to a child pine
started here, or remoting to one on another machine."))

(defgeneric resume-there (image name)
  (:documentation "Tell IMAGE to take the restart called NAME. The thread over
there is still standing in the fault, so this is the same act as taking one
here."))

(defun there (f)
  "The image a fault came home from."
  (cdr (assoc f (d:held *elsewhere*))))

(defun standing ()
  "Every fault that came home and is still waiting to be told what to do."
  (mapcar #'car (d:held *elsewhere*)))

(defun came-home (image said offers &optional (label "elsewhere"))
  "A fault in IMAGE joins this image's own, standing in what it offers there.
Whoever takes one of those -- the debugger buffer, or nobody, in which case it
gives up -- is what the thread over there is told."
  (let ((f (fault:elsewhere (make-condition 'simple-error
                                            :format-control "~a: ~a"
                                            :format-arguments (list label said))
                            offers
                            (format nil "in ~a" label))))
    (d:swap! *elsewhere*
             (lambda (all) (cons (cons f image) (remove f all :key #'car))))
    (task:spawn (format nil "~a fault" label)
                (lambda ()
                  (unwind-protect
                       (resume-there image (or (fault:await f) "ABORT"))
                    (d:swap! *elsewhere*
                             (lambda (all) (remove f all :key #'car))))))
    f))
