(require :asdf)

(defpackage #:pine/bench/gc
  (:use #:cl)
  (:export #:watching))
(in-package #:pine/bench/gc)

(defvar *pauses* nil "Every collection, as microseconds it took.")
(defvar *was* 0)
(defvar *consed* 0)
(defvar *started* 0)

(defun %pause ()
  (let ((now sb-ext:*gc-run-time*))
    (push (- now *was*) *pauses*)
    (setf *was* now)))

(defun %sorted () (sort (copy-list *pauses*) #'<))

(defun %at (sorted p)
  (if (null sorted)
      0
      (nth (min (1- (length sorted)) (floor (* p (length sorted)))) sorted)))

(defun %ms (us) (/ us 1000.0))

(defun report ()
  (let* ((wall (/ (- (get-internal-real-time) *started*)
                  (float internal-time-units-per-second)))
         (sorted (%sorted))
         (n (length sorted))
         (total (reduce #'+ sorted :initial-value 0))
         (consed (- (sb-ext:get-bytes-consed) *consed*)))
    (format t "~&~%what the collector did~%~%")
    (format t "~&~26a ~:d~%" "collections" n)
    (format t "~&~26a ~,3f s of ~,2f s wall (~,1f%)~%" "time collecting"
            (/ total 1000000.0) wall
            (if (plusp wall) (* 100 (/ (/ total 1000000.0) wall)) 0))
    (when (plusp n)
      (format t "~&~26a ~,3f ms~%" "mean pause" (%ms (/ total n)))
      (format t "~&~26a ~,3f ms~%" "p50 pause" (%ms (%at sorted 0.50)))
      (format t "~&~26a ~,3f ms~%" "p95 pause" (%ms (%at sorted 0.95)))
      (format t "~&~26a ~,3f ms~%" "p99 pause" (%ms (%at sorted 0.99)))
      (format t "~&~26a ~,3f ms~%" "worst pause" (%ms (car (last sorted))))
      (format t "~&~26a ~,1f /s~%" "collections per second"
              (if (plusp wall) (/ n wall) 0)))
    (format t "~&~26a ~:d MB~%" "allocated in all" (round consed 1048576))
    (format t "~&~26a ~:d MB/s~%" "allocation rate"
            (if (plusp wall) (round (/ consed 1048576) wall) 0))
    (format t "~&~%A pause is only felt where something is waiting on it. What~%~
                 decides whether one happens at all is what the hot path conses:~%~
                 a path that allocates nothing is never interrupted, however~%~
                 long a collection would have taken.~%")))

(defun watching ()
  (setf *was* sb-ext:*gc-run-time*
        *consed* (sb-ext:get-bytes-consed)
        *started* (get-internal-real-time)
        *pauses* nil)
  (push #'%pause sb-ext:*after-gc-hooks*)
  (push #'report sb-ext:*exit-hooks*)
  t)
