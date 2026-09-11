(require :asdf)
(asdf:load-system :pine/fs)

(defpackage #:pine/bench/blast
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:bt #:bordeaux-threads)))
(in-package #:pine/bench/blast)

(defun env (name default)
  (or (ignore-errors (parse-integer (uiop:getenv name))) default))

(defvar *seconds* (env "SECONDS" 20))
(defvar *writers* (env "WRITERS" 8))
(defvar *readers* (env "READERS" 8))
(defvar *churners* (env "CHURNERS" 4))
(defvar *width* (env "WIDTH" 64))
(defvar *layers* (env "LAYERS" 4))
(defvar *pause* (env "PAUSE" 0)
  "Microseconds a writer waits between writes. Zero is saturation, which no
machine does; a key is ten a second and a device tick is one.")

(defvar *stop* nil)
(defvar *wrong* (d:no-map))
(defvar *counts* (d:no-map))
(defvar *factor* nil)

(defun bad (what) (sb-ext:atomic-update *wrong* (lambda (m) (d:with m what (1+ (or (d:lookup m what) 0))))))
(defun did (what) (sb-ext:atomic-update *counts* (lambda (m) (d:with m what (1+ (or (d:lookup m what) 0))))))
(defun tally (what) (or (d:lookup *counts* what) 0))

(defun %at (name) (fs:at nil name))

(defun build ()
  "A graph whose whole answer is decided by one node.

One number is written. Everything in the first layer is that number, everything in
each layer above adds two below it, and the top adds all of them up. So the answer
is that number times a factor the shape decides, and the one thing that can be
checked without knowing which write won is whether the answer is that factor times
*something*. Anything else was added up half from one state and half from another.

One input on purpose. A graph with many would tear because writing several nodes is
writing several nodes, and nothing here claims otherwise. What is being asked is
whether the *graph* ever hands anybody a number that was never true."
  (fs:make-root)
  (fs:forget-listeners)
  (setf (fs:contents (fs:ensure nil "n")) 0)
  (let ((below (loop :for i :below *width*
                     :collect (let ((name (format nil "l0/~d" i)))
                                (fs:attach
                                 (make-instance 'fs:derived :name (format nil "~d" i) :recompute
                                              (lambda ()
                                                (fs:contents (%at "n"))))
                                 (fs:ensure nil "l0"))
                                name))))
    (loop :for layer :from 1 :below *layers*
          :do (setf below
                    (loop :for i :below *width*
                          :collect
                          (let* ((a (nth (mod (* 2 i) (length below)) below))
                                 (b (nth (mod (1+ (* 2 i)) (length below)) below))
                                 (name (format nil "l~d/~d" layer i)))
                            (fs:attach
                             (make-instance 'fs:derived :name (format nil "~d" i) :recompute
                                          (lambda ()
                                            (+ (fs:contents (%at a))
                                               (fs:contents (%at b)))))
                             (fs:ensure nil (format nil "l~d" layer)))
                            name))))
    (let ((top below))
      (fs:attach
       (make-instance 'fs:derived :name "all" :recompute (lambda ()
                            (loop :for each :in top
                                  :sum (fs:contents (%at each)))))
       (fs:root)))))

(defun factor ()
  (setf (fs:contents (%at "n")) 1)
  (fs:contents (%at "all")))

(defun writer (me)
  (lambda ()
    (loop :until *stop*
          :do (setf (fs:contents (%at "n")) (random 1000))
              (did :writes)
              (if (plusp *pause*)
                  (sleep (/ *pause* 1000000.0))
                  (when (zerop (mod me 3)) (bt:thread-yield))))))

(defun reader ()
  (lambda ()
    (loop :until *stop*
          :do (let ((all (fs:contents (%at "all"))))
                (did :reads)
                (unless (and (integerp all) (zerop (mod all *factor*)))
                  (bad :torn))))))

(defun churner (me)
  "Makes and erases nodes while everything else runs. The point is the edges: one
made and taken away while somebody is working it out must leave nothing behind in
anybody's reader set."
  (lambda ()
    (let ((n 0))
      (loop :until *stop*
            :do (let ((name (format nil "~d-~d" me (incf n))))
                  (handler-case
                      (let ((under (fs:ensure nil "churn")))
                        (fs:attach
                         (make-instance 'fs:derived :name name :recompute (lambda () (fs:contents (%at "n"))))
                         under)
                        (fs:contents (fs:child under name))
                        (fs:unlink under name)
                        (did :churn))
                    (error () (bad :churn-broke)))
                  (when (> n 100000) (setf n 0)))))))

(defun loose ()
  "Readers of /n that stand nowhere: a churner's node erased and left behind."
  (let ((n 0))
    (d:do-each (each (fs::dependents (%at "n")) n)
      (unless (fs:over each) (incf n)))))

(defun main ()
  (format t "~&~%blasting the namespace for ~d s~%" *seconds*)
  (format t "~&~d layers of ~d, ~d writers, ~d readers, ~d churners~%~%"
          *layers* *width* *writers* *readers* *churners*)
  (build)
  (setf *factor* (factor))
  (fs:contents (%at "all"))
  (let ((at (get-internal-real-time))
        (threads (append
                  (loop :for i :below *writers* :collect (bt:make-thread (writer i)))
                  (loop :repeat *readers* :collect (bt:make-thread (reader)))
                  (loop :for i :below *churners*
                        :collect (bt:make-thread (churner i))))))
    (sleep *seconds*)
    (setf *stop* t)
    (mapc #'bt:join-thread threads)
    (let ((took (/ (- (get-internal-real-time) at)
                   (float internal-time-units-per-second))))
      (format t "~&~14@a ~12:d ~10,1f /s~%" "writes" (tally :writes)
              (/ (tally :writes) took))
      (format t "~&~14@a ~12:d ~10,1f /s~%" "reads" (tally :reads)
              (/ (tally :reads) took))
      (format t "~&~14@a ~12:d ~10,1f /s~%" "made+erased" (tally :churn)
              (/ (tally :churn) took))
      (format t "~&~%")
      (let* ((all (fs:contents (%at "all")))
             (n (fs:contents (%at "n")))
             (left (loose))
             (broke (d:pairs *wrong*)))
        (format t "~&~34@a ~a~%" "every answer whole:"
                (if (null broke) "yes" (format nil "NO ~a" broke)))
        (format t "~&~34@a ~a~%" "settled to one state:"
                (if (eql all (* *factor* n)) "yes"
                    (format nil "NO ~a wanted ~a" all (* *factor* n))))
        (format t "~&~34@a ~a~%" "nothing left in a reader set:"
                (if (zerop left) "yes" (format nil "NO ~d" left)))
        (if (and (null broke) (eql all (* *factor* n)) (zerop left))
            (format t "~&~%the graph held.~%")
            (progn (format t "~&~%the graph did not hold.~%")
                   (sb-ext:exit :code 1)))))))

(main)
(sb-ext:exit)
