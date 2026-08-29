(require :asdf)
(asdf:load-system :pine/kernel)

(defpackage #:pine/bench/blast
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:place #:pine/kernel/place)
                    (#:graph #:pine/kernel/graph) (#:tell #:pine/kernel/tell)
                    (#:tree #:pine/kernel/tree) (#:watch #:pine/kernel/watch)
                    (#:pool #:pine/kernel/pool) (#:log #:pine/kernel/log)
                    (#:k #:pine/kernel/call) (#:bt #:bordeaux-threads)))
(in-package #:pine/bench/blast)

(defun env (name default)
  (or (ignore-errors (parse-integer (uiop:getenv name))) default))

(defvar *seconds* (env "SECONDS" 20))
(defvar *writers* (env "WRITERS" 8))
(defvar *readers* (env "READERS" 8))
(defvar *churners* (env "CHURNERS" 4))
(defvar *watchers* (env "WATCHERS" 200))
(defvar *inputs* (env "INPUTS" 64))
(defvar *layers* (env "LAYERS" 4))
(defvar *width* (env "WIDTH" 64))

(defvar *stop* nil)
(defvar *wrong* (d:table))
(defvar *counts* (d:table))

(defun bad (what)
  (d:update! *wrong* what (lambda (n) (1+ (or n 0)))))

(defun did (what &optional (n 1))
  (d:update! *counts* what (lambda (had) (+ n (or had 0)))))

(defun tally (what) (or (d:lookup (d:all *counts*) what) 0))

(defun sum-to (n) (/ (* n (1- n)) 2))

(defun build ()
  "A graph whose whole answer is decided by one place.

One number is written. Everything in the first layer is twice it, everything in
each layer above adds two below and is twice again, and the top adds all of them
up. So the answer is one number times a factor the shape decides, and there is
exactly one thing that can be checked without knowing which write won: whether
the answer is that factor times *something*.

One input on purpose. A graph with many inputs would tear because writing
several places is writing several places -- holding the news back does not make
the memory change at once, and nothing here claims it does. What is being asked
here is a different question: whether the *graph* ever hands anybody a number
worked out half from one state and half from another."
  (setf tree:*root* (tree:make-root))
  (k:write "/n" 0)
  (let ((below (loop :for i :below *inputs*
                     :collect (let ((name (format nil "/l0/~d" i)))
                                (k:make name :derived (lambda () (k:read "/n")))
                                name))))
    (loop :for layer :from 1 :below *layers*
          :do (setf below
                    (loop :for i :below *width*
                          :collect (let ((a (nth (mod (* 2 i) (length below)) below))
                                         (b (nth (mod (1+ (* 2 i)) (length below))
                                                 below))
                                         (name (format nil "/l~d/~d" layer i)))
                                     (k:make name :derived
                                             (lambda () (+ (k:read a) (k:read b))))
                                     name))))
    (let ((top below))
      (k:make "/all" :derived
              (lambda () (loop :for each :in top :sum (k:read each))))))
  (values))

(defun factor ()
  (k:write "/n" 1)
  (k:read "/all"))

(defvar *factor* nil)

(defun writer (me)
  (lambda ()
    (loop :until *stop*
          :do (k:write "/n" (random 1000))
              (did :writes)
              (when (zerop (mod me 3)) (bt:thread-yield)))))

(defun grouper ()
  "One writer moving two places together, and one watcher told about the pair.

What holding the news claims, and what it does not. It claims that two writes are
one telling: whoever is watching hears once, not twice. It does not claim that
somebody reading at the same moment sees one state or the other -- writing two
places is writing two places, and the state in between is a state that really
stood. So the count of tellings is checked against the count of groups, and the
value is not, because both values are true ones."
  (k:write "/pair/a" 0)
  (k:write "/pair/b" 0)
  (k:make "/pair/same" :derived
          (lambda () (eql (k:read "/pair/a") (k:read "/pair/b"))))
  (k:watch "/pair/same"
           (lambda (p now) (declare (ignore p now)) (did :pairs))
           :when :always)
  (bt:make-thread
   (lambda ()
     (let ((n 0))
       (loop :until *stop*
             :do (incf n)
                 (did :groups)
                 (k:together (k:write "/pair/a" n) (k:write "/pair/b" n)))))))

(defun reader ()
  (lambda ()
    (loop :until *stop*
          :do (let ((all (k:read "/all")))
                (did :reads)
                (unless (and (integerp all) (zerop (mod all *factor*)))
                  (bad :torn))))))

(defun churner (me)
  "Makes and erases places while everything else is running.

The point is the edges. A place made and taken away while somebody is working it
out must leave nothing in anybody's reader set, and must not stop anything else
from being worked out."
  (lambda ()
    (let ((n 0))
      (loop :until *stop*
            :do (let ((name (format nil "/churn/~d/~d" me (incf n))))
                  (handler-case
                      (progn (k:make name :derived (lambda () (k:read "/n")))
                             (k:read name)
                             (k:erase name)
                             (did :churn))
                    (error () (bad :churn-broke)))
                  (when (> n 100000) (setf n 0)))))))

(defun watching ()
  (watch:attend)
  (dotimes (i *watchers*)
    (k:watch (format nil "/l~d/~d" (1- *layers*) (mod i *width*))
             (lambda (p now)
               (declare (ignore p))
               (did :told)
               (unless (integerp now) (bad :watcher-saw-rubbish))))))

(defun leaks ()
  "What is left in the reader sets that should not be.

/n is read by the first layer and by nothing else. A churner's place that was
erased and left behind shows up here as a reader that stands nowhere."
  (let ((loose 0))
    (d:do-each (each (place:readers (tree:reach "/n")) loose)
      (unless (place:under each) (incf loose)))))

(defun secs (from)
  (/ (- (get-internal-real-time) from) (float internal-time-units-per-second)))

(defun main ()
  (format t "~&~%blasting the kernel for ~d s~%" *seconds*)
  (format t "~&~d inputs, ~d layers of ~d, ~d watchers~%"
          *inputs* *layers* *width* *watchers*)
  (format t "~&~d writers, ~d readers, ~d churners, ~d hands~%~%"
          *writers* *readers* *churners* (max 1 (1- (pool::cores))))
  (build)
  (setf *factor* (factor))
  (pool:start)
  (log:keeping "/tmp/pine-blast.log")
  (watching)
  (grouper)
  (k:read "/all")
  (let ((at (get-internal-real-time))
        (threads (cl:append
                  (loop :for i :below *writers* :collect (bt:make-thread (writer i)))
                  (loop :repeat *readers* :collect (bt:make-thread (reader)))
                  (loop :for i :below *churners*
                        :collect (bt:make-thread (churner i))))))
    (sleep *seconds*)
    (setf *stop* t)
    (mapc #'bt:join-thread threads)
    (let ((took (secs at)))
      (log:settled)
      (format t "~&~10@a ~12:d  ~10,1f /s~%" "writes" (tally :writes)
              (/ (tally :writes) took))
      (format t "~&~10@a ~12:d  ~10,1f /s~%" "reads" (tally :reads)
              (/ (tally :reads) took))
      (format t "~&~10@a ~12:d  ~10,1f /s~%" "made+erased" (tally :churn)
              (/ (tally :churn) took))
      (format t "~&~10@a ~12:d  ~10,1f /s~%" "told" (tally :told)
              (/ (tally :told) took))
      (format t "~&~10@a ~12:d  ~10,1f /s~%" "groups" (tally :groups)
              (/ (tally :groups) took))
      (format t "~&~10@a ~12:d~%" "told of" (tally :pairs))
      (format t "~&~%")
      (let ((all (k:read "/all"))
            (n (k:read "/n"))
            (loose (leaks))
            (broke (d:pairs (d:all *wrong*))))
        (format t "~&~30@a ~a~%" "every answer whole:"
                (if (null broke) "yes" (format nil "NO ~a" broke)))
        (format t "~&~30@a ~a~%" "settled to one state:"
                (if (eql all (* *factor* n)) "yes"
                    (format nil "NO ~a wanted ~a" all (* *factor* n))))
        (format t "~&~30@a ~a~%" "two writes told as one:"
                (if (<= (tally :pairs) (tally :groups)) "yes"
                    (format nil "NO ~d tellings for ~d groups"
                            (tally :pairs) (tally :groups))))
        (format t "~&~30@a ~a~%" "nothing left in a reader set:"
                (if (zerop loose) "yes" (format nil "NO ~d" loose)))
        (format t "~&~30@a ~:d~%" "log entries:" (length (log:entries)))
        (format t "~&~30@a ~a~%" "log folds to what stands:"
                (let ((said (d:lookup (log:written) "/n")))
                  (if (eql said n) "yes" (format nil "NO ~a wanted ~a" said n))))
        (pool:stop)
        (log:forget-keeping)
        (ignore-errors (delete-file "/tmp/pine-blast.log"))
        (if (and (null broke) (eql all (* *factor* n)) (zerop loose)
                 (<= (tally :pairs) (tally :groups)))
            (format t "~&~%the kernel held.~%")
            (progn (format t "~&~%the kernel did not hold.~%") (sb-ext:exit :code 1)))))))

(main)
(sb-ext:exit)
