(require :asdf)
(asdf:load-system :pine/kernel)

(defpackage #:pine/bench/kernel
  (:use #:cl)
  (:local-nicknames (#:place #:pine/kernel/place) (#:graph #:pine/kernel/graph)
                    (#:tree #:pine/kernel/tree) (#:hands #:pine/run/hands)
                    (#:k #:pine/kernel/call)))
(in-package #:pine/bench/kernel)

(defvar *wide* (or (ignore-errors (parse-integer (uiop:getenv "WIDE"))) 64)
  "How many places are worked out from one write.")

(defvar *work* (or (ignore-errors (parse-integer (uiop:getenv "WORK"))) 200000)
  "How much work each one is. Real arithmetic, not a sleep: a sleep would show a
pool doing nothing faster than one thread doing nothing.")

(defvar *rounds* (or (ignore-errors (parse-integer (uiop:getenv "ROUNDS"))) 20))

(defun burn (n seed)
  (let ((x seed))
    (dotimes (i n x)
      (setf x (logand most-positive-fixnum (+ (* x 6364136223846793005) 1))))))

(defun wide-graph (n)
  (setf tree:*root* (tree:make-root))
  (k:write "/seed" 1)
  (dotimes (i n)
    (let ((i i))
      (k:make (format nil "/each/~d" i) :derived
              (lambda () (burn *work* (+ i (k:read "/seed")))))))
  (loop :for i :below n :collect (tree:reach (format nil "/each/~d" i))))

(defun ns () (get-internal-real-time))

(defun took (from)
  (/ (- (ns) from) (float internal-time-units-per-second)))

(defun round-of (places n)
  (k:write "/seed" n)
  (graph:all-worked places))

(defun run (label places)
  (round-of places 0)
  (let ((at (ns)))
    (dotimes (i *rounds*) (round-of places (1+ i)))
    (let ((secs (took at)))
      (format t "~&~12a ~6,3f s   ~8,1f rounds/s~%" label secs (/ *rounds* secs))
      secs)))

(defvar *workers*
  (or (ignore-errors (parse-integer (uiop:getenv "WORKERS")))
      (max 1 (1- (parse-integer (uiop:run-program '("nproc")
                                                  :output '(:string :stripped t)))))))

(defun main ()
  (format t "~&~d places worked out from one write, ~d rounds~%" *wide* *rounds*)
  (let* ((places (wide-graph *wide*))
         (alone (run "one thread" places))
         (sys (sento.actor-system:make-actor-system
               (list :dispatchers
                     (list :shared (list :workers 4 :strategy :random)
                           :work (list :workers *workers*
                                       :strategy (if (equal "rr" (uiop:getenv "STRAT"))
                                                     :round-robin :random)))))))
    (hands:take-up sys)
    (format t "~&sento, a dispatcher of its own, ~d workers~%" (hands:hands))
    (let ((many (run "the dispatcher" places)))
      (hands:let-go)
      (ignore-errors (sento.actor-context:shutdown sys :wait t))
      (format t "~&~%scaled ~,2fx on ~d workers~%" (/ alone many) *workers*)
      (let ((seen (mapcar #'place:held places)))
        (format t "~&every place worked out: ~a~%"
                (if (every #'integerp seen) "yes" "NO"))
        (format t "~&every place fresh:      ~a~%"
                (if (every #'graph:freshp places) "yes" "NO"))))))

(main)
(sb-ext:exit)
