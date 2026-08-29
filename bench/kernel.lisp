(require :asdf)
(asdf:load-system :pine/kernel)

(defpackage #:pine/bench/kernel
  (:use #:cl)
  (:local-nicknames (#:place #:pine/kernel/place) (#:graph #:pine/kernel/graph)
                    (#:tree #:pine/kernel/tree) (#:pool #:pine/kernel/pool)
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

(defun main ()
  (format t "~&~d places worked out from one write, ~d rounds, ~d hands available~%"
          *wide* *rounds* (max 1 (1- (pool::cores))))
  (let* ((places (wide-graph *wide*))
         (alone (run "one thread" places)))
    (pool:start)
    (format t "~&pool of ~d~%" (pool:hands))
    (let ((many (run "the pool" places)))
      (pool:stop)
      (format t "~&~%scaled ~,2fx on ~d hands~%" (/ alone many) (max 1 (1- (pool::cores))))
      (let ((seen (mapcar #'place:held places)))
        (format t "~&every place worked out: ~a~%"
                (if (every #'integerp seen) "yes" "NO"))
        (format t "~&every place fresh:      ~a~%"
                (if (every #'graph:freshp places) "yes" "NO"))))))

(main)
(sb-ext:exit)
