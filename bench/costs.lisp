(require :asdf)
(asdf:load-system :pine/kernel)

(defpackage #:pine/bench/costs
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:place #:pine/kernel/place)
                    (#:graph #:pine/kernel/graph) (#:tell #:pine/kernel/tell)
                    (#:tree #:pine/kernel/tree) (#:watch #:pine/kernel/watch)
                    (#:log #:pine/kernel/log) (#:k #:pine/kernel/call)))
(in-package #:pine/bench/costs)

(defvar *runs* 300000)

(defun ns-each (n thunk)
  (let ((at (get-internal-real-time)))
    (dotimes (i n) (funcall thunk))
    (let ((secs (max 1d-6 (/ (- (get-internal-real-time) at)
                             (float internal-time-units-per-second)))))
      (values (/ (* secs 1e9) n) (/ n secs)))))

(defun say (what n thunk)
  (multiple-value-bind (ns per) (ns-each n thunk)
    (format t "~&~44@a ~8,0f ns  ~12:d /s~%" what ns (round per))))

(defun fresh () (setf tree:*root* (tree:make-root)) (tell:forget-all)
  (watch:forget-all))

(defun deep (n)
  "A chain N long: each one reads the one below it."
  (k:write "/n" 1)
  (let ((below "/n"))
    (dotimes (i n)
      (let ((it below) (name (format nil "/deep/~d" i)))
        (k:make name :derived (lambda () (1+ (k:read it))))
        (setf below name)))
    below))

(defun wide (n)
  (k:write "/n" 1)
  (dotimes (i n)
    (k:make (format nil "/wide/~d" i) :derived (lambda () (k:read "/n"))))
  (loop :for i :below n :collect (tree:reach (format nil "/wide/~d" i))))

(defun main ()
  (format t "~&~%what one thing costs, on one thread, with nothing else running~%~%")

  (fresh)
  (k:write "/x" 42)
  (say "read a place that holds a value" *runs* (lambda () (k:read "/x")))
  (say "write a place" *runs* (lambda () (k:write "/x" 42)))
  (say "swap a place" *runs* (lambda () (k:swap "/x" #'1+)))
  (say "reach a name three deep" *runs*
       (lambda () (tree:reach "/a/b/c")))

  (fresh)
  (k:write "/n" 1)
  (k:make "/twice" :derived (lambda () (* 2 (k:read "/n"))))
  (k:read "/twice")
  (say "read a worked-out place, nothing moved" *runs*
       (lambda () (k:read "/twice")))
  (say "write it, then read it (worked out again)" (floor *runs* 10)
       (lambda () (k:write "/n" 2) (k:read "/twice")))

  (fresh)
  (let ((last (deep 32)))
    (k:read last)
    (say "read down a chain of 32, nothing moved" (floor *runs* 10)
         (lambda () (k:read last)))
    (say "write the bottom, read the top of 32" (floor *runs* 100)
         (lambda () (k:write "/n" (random 100)) (k:read last))))

  (fresh)
  (let ((places (wide 1000)))
    (graph:all-worked places)
    (say "read one of a thousand, nothing moved" *runs*
         (lambda () (k:read "/wide/500")))
    (say "make a place" (floor *runs* 20)
         (lambda () (k:make "/made" :derived (lambda () (k:read "/n")))))
    (say "make and erase a place" (floor *runs* 20)
         (lambda () (k:make "/made" :derived (lambda () (k:read "/n")))
           (k:erase "/made"))))

  (format t "~&~%what a write costs as things listen to it~%~%")
  (dolist (n '(0 10 100 1000))
    (fresh)
    (watch:attend)
    (k:write "/n" 1)
    (dotimes (i n)
      (k:make (format nil "/w/~d" i) :derived (lambda () (k:read "/n")))
      (k:watch (format nil "/w/~d" i) (lambda (p v) (declare (ignore p v)))))
    (say (format nil "write, with ~d watching" n) (floor *runs* 30)
         (lambda () (k:write "/n" (random 100)))))

  (format t "~&~%what a write costs with the log on~%~%")
  (fresh)
  (let ((where "/tmp/pine-costs.log"))
    (ignore-errors (delete-file where))
    (log:keeping where)
    (say "write, written down" (floor *runs* 30)
         (lambda () (k:write "/x" (random 100))))
    (log:settled)
    (log:forget-keeping)
    (ignore-errors (delete-file where)))

  (format t "~&~%how big a namespace it will hold~%~%")
  (fresh)
  (let ((at (get-internal-real-time)))
    (dotimes (i 100000) (k:write (format nil "/big/~d/~d" (mod i 100) i) i))
    (format t "~&~44@a ~,2f s~%" "make 100,000 places"
            (/ (- (get-internal-real-time) at)
               (float internal-time-units-per-second))))
  (say "read one of a hundred thousand" *runs*
       (lambda () (k:read "/big/50/50050")))
  (sb-ext:gc :full t)
  (format t "~&~44@a ~,1f MB~%" "and what it weighs, once swept"
          (/ (sb-kernel:dynamic-usage) 1024.0 1024.0)))

(main)
(sb-ext:exit)
