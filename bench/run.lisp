;;;; Load the substrate and run the hot-path benchmarks, emitting org tables on
;;;; stdout. Invoked by `make bench'. Correctness is `make test'.
(asdf:load-system :pine)
(handler-case
    (progn (asdf:load-system :pine/cairo)
           (push :pine-cairo *features*))
  (error (e) (format t "~&(pine/cairo unavailable: ~a)~%" e)))
(let ((here (or *load-pathname* *default-pathname-defaults*)))
  (load (merge-pathnames "bench.lisp" here)))
(funcall (read-from-string "pine.bench:run-all"))
(finish-output)
;; :ABORT skips the unwind. The benchmarks leave actor systems and a spawned
;; agent image standing on purpose, and unwinding out of --load is what printed
;; "compilation unit aborted" at the end of every run.
(sb-ext:exit :code 0 :abort t)
