;;;; Load the substrate, run the correctness probes, then the hot-path
;;;; benchmarks, emitting org tables on stdout. Invoked by `make bench`.
(asdf:load-system :pine)
(handler-case
    (progn (asdf:load-system :pine/cairo)
           (push :pine-cairo *features*))
  (error (e) (format t "~&(pine/cairo unavailable: ~a)~%" e)))
(let ((here (or *load-pathname* *default-pathname-defaults*)))
  (load (merge-pathnames "check.lisp" here))
  (unless (funcall (read-from-string "pine.check:run-checks"))
    (format t "~&refusing to benchmark on failing checks~%")
    (sb-ext:exit :code 1))
  (load (merge-pathnames "bench.lisp" here)))
(funcall (read-from-string "pine.bench:run-all"))
(finish-output)
(sb-ext:exit)
