;;;; Load the substrate, run the model tests, then the hot-path benchmarks,
;;;; emitting org tables on stdout. Invoked by `make bench`.
(asdf:load-system :pine/test)
(handler-case
    (progn (asdf:load-system :pine/cairo)
           (push :pine-cairo *features*))
  (error (e) (format t "~&(pine/cairo unavailable: ~a)~%" e)))
(unless (funcall (read-from-string "fiveam:results-status")
                 (funcall (read-from-string "fiveam:run") :pine.model))
  (format t "~&refusing to benchmark on failing tests~%")
  (sb-ext:exit :code 1))
(let ((here (or *load-pathname* *default-pathname-defaults*)))
  (load (merge-pathnames "bench.lisp" here)))
(funcall (read-from-string "pine.bench:run-all"))
(finish-output)
(sb-ext:exit)
