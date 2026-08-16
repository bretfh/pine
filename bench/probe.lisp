(require :asdf)
(asdf:load-system :pine/test)

(in-package :pine/test)

(defun probe-suite (name times)
  (let ((suite (intern (string-upcase name) :keyword))
        (failed nil))
    (dotimes (round times)
      (let* ((results (fiveam:run suite))
             (bad (remove-if-not (lambda (r) (typep r 'fiveam::test-failure)) results)))
        (format t "~&round ~d/~d: ~d check~:p, ~d failure~:p~%"
                (1+ round) times (length results) (length bad))
        (dolist (r bad)
          (push (1+ round) failed)
          (format t "  ~a: ~a~%"
                  (fiveam::name (fiveam::test-case r))
                  (fiveam::reason r)))
        (finish-output)))
    (format t "~&~:[every round was green~;rounds that failed: ~:*~{~d~^ ~}~]~%"
            (nreverse failed))
    (null failed)))

(let ((suite (or (uiop:getenv "SUITE") "pine/edit"))
      (times (parse-integer (or (uiop:getenv "TIMES") "5"))))
  (sb-ext:exit :code (if (probe-suite suite times) 0 1)))
