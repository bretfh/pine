;;;; make probe SUITE=pine.async TIMES=10
;;;;
;;;; Run one suite over and over in a single image and say which round failed.
;;;; A check that fails once in five is not a check you can read by running the
;;;; whole suite again: what matters is whether it fails on its own, and whether
;;;; it fails only after something else has run.
;;;;
;;;; One image, so what a round leaves behind the next round finds: a buffer a
;;;; test made in round one is still there in round two with the text round one
;;;; gave it. A round that fails where the one before it passed is saying that,
;;;; and it is a different fact from a round that fails on its own.

(require :asdf)
(asdf:load-system :pine/test)

(in-package :pine.test)

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

(let ((suite (or (uiop:getenv "SUITE") "pine.async"))
      (times (parse-integer (or (uiop:getenv "TIMES") "5"))))
  (sb-ext:exit :code (if (probe-suite suite times) 0 1)))
