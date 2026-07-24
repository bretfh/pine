(defpackage :pine.test
  (:use :cl :fiveam)
  (:export #:run-tests))

(in-package :pine.test)

(def-suite :pine)

(defun run-tests ()
  "Run the whole pine suite; true when everything passed."
  (let ((results (run :pine)))
    (explain! results)
    (results-status results)))

(defvar *seed* 42)

(defun rnd (n)
  "Deterministic pseudo-random below N, so failures reproduce."
  (setf *seed* (mod (+ (* *seed* 1103515245) 12345) (expt 2 31)))
  (mod (floor *seed* 65536) (max 1 n)))
