;;;; make ts-probe
;;;;
;;;; Ask the tree-sitter binding the questions the parser asks it, many times,
;;;; and say what came back. A binding that answers a one-byte C bool through a
;;;; four-byte read is right most of the time and wrong the rest, which reads as
;;;; a buffer that sometimes has no colour rather than as a broken binding.

(require :asdf)
(asdf:load-system :pine)

(in-package :pine.ts.runtime)

(defun probe-claims (language times)
  "Claim LANGUAGE on a fresh parser TIMES over, and count the refusals."
  (let* ((runtime (make-ts-runtime))
         (entry (ensure-language runtime language)))
    (if (null entry)
        (format t "~&no ~(~a~) grammar at all~%" language)
        (let ((lang (entry-language-ptr entry))
              (refused 0))
          (format t "~&language pointer ~a abi ~a (speaks ~a..~a)~%" lang
                  (ignore-errors (ts-language-version lang))
                  (ts-min-compatible-version) (ts-language-version-max))
          (dotimes (i times)
            (let ((parser (ts-parser-new)))
              (unless (ts-parser-set-language parser lang) (incf refused))
              (ts-parser-delete parser)))
          (format t "~&~d of ~d claims refused~%" refused times)
          refused))))

(defun probe-parses (language times)
  "Parse the same text TIMES over on fresh parse states, and count the nulls."
  (let ((runtime (make-ts-runtime))
        (empty 0))
    (dotimes (i times)
      (let ((ps (make-parse-state runtime language)))
        (cond ((null ps) (incf empty))
              (t (parse-lines! ps (fset:seq "(defun f (x) x)"))
                 (unless (ps-tree ps) (incf empty))
                 (free-parse-state ps)))))
    (format t "~&~d of ~d parses answered no tree~%" empty times)
    empty))

(defun probe-threads (language threads)
  "Start THREADS parse states at once, the way several buffers opening at once
does, and count the ones that came back with nothing."
  (let* ((runtime (make-ts-runtime))
         (results (make-array threads :initial-element nil))
         (running (loop :for i :from 0 :below threads
                        :collect (let ((i i))
                                   (bordeaux-threads:make-thread
                                    (lambda ()
                                      (let ((ps (make-parse-state runtime language)))
                                        (when ps
                                          (parse-lines! ps (fset:seq "(defun f (x) x)")))
                                        (setf (aref results i)
                                              (and ps (ps-tree ps) t)))))))))
    (mapc #'bordeaux-threads:join-thread running)
    (let ((bad (count nil results)))
      (format t "~&~d of ~d parsers started at once came back with no tree~%"
              bad threads)
      bad)))

(let ((bad 0))
  (incf bad (or (probe-claims :commonlisp 200) 0))
  (incf bad (or (probe-parses :commonlisp 100) 0))
  (incf bad (or (probe-threads :commonlisp 16) 0))
  (sb-ext:exit :code (if (zerop bad) 0 1)))
