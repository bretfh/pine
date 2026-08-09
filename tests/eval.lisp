(in-package :pine.test)

(def-suite* :pine.eval :in :pine)

(defparameter +probe-source+
  (format nil "(in-package :cl-user)~%(defun probe (x) (list x))~%(probe 41)~%"))

(defmacro with-lisp-buffer ((&key (text +probe-source+)) &body body)
  `(unwind-protect
        (progn
          (pine:start)
          (setf (pine.fs.node:contents (pine.edit.buffer:current)) ,text)
          (pine.edit.buffer:goto! (pine.edit.buffer:current) 0 0)
          ,@body)
     (pine:stop)))

(test a-buffer-reads-in-the-package-it-declares
  (with-lisp-buffer ()
    (is (eq (find-package :cl-user)
            (pine.edit.eval:package-of (pine.edit.buffer:current))))))

(test the-image-says-where-a-symbol-is-defined
  (with-lisp-buffer ()
    (let ((found (pine.edit.eval:definition (pine.edit.buffer:current) "pine:start")))
      (is-true found "M-. has nothing to jump to")
      (is (search "boot.lisp" (first (first found))))
      (is (plusp (second (first found))) "and a line to land on"))))

(test the-image-says-what-a-call-takes-and-what-it-is-for
  (with-lisp-buffer ()
    (let ((b (pine.edit.buffer:current)))
      (is (equal "list (&rest args)" (pine.edit.eval:arglist b "list")))
      (is (search "Return the 1st object"
                  (pine.edit.eval:documentation b "car"))))))

(test completing-a-name-asks-the-package-the-buffer-is-in
  (with-lisp-buffer ()
    (let ((found (pine.edit.eval:complete (pine.edit.buffer:current) "list-all-pack")))
      (is (equal '("list-all-packages") found)))))

(test the-form-before-point-is-the-one-that-evaluates
  (with-lisp-buffer ()
    (let* ((b (pine.edit.buffer:current))
           (text (pine.edit.buffer:text-of b)))
      (pine.edit.buffer:goto! b 2 10)
      (multiple-value-bind (from to)
          (pine.edit.eval:sexp-before text (pine.edit.eval:offset-of b))
        (is (equal "(probe 41)" (subseq text from to)))))))

(test the-definition-point-is-in-is-the-whole-toplevel-form
  (with-lisp-buffer ()
    (let* ((b (pine.edit.buffer:current))
           (text (pine.edit.buffer:text-of b)))
      (pine.edit.buffer:goto! b 1 12)
      (multiple-value-bind (from to)
          (pine.edit.eval:defun-around text (pine.edit.eval:offset-of b))
        (is (equal "(defun probe (x) (list x))" (subseq text from to)))))))

(test evaluating-the-form-before-point-defines-it-in-the-image
  (with-lisp-buffer ()
    (let ((b (pine.edit.buffer:current)))
      (pine.edit.buffer:goto! b 1 26)
      (pine.repl.command:run "eval-last-expression")
      (is-true (fboundp (find-symbol "PROBE" :cl-user))
               "C-x C-e did not reach the image")
      (is (equal '(41) (funcall (find-symbol "PROBE" :cl-user) 41)))
      (unintern (find-symbol "PROBE" :cl-user) :cl-user))))

(test M-dot-jumps-and-M-comma-comes-back
  (with-lisp-buffer ()
    (let ((b (pine.edit.buffer:current)))
      (pine.edit.buffer:goto! b 1 8)
      (pine.edit.key:dispatch nil (pine.edit.key:parse-key "M-."))
      (is (not (eq b (pine.edit.buffer:current)))
          "M-. on a symbol pine defines opens the file it is defined in")
      (pine.edit.key:dispatch nil (pine.edit.key:parse-key "M-,"))
      (is (eq b (pine.edit.buffer:current)) "M-, came back")
      (is (equal '(1 8) (pine.edit.buffer:point b)) "to where point was"))))

(test the-lisp-answers-are-the-modes-and-not-this-files
  (with-lisp-buffer ()
    (let ((b (pine.edit.buffer:current)))
      (is-true (pine.repl.mode:handler b :definition))
      (is-true (pine.repl.mode:handler b :complete))
      (is (null (pine.repl.mode:handler (pine.repl.mode:mode-named "fundamental")
                                        :definition))
          "a mode that is not a lisp mode answers none of it"))))
