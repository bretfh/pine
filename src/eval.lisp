(defpackage #:pine.eval
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:b #:pine.text))
  (:shadow #:load)
  (:export #:target #:in-target #:form-string
           #:last-sexp #:defun-at-point #:buffer #:load
           #:definition #:complete-symbol #:arglist #:*on-done*
           #:target-was))

(in-package #:pine.eval)
(named-readtables:in-readtable pine.path:syntax)


(defvar *on-done* nil
  "Called with the evaluation and where it came from, or NIL.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(defun target-was ()
  "Where an eval went before a fault took the target. A path, so the value
survives the image that set it and a second frontend has its own."
  (or (ns:read /eval/target-was) :local))

(defun (setf target-was) (value)
  (ns:write /eval/target-was value))

(defun target ()
  (or (ns:read /eval/target) :local))

(defun (setf target) (value)
  (ns:write /eval/target value))

(defun in-target (str package &key on-done bindings)
  (let ((where (target)))
    (if (or (null where) (eq where :local))
        (if pine.core.actor:*local-agent*
            (pine.core.actor:agent-eval nil pine.core.actor:*local-agent* str
                                        :package package :bindings bindings
                                        :on-done on-done)
            (pine.err:evaluate-string str :package package
                                          :bindings bindings :on-done on-done))
        (pine.core.actor:agent-eval pine.core.server:*server* where str
                                    :package package :on-done on-done))))

(defun %current ()
  (let ((at (ns:read /buf/current)))
    (and at (p:leaf at))))

(defgeneric finished (status ev at)
  (:documentation "What an evaluation that ended in STATUS leaves behind. A
status pine does not ship says for itself what it shows.")
  (:method (status ev at) (declare (ignore status ev at)) nil))

(defmethod finished ((status (eql :ok)) ev at)
  (let ((text (format nil "=> ~{~s~^, ~}" (pine.err:evaluation-values ev))))
    (when at (ignore-errors (pine.buf:overlay (car at) (cdr at) text)))
    (pine.echo:message text)))

(defmethod finished ((status (eql :aborted)) ev at)
  (declare (ignore ev at))
  (pine.echo:message "aborted"))

(defun %done (ev at)
  "What a finished evaluation leaves behind: its value in the echo line, and
beside the form when AT is the (BUFFER . LINE) it came from."
  (finished (pine.err:evaluation-status ev) ev at)
  (when *on-done* (funcall *on-done* ev at)))

(defun form-string (str package &key at bindings)
  (in-target str package
             :bindings bindings
             :on-done (lambda (ev) (%done ev at))))

(defun %offset->lc (text offset)
  (let ((line 0) (col 0))
    (dotimes (i (min offset (length text)) (values line col))
      (if (char= (char text i) #\Newline)
          (setf line (1+ line) col 0)
          (incf col)))))

(defun %token-at (text offset)
  (let ((s (min offset (length text))) (e (min offset (length text))) (n (length text)))
    (loop :while (and (> s 0) (not (b:sexp-delimiter-p (char text (1- s))))) :do (decf s))
    (loop :while (and (< e n) (not (b:sexp-delimiter-p (char text e)))) :do (incf e))
    (when (< s e) (subseq text s e))))

(defun %token-start (text offset)
  (let ((s (min offset (length text))))
    (loop :while (and (> s 0) (not (b:sexp-delimiter-p (char text (1- s))))) :do (decf s))
    s))

(defun %grammar (name)
  (pine.mode:setting (ns:read (pine.buf:at name :mode)) :grammar))

(defun %runtime ()
  (and pine.core.server:*server*
       (pine.core.server:ts-runtime pine.core.server:*server*)))

(defun defun-at-point ()
  (let ((name (%current)))
    (when name
      (let* ((state (pine.buf:state name))
             (text (b:state->string state))
             (snap (b:state->snapshot state))
             (lang (%grammar name)))
        (if (null lang)
            (last-sexp)
            (multiple-value-bind (sl sc el ec)
                (pine.ts.runtime:defun-bounds-pos (%runtime) lang text
                                                  (b:point-line snap)
                                                  (b:point-col snap))
              (if sl
                  (form-string (subseq text (b:line-col->offset text sl sc)
                                       (b:line-col->offset text el ec))
                               (b:buffer-package state)
                               :at (cons name el))
                  (last-sexp))))))))

(defun last-sexp ()
  (let ((name (%current)))
    (when name
      (let* ((state (pine.buf:state name))
             (text (b:state->string state))
             (snap (b:state->snapshot state))
             (offset (min (b:point->offset snap) (length text))))
        (multiple-value-bind (start end) (b:preceding-sexp-bounds text offset)
          (if start
              (form-string (subseq text start end) (b:buffer-package state)
                           :at (cons name (%offset->lc text end)))
              (pine.echo:message "no form before point")))))))

(defun buffer ()
  (let ((name (%current)))
    (when name
      (let* ((state (pine.buf:state name))
             (text (b:state->string state))
             (package (b:buffer-package state))
             (thunk (lambda ()
                      (let ((*package* package) (pos 0) (count 0))
                        (loop
                          (multiple-value-bind (form new-pos)
                              (read-from-string text nil :eof :start pos)
                            (when (eq form :eof) (return count))
                            (cl:eval form)
                            (incf count)
                            (setf pos new-pos))))))
             (done (lambda (ev)
                     (pine.echo:message
                      (case (pine.err:evaluation-status ev)
                        (:ok (format nil "eval-buffer: ~a forms"
                                     (first (pine.err:evaluation-values ev))))
                        (:aborted "eval-buffer aborted")
                        (t "eval-buffer: error"))))))
        (if pine.core.actor:*local-agent*
            (pine.core.actor:agent-run nil pine.core.actor:*local-agent* thunk
                                       :package package :on-done done)
            (pine.err:evaluate-thunk thunk :package package :on-done done))))))

(defun load ()
  (let* ((name (%current))
         (file (and name (ns:read (pine.buf:at name :file)))))
    (if file
        (form-string (format nil "(load (compile-file ~s))" (namestring file))
                     (find-package :cl-user))
        (pine.echo:message "buffer has no file"))))

(defun %open (label srcs)
  (let* ((src (first srcs))
         (path (and src (sb-introspect:definition-source-pathname src)))
         (coff (and src (sb-introspect:definition-source-character-offset src))))
    (cond
      ((and path (probe-file path))
       (ns:write /buf/current (fset:seq :visit (namestring path)))
       (when coff
         (let ((name (%current)))
           (when name
             (let ((text (b:state->string (pine.buf:state name))))
               (multiple-value-bind (l c) (%offset->lc text coff)
                 (ns:write (pine.buf:at name :point) (fset:seq l c)))))))
       (pine.echo:message (format nil "~a" (file-namestring path))))
      (t (pine.echo:message (format nil "no source for ~a" label))))))

(defun definition ()
  (let ((name (%current)))
    (when name
      (let* ((state (pine.buf:state name))
             (text (b:state->string state))
             (snap (b:state->snapshot state))
             (off (min (b:point->offset snap) (length text)))
             (tok (%token-at text off))
             (pkg (b:buffer-package state)))
        (if (null tok)
            (pine.echo:message "no symbol at point")
            (let* ((sym (let ((*package* pkg)) (ignore-errors (read-from-string tok))))
                   (srcs (and (symbolp sym)
                              (loop :for kind :in '(:function :macro :generic-function
                                                    :variable :class)
                                    :thereis (ignore-errors
                                              (sb-introspect:find-definition-sources-by-name
                                               sym kind))))))
              (%open tok srcs)))))))

(defun %candidates (prefix pkg)
  (let ((up (string-upcase prefix)) (out nil))
    (do-symbols (s pkg)
      (let ((name (symbol-name s)))
        (when (and (>= (length name) (length up))
                   (string= up name :end2 (length up)))
          (pushnew (string-downcase name) out :test #'string=))))
    (sort out #'string<)))

(defun %replace-prefix (name prefix choice)
  (multiple-value-bind (line col) (pine.buf:point name)
    (ns:write (pine.buf:at name :text)
              (fset:seq :delete (fset:seq line (max 0 (- col (length prefix))))
                        (fset:seq line col))))
  (ns:write (pine.buf:at name :text) (fset:seq :insert choice)))

(defun complete-symbol ()
  (let ((name (%current)))
    (when name
      (let* ((state (pine.buf:state name))
             (text (b:state->string state))
             (snap (b:state->snapshot state))
             (off (min (b:point->offset snap) (length text)))
             (start (%token-start text off))
             (prefix (subseq text start off))
             (pkg (b:buffer-package state)))
        (unless (zerop (length prefix))
          (let ((cands (%candidates prefix pkg)))
            (cond
              ((null cands) (pine.echo:message "no completions"))
              ((null (rest cands)) (%replace-prefix name prefix (first cands)))
              (t (ns:write /echo
                           (fset:map
                            (:prompt "Complete: ")
                            (:complete cands)
                            (:then (lambda (choice)
                                     (%replace-prefix name prefix choice)))))))))))))

(defun arglist ()
  (let ((name (%current)))
    (when name
      (let* ((state (pine.buf:state name))
             (text (b:state->string state))
             (snap (b:state->snapshot state))
             (off (min (b:point->offset snap) (length text)))
             (tok (%token-at text off))
             (pkg (b:buffer-package state)))
        (when tok
          (let ((sym (let ((*package* pkg)) (ignore-errors (read-from-string tok)))))
            (if (and (symbolp sym) (fboundp sym))
                (pine.echo:message
                 (format nil "~a ~(~a~)" tok (sb-introspect:function-lambda-list sym)))
                (pine.echo:message (format nil "~a: not a function" tok)))))))))
