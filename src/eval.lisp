(defpackage #:pine.eval
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:b #:pine.text))
  (:shadow #:load)
  (:export #:target #:in-target #:form-string
           #:last-sexp #:defun-at-point #:buffer #:load
           #:definition #:references #:hover #:complete-symbol #:arglist
           #:*on-done* #:target-was #:visit-place #:server))

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

(defun in-target (str package &key readtable on-done bindings)
  "Evaluate STR wherever /eval/target names. READTABLE is a named-readtable
name: it travels as a symbol, so a buffer written in a config's own reader
evaluates the same in this image and in one three hosts away."
  (let ((where (target)))
    (if (or (null where) (eq where :local))
        (if pine.core.actor:*local-agent*
            (pine.core.actor:agent-eval nil pine.core.actor:*local-agent* str
                                        :package package :readtable readtable
                                        :bindings bindings :on-done on-done)
            (pine.err:evaluate-string str :package package :readtable readtable
                                          :bindings bindings :on-done on-done))
        (pine.core.actor:agent-eval pine.core.server:*server* where str
                                    :package package :readtable readtable
                                    :on-done on-done))))

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

(defun form-string (str package &key at readtable bindings)
  (in-target str package
             :readtable readtable
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
                               :readtable (b:buffer-readtable-name state)
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
                           :readtable (b:buffer-readtable-name state)
                           :at (cons name (%offset->lc text end)))
              (pine.echo:message "no form before point")))))))

(defun buffer ()
  (let ((name (%current)))
    (when name
      (let* ((state (pine.buf:state name))
             (text (b:state->string state))
             (package (b:buffer-package state))
             (readtable (b:buffer-readtable state))
             (thunk (lambda ()
                      (let ((*package* package) (*readtable* readtable)
                            (pos 0) (count 0))
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

;;;; What the image knows, as producers. For lisp the running image already
;;;; holds what a language server would compute, and it holds it for whichever
;;;; image /eval/target names, so a definition inside an agent resolves over the
;;;; same remoting hop the debugger uses. Nothing outside this file asks
;;;; sb-introspect: a surface asks the buffer and the buffer asks the chain.

(defun %symbol-in (name of)
  "The symbol OF names, or the one at point in NAME, read in the buffer's own
package. (values SYMBOL TOKEN)."
  (let* ((state (pine.buf:state name))
         (pkg (b:buffer-package state))
         (rt (b:buffer-readtable state))
         (token (or of
                    (let* ((text (b:state->string state))
                           (snap (b:state->snapshot state))
                           (off (min (b:point->offset snap) (length text))))
                      (%token-at text off)))))
    (when token
      (values (let ((*package* pkg) (*readtable* rt))
                (ignore-errors (read-from-string token)))
              token))))

(defun %placed (src kind)
  "A definition source as (FILE LINE COL KIND), or NIL when it has no file."
  (let ((file (sb-introspect:definition-source-pathname src))
        (off (sb-introspect:definition-source-character-offset src)))
    (when (and file (probe-file file))
      (let ((text (pine.buf:read-file file)))
        (multiple-value-bind (line col)
            (if (and text off) (%offset->lc text off) (values 0 0))
          (list (namestring file) line col kind))))))

(defun %defined-at (sym)
  (loop :for kind :in '(:function :macro :generic-function :variable :class)
        :append (loop :for src :in (ignore-errors
                                    (sb-introspect:find-definition-sources-by-name
                                     sym kind))
                      :for placed = (%placed src kind)
                      :when placed :collect placed)))

(defun definition-producer (name of)
  (multiple-value-bind (sym) (%symbol-in name of)
    (when (symbolp sym) (%defined-at sym))))

(defun references-producer (name of)
  (multiple-value-bind (sym) (%symbol-in name of)
    (when (and sym (symbolp sym))
      (loop :for (nil . src) :in (ignore-errors (sb-introspect:who-calls sym))
            :for placed = (%placed src :caller)
            :when placed :collect placed))))

(defun complete-producer (name of)
  (let ((prefix (or of "")))
    (when (plusp (length prefix))
      (%candidates prefix (b:buffer-package (pine.buf:state name))))))

(defun arglist-producer (name of)
  (multiple-value-bind (sym token) (%symbol-in name of)
    (when (and sym (symbolp sym) (fboundp sym))
      (format nil "~a ~(~a~)" token (sb-introspect:function-lambda-list sym)))))

(defun hover-producer (name of)
  (multiple-value-bind (sym token) (%symbol-in name of)
    (when (and sym (symbolp sym))
      (let ((doc (or (documentation sym 'function) (documentation sym 'variable)))
            (args (arglist-producer name of)))
        (cond ((and args doc) (format nil "~a  --  ~a" args doc))
              (args args)
              (doc (format nil "~a: ~a" token doc)))))))

(defclass server (ns:server) ()
  (:default-initargs :name :eval :after (list :mode))
  (:documentation "What the running image can say about a lisp buffer, as the
producers lisp-mode answers with."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  (declare (ignore s))
  (ns:write /mode/lisp/answers
            {:definition (pine.data:fn [buf of] (definition-producer buf of))
             :references (pine.data:fn [buf of] (references-producer buf of))
             :complete   (pine.data:fn [buf of] (complete-producer buf of))
             :arglist    (pine.data:fn [buf of] (arglist-producer buf of))
             :hover      (pine.data:fn [buf of] (hover-producer buf of))})
  nil)

(ns:register (make-instance 'server))

(defun visit-place (place)
  "Open the (FILE LINE COL KIND) PLACE names in its own buffer and put point
there. FIND-FILE, because a jump opens the file it names: writing [:visit] to
the buffer that is current reads the file into that buffer instead."
  (destructuring-bind (file line col &optional kind) place
    (declare (ignore kind))
    (let ((name (pine.buf:find-file file)))
      (when name
        (ns:write (pine.buf:at name :point) (fset:seq line col))
        (pine.echo:message (format nil "~a:~d" (file-namestring file) (1+ line)))))))

(defun definition ()
  "Go to where what is at point is defined. What that means is the buffer's
modes' to say; this only takes the first answer and jumps."
  (let* ((name (%current))
         (found (and name (ns:read (pine.buf:at name :definition)))))
    (if found
        (visit-place (first found))
        (pine.echo:message "no definition"))))

(defun references ()
  "Every place that mentions what is at point, as the modes answer."
  (let* ((name (%current))
         (found (and name (ns:read (pine.buf:at name :references)))))
    (cond ((null found) (pine.echo:message "no references"))
          ((null (rest found)) (visit-place (first found)))
          (t (pine.echo:message
              (format nil "~d reference~:p: ~{~a~^, ~}" (length found)
                      (remove-duplicates (mapcar (lambda (p)
                                                   (file-namestring (first p)))
                                                 found)
                                         :test #'equal)))))))

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
             (prefix (subseq text start off)))
        (unless (zerop (length prefix))
          (let ((cands (ns:read (pine.buf:at name :complete prefix))))
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
  (let* ((name (%current))
         (said (and name (ns:read (pine.buf:at name :arglist)))))
    (pine.echo:message (or said "nothing at point takes arguments"))))

(defun hover ()
  (let* ((name (%current))
         (said (and name (ns:read (pine.buf:at name :hover)))))
    (pine.echo:message (or said ""))))
