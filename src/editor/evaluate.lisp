(defpackage #:pine.editor.evaluate
  (:use #:cl)
  (:export #:%eval-form-string #:complete-symbol #:eval-buffer #:eval-defun #:eval-last-sexp #:find-definition #:load-file #:symbol-arglist))

(in-package #:pine.editor.evaluate)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(defun %eval-form-string (str package &key at)
  ;; errors reach *on-debug* (local) or come home from a process agent via
  ;; agent-debug; the client binding rides along for :local. AT = (BUFFER .
  ;; LINE) puts the result inline on the form's line.
  (pine.editor.target:eval-in-target str package
                  :on-done (lambda (ev) (pine.editor.debugger:%eval-done ev at))
                  :bindings (list (cons 'pine.editor.frame:*client*
                                        (pine.editor.frame:current-client)))))

(defun eval-defun ()
  "Evaluate the top-level form point is inside (C-M-x), via the buffer's tree."
  (let ((buf (pine.editor.motion:cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.text.buffer:state->string state))
             (snap (pine.text.buffer:state->snapshot state))
             (lang (pine.editor.motion:%buffer-ts-lang)))
        (if (null lang)
            (eval-last-sexp)
            (multiple-value-bind (sl sc el ec)
                (pine.ts:defun-bounds-pos (pine.editor.motion:%ts-runtime) lang text
                                          (pine.text.buffer:point-line snap)
                                          (pine.text.buffer:point-col snap))
              (if sl
                  (%eval-form-string
                   (subseq text (pine.editor.motion:%lc->offset text sl sc) (pine.editor.motion:%lc->offset text el ec))
                   (pine.editor.motion:%buffer-package state)
                   :at (cons buf el))
                  (eval-last-sexp))))))))

(defun %offset->lc (text offset)
  (let ((line 0) (col 0))
    (dotimes (i (min offset (length text)) (values line col))
      (if (char= (char text i) #\Newline) (setf line (1+ line) col 0) (incf col)))))

(defun %token-at (text offset)
  "The symbol token surrounding OFFSET, bounded by sexp delimiters."
  (let ((s (min offset (length text))) (e (min offset (length text))) (n (length text)))
    (loop while (and (> s 0) (not (pine.editor.motion:%sexp-delim-p (char text (1- s))))) do (decf s))
    (loop while (and (< e n) (not (pine.editor.motion:%sexp-delim-p (char text e)))) do (incf e))
    (when (< s e) (subseq text s e))))

(defun %open-definition (label srcs)
  (let* ((src (first srcs))
         (path (and src (sb-introspect:definition-source-pathname src)))
         (coff (and src (sb-introspect:definition-source-character-offset src))))
    (cond
      ((and path (probe-file path))
       (pine.editor.file:find-file (namestring path))
       (when coff
         (let ((nbuf (pine.editor.motion:cur-buffer)))
           (when nbuf
             (let ((ntext (pine.text.buffer:state->string
                           (sento.actor:ask-s nbuf '(:get-state) :time-out 5))))
               (multiple-value-bind (l c) (%offset->lc ntext coff)
                 (sento.actor:tell nbuf (list :move-point :line l :col c)))))))
       (pine.editor.echo:message (format nil "~a" (file-namestring path))))
      (t (pine.editor.echo:message (format nil "no source for ~a" label))))))

(defun find-definition ()
  "Jump to the source of the symbol at point (M-.), via sb-introspect."
  (let ((buf (pine.editor.motion:cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.text.buffer:state->string state))
             (snap (pine.text.buffer:state->snapshot state))
             (off (min (pine.editor.motion:%point->offset snap) (length text)))
             (tok (%token-at text off))
             (pkg (pine.editor.motion:%buffer-package state)))
        (if (null tok)
            (pine.editor.echo:message "no symbol at point")
            (let* ((sym (let ((*package* pkg)) (ignore-errors (read-from-string tok))))
                   (srcs (and (symbolp sym)
                              (loop for kind in '(:function :macro :generic-function
                                                  :variable :class)
                                    thereis (ignore-errors
                                             (sb-introspect:find-definition-sources-by-name
                                              sym kind))))))
              (%open-definition tok srcs)))))))

(defun %token-start (text offset)
  (let ((s (min offset (length text))))
    (loop while (and (> s 0) (not (pine.editor.motion:%sexp-delim-p (char text (1- s))))) do (decf s))
    s))

(defun %symbol-candidates (prefix pkg)
  "Downcased names of symbols accessible in PKG that start with PREFIX."
  (let ((up (string-upcase prefix)) (out nil))
    (do-symbols (s pkg)
      (let ((name (symbol-name s)))
        (when (and (>= (length name) (length up))
                   (string= up name :end2 (length up)))
          (pushnew (string-downcase name) out :test #'string=))))
    (sort out #'string<)))

(defun complete-symbol ()
  "Complete the symbol at point against the buffer's package; Tab when there is
no symbol to complete."
  (let ((buf (pine.editor.motion:cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.text.buffer:state->string state))
             (snap (pine.text.buffer:state->snapshot state))
             (off (min (pine.editor.motion:%point->offset snap) (length text)))
             (start (%token-start text off))
             (prefix (subseq text start off))
             (pkg (pine.editor.motion:%buffer-package state)))
        (if (zerop (length prefix))
            (pine.editor.command:call-command "insert-tab")
            (let ((cands (%symbol-candidates prefix pkg)))
              (cond
                ((null cands) (pine.editor.echo:message "no completions"))
                ((null (rest cands)) (%replace-prefix buf prefix (first cands)))
                (t (pine.editor.minibuffer:completing-read "Complete: " cands
                     (lambda (choice) (%replace-prefix buf prefix choice)))))))))))

(defun %replace-prefix (buf prefix choice)
  (dotimes (i (length prefix)) (sento.actor:tell buf '(:backspace)))
  (pine.editor.ask:tell buf :insert :text choice))

(defun symbol-arglist ()
  "Echo the lambda list of the function named at point (M-x arglist)."
  (let ((buf (pine.editor.motion:cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.text.buffer:state->string state))
             (snap (pine.text.buffer:state->snapshot state))
             (off (min (pine.editor.motion:%point->offset snap) (length text)))
             (tok (%token-at text off))
             (pkg (pine.editor.motion:%buffer-package state)))
        (when tok
          (let ((sym (let ((*package* pkg)) (ignore-errors (read-from-string tok)))))
            (if (and (symbolp sym) (fboundp sym))
                (pine.editor.echo:message
                 (format nil "~a ~(~a~)" tok (sb-introspect:function-lambda-list sym)))
                (pine.editor.echo:message (format nil "~a: not a function" tok)))))))))

(defun load-file ()
  "Compile and load the current buffer's file into the eval target's image."
  (let* ((buf (pine.editor.motion:cur-buffer))
         (state (and buf (sento.actor:ask-s buf '(:get-state) :time-out 5)))
         (path (and state (pine.text.buffer:buffer-local state :pathname nil))))
    (if path
        (%eval-form-string (format nil "(load (compile-file ~s))" (namestring path))
                           (find-package :cl-user))
        (pine.editor.echo:message "buffer has no file"))))

(defun eval-last-sexp ()
  (let ((buf (pine.editor.motion:cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.text.buffer:state->string state))
             (snap (pine.text.buffer:state->snapshot state))
             (offset (min (pine.editor.motion:%point->offset snap) (length text))))
        (multiple-value-bind (start end) (pine.editor.motion:%preceding-sexp-bounds text offset)
          (if start
              (%eval-form-string (subseq text start end) (pine.editor.motion:%buffer-package state)
                                 :at (cons buf (%offset->lc text end)))
              (pine.editor.echo:message "no form before point")))))))

(defun eval-buffer ()
  ;; one evaluation on the eval thread reads and runs every form in order, so
  ;; *package* changes (in-package) carry across forms, a looping form can't
  ;; hang the editor, and a reader/eval error reaches the shared debugger
  ;; surface like every other eval path.
  (let ((buf (pine.editor.motion:cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.text.buffer:state->string state))
             (package (pine.editor.motion:%buffer-package state))
             (c (pine.editor.frame:current-client))
             (thunk (lambda ()
                      (let ((pine.editor.frame:*client* c) (*package* package)
                            (pos 0) (count 0))
                        (loop
                          (multiple-value-bind (form new-pos)
                              (read-from-string text nil :eof :start pos)
                            (when (eq form :eof) (return count))
                            (eval form) (incf count) (setf pos new-pos))))))
             (done (lambda (ev)
                     (pine.editor.debugger:%eval-notify
                      (case (pine.core.eval:evaluation-status ev)
                        (:ok (format nil "eval-buffer: ~a forms"
                                     (first (pine.core.eval:evaluation-values ev))))
                        (:aborted "eval-buffer aborted")
                        (t "eval-buffer: error"))))))
        ;; one eval path: route the whole-buffer eval through the local agent.
        (if pine.core.actor:*local-agent*
            (pine.core.actor:agent-run nil pine.core.actor:*local-agent* thunk
                                  :package package :on-done done)
            (pine.core.eval:evaluate-thunk thunk :package package :on-done done))))))
