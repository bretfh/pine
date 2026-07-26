(defpackage #:pine.editor.help
  (:use #:cl)
  (:export #:%bindings-text #:%describe-key-text #:%mode-text #:%variables-text))

(in-package #:pine.editor.help)

;;;; Help / self-documentation. Help buffers are read-only layout buffers
;;;; (pine.editor.debugger:%text-layout via pine.editor.layout:show-layout); describe-key echoes.

(defun %describe-key-text (key)
  (let ((entry (pine.editor.command:key-binding (pine.editor.frame:current-client) key))
        (s (pine.editor.key:key->string key)))
    (cond ((consp entry) (format nil "~a is a prefix key" s))
          ((stringp entry) (format nil "~a runs the command ~a" s entry))
          ((pine.editor.command:self-insert-key-p key)
           (format nil "~a runs self-insert-command" s))
          (t (format nil "~a is undefined" s)))))

(defun %bindings-text ()
  (let* ((client (pine.editor.frame:current-client))
         (rows (loop for km in (pine.editor.frame:active-keymaps client)
                     append (pine.editor.keymap:keymap-bindings km t))))
    (with-output-to-string (out)
      (format out "Active bindings~%~%")
      (loop for (keys . cmd) in (sort (remove-duplicates rows :test #'equal
                                                         :key #'car :from-end t)
                                      #'string< :key #'car)
            do (format out "~16a  ~a~%" keys cmd)))))

(pine.state.var:defonce :tab-width :default 8
  :documentation "Tab stop width for the plain-text indent fallback.")

(pine.state.var:defonce :format-on-save :default nil
  :documentation "When non-nil, save-file reindents the whole buffer first.")

(pine.state.var:defonce :debug-on-error :default nil
  :documentation "When non-nil, an error in a command opens the *debugger*
restart menu instead of only echoing the message. Same knob as Emacs's
debug-on-error; edit-actor and eval errors always reach the debugger.")

(defun %variables-text ()
  (with-output-to-string (out)
    (format out "Editor variables~%~%")
    (dolist (name (pine.state.var:all-variable-names))
      (let ((v (pine.state.var:find-variable name))
            (buf (pine.editor.frame:buffer-in-scope)))
        (format out "~a = ~s [~(~a~)]~%    default ~s~a~%"
                name (pine.state.var:var name buf) (pine.state.var:variable-scope name buf)
                (pine.state.var:evar-default v)
                (let ((d (pine.state.var:evar-documentation v)))
                  (if (plusp (length d)) (format nil "~%    ~a" d) "")))))))

(defun %mode-text ()
  (let* ((client (pine.editor.frame:current-client))
         (major (pine.editor.frame:current-buffer-mode))
         (minors (pine.editor.frame:active-minor-modes client)))
    (with-output-to-string (out)
      (format out "Major mode: ~a (~a)~%"
              (pine.editor.mode:mode-name major) (pine.editor.mode:mode-indicator major))
      (loop for m = major then (pine.editor.mode:parent-mode m) while m
            do (format out "  ~a~%" (pine.editor.mode:mode-name m)))
      (let ((lang (and (typep major 'pine.editor.mode:major-mode)
                       (pine.editor.mode:ts-language major))))
        (when lang (format out "  tree-sitter language: ~a~%" lang)))
      (format out "~%Minor modes:~%")
      (if minors
          (dolist (m minors)
            (format out "  ~a (~a) precedence ~a~a~%"
                    (pine.editor.mode:mode-name m) (pine.editor.mode:mode-indicator m)
                    (pine.editor.mode:precedence m)
                    (if (pine.editor.mode:transparent m) " transparent" "")))
          (format out "  none~%")))))
