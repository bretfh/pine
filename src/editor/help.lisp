(defpackage #:pine.editor.help
  (:use #:cl)
  (:export #:bindings-text #:describe-key-text #:mode-text #:variables-text
           #:mount #:setting))

(in-package #:pine.editor.help)
(named-readtables:in-readtable pine.path:syntax)

;;;; Help / self-documentation. Help buffers are read-only layout buffers
;;;; (pine.editor.debugger:text-layout via pine.editor.view:show); describe-key echoes.

(defun describe-key-text (key)
  (let ((entry (pine.editor.command:key-binding (pine.editor.frame:current-client) key))
        (s (pine.editor.key:key->string key)))
    (cond ((consp entry) (format nil "~a is a prefix key" s))
          ((stringp entry) (format nil "~a runs the command ~a" s entry))
          ((pine.editor.command:self-insert-key-p key)
           (format nil "~a runs self-insert-command" s))
          (t (format nil "~a is undefined" s)))))

(defun bindings-text ()
  (let* ((client (pine.editor.frame:current-client))
         (rows (loop for root in (pine.editor.frame:active-keymaps client)
                     append (pine.editor.keymap:bindings
                             (pine.path:leaf root)))))
    (with-output-to-string (out)
      (format out "Active bindings~%~%")
      (loop for (keys . cmd) in (sort (remove-duplicates rows :test #'equal
                                                         :key #'car :from-end t)
                                      #'string< :key #'car)
            do (format out "~16a  ~a~%" keys cmd)))))

;;;; A setting is a path at the root, and a buffer that wants its own writes
;;;; the same leaf under itself: (write /tab-width 8) everywhere,
;;;; (write /buf/foo.py/tab-width 4) here. There is no declaration, no scope
;;;; table and no default slot, because the fallback is where the value is.

(defun mount ()
  "The settings pine ships. Written at mount so a fresh namespace has them and
a config that wrote its own keeps it."
  (dolist (setting '((/tab-width 8) (/format-on-save nil) (/debug-on-error nil)))
    (destructuring-bind (path value) setting
      (when (and value (null (pine.ns:read path)))
        (pine.ns:write path value)))))

(defun setting (name)
  "NAME's value for the buffer in scope, falling back to the root."
  (let ((buf (pine.editor.frame:buffer-in-scope)))
    (pine.ns:read (if buf (pine.buf:at buf name) (pine.path:path name)))))

(defun variables-text ()
  "Every setting there is: the leaves at the root that are not directories, and
what the buffer in scope reads for each."
  (with-output-to-string (out)
    (format out "Settings~%~%")
    (let ((root (pine.ns:read (pine.path:root) (fset:empty-map)))
          (buf (pine.editor.frame:buffer-in-scope)))
      (dolist (name (sort (let ((acc nil))
                            (fset:do-map (key value root)
                              (unless (fset:map? value)
                                (push (pine.path:name key) acc)))
                            acc)
                          #'string<))
        (let ((here (and buf (pine.ns:held (pine.buf:at buf name)))))
          (format out "~a = ~s [~a]~%" name
                  (if here here (pine.ns:read (pine.path:path name)))
                  (if here "this buffer" "everywhere")))))))

(defun mode-text ()
  (let* ((client (pine.editor.frame:current-client))
         (major (pine.editor.frame:current-buffer-mode))
         (minors (pine.editor.frame:active-minor-modes client)))
    (with-output-to-string (out)
      (format out "Major mode: ~a (~a)~%"
              major (or (pine.mode:setting major :indicator) ""))
      (dolist (m (pine.mode:chain major))
        (format out "  ~a~%" m))
      (let ((lang (pine.mode:setting major :grammar)))
        (when lang (format out "  tree-sitter language: ~a~%" lang)))
      (format out "~%Minor modes:~%")
      (if minors
          (dolist (m minors)
            (format out "  ~a (~a) precedence ~a~%"
                    m
                    (or (pine.ns:read (pine.path:path (pine.path:parse "/minor")
                                                      m :indicator))
                        "")
                    (or (pine.ns:read (pine.path:path (pine.path:parse "/minor")
                                                      m :precedence))
                        0)))
          (format out "  none~%")))))
