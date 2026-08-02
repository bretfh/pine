(defpackage #:pine.editor.help
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path)
                    (#:frame #:pine.editor.frame) (#:b #:pine.ui.build))
  (:export #:describe-key-text #:bindings #:variables #:modes
           #:server #:setting))

(in-package #:pine.editor.help)
(named-readtables:in-readtable pine.path:syntax)

;;;; What pine can be asked about itself. Every one of these is a listing over
;;;; /key, /cmd, /mode or the root, so it is rows over paths rather than a
;;;; string built by hand, and it follows what it read.

(defun describe-key-text (key)
  (let ((entry (pine.key:key-binding key))
        (s (pine.key:key->string key)))
    (cond ((consp entry) (format nil "~a is a prefix key" s))
          ((stringp entry) (format nil "~a runs the command ~a" s entry))
          ((pine.key:self-insert-key-p key)
           (format nil "~a runs self-insert-command" s))
          (t (format nil "~a is undefined" s)))))

(defun %head (text) (b:label text :class "help-head"))
(defun %entry (text) (b:label text :class "help-entry"))

(defun bindings (&optional name)
  "Every binding in force, most specific map first."
  (declare (ignore name))
  (let ((rows (loop :for root :in (frame:active-keymaps)
                    :append (pine.key:bindings (p:leaf root)))))
    (apply #'b:column :align :stretch
           (%head "Active bindings")
           (loop :for (keys . cmd) :in (sort (remove-duplicates
                                              rows :test #'equal
                                                   :key #'car :from-end t)
                                             #'string< :key #'car)
                 :collect (%entry (format nil "~16a  ~a" keys cmd))))))

;;;; A setting is a path at the root, and a buffer that wants its own writes
;;;; the same leaf under itself: (write /tab-width 8) everywhere,
;;;; (write /buf/foo.py/tab-width 4) here. There is no declaration, no scope
;;;; table and no default slot, because the fallback is where the value is.

(defclass server (ns:server) ()
  (:default-initargs :name :settings
                     :serves (list /tab-width /format-on-save /debug-on-error
                                   /park-seconds /proc-interval
                                   /faults-kept /evaluations-kept))
  (:documentation "The settings pine ships, as ordinary paths."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  (dolist (setting '((/tab-width 8) (/format-on-save nil) (/debug-on-error nil)
                     (/park-seconds 300) (/proc-interval 1)
                     (/faults-kept 50) (/evaluations-kept 50)))
    (destructuring-bind (path value) setting
      (when (and value (null (ns:read path)))
        (ns:write path value)))))

(defun setting (name)
  "NAME's value for the buffer in scope, falling back to the root."
  (let ((buf (frame:buffer-in-scope)))
    (ns:read (if buf (pine.buf:at buf name) (p:path name)))))

(defun %settings ()
  (let ((root (ns:read (p:root) (fset:empty-map)))
        (acc nil))
    (fset:do-map (key value root)
      (unless (fset:map? value) (push (p:name key) acc)))
    (sort acc #'string<)))

(defun variables (&optional name)
  "Every setting there is, and what the buffer in scope reads for each."
  (declare (ignore name))
  (let ((buf (frame:buffer-in-scope)))
    (apply #'b:column :align :stretch
           (%head "Settings")
           (loop :for setting :in (%settings)
                 :for here := (and buf (ns:held (pine.buf:at buf setting)))
                 :collect (%entry (format nil "~a = ~s [~a]" setting
                                          (or here (ns:read (p:path setting)))
                                          (if here "this buffer" "everywhere")))))))

(defun modes (&optional name)
  "The mode this buffer is in, what it falls back to, and its minor modes."
  (declare (ignore name))
  (let ((major (frame:current-buffer-mode))
        (minors (frame:active-minor-modes)))
    (apply #'b:column :align :stretch
           (%head (format nil "Major mode: ~a (~a)" major
                          (or (pine.mode:setting major :indicator) "")))
           (append
            (loop :for m :in (pine.mode:chain major) :collect (%entry (format nil "  ~a" m)))
            (let ((lang (pine.mode:setting major :grammar)))
              (when lang
                (list (%entry (format nil "  tree-sitter language: ~a" lang)))))
            (list (%entry "") (%head "Minor modes"))
            (if minors
                (loop :for m :in minors
                      :collect (%entry
                                (format nil "  ~a (~a) precedence ~a" m
                                        (or (ns:read (p:path /minor m :indicator)) "")
                                        (or (ns:read (p:path /minor m :precedence)) 0))))
                (list (%entry "  none")))))))

(ns:register (make-instance 'server))
