(defpackage #:pine.keymap
  (:use #:cl)
  (:export #:keymap #:keymap-p #:make-keymap #:keymap-name #:keymap-parent
           #:define-key #:keymap-lookup #:prefix-p))

(in-package #:pine.keymap)

(defstruct (keymap (:constructor %make-keymap) (:copier nil))
  (name nil)
  (parent nil)
  (table (make-hash-table :test 'eq) :read-only t))

(defun make-keymap (&key name parent) (%make-keymap :name name :parent parent))

(declaim (inline prefix-p))
(defun prefix-p (entry) (hash-table-p entry))

(defun define-key (keymap keys command)
  "KEYS is a pine.key:key or a list of them (a chord). COMMAND is a string name."
  (let ((table (keymap-table keymap))
        (keys (if (listp keys) keys (list keys))))
    (loop for (k . rest) on keys do
      (if rest
          (let ((next (gethash k table)))
            (unless (hash-table-p next)
              (setf next (make-hash-table :test 'eq)
                    (gethash k table) next))
            (setf table next))
          (setf (gethash k table) command)))
    command))

(defun keymap-lookup (keymap key)
  "Command name, prefix sub-table, or nil. Local bindings shadow the parent."
  (or (gethash key (keymap-table keymap))
      (let ((p (keymap-parent keymap)))
        (and p (keymap-lookup p key)))))
