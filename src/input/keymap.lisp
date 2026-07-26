(defpackage #:pine.editor.keymap
  (:use #:cl)
  (:export #:keymap #:keymap-p #:make-keymap #:keymap-name #:keymap-parent
           #:define-key #:define-keys #:keymap-lookup #:prefix-p
           #:keymap-tables #:keymap-bindings))

(in-package #:pine.editor.keymap)

(defstruct (keymap (:constructor %make-keymap) (:copier nil))
  (name nil)
  (parent nil)
  (table (make-hash-table :test 'eq) :read-only t))

(defun make-keymap (&key name parent) (%make-keymap :name name :parent parent))

(declaim (inline prefix-p))
(defun prefix-p (entry) (hash-table-p entry))

(defun define-key (keymap keys command)
  "KEYS is a pine.editor.key:key or a list of them (a chord). COMMAND is a command
name string. A keymap stores names and never resolves them, so it knows
nothing about the command registry and a binding may name a command that does
not exist yet."
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

(defgeneric keymap (object)
  (:documentation "OBJECT's keymap.")
  (:method ((k keymap)) k))

(defmacro define-keys (designator &body pairs)
  "Bind CHORD COMMAND pairs in the keymap DESIGNATOR names."
  (let ((map (gensym "MAP")))
    `(let ((,map (keymap ,designator)))
       ,@(loop :for (chord command) :on pairs :by #'cddr
               :collect `(define-key ,map (pine.editor.key:parse-chord ,chord) ,command))
       ,map)))

(defun keymap-lookup (keymap key)
  "Command name, prefix sub-table, or nil. Local bindings shadow the parent."
  (or (gethash key (keymap-table keymap))
      (let ((p (keymap-parent keymap)))
        (and p (keymap-lookup p key)))))

(defun keymap-tables (keymap)
  "The keymap's table and every ancestor's table, nearest first. Dispatch
resolves against tables, not keymaps, so a chord can continue in a parent
map that the child's own prefix table would otherwise shadow."
  (loop for m = keymap then (keymap-parent m)
        while m collect (keymap-table m)))

(defun keymap-bindings (keymap &optional include-parent)
  "List of (KEY-STRING . COMMAND) in KEYMAP. Chords render space-joined. With
INCLUDE-PARENT, unshadowed parent bindings are appended."
  (let ((acc '()))
    (labels ((walk (table prefix)
               (maphash
                (lambda (k v)
                  (let ((seq (if prefix
                                 (concatenate 'string prefix " " (pine.editor.key:key->string k))
                                 (pine.editor.key:key->string k))))
                    (if (hash-table-p v)
                        (walk v seq)
                        (push (cons seq v) acc))))
                table)))
      (walk (keymap-table keymap) nil))
    (when (and include-parent (keymap-parent keymap))
      (let ((local (mapcar #'car acc)))
        (dolist (pb (keymap-bindings (keymap-parent keymap) t))
          (unless (member (car pb) local :test #'string=)
            (push pb acc)))))
    acc))
