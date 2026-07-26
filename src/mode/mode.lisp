(in-package #:pine.mode)

;;;; Mode classes. Modes are CLOS classes; the active modes of a buffer are
;;;; composed into one dispatch class (MOP) so command execution and behavior
;;;; layer minor -> major via method combination.

(defclass mode ()
  ((name      :initarg :name      :reader mode-name      :initform nil)
   (keymap    :initarg :keymap    :reader mode-keymap    :initform nil)
   (indicator :initarg :indicator :reader mode-indicator :initform "")))

(defclass major-mode (mode)
  ((parent-mode :initarg :parent-mode :reader parent-mode :initform nil)
   (ts-language :initarg :ts-language :reader ts-language  :initform nil)))

(defclass minor-mode (mode)
  ((precedence  :initarg :precedence  :reader precedence  :initform 0)
   (transparent :initarg :transparent :reader transparent :initform nil)))

(defclass base-mode (major-mode) ())
(defclass text-mode (base-mode) ())
(defclass lisp-mode (text-mode) ())
(defclass repl-mode (text-mode) ())
(defclass terminal-mode (base-mode) ())
(defclass debugger-mode (base-mode) ())

(defclass overwrite-mode (minor-mode) ())
(defclass minibuffer-mode (minor-mode) ())
(defclass layout-mode (minor-mode) ())

;;;; Registry (singletons keyed by keyword name) + global keymap

(defvar *modes* (make-hash-table :test 'eq))
(defvar *global-keymap* nil)

(defun register-mode (m) (setf (gethash (mode-name m) *modes*) m))
(defun find-mode (name) (gethash name *modes*))

(defun all-mode-names ()
  "Every registered mode's keyword name."
  (loop :for name :being :the :hash-keys :of *modes* :collect name))

(defun global-keymap () *global-keymap*)


(defvar *auto-modes* (make-hash-table :test 'equalp)
  "File extension (no dot) -> mode keyword, consulted by MODE-FOR-FILE before the
built-in mappings. Users add to it with AUTO-MODE from init.lisp.")

(defun auto-mode (ext mode)
  "Open files with extension EXT (a string, no dot) in MODE (a mode keyword).
Replaces any prior mapping, so reloading init.lisp is safe."
  (setf (gethash (string ext) *auto-modes*) mode))

(defun mode-for-file (path)
  (let ((ext (pathname-type (pathname path))))
    (cond ((null ext) nil)
          ((gethash ext *auto-modes*))
          ((member ext '("lisp" "cl" "asd" "asdf" "lsp") :test #'string-equal)
           :lisp-mode)
          (t nil))))

;;;; A set of active modes composed into one class, so command execution and
;;;; buffer behaviour layer minor -> major through method combination. Which
;;;; modes are active is a client's business (pine.client); building the class
;;;; from them is this layer's.

(defvar *dispatch-classes* (make-hash-table :test 'equal))

(defun modes-dispatch-class (classes)
  (let ((key (mapcar #'class-name classes)))
    (or (gethash key *dispatch-classes*)
        (setf (gethash key *dispatch-classes*)
              (c2mop:ensure-class (gensym "PINE-MODES")
                                  :direct-superclasses classes)))))

;;;; The buffer-behavior interface. The buffer actor's receive calls
;;;; (dispatch-message MODE SELF TAG PLIST); the verb methods live in
;;;; mode/edit.lisp, layered base -> text -> subclasses.

(defgeneric dispatch-message (mode self tag plist)
  (:documentation "Handle TAG (a keyword verb) with PLIST for the buffer actor
SELF, under MODE. Specialize on the mode class; call-next-method layers a
subclass over its parent."))

;;;; Defaults

(defun install-default-modes ()
  (setf *global-keymap* (pine.keymap:make-keymap :name :global))
  (let* ((base (register-mode
                (make-instance 'base-mode :name :base-mode :indicator "BASE"
                               :keymap (pine.keymap:make-keymap :name :base))))
         (text (register-mode
                (make-instance 'text-mode :name :text-mode :parent-mode base
                               :indicator "TEXT"
                               :keymap (pine.keymap:make-keymap
                                        :name :text :parent (mode-keymap base))))))
    (register-mode (make-instance 'lisp-mode :name :lisp-mode :parent-mode text
                                  :ts-language :commonlisp :indicator "LISP"
                                  :keymap (pine.keymap:make-keymap
                                           :name :lisp :parent (mode-keymap text))))
    (register-mode (make-instance 'repl-mode :name :repl-mode :parent-mode text
                                  :indicator "REPL"
                                  :keymap (pine.keymap:make-keymap
                                           :name :repl :parent (mode-keymap text))))
    (register-mode (make-instance 'terminal-mode :name :terminal-mode :parent-mode base
                                  :indicator "TERM"
                                  :keymap (pine.keymap:make-keymap
                                           :name :term :parent (mode-keymap base))))
    (register-mode (make-instance 'debugger-mode :name :debugger-mode :parent-mode base
                                  :indicator "DEBUG"
                                  :keymap (pine.keymap:make-keymap
                                           :name :debugger :parent (mode-keymap base))))
    (register-mode (make-instance 'overwrite-mode :name :overwrite-mode
                                  :precedence 10 :transparent t :indicator "Ovwrt"
                                  :keymap (pine.keymap:make-keymap :name :overwrite)))
    ;; the minibuffer's completion/exit keys; all other keys fall through to
    ;; text-mode, so the prompt has full editing
    (register-mode (make-instance 'minibuffer-mode :name :minibuffer-mode
                                  :precedence 20 :indicator ""
                                  :keymap (pine.keymap:make-keymap :name :minibuffer)))
    ;; layout buffers: selection nav + activation on the node tree
    (register-mode (make-instance 'layout-mode :name :layout-mode
                                  :precedence 15 :indicator ""
                                  :keymap (pine.keymap:make-keymap :name :layout)))
    base))

