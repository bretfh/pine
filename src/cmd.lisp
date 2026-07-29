(defpackage #:pine.cmd
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:shadow #:last)
  (:export #:at #:run #:names #:defcmd #:mount
           #:times #:prefix #:last #:key #:said))

(in-package #:pine.cmd)
(named-readtables:in-readtable pine.path:syntax)

;;;; A command is a path holding a handler or a write-map. Writing it defines
;;;; it: M-x reads the directory, a key binds to it, a widget clicks it. There
;;;; is no command object, no registry and no interactive spec -- a command
;;;; that wants the prefix argument reads it, like anything else.

(defun at (name)
  "The path a command is named by. A symbol names the command its downcased
name does, so 'greet and \"greet\" are one command."
  (p:path /cmd (etypecase name
                 (string name)
                 (symbol (string-downcase (symbol-name name))))))

(defun names ()
  "Every command there is, which is what M-x reads."
  (sort (mapcar (lambda (path) (p:leaf path))
                (pine.data:keys (ns:read /cmd/* {})))
        #'string<))

(defun run (x)
  "Do what X says: a path is read and what is there is done, a map is written,
a function is called. This is what a key, a widget's click and M-x all reach."
  (cond ((null x) nil)
        ((p:pathp x) (run (ns:read x)))
        ((fset:map? x) (ns:write x))
        ((functionp x) (funcall x))
        ((or (stringp x) (symbolp x)) (run (at x)))
        (t (error "~s is not something to run." x))))

;;;; What the dispatch is carrying: the chord typed so far, the prefix
;;;; argument, the key that fired this command and the command before it. One
;;;; side of a conversation between the keyboard and the commands, so it is
;;;; here and not a place anyone addresses.

(defvar *said* (sento.atomic:make-atomic-reference :value (fset:empty-map)))

(defun said (key &optional default)
  (let ((value (fset:lookup (sento.atomic:atomic-get *said*) key)))
    (if (null value) default value)))

(defun (setf said) (value key)
  (sento.atomic:atomic-swap *said*
                            (lambda (m) (if (null value)
                                            (fset:less m key)
                                            (fset:with m key value))))
  value)

(defun prefix () (said :prefix))
(defun (setf prefix) (value) (setf (said :prefix) value))

(defun last () (said :last))
(defun (setf last) (value) (setf (said :last) value))

(defun key () (said :key))
(defun (setf key) (value) (setf (said :key) value))

(defun times (&optional (default 1))
  "How many times the prefix argument says to do it. C-u is four, C-u C-u is
sixteen, a typed number is itself, and a bare minus is -1."
  (let ((arg (prefix)))
    (cond ((null arg) default)
          ((integerp arg) arg)
          ((consp arg) (car arg))
          ((eq arg '-) -1)
          (t default))))

;;;; The commands pine ships. Each is written at mount, so a fresh namespace
;;;; has them and a second pine in this image gets its own.

(defvar *builtin* (fset:empty-map)
  "Name to handler, for what DEFCMD declared as this image loaded.")

(defmacro defcmd (name (&rest args) &body body)
  "Define the command NAME, named by a string or a symbol.

It is written now, which is what a config wants, and remembered so that MOUNT
writes it again into a namespace that has not seen it -- a fresh pine, or the
one a test holds."
  (declare (ignore args))
  `(let ((handler (lambda () ,@body)))
     (setf *builtin* (fset:with *builtin* (p:leaf (at ',name)) handler))
     (ns:write (at ',name) handler)))

(defun provider ()
  "What /cmd is. The clause says only what the path is for, so a command is
still an ordinary value someone wrote."
  (ns:provider
   (/cmd/?name {:doc "a handler, a write-map or a path to run"})
   (/cmd {:doc "every command there is, which is what M-x reads"})))

(defun mount ()
  (ns:write /cmd (provider))
  (fset:do-map (name handler *builtin*)
    (ns:write (at name) handler))
  nil)
