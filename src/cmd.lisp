(defpackage #:pine.cmd
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:at #:run #:runnablep #:names #:defcmd #:server))

(in-package #:pine.cmd)
(named-readtables:in-readtable pine.path:syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

;;;; A command is a path holding a handler or a write-map. Writing it defines
;;;; it: M-x reads the directory, a key binds to it, a widget clicks it. There
;;;; is no command object and no interactive spec.

(defvar *builtin* (sento.atomic:make-atomic-reference :value (fset:empty-map))
  "Name to handler, for what DEFCMD declared as this image loaded. A registry:
what this image's code can do, not what any space holds.")

(defun at (name)
  "The path a command is named by. A symbol names the command its downcased
name does, so 'greet and \"greet\" are one command."
  (p:path /cmd (etypecase name
                 (string name)
                 (symbol (string-downcase (symbol-name name))))))

(defmacro defcmd (name (&rest args) &body body)
  "Define the command NAME. It is written now, and remembered so that RAISE
writes it again into a namespace that has not seen it."
  (declare (ignore args))
  `(let ((handler (lambda () ,@body))
         (key (p:leaf (at ',name))))
     (sento.atomic:atomic-swap *builtin*
                               (lambda (all) (fset:with all key handler)))
     (ns:write (at ',name) handler)))

(defun names ()
  "Every command there is, which is what M-x reads."
  (sort (mapcar (lambda (path) (p:leaf path))
                (pine.data:keys (ns:read /cmd/* {})))
        #'string<))

(defun runnablep (x)
  "Whether RUN knows what to do with X: a command path, a write-map, a map
carrying :RUN, a function, or a name."
  (and x (or (p:pathp x) (fset:map? x) (functionp x)
             (stringp x) (symbolp x))
       t))

(defun %here ()
  "The buffer a command is being run on, as a name, or NIL. Read off
/buf/current, so a command knows where it is without a client."
  (let ((at (ns:held /buf/current)))
    (cond ((p:pathp at) (p:leaf at))
          ((stringp at) at))))

(defun %takes-one-p (fn)
  "Whether FN will accept an argument."
  (let ((args (ignore-errors (sb-introspect:function-lambda-list fn))))
    (and (consp args)
         (or (not (member (first args) lambda-list-keywords))
             (member (first args) '(&optional &rest)))
         t)))

(defun %call (fn)
  "Call FN, with the buffer it is being run on when it takes one. The lambda
list is the declaration, and it is read rather than discovered by calling."
  (if (%takes-one-p fn)
      (funcall fn (%here))
      (funcall fn)))

(defun run (x)
  "Do what X says: a path is read and what is there is done, a map is written,
a function is called. A map carrying :RUN is what to do rather than what to
write."
  (cond ((null x) nil)
        ((p:pathp x) (run (ns:read x)))
        ((and (fset:map? x) (fset:lookup x :run)) (run (fset:lookup x :run)))
        ((fset:map? x) (ns:write x))
        ((functionp x) (%call x))
        ((or (stringp x) (symbolp x)) (run (at x)))
        (t (error "~s is not something to run." x))))

(defun provider ()
  (ns:provider
   (/cmd/?name/doc {:doc "what the command is for"})
   (/cmd/?name {:doc "a handler, a write-map or a path to run"})
   (/cmd {:doc "every command there is, which is what M-x reads"})))

(defclass server (ns:server) ()
  (:default-initargs :name :cmd :serves (list /cmd))
  (:documentation "Every command there is, which is what M-x reads."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  (ns:write /cmd (provider))
  (fset:do-map (name handler (sento.atomic:atomic-get *builtin*))
    (ns:write (at name) handler))
  nil)

(ns:register (make-instance 'server))
