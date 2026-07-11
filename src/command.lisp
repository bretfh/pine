(defpackage #:pine.command
  (:use #:cl)
  (:export #:command #:command-name #:command-fn
           #:define-command #:register-command #:find-command #:all-command-names
           #:execute #:call-command #:dispatch #:self-insert))

(in-package #:pine.command)

(defclass command ()
  ((name :initarg :name :reader command-name)
   (fn   :initarg :fn   :reader command-fn)))

(defvar *commands* (make-hash-table :test 'equal))

(defun register-command (command)
  (setf (gethash (command-name command) *commands*) command))

(defun find-command (name)
  (etypecase name
    (command name)
    (string  (gethash name *commands*))))

(defun all-command-names ()
  (sort (loop for k being the hash-keys of *commands* collect k) #'string<))

(defmacro define-command (name (&rest lambda-list) &body body)
  `(register-command (make-instance 'command :name ,name
                                    :fn (lambda ,lambda-list ,@body))))

(defgeneric execute (modes command argument)
  (:documentation "Run COMMAND. MODES is the active-modes instance so modes can
layer :before/:after/:around methods. ARGUMENT is the prefix argument.")
  (:method (modes command argument)
    (declare (ignore modes argument))
    (funcall (command-fn command))))

(defun call-command (name-or-command)
  (let ((cmd (find-command name-or-command))
        (client (pine.client:current-client)))
    (when cmd
      (handler-case
          (execute (pine.mode:active-modes-instance client) cmd nil)
        (error (c) (pine.echo:message (format nil "error: ~a" c))))
      (setf (pine.client:last-command client) (command-name cmd)))))

(defun self-insert (client key)
  (when (and (= 1 (length (pine.key:key-sym key)))
             (not (pine.key:key-ctrl key))
             (not (pine.key:key-meta key))
             (not (pine.key:key-super key)))
    (let ((buf (pine.client:current-buffer client)))
      (when buf (pine.buffer:tell buf :insert :text (pine.key:key-sym key))))))

(defun %resolve (client key)
  (loop for km in (pine.mode:active-keymaps client)
        for entry = (pine.keymap:keymap-lookup km key)
        when entry return entry))

(defun dispatch (client key)
  "Feed one pine.key:key: resolve via the pending prefix or the active keymaps."
  (handler-case
      (let* ((pending (pine.client:pending-keys client))
             (entry (if pending
                        (gethash key pending)
                        (%resolve client key))))
        (cond
          ((pine.keymap:prefix-p entry)
           (setf (pine.client:pending-keys client) entry))
          (entry
           (setf (pine.client:pending-keys client) nil)
           (call-command entry))
          (t
           (setf (pine.client:pending-keys client) nil)
           (self-insert client key))))
    (error (c)
      (setf (pine.client:pending-keys client) nil)
      (pine.echo:message (format nil "error: ~a" c)))))
