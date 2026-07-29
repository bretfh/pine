(defpackage #:pine.editor.command
  (:use #:cl)
  (:export #:call-command #:dispatch #:command-error
           #:self-insert #:self-insert-key-p
           #:key-binding #:read-next-key
           #:*terminal-handler*))

(in-package #:pine.editor.command)

(defvar *terminal-handler* nil)

;;;; Keys, and what they run. A command is a path under /cmd; nothing here
;;;; holds one. What this has is the walk from a chord to a binding and the
;;;; bookkeeping a command loop needs: the chord typed so far, the prefix
;;;; argument, and what ran last.

(defun command-error (condition)
  "Surface an error from the interactive command/edit loop. With :debug-on-error
non-nil, route it through the same debugger surface evaluations use (*on-debug*
-> the *debugger* restart menu); otherwise show it in the echo area. Either way
the command loop lives on -- this never blocks the session thread. Call from a
handler-bind so the backtrace is captured while the stack is still live."
  (if (and (pine.ns:read (pine.path:parse "/debug-on-error"))
           pine.err:*on-debug*)
      (ignore-errors
       (funcall pine.err:*on-debug* (pine.err:make-error-evaluation condition)))
      (pine.editor.echo:message (format nil "error: ~a" condition))))

(defmacro %guarding-errors (&body body)
  "Run BODY; on an unhandled error surface it via %command-error and return NIL.
The surface runs inside the handler (stack live), then we unwind out of BODY."
  `(block %guarded
     (handler-bind ((error (lambda (c)
                             (command-error c)
                             (return-from %guarded nil))))
       ,@body)))

(defun call-command (name)
  "Run the command NAME, which is a path, a string or a symbol.

The prefix argument is cleared after, unless the command set one: C-u and the
digit arguments are commands like any other, and what makes them different is
that they wrote the prefix rather than used it."
  (let ((command (if (pine.path:pathp name) name (pine.cmd:at name)))
        (before (pine.cmd:prefix)))
    (when (pine.ns:read command)
      (%guarding-errors (pine.cmd:run command))
      (setf (pine.cmd:last) (pine.path:leaf command))
      (when (eq before (pine.cmd:prefix))
        (setf (pine.cmd:prefix) nil)))))

(defun self-insert-key-p (key)
  "True when KEY should insert its own character (printable, no C-/M-/super)."
  (and (= 1 (length (pine.editor.key:key-sym key)))
       (not (pine.editor.key:key-ctrl key))
       (not (pine.editor.key:key-meta key))
       (not (pine.editor.key:key-super key))))

(defun self-insert (client key)
  (when (and key (self-insert-key-p key))
    (let ((buf (pine.editor.frame:current-buffer client)))
      (when buf
        (dotimes (i (max 1 (pine.cmd:times)))
          (pine.text.buffer:edit buf (fset:seq :insert (pine.editor.key:key-sym key))))))))

(pine.cmd:defcmd "self-insert-command" ()
  (self-insert (pine.editor.frame:current-client) (pine.cmd:key)))

(defun %roots (client)
  "Where a key is looked up, in the order it is looked up."
  (pine.editor.keymap:roots (pine.editor.frame:current-buffer-mode)
                            (pine.editor.frame:active-minor-modes client)))

(defun %step (roots chord key)
  "One dispatch step: KEY after the chord so far, against ROOTS in order.

The first entry found decides -- a binding fires, a directory keeps reading --
and every root is asked at every step, so a mode's C-c prefix never hides a
global C-c chord. Answers (values binding prefix-p)."
  (let* ((so-far (append chord (list (pine.editor.key:key->string key))))
         (found (loop :for root :in roots
                      :for value = (pine.ns:read (apply #'pine.path:path root so-far))
                      :when value :collect value)))
    (let ((binding (find-if-not #'pine.editor.keymap:prefix-p found)))
      (if binding
          (values binding nil)
          (values nil (and found t))))))

(defun key-binding (client key)
  "KEY's binding in the maps this buffer is under: what it runs, T when it is
a prefix, or NIL."
  (multiple-value-bind (binding prefix) (%step (%roots client) nil key)
    (or binding prefix)))

(defun read-next-key (client fn)
  "Capture the next dispatched key and hand it to FN instead of running its
binding. One-shot. The basis for describe-key, quoted-insert, etc."
  (declare (ignore client))
  (setf (pine.cmd:said :reader) fn))

(defun %run (binding)
  "Do what a key is bound to, and keep the command bookkeeping a command loop
needs: what ran last, and clearing a prefix argument the command did not set."
  (if (pine.path:pathp binding)
      (call-command binding)
      (let ((before (pine.cmd:prefix)))
        (%guarding-errors (pine.cmd:run binding))
        (when (eq before (pine.cmd:prefix))
          (setf (pine.cmd:prefix) nil)))))

(defun dispatch (client key)
  "Feed one pine.editor.key:key. What is pending is the chord typed so far, as
the segments of the path a binding would be at. A key that dead-ends a chord
echoes \"SEQ is undefined\" -- unless it is bound to keyboard-quit at top
level, which always escapes a chord."
  (setf (pine.cmd:key) key)
  (let ((reader (pine.cmd:said :reader)))
    (when reader
      (setf (pine.cmd:said :reader) nil)
      (return-from dispatch (funcall reader key))))
  ;; in a terminal, keys go to the pty -- unless a prefix (C-x ...) is pending.
  (when (and *terminal-handler* (null (pine.cmd:said :pending))
             (funcall *terminal-handler* client key))
    (return-from dispatch))
  (%guarding-errors
    (handler-bind ((error (lambda (c) (declare (ignore c))
                            (setf (pine.cmd:said :pending) nil))))
      (let* ((pending (pine.cmd:said :pending))
             (roots (%roots client)))
        (multiple-value-bind (binding prefix) (%step roots pending key)
          (cond
            (binding
             (setf (pine.cmd:said :pending) nil)
             (%run binding))
            (prefix
             (setf (pine.cmd:said :pending)
                   (append pending (list (pine.editor.key:key->string key)))))
            (pending
             (setf (pine.cmd:said :pending) nil
                   (pine.cmd:prefix) nil)
             (let ((top (%step roots nil key)))
               (if (and (pine.path:pathp top)
                        (equal "keyboard-quit" (pine.path:leaf top)))
                   (%run top)
                   (pine.editor.echo:message
                    (format nil "~{~a~^ ~} is undefined"
                            (append pending
                                    (list (pine.editor.key:key->string key))))))))
            (t
             (if (self-insert-key-p key)
                 (call-command "self-insert-command")
                 ;; an unbound non-self-inserting key still terminates a prefix arg
                 (setf (pine.cmd:prefix) nil)))))))))
