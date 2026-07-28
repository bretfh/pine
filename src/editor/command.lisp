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
  (if (and (ignore-errors (pine.state.var:var :debug-on-error))
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

(defun %active-tables (client)
  "Every active keymap's tables in priority order: minor modes first, then
the major mode with its parent chain, then the global map."
  (loop for km in (pine.editor.frame:active-keymaps client)
        append (pine.editor.keymap:keymap-tables km)))

(defun %step (tables key)
  "One dispatch step: KEY against TABLES in priority order. The first entry
found decides -- a command fires, a prefix keeps reading -- and every
table's continuation for KEY stays live, so a mode's C-c prefix never
hides a global C-c chord. Returns (values command continuation-tables)."
  (let ((entries (loop for tbl in tables
                       for e = (gethash key tbl)
                       when e collect e)))
    (if (stringp (first entries))
        (values (first entries) nil)
        (values nil (remove-if-not #'hash-table-p entries)))))

(defun key-binding (client key)
  "KEY's binding in CLIENT's active keymaps: a command name string, a list
of prefix continuation tables, or nil."
  (multiple-value-bind (cmd conts) (%step (%active-tables client) key)
    (or cmd conts)))

(defun read-next-key (client fn)
  "Capture the next dispatched key and hand it to FN instead of running its
binding. One-shot. The basis for describe-key, quoted-insert, etc."
  (declare (ignore client))
  (setf (pine.cmd:said :reader) fn))

(defun %seq-string (pending key)
  "The chord typed so far as a string: PENDING's prefix (if any) plus KEY."
  (let ((s (pine.editor.key:key->string key)))
    (if pending (concatenate 'string (car pending) " " s) s)))

(defun dispatch (client key)
  "Feed one pine.editor.key:key. Pending state is (SEQ-STRING . TABLES): the chord
typed so far and the live continuation tables from every active keymap.
A key that dead-ends a chord echoes \"SEQ is undefined\" -- unless it is
bound to keyboard-quit at top level, which always escapes a chord."
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
             (tables (if pending (cdr pending) (%active-tables client))))
        (multiple-value-bind (cmd conts) (%step tables key)
          (cond
            (cmd
             (setf (pine.cmd:said :pending) nil)
             (call-command cmd))
            (conts
             (setf (pine.cmd:said :pending)
                   (cons (%seq-string pending key) conts)))
            (pending
             (setf (pine.cmd:said :pending) nil
                   (pine.cmd:prefix) nil)
             (let ((top (%step (%active-tables client) key)))
               (if (equal top "keyboard-quit")
                   (call-command top)
                   (pine.editor.echo:message
                    (format nil "~a is undefined" (%seq-string pending key))))))
            (t
             (if (self-insert-key-p key)
                 (call-command "self-insert-command")
                 ;; an unbound non-self-inserting key still terminates a prefix arg
                 (setf (pine.cmd:prefix) nil)))))))))
