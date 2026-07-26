(defpackage #:pine.editor.command
  (:use #:cl)
  (:export #:command #:command-name #:command-fn #:command-arguments #:command-prefix-p
           #:command-key
           #:define-command #:register-command #:find-command #:all-command-names
           #:execute #:call-command #:dispatch #:command-error
           #:self-insert #:self-insert-key-p
           #:prefix-numeric-value #:this-command-key
           #:key-binding #:read-next-key
           #:*terminal-handler*))

(in-package #:pine.editor.command)

(defvar *terminal-handler* nil)

(defclass command ()
  ((name      :initarg :name      :reader command-name)
   (fn        :initarg :fn        :reader command-fn)
   ;; interactive spec: a list of argument descriptors gathered before the fn
   ;; runs (:universal :number :region ...). nil = a 0-arg command.
   (arguments :initarg :arguments :reader command-arguments :initform nil)
   ;; prefix commands (C-u, digit-argument) set the prefix arg instead of
   ;; consuming it, so dispatch must not clear it after they run.
   (prefix-p  :initarg :prefix-p  :reader command-prefix-p  :initform nil)))

(defvar *commands* (make-hash-table :test 'equal))

(defun command-key (designator)
  "A command's registry name: a string as-is, a symbol downcased -- so
'greet and \"greet\" name the same command."
  (etypecase designator
    (string designator)
    (symbol (string-downcase (symbol-name designator)))))

(defun register-command (command)
  (setf (gethash (command-name command) *commands*) command))

(defun find-command (name)
  (etypecase name
    (command name)
    ((or string symbol) (gethash (command-key name) *commands*))))

(defun all-command-names ()
  (sort (loop for k being the hash-keys of *commands* collect k) #'string<))

(defmacro define-command (name (&rest lambda-list) &body body)
  "Define and register a command. NAME is a symbol (registered by its
downcased name) or a string. An optional interactive spec may lead the body
as (:interactive DESCRIPTOR...) or (:prefix); the descriptors gather the
LAMBDA-LIST arguments before the body runs."
  (let ((arguments nil) (prefix-p nil))
    (loop while (and (consp (first body)) (keywordp (car (first body))))
          for clause = (pop body)
          do (case (car clause)
               (:interactive (setf arguments (cdr clause)))
               (:prefix      (setf prefix-p t))
               (t (push clause body) (loop-finish))))
    `(register-command
      (make-instance 'command :name (command-key ',name)
                     :arguments ',arguments :prefix-p ,prefix-p
                     :fn (lambda ,lambda-list ,@body)))))

;;;; Prefix argument (C-u / digit-argument). Stored raw on the client:
;;;; nil = none, an integer = explicit, (4) = one bare C-u, (16) = C-u C-u.

(defun prefix-numeric-value (arg &optional (default 1))
  "The numeric value of a raw prefix ARG (Emacs prefix-numeric-value)."
  (cond ((null arg) default)
        ((integerp arg) arg)
        ((consp arg) (car arg))
        ((eq arg '-) -1)
        (t default)))

(defun this-command-key (client) (pine.client:this-command-key client))

(defun %region-bounds (client)
  "Region as (start-line start-col end-line end-col) from mark and point,
normalized so start precedes end. nil if no mark."
  (let ((buf (pine.client:current-buffer client)))
    (when buf
      (multiple-value-bind (sl sc el ec)
          (pine.buffer:region-bounds
           (sento.actor:ask-s buf '(:get-state) :time-out 5))
        (and sl (list sl sc el ec))))))

(defun gather-arguments (command client argument)
  "Turn COMMAND's interactive descriptors into the actual argument list,
using the raw prefix ARGUMENT and current client state."
  (loop for d in (command-arguments command)
        append (ecase d
                 (:universal (list argument))
                 (:number    (list (prefix-numeric-value argument)))
                 (:region    (or (%region-bounds client) (list nil nil nil nil))))))

(defgeneric execute (modes command argument)
  (:documentation "Run COMMAND. MODES is the active-modes instance so modes can
layer :before/:after/:around methods. ARGUMENT is the raw prefix argument.")
  (:method (modes command argument)
    (declare (ignore modes))
    (apply (command-fn command)
           (gather-arguments command (pine.client:current-client) argument))))

(defun command-error (condition)
  "Surface an error from the interactive command/edit loop. With :debug-on-error
non-nil, route it through the same debugger surface evaluations use (*on-debug*
-> the *debugger* restart menu); otherwise show it in the echo area. Either way
the command loop lives on -- this never blocks the session thread. Call from a
handler-bind so the backtrace is captured while the stack is still live."
  (if (and (ignore-errors (pine.state.var:var :debug-on-error))
           pine.core.eval:*on-debug*)
      (ignore-errors
       (funcall pine.core.eval:*on-debug* (pine.core.eval:make-error-evaluation condition)))
      (pine.echo:message (format nil "error: ~a" condition))))

(defmacro %guarding-errors (&body body)
  "Run BODY; on an unhandled error surface it via %command-error and return NIL.
The surface runs inside the handler (stack live), then we unwind out of BODY."
  `(block %guarded
     (handler-bind ((error (lambda (c)
                             (command-error c)
                             (return-from %guarded nil))))
       ,@body)))

(defun call-command (name-or-command)
  (let ((cmd (find-command name-or-command))
        (client (pine.client:current-client)))
    (when cmd
      (let ((arg (pine.client:prefix-arg client)))
        (%guarding-errors
          (execute (pine.client:active-modes-instance client) cmd arg))
        (setf (pine.client:last-command client) (command-name cmd))
        (unless (command-prefix-p cmd)
          (setf (pine.client:prefix-arg client) nil))))))

(defun self-insert-key-p (key)
  "True when KEY should insert its own character (printable, no C-/M-/super)."
  (and (= 1 (length (pine.editor.key:key-sym key)))
       (not (pine.editor.key:key-ctrl key))
       (not (pine.editor.key:key-meta key))
       (not (pine.editor.key:key-super key))))

(defun self-insert (client key)
  (when (and key (self-insert-key-p key))
    (let ((buf (pine.client:current-buffer client)))
      (when buf
        (let ((n (prefix-numeric-value (pine.client:prefix-arg client))))
          (dotimes (i (max 1 n))
            (sento.actor:tell buf (list :insert :text (pine.editor.key:key-sym key)))))))))

(register-command
 (make-instance 'command :name "self-insert-command"
                :fn (lambda ()
                      (let ((client (pine.client:current-client)))
                        (self-insert client (pine.client:this-command-key client))))))

(defun %active-tables (client)
  "Every active keymap's tables in priority order: minor modes first, then
the major mode with its parent chain, then the global map."
  (loop for km in (pine.client:active-keymaps client)
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
  (setf (pine.client:pending-key-reader client) fn))

(defun %seq-string (pending key)
  "The chord typed so far as a string: PENDING's prefix (if any) plus KEY."
  (let ((s (pine.editor.key:key->string key)))
    (if pending (concatenate 'string (car pending) " " s) s)))

(defun dispatch (client key)
  "Feed one pine.editor.key:key. Pending state is (SEQ-STRING . TABLES): the chord
typed so far and the live continuation tables from every active keymap.
A key that dead-ends a chord echoes \"SEQ is undefined\" -- unless it is
bound to keyboard-quit at top level, which always escapes a chord."
  (setf (pine.client:this-command-key client) key)
  (let ((reader (pine.client:pending-key-reader client)))
    (when reader
      (setf (pine.client:pending-key-reader client) nil)
      (return-from dispatch (funcall reader key))))
  ;; in a terminal, keys go to the pty -- unless a prefix (C-x ...) is pending.
  (when (and *terminal-handler* (null (pine.client:pending-keys client))
             (funcall *terminal-handler* client key))
    (return-from dispatch))
  (%guarding-errors
    (handler-bind ((error (lambda (c) (declare (ignore c))
                            (setf (pine.client:pending-keys client) nil))))
      (let* ((pending (pine.client:pending-keys client))
             (tables (if pending (cdr pending) (%active-tables client))))
        (multiple-value-bind (cmd conts) (%step tables key)
          (cond
            (cmd
             (setf (pine.client:pending-keys client) nil)
             (call-command cmd))
            (conts
             (setf (pine.client:pending-keys client)
                   (cons (%seq-string pending key) conts)))
            (pending
             (setf (pine.client:pending-keys client) nil
                   (pine.client:prefix-arg client) nil)
             (let ((top (%step (%active-tables client) key)))
               (if (equal top "keyboard-quit")
                   (call-command top)
                   (pine.echo:message
                    (format nil "~a is undefined" (%seq-string pending key))))))
            (t
             (if (self-insert-key-p key)
                 (call-command "self-insert-command")
                 ;; an unbound non-self-inserting key still terminates a prefix arg
                 (setf (pine.client:prefix-arg client) nil)))))))))
