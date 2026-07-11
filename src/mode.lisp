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

;;;; Registry (singletons keyed by keyword name) + global keymap

(defvar *modes* (make-hash-table :test 'eq))
(defvar *global-keymap* nil)

(defun register-mode (m) (setf (gethash (mode-name m) *modes*) m))
(defun find-mode (name) (gethash name *modes*))
(defun global-keymap () *global-keymap*)

;;;; Buffer association

(defun buffer-mode (buffer-or-snap)
  (let ((name (pine.buffer:buffer-local buffer-or-snap :mode :base-mode)))
    (or (find-mode name) (find-mode :base-mode))))

(defun current-buffer-mode ()
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client))
         (name (and buf (gethash buf (pine.client:buffer-modes client)))))
    (or (and name (find-mode name)) (find-mode :base-mode))))

(defun set-buffer-mode (buffer-actor mode-name)
  (unless (find-mode mode-name) (error "No mode named ~s" mode-name))
  (setf (gethash buffer-actor (pine.client:buffer-modes (pine.client:current-client)))
        mode-name)
  (sento.actor:tell buffer-actor (list :set-local :key :mode :value mode-name))
  (find-mode mode-name))

(defun mode-for-file (path)
  (let ((ext (pathname-type (pathname path))))
    (cond ((null ext) nil)
          ((member ext '("lisp" "cl" "asd" "asdf" "lsp") :test #'string-equal)
           :lisp-mode)
          (t nil))))

;;;; Active modes -> keymaps + a synthesized dispatch class

(defun buffer-active-modes (client)
  (declare (ignore client))
  (list (current-buffer-mode)))

(defun active-keymaps (client)
  (declare (ignore client))
  (list (mode-keymap (current-buffer-mode)) *global-keymap*))

(defvar *dispatch-classes* (make-hash-table :test 'equal))

(defun modes-dispatch-class (classes)
  (let ((key (mapcar #'class-name classes)))
    (or (gethash key *dispatch-classes*)
        (setf (gethash key *dispatch-classes*)
              (c2mop:ensure-class (gensym "PINE-MODES")
                                  :direct-superclasses classes)))))

(defun active-modes-instance (client)
  (make-instance (modes-dispatch-class
                  (mapcar #'class-of (buffer-active-modes client)))))

;;;; Buffer behavior. The buffer actor's receive calls
;;;; (dispatch-message MODE SELF TAG PLIST). base-mode has the infrastructure
;;;; verbs; text-mode layers editing; subclasses override.

(defgeneric dispatch-message (mode self tag plist))

(defmethod dispatch-message ((mode base-mode) self tag plist)
  (declare (ignore self))
  (destructuring-bind (state undo subs hl) sento.actor:*state*
    (case tag
      (:get-state (sento.actor:reply state))
      (:get-snapshot (sento.actor:reply (pine.buffer:state->snapshot state)))
      (:get-text (sento.actor:reply (pine.buffer:state->string state)))
      (:get-local
       (sento.actor:reply
        (pine.buffer:buffer-local state (getf plist :key) (getf plist :default))))
      (:subscribe
       (let ((r (getf plist :renderer)))
         (setf sento.actor:*state* (list state undo (adjoin r subs :test #'eq) hl))
         (sento.actor:tell r
           (list :snapshot :snapshot (pine.buffer:state->snapshot-with-hl state hl)))))
      (:unsubscribe
       (let ((r (getf plist :renderer)))
         (setf sento.actor:*state* (list state undo (remove r subs :test #'eq) hl))))
      (:highlights
       (let ((h (getf plist :highlights)))
         (setf sento.actor:*state* (list state undo subs h))
         (pine.buffer:notify-subscribers subs state h)))
      (:undo
       (when undo
         (let ((prev (first undo)))
           (setf sento.actor:*state* (list prev (rest undo) subs hl))
           (pine.buffer:notify-subscribers subs prev hl))))
      ((:set-local :set-meta)
       (let ((new (pine.buffer:set-meta state (getf plist :key) (getf plist :value))))
         (setf sento.actor:*state* (list new undo subs hl))
         (pine.buffer:notify-subscribers subs new hl)))
      (:replace-content
       (let ((new (pine.buffer:set-meta
                   (pine.buffer:load-content (getf plist :content))
                   :name (or (fset:@ (pine.buffer:meta state) :name) ""))))
         (setf sento.actor:*state* (list new undo subs hl))
         (pine.buffer:notify-subscribers subs new hl))))))

(defmethod dispatch-message ((mode text-mode) self tag plist)
  (destructuring-bind (state undo subs hl) sento.actor:*state*
    (case tag
      (:insert
       (let* ((snap (pine.buffer:state->snapshot state))
              (l (pine.buffer:point-line snap))
              (c (pine.buffer:point-col snap))
              (new (pine.buffer:insert-string state l c (getf plist :text))))
         (setf sento.actor:*state* (list new (cons state undo) subs hl))
         (pine.buffer:notify-subscribers subs new hl)))
      (:newline
       (let* ((snap (pine.buffer:state->snapshot state))
              (l (pine.buffer:point-line snap))
              (c (pine.buffer:point-col snap))
              (new (pine.buffer:insert-newline state l c)))
         (setf sento.actor:*state* (list new (cons state undo) subs hl))
         (pine.buffer:notify-subscribers subs new hl)))
      (:backspace
       (let* ((snap (pine.buffer:state->snapshot state))
              (l (pine.buffer:point-line snap))
              (c (pine.buffer:point-col snap)))
         (cond
           ((plusp c)
            (let ((new (pine.buffer:move-mark
                        (pine.buffer:delete-char state l (1- c)) :point l (1- c))))
              (setf sento.actor:*state* (list new (cons state undo) subs hl))
              (pine.buffer:notify-subscribers subs new hl)))
           ((plusp l)
            (let* ((prev-len (length (fset:@ (pine.buffer:lines state) (1- l))))
                   (new (pine.buffer:move-mark
                         (pine.buffer:delete-char state (1- l) prev-len)
                         :point (1- l) prev-len)))
              (setf sento.actor:*state* (list new (cons state undo) subs hl))
              (pine.buffer:notify-subscribers subs new hl))))))
      (:move-point
       (let ((new (pine.buffer:move-mark state :point
                                         (getf plist :line) (getf plist :col))))
         (setf sento.actor:*state* (list new undo subs hl))
         (pine.buffer:notify-subscribers subs new hl)))
      (:delete-region
       (let ((new (pine.buffer:delete-region state
                                             (getf plist :start-line) (getf plist :start-col)
                                             (getf plist :end-line) (getf plist :end-col))))
         (setf sento.actor:*state* (list new (cons state undo) subs hl))
         (pine.buffer:notify-subscribers subs new hl)))
      (:append-with-prompt
       (let* ((text (getf plist :text)) (pr (getf plist :prompt))
              (snap (pine.buffer:state->snapshot state))
              (last-line (1- (pine.buffer:line-count snap)))
              (last-col (length (fset:@ (pine.buffer:lines state) last-line)))
              (s1 (pine.buffer:move-mark state :point last-line last-col))
              (s2 (pine.buffer:insert-newline s1 last-line last-col))
              (s3 (pine.buffer:insert-string s2 (1+ last-line) 0 text))
              (s4-snap (pine.buffer:state->snapshot s3))
              (s4-line (1- (pine.buffer:line-count s4-snap)))
              (s4-col (length (fset:@ (pine.buffer:lines s3) s4-line)))
              (s5 (pine.buffer:insert-newline s3 s4-line s4-col))
              (s6 (pine.buffer:insert-string s5 (1+ s4-line) 0 pr)))
         (setf sento.actor:*state* (list s6 (cons state undo) subs hl))
         (pine.buffer:notify-subscribers subs s6 hl)))
      (t (call-next-method)))))

(defmethod dispatch-message ((mode repl-mode) self tag plist)
  (declare (ignore self plist))
  (case tag
    (:newline (pine.shell:repl-submit))
    (t (call-next-method))))

(defmethod dispatch-message ((mode terminal-mode) self tag plist)
  (case tag
    (:insert (pine.term:term-write self (getf plist :text)))
    (:newline (pine.term:term-write self (string #\Newline)))
    (:backspace (pine.term:term-write self (string (code-char 127))))
    (:get-text
     (let ((term (pine.term:terminal-for-buffer self)))
       (sento.actor:reply (if term (pine.term:gterm-text term) ""))))
    ((:move-point :delete-region :undo :replace-content :append-with-prompt) nil)
    (t (call-next-method))))

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
    base))
