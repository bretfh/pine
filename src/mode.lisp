(in-package :pine.mode)


;;;; Mode classes

(defclass base-mode ()
  ((name         :initarg :name         :accessor name         :initform :base-mode)
   (parent-mode  :initarg :parent-mode  :accessor parent-mode  :initform nil)
   (keymap       :initarg :keymap       :accessor keymap
                 :initform (make-hash-table :test 'equal))
   (ts-language  :initarg :ts-language  :accessor ts-language  :initform nil)
   (enter-hook   :initarg :enter-hook   :accessor enter-hook   :initform nil)
   (exit-hook    :initarg :exit-hook    :accessor exit-hook    :initform nil)
   (fallback     :initarg :fallback     :accessor fallback     :initform nil)
   (indicator    :initarg :indicator    :accessor indicator    :initform "BASE")))


;;;; Registry

(defun modes-table ()
  (pine.server:modes
   (pine.client:server-of (pine.client:current-client))))

(defun register-mode (m)
  (setf (gethash (name m) (modes-table)) m)
  m)

(defun find-mode (name)
  (gethash name (modes-table)))


;;;; defmode — classes derive, singletons register

(defmacro defmode (name &key parent keymap ts-language
                             enter-hook exit-hook fallback indicator)
  (let ((class-sym (intern (symbol-name name) :pine.mode))
        (parent-sym (if parent
                        (intern (symbol-name parent) :pine.mode)
                        'base-mode)))
    `(progn
       (unless (find-class ',class-sym nil)
         (defclass ,class-sym (,parent-sym) ()))
       (register-mode
        (make-instance ',class-sym
                       :name ,name
                       :parent-mode ,(if parent `(find-mode ,parent) nil)
                       ,@(when keymap `(:keymap ,keymap))
                       ,@(when ts-language `(:ts-language ,ts-language))
                       ,@(when enter-hook `(:enter-hook ,enter-hook))
                       ,@(when exit-hook `(:exit-hook ,exit-hook))
                       ,@(when fallback `(:fallback ,fallback))
                       :indicator ,(or indicator ""))))))


;;;; Derive-chain walks the parent-mode pointer

(defun derive-chain (mode)
  (loop for m = mode then (parent-mode m)
        while m
        collect m))

(defun mode-lookup (mode id)
  (loop for m in (derive-chain mode)
        for km = (keymap m)
        for hit = (when km (gethash id km))
        when hit return hit))

(defun mode-fallback (mode)
  (loop for m in (derive-chain mode)
        when (fallback m) return (fallback m)))


;;;; Buffer association

(defun buffer-mode (buffer-or-snap)
  (let ((name (pine.buffer:buffer-local buffer-or-snap :mode :base-mode)))
    (or (find-mode name) (find-mode :base-mode))))

(defun current-buffer-mode ()
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client))
         (name (and buf (gethash buf (pine.client:buffer-modes client)))))
    (or (and name (find-mode name))
        (find-mode :base-mode))))

(defun set-buffer-mode (buffer-actor mode-name)
  (let* ((client (pine.client:current-client))
         (cache (pine.client:buffer-modes client))
         (old-name (gethash buffer-actor cache))
         (old (and old-name (find-mode old-name)))
         (new (find-mode mode-name)))
    (unless new
      (error "No mode named ~s" mode-name))
    (when old
      (let ((hook (exit-hook old)))
        (when hook (handler-case (funcall hook buffer-actor)
                     (error () nil)))))
    (setf (gethash buffer-actor cache) mode-name)
    (sento.actor:tell buffer-actor
                      (list :set-local :key :mode :value mode-name))
    (loop for m in (reverse (derive-chain new))
          for hook = (enter-hook m)
          when hook
            do (handler-case (funcall hook buffer-actor)
                 (error () nil)))
    new))


;;;; Message dispatch
;;;;
;;;; A buffer actor's receive calls (dispatch-message MODE SELF TAG PLIST).
;;;; Methods touch sento.actor:*state* directly for state updates and call
;;;; sento.actor:reply for queries. base-mode handles the infrastructure
;;;; verbs; text-mode layers on the editing verbs; mode-specific subclasses
;;;; override what they need.

(defgeneric dispatch-message (mode self tag plist))

(defmethod dispatch-message ((mode base-mode) self tag plist)
  (declare (ignore self))
  (destructuring-bind (state undo subs hl) sento.actor:*state*
    (case tag
      (:get-state
       (sento.actor:reply state))

      (:get-snapshot
       (sento.actor:reply (pine.buffer:state->snapshot state)))

      (:get-text
       (sento.actor:reply (pine.buffer:state->string state)))

      (:get-local
       (sento.actor:reply
        (pine.buffer:buffer-local state (getf plist :key) (getf plist :default))))

      (:subscribe
       (let ((r (getf plist :renderer)))
         (setf sento.actor:*state*
               (list state undo (adjoin r subs :test #'eq) hl))
         (sento.actor:tell r
           (list :snapshot
                 :snapshot (pine.buffer:state->snapshot-with-hl state hl)))))

      (:unsubscribe
       (let ((r (getf plist :renderer)))
         (setf sento.actor:*state*
               (list state undo (remove r subs :test #'eq) hl))))

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


(defclass text-mode (base-mode) ())

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
                        (pine.buffer:delete-char state l (1- c))
                        :point l (1- c))))
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
                                         (getf plist :line)
                                         (getf plist :col))))
         (setf sento.actor:*state* (list new undo subs hl))
         (pine.buffer:notify-subscribers subs new hl)))

      (:delete-region
       (let ((new (pine.buffer:delete-region state
                                             (getf plist :start-line)
                                             (getf plist :start-col)
                                             (getf plist :end-line)
                                             (getf plist :end-col))))
         (setf sento.actor:*state* (list new (cons state undo) subs hl))
         (pine.buffer:notify-subscribers subs new hl)))

      (:append-with-prompt
       (let* ((text (getf plist :text))
              (pr   (getf plist :prompt))
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


(defclass lisp-mode (text-mode) ())

(defclass repl-mode (text-mode) ())

(defmethod dispatch-message ((mode repl-mode) self tag plist)
  (case tag
    (:newline
     (pine.shell:repl-submit))
    (t (call-next-method))))


(defclass terminal-mode (base-mode) ())

(defun %terminal-state (self term)
  (destructuring-bind (prior &rest rest) sento.actor:*state*
    (declare (ignore rest))
    (let* ((text (if term (pine.term:gterm-text term) ""))
           (name (pine.buffer:buffer-local prior :name ""))
           (row  (if term (pine.term:gterm-cursor-row-of term) 0))
           (col  (if term (pine.term:gterm-cursor-col-of term) 0))
           (state (pine.buffer:load-content text))
           (state (pine.buffer:set-meta state :name name))
           (state (pine.buffer:set-meta state :mode :terminal-mode))
           (state (pine.buffer:move-mark state :point row col)))
      state)))

(defmethod dispatch-message ((mode terminal-mode) self tag plist)
  (case tag
    (:insert
     (pine.term:term-write self (getf plist :text)))
    (:newline
     (pine.term:term-write self (string #\Newline)))
    (:backspace
     (pine.term:term-write self (string (code-char 127))))
    (:get-text
     (let ((term (pine.term:terminal-for-buffer self)))
       (sento.actor:reply (if term (pine.term:gterm-text term) ""))))
    (:get-snapshot
     (let* ((term (pine.term:terminal-for-buffer self))
            (state (%terminal-state self term)))
       (sento.actor:reply (pine.buffer:state->snapshot state))))
    (:get-state
     (let* ((term (pine.term:terminal-for-buffer self))
            (state (%terminal-state self term)))
       (sento.actor:reply state)))
    ((:move-point :delete-region :undo :replace-content :append-with-prompt)
     nil)
    (t (call-next-method))))


;;;; Defaults installed at startup

(defun install-default-modes ()
  (register-mode
   (make-instance 'base-mode :name :base-mode :indicator "BASE"))
  (register-mode
   (make-instance 'text-mode :name :text-mode
                             :parent-mode (find-mode :base-mode)
                             :indicator "TEXT"
                             :fallback #'%text-mode-fallback))
  (register-mode
   (make-instance 'lisp-mode :name :lisp-mode
                             :parent-mode (find-mode :text-mode)
                             :ts-language :commonlisp
                             :indicator "LISP"
                             :fallback #'%text-mode-fallback))
  (register-mode
   (make-instance 'repl-mode :name :repl-mode
                             :parent-mode (find-mode :text-mode)
                             :indicator "REPL"
                             :fallback #'%text-mode-fallback))
  (register-mode
   (make-instance 'terminal-mode :name :terminal-mode
                                 :parent-mode (find-mode :base-mode)
                                 :indicator "TERM"
                                 :fallback #'%terminal-mode-fallback)))

(defun mode-for-file (path)
  "Pick a mode keyword from a file path, by extension. Nil if unknown."
  (let ((ext (pathname-type (pathname path))))
    (cond
      ((null ext) nil)
      ((member ext '("lisp" "cl" "asd" "asdf" "lsp") :test #'string-equal)
       :lisp-mode)
      (t nil))))

(defun %text-mode-fallback (client key mods text)
  (declare (ignore key))
  (let ((+ctrl+ #x04000000)
        (+meta+ #x10000000)
        (buf (pine.client:current-buffer client)))
    (when (and buf
               text (plusp (length text))
               (graphic-char-p (char text 0))
               (zerop (logand mods +ctrl+))
               (zerop (logand mods +meta+)))
      (pine.buffer:tell buf :insert :text text))))

(defun %terminal-mode-fallback (client key mods text)
  (let ((+ctrl+ #x04000000)
        (buf (pine.client:current-buffer client)))
    (when buf
      (cond
        ((and (plusp (logand mods +ctrl+))
              (>= key 65) (<= key 90))
         (pine.buffer:tell buf :insert :text
                           (string (code-char (- key 64)))))
        ((and text (plusp (length text)))
         (pine.buffer:tell buf :insert :text text))
        (t (let ((seq (pine.term:key-bytes key mods)))
             (when seq (pine.buffer:tell buf :insert :text seq))))))))
