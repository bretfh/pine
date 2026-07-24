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
  ;; the buffer's own :mode meta drives highlighting, so set it first and
  ;; unconditionally; recording it on the client (for the modeline) is
  ;; best-effort and must not stop the buffer from learning its mode.
  (sento.actor:tell buffer-actor (list :set-local :key :mode :value mode-name))
  (let ((cli (ignore-errors (pine.client:current-client))))
    (when cli
      (setf (gethash buffer-actor (pine.client:buffer-modes cli)) mode-name)))
  (find-mode mode-name))

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

;;;; Minor modes. Per-buffer, precedence-numbered (higher = more specific).
;;;; The enabled set feeds both the active-keymap list and the synthesized
;;;; dispatch class, so a minor mode augments via keymap bindings and via
;;;; execute method combination (:before/:after transparent, :around opaque).

(defun %minor-names (client)
  (gethash (pine.client:current-buffer client)
           (pine.client:buffer-minor-modes client)))

(defun (setf %minor-names) (names client)
  (setf (gethash (pine.client:current-buffer client)
                 (pine.client:buffer-minor-modes client))
        names))

(defun buffer-minor-modes (client)
  "Active minor-mode singletons for the current buffer, most specific first."
  (stable-sort
   (loop for name in (%minor-names client)
         for m = (find-mode name)
         when (typep m 'minor-mode) collect m)
   #'> :key #'precedence))

(defun minor-mode-enabled-p (client name)
  (and (member name (%minor-names client)) t))

(defun enable-minor-mode (client name)
  (unless (typep (find-mode name) 'minor-mode)
    (error "~s is not a minor mode" name))
  (pushnew name (%minor-names client))
  t)

(defun disable-minor-mode (client name)
  (setf (%minor-names client) (remove name (%minor-names client)))
  nil)

(defun toggle-minor-mode (client name)
  (if (minor-mode-enabled-p client name)
      (disable-minor-mode client name)
      (enable-minor-mode client name)))

(defun active-minor-mode-indicators (client)
  (loop for m in (buffer-minor-modes client) collect (mode-indicator m)))

;;;; Active modes -> keymaps + a synthesized dispatch class

(defun buffer-active-modes (client)
  "Minor modes (most specific first) then the major mode. This is the
superclass order of the synthesized dispatch class, so minor-mode methods
run before the major mode's under CLOS method combination."
  (append (buffer-minor-modes client) (list (current-buffer-mode))))

(defun active-keymaps (client)
  (append (mapcar #'mode-keymap (buffer-minor-modes client))
          (list (mode-keymap (current-buffer-mode)) *global-keymap*)))

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


;;;; The buffer-behavior interface. The buffer actor's receive calls
;;;; (dispatch-message MODE SELF TAG PLIST); the verb methods live in
;;;; mode/edit.lisp, layered base -> text -> subclasses.

(defgeneric dispatch-message (mode self tag plist))

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

;;;; overwrite-mode: transparent augmentation of self-insert. It runs BEFORE
;;;; the base insert (method combination), deleting the char under point so the
;;;; inserted char overwrites it, then falls through to the normal insert.

(defun %overwrite-forward ()
  (let ((buf (pine.client:current-buffer (pine.client:current-client))))
    (when buf
      (multiple-value-bind (l c) (pine.buffer:ask buf :point)
        (let ((line (pine.buffer:ask buf :line l)))
          (when (and line (< c (length line)))
            (pine.buffer:tell buf :delete-region
                              :start-line l :start-col c
                              :end-line l :end-col (1+ c))))))))

(defmethod pine.command:execute :before ((modes overwrite-mode) command argument)
  (declare (ignore argument))
  (when (string= (pine.command:command-name command) "self-insert-command")
    (%overwrite-forward)))
