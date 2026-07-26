(in-package :pine.client)

;;;; Which modes are active. A mode is a class and a singleton, both of which
;;;; pine.mode owns; which ones a buffer has on right now is this client's
;;;; state, so it lives here, above the text layer the buffer actor dispatches
;;;; through.

(defun buffer-mode (buffer-or-snap)
  (let ((name (pine.buffer:buffer-local buffer-or-snap :mode :base-mode)))
    (or (pine.mode:find-mode name) (pine.mode:find-mode :base-mode))))

(defun current-buffer-mode ()
  (let* ((c (current-client))
         (buf (current-buffer c))
         (name (and buf (gethash buf (buffer-modes c)))))
    (or (and name (pine.mode:find-mode name)) (pine.mode:find-mode :base-mode))))

(defun set-buffer-mode (buffer-actor mode-name)
  (unless (pine.mode:find-mode mode-name) (error "No mode named ~s" mode-name))
  ;; the buffer's own :mode meta drives highlighting, so set it first and
  ;; unconditionally; recording it on the client (for the modeline) is
  ;; best-effort and must not stop the buffer from learning its mode.
  (sento.actor:tell buffer-actor (list :set-local :key :mode :value mode-name))
  (let ((c *client*))
    (when c
      (setf (gethash buffer-actor (buffer-modes c)) mode-name)))
  (pine.mode:find-mode mode-name))

;;;; Minor modes. Per-buffer, precedence-numbered (higher = more specific).
;;;; The enabled set feeds both the active-keymap list and the synthesized
;;;; dispatch class, so a minor mode augments via keymap bindings and via
;;;; execute method combination (:before/:after transparent, :around opaque).

(defun %minor-names (client)
  (gethash (current-buffer client) (buffer-minor-modes client)))

(defun (setf %minor-names) (names client)
  (setf (gethash (current-buffer client) (buffer-minor-modes client)) names))

(defun active-minor-modes (client)
  "Active minor-mode singletons for the current buffer, most specific first."
  (stable-sort
   (loop :for name :in (%minor-names client)
         :for m = (pine.mode:find-mode name)
         :when (typep m 'pine.mode:minor-mode) :collect m)
   #'> :key #'pine.mode:precedence))

(defun minor-mode-enabled-p (client name)
  (and (member name (%minor-names client)) t))

(defun enable-minor-mode (client name)
  (unless (typep (pine.mode:find-mode name) 'pine.mode:minor-mode)
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
  (loop :for m :in (active-minor-modes client)
        :collect (pine.mode:mode-indicator m)))

;;;; Active modes -> keymaps + a synthesized dispatch class

(defun buffer-active-modes (client)
  "Minor modes (most specific first) then the major mode. This is the
superclass order of the synthesized dispatch class, so minor-mode methods
run before the major mode's under CLOS method combination."
  (append (active-minor-modes client) (list (current-buffer-mode))))

(defun active-keymaps (client)
  (append (mapcar #'pine.mode:mode-keymap (active-minor-modes client))
          (list (pine.mode:mode-keymap (current-buffer-mode))
                (pine.mode:global-keymap))))

(defun active-modes-instance (client)
  (make-instance (pine.mode:modes-dispatch-class
                  (mapcar #'class-of (buffer-active-modes client)))))
