(in-package :pine.editor.frame)

(defun make-buffer (name &key (content ""))
  (let* ((c (current-client))
         (srv (server-of c))
         (table (pine.text.buffer:buffer-table srv))
         (existing (gethash name table)))
    (when existing (return-from make-buffer existing))
    (let ((actor (pine.text.buffer:make-buffer-actor (pine.core.server:actor-system srv) name :content content)))
      (setf (gethash name table) actor)
      (sento.actor:tell (pine.core.server:buffer-registry srv)
                        (list :register :name name :actor actor))
      (when (null (current-buffer c))
        (setf (current-buffer c) actor))
      actor)))

(defun kill-buffer (name)
  (let* ((c (current-client))
         (srv (server-of c))
         (table (pine.text.buffer:buffer-table srv))
         (actor (gethash name table)))
    (when actor
      (when (eq actor (current-buffer c))
        (setf (current-buffer c) nil))
      (remhash name table)
      (sento.actor-context:stop (pine.core.server:actor-system srv) actor)
      (sento.actor:tell (pine.core.server:buffer-registry srv)
                        (list :unregister :name name)))))

(defun switch-buffer (name)
  (let* ((c (current-client))
         (srv (server-of c))
         (actor (gethash name (pine.text.buffer:buffer-table srv))))
    (when actor
      (setf (current-buffer c) actor)
      actor)))


(defun list-buffers ()
  (let ((srv (server-of (current-client))))
    (loop for k being the hash-keys of (pine.text.buffer:buffer-table srv) collect k)))

(defun buffer-count ()
  (let ((srv (server-of (current-client))))
    (hash-table-count (pine.text.buffer:buffer-table srv))))

(defun current-buffer-text ()
  (let* ((c (current-client))
         (buf (current-buffer c)))
    (when buf
      (sento.actor:ask-s buf '(:get-text) :time-out 5))))

(defun current-buffer-snapshot ()
  (let* ((c (current-client))
         (buf (current-buffer c)))
    (when buf
      (sento.actor:ask-s buf '(:get-snapshot) :time-out 5))))


(defun buffer (x)
  "Coerce X to a buffer actor.
- nil            -> nil
- string         -> lookup by name in current server's buffer-table
- :current       -> current client's current-buffer
- :focused       -> focused window's buffer-ref
- actor ref      -> passthrough
Unknown keywords error; nothing silently falls through."
  (cond
    ((null x) nil)
    ((stringp x)
     (let ((srv (server-of (current-client))))
       (gethash x (pine.text.buffer:buffer-table srv))))
    ((eq x :current)
     (current-buffer (current-client)))
    ((eq x :focused)
     (let ((w (focused-window (current-client))))
       (when w (pine.text.buffer:buffer-ref w))))
    ((keywordp x)
     (error "unknown buffer target ~s; use :current, :focused, or a string name"
            x))
    (t x)))
