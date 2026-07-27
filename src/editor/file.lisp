(defpackage #:pine.editor.file
  (:use :cl)
  (:local-nicknames (#:world #:pine.state.world))
  (:export
   #:read-file
   #:write-file
   #:find-file
   #:save-current-buffer
   #:record-places
   #:buffer-entry
   #:restore-buffer))

(in-package #:pine.editor.file)

(defun read-file (path)
  (with-open-file (s path :direction :input :if-does-not-exist nil)
    (when s
      (with-output-to-string (out)
        (loop for line = (read-line s nil nil)
              while line
              do (write-string line out)
                 (write-char #\Newline out))))))

(defun write-file (path text)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string text s))
  path)

(defun %clamped-place (content line col)
  "LINE/COL clamped into CONTENT: line to the lines that exist, col to that
line's length, so a stored place never puts point outside the buffer."
  (let* ((lines (or (uiop:split-string content :separator '(#\Newline)) (list "")))
         (l (max 0 (min line (1- (length lines)))))
         (c (max 0 (min col (length (nth l lines))))))
    (values l c)))

(defun %open-file (path)
  "The silent half of find-file: read PATH into a named buffer with its
pathname local, mode by extension, and point at the stored place. (values
BUF NAME EXISTS). No switching, no rendering and no echo, so the world
restore can reopen buffers in bulk."
  (let* ((expanded (merge-pathnames path))
         (exists (probe-file expanded))
         (namestring (namestring expanded))
         (name (file-namestring expanded))
         (content (if exists (or (read-file expanded) "") "")))
    (when (string= name "") (setf name namestring))
    (let ((buf (pine.editor.frame:make-buffer name :content content))
          (place (and exists (world:value (list :place namestring)))))
      (world:push :recent-files namestring :unique t :max 100)
      (sento.actor:tell buf (list :set-local :key :pathname :value namestring))
      (if place
          (multiple-value-bind (l c)
              (%clamped-place content (first place) (second place))
            (sento.actor:tell buf (list :move-point :line l :col c)))
          (sento.actor:tell buf (list :move-point :line 0 :col 0)))
      (let ((mode-kw (or (pine.editor.mode:mode-for-file namestring) :text-mode)))
        (pine.editor.frame:set-buffer-mode buf mode-kw))
      (values buf name exists namestring))))

(defun find-file (path)
  (multiple-value-bind (buf name exists namestring) (%open-file path)
    (pine.editor.frame:switch-buffer name)
    (sento.actor:tell (pine.editor.frame:renderer (pine.editor.frame:current-client))
                      (list :switch-buffer :buffer buf :name name))
    (pine.ui.render:subscribe-to-buffer buf)
    (world:save :buffers)
    (pine.editor.echo:message
            (if exists (format nil "~a" namestring)
                (format nil "(new file) ~a" namestring)))
    buf))

(defun save-current-buffer ()
  (let ((buf (pine.editor.frame:current-buffer (pine.editor.frame:current-client))))
    (when buf
      (let* ((state (pine.core.actor:ask buf '(:get-state) :timeout 5))
             (path (pine.text.buffer:buffer-local state :pathname))
             (text (pine.text.buffer:state->string state)))
        (if path
            (progn
              (write-file path text)
              (record-place buf path)
              (world:push :recent-files path :unique t :max 100)
              (pine.editor.echo:message (format nil "wrote ~a" path)))
            (pine.editor.echo:message "no file path for this buffer"))))))

(defun record-place (buf path)
  "Store BUF's point under (:place PATH), so the next find-file resumes there."
  (ignore-errors
   (multiple-value-bind (line col) (pine.editor.ask:ask buf :point)
     (when line
       (setf (world:value (list :place path)) (list line col))))))

(defun record-places ()
  "Store the point of every file-backed buffer. The shutdown sweep."
  (let ((srv pine.core.server:*server*))
    (when srv
      (loop for buf being the hash-values of (or (pine.core.server:buffer-table srv)
                                                 (make-hash-table))
            do (ignore-errors
                (let* ((state (pine.core.actor:ask buf '(:get-state) :timeout 2))
                       (path (pine.text.buffer:buffer-local state :pathname)))
                  (when path (record-place buf path))))))))

;;;; World: the buffers. Computed from the live table at save time. A buffer
;;;; with a file is its path, reopened from disk; one without is its text,
;;;; because there is nowhere else for that to come back from. A stale entry can
;;;; only show less, never corrupt.

(defun tool-buffer-p (name)
  "True for a buffer that is a view of live state rather than text someone
wrote: the debugger, the supervisor, help. Its content means nothing without
the state it was projected from, so it does not persist."
  (and (stringp name)
       (plusp (length name))
       (char= #\* (char name 0))))

(defun %buffer-entry (buf)
  "BUF as a readable plist: (:path P :mode M) with a file, else
(:name N :mode M :content C). NIL for a buffer with nothing worth keeping."
  (let* ((state (pine.core.actor:ask buf '(:get-state) :timeout 2))
         (mode (pine.text.buffer:buffer-local state :mode))
         (name (pine.text.buffer:buffer-local state :name ""))
         (path (pine.text.buffer:buffer-local state :pathname)))
    (cond
      (path (list :path path :mode mode))
      ((tool-buffer-p name) nil)
      ((pine.text.buffer:buffer-local state :layout-builder) nil)
      (t (let ((content (pine.text.buffer:state->string state)))
           (when (plusp (length content))
             (list :name name :mode mode :content content)))))))

(defun %buffers ()
  (let ((srv pine.core.server:*server*) (acc nil))
    (when (and srv (pine.core.server:buffer-table srv))
      (loop :for buf :being :the :hash-values :of (pine.core.server:buffer-table srv)
            :do (pine.core.eval:attempt
                 (lambda () (let ((entry (%buffer-entry buf)))
                              (when entry (push entry acc))))
                 "saving a buffer to the world")))
    acc))

(defun %restore-entry (entry)
  (destructuring-bind (&key path name mode content) entry
    (cond
      ((and path (probe-file path))
       (let ((buf (%open-file path)))
         (when (and mode (not (eq mode (pine.editor.mode:mode-for-file path))))
           (pine.editor.frame:set-buffer-mode buf mode))
         buf))
      (name
       (let ((buf (pine.editor.frame:make-buffer name :content (or content ""))))
         (when mode (pine.editor.frame:set-buffer-mode buf mode))
         buf)))))

(defun buffer-entry (name)
  "The saved entry for the buffer called NAME, or nil. What a buffer whose
thread died is rebuilt from."
  (find-if (lambda (entry)
             (let ((path (getf entry :path)))
               (equal name (if path (file-namestring path) (getf entry :name)))))
           (world:value :buffers)))

(defun restore-buffer (name)
  "Rebuild the buffer called NAME from the world. Answers the actor, or nil."
  (let ((entry (buffer-entry name)))
    (when entry (%restore-entry entry))))

(defmethod world:snapshot ((name (eql :buffers))) (%buffers))

(defmethod world:revive ((name (eql :buffers)) entries)
  (dolist (entry entries)
    (pine.core.eval:attempt (lambda () (%restore-entry entry))
                            "restoring a buffer from the world")))
