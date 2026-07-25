(in-package :pine.file)

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
line's length -- stored places must never put point outside the buffer."
  (let* ((lines (or (uiop:split-string content :separator '(#\Newline)) (list "")))
         (l (max 0 (min line (1- (length lines)))))
         (c (max 0 (min col (length (nth l lines))))))
    (values l c)))

(defun %open-file (path)
  "The silent half of find-file: read PATH into a named buffer with its
pathname local, mode by extension, and point at the stored place. (values
BUF NAME EXISTS) -- no switching, no rendering, no echo, so the world
restore can reopen buffers in bulk."
  (let* ((expanded (merge-pathnames path))
         (exists (probe-file expanded))
         (namestring (namestring expanded))
         (name (file-namestring expanded))
         (content (if exists (or (read-file expanded) "") "")))
    (when (string= name "") (setf name namestring))
    (let ((buf (pine.buffer:make-buffer name :content content))
          (place (and exists (pine.store:store (list :place namestring)))))
      (pine.store:store-push :recent-files namestring :unique t :max 100)
      (sento.actor:tell buf (list :set-local :key :pathname :value namestring))
      (if place
          (multiple-value-bind (l c)
              (%clamped-place content (first place) (second place))
            (sento.actor:tell buf (list :move-point :line l :col c)))
          (sento.actor:tell buf (list :move-point :line 0 :col 0)))
      (let ((mode-kw (or (pine.mode:mode-for-file namestring) :text-mode)))
        (pine.mode:set-buffer-mode buf mode-kw))
      (values buf name exists namestring))))

(defun find-file (path)
  (multiple-value-bind (buf name exists namestring) (%open-file path)
    (pine.buffer:switch-buffer name)
    (sento.actor:tell (pine.client:renderer (pine.client:current-client))
                      (list :switch-buffer :buffer buf :name name))
    (pine.render:subscribe-to-buffer buf)
    (pine.world:save-world :buffers)
    (pine.echo:message
            (if exists (format nil "~a" namestring)
                (format nil "(new file) ~a" namestring)))
    buf))

(defun save-current-buffer ()
  (let ((buf (pine.client:current-buffer (pine.client:current-client))))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (path (pine.buffer:buffer-local state :pathname))
             (text (pine.buffer:state->string state)))
        (if path
            (progn
              (write-file path text)
              (record-place buf path)
              (pine.store:store-push :recent-files path :unique t :max 100)
              (pine.echo:message (format nil "wrote ~a" path)))
            (pine.echo:message "no file path for this buffer"))))))

(defun record-place (buf path)
  "Store BUF's point under (:place PATH), so the next find-file resumes there."
  (ignore-errors
   (multiple-value-bind (line col) (pine.buffer:ask buf :point)
     (when line
       (setf (pine.store:store (list :place path)) (list line col))))))

(defun record-places ()
  "Store the point of every file-backed buffer. The shutdown sweep."
  (let ((srv pine.server:*server*))
    (when srv
      (loop for buf being the hash-values of (or (pine.server:buffer-table srv)
                                                 (make-hash-table))
            do (ignore-errors
                (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 2))
                       (path (pine.buffer:buffer-local state :pathname)))
                  (when path (record-place buf path))))))))

;;;; World: the open files. Computed from the live buffer table at save time
;;;; -- no shadow list -- and reopened from disk on restore, so a stale entry
;;;; can only show less, never corrupt.

(defun %file-buffers ()
  "((PATH MODE-KW) ...) for every live buffer backed by a file."
  (let ((srv pine.server:*server*) (acc nil))
    (when (and srv (pine.server:buffer-table srv))
      (loop for buf being the hash-values of (pine.server:buffer-table srv)
            do (ignore-errors
                (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 2))
                       (path (pine.buffer:buffer-local state :pathname)))
                  (when path
                    (push (list path (pine.buffer:buffer-local state :mode))
                          acc))))))
    acc))

(pine.world:register :buffers
  :save #'%file-buffers
  :restore (lambda (entries)
             (loop for (path mode) in entries
                   when (probe-file path)
                     do (ignore-errors
                         (let ((buf (%open-file path)))
                           (when (and mode
                                      (not (eq mode (pine.mode:mode-for-file path))))
                             (pine.mode:set-buffer-mode buf mode)))))))
