(in-package :pine.editor)

(defun kill-ring-push (text)
  (let ((client (pine.client:current-client)))
    (when (and text (plusp (length text)))
      (if (and (member (pine.client:last-command client)
                       '("kill-line" "kill-region" "kill-word" "backward-kill-word")
                       :test #'equal)
               (pine.client:kill-ring client))
          (setf (first (pine.client:kill-ring client))
                (concatenate 'string (first (pine.client:kill-ring client)) text))
          (progn
            (push text (pine.client:kill-ring client))
            (when (> (length (pine.client:kill-ring client))
                     (pine.client:kill-ring-max client))
              (setf (pine.client:kill-ring client)
                    (subseq (pine.client:kill-ring client) 0
                            (pine.client:kill-ring-max client))))))
      (setf (pine.state.store:store :kill-ring) (pine.client:kill-ring client)))))

(defun kill-ring-top ()
  (first (pine.client:kill-ring (pine.client:current-client))))


;;;; Mark and region

(defun set-mark ()
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client)))
    (when buf
      (let ((snap (pine.client:current-buffer-snapshot)))
        (when snap
          (sento.actor:tell buf
            (list :set-meta :key :mark-line :value (pine.buffer:point-line snap)))
          (sento.actor:tell buf
            (list :set-meta :key :mark-col :value (pine.buffer:point-col snap)))
          (pine.echo:message "mark set"))))))

;;;; Kill commands

(defun kill-region-cmd ()
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client)))
    (when buf
      (let ((state (sento.actor:ask-s buf '(:get-state) :time-out 5)))
        (multiple-value-bind (sl sc el ec) (pine.buffer:region-bounds state)
          (when sl
            (let ((text (pine.buffer:region-string state sl sc el ec)))
              (kill-ring-push text)
              (sento.actor:tell buf
                (list :delete-region :start-line sl :start-col sc
                      :end-line el :end-col ec)))))))))

(defun kill-line-cmd ()
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client)))
    (when buf
      (let* ((snap (pine.client:current-buffer-snapshot))
             (pl (pine.buffer:point-line snap))
             (pc (pine.buffer:point-col snap))
             (line (fset:@ (pine.buffer:lines snap) pl))
             (len (length line)))
        (if (< pc len)
            (let ((text (subseq line pc)))
              (kill-ring-push text)
              (sento.actor:tell buf
                (list :delete-region :start-line pl :start-col pc
                      :end-line pl :end-col len)))
            (when (< (1+ pl) (pine.buffer:line-count snap))
              (kill-ring-push (string #\Newline))
              (sento.actor:tell buf
                (list :delete-region :start-line pl :start-col pc
                      :end-line (1+ pl) :end-col 0))))))))

(defun kill-words-cmd (n)
  "Kill N words forward (negative = backward): the region from point to where
point-after-move :word lands, into the kill ring."
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (snap (pine.buffer:state->snapshot state))
             (pl (pine.buffer:point-line snap))
             (pc (pine.buffer:point-col snap)))
        (multiple-value-bind (tl tc) (pine.buffer:point-after-move snap :word n)
          (multiple-value-bind (sl sc el ec)
              (if (or (< tl pl) (and (= tl pl) (< tc pc)))
                  (values tl tc pl pc)
                  (values pl pc tl tc))
            (unless (and (= sl el) (= sc ec))
              (kill-ring-push (pine.buffer:region-string state sl sc el ec))
              (sento.actor:tell buf
                (list :delete-region :start-line sl :start-col sc
                      :end-line el :end-col ec)))))))))

;;;; The most recent yank, so yank-pop can delete it before inserting the next
;;;; ring entry: (buffer start-line start-col inserted-text).
(defvar *last-yank* nil)

(defun %advance-pos (line col text)
  "Line/col after inserting TEXT starting at (LINE COL)."
  (loop for ch across text
        do (if (char= ch #\Newline) (setf line (1+ line) col 0) (incf col)))
  (values line col))

(defun yank-cmd ()
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client))
         (text (kill-ring-top)))
    (when (and text buf)
      (let ((snap (pine.client:current-buffer-snapshot)))
        (when snap
          (setf *last-yank* (list buf (pine.buffer:point-line snap)
                                  (pine.buffer:point-col snap) text))))
      (sento.actor:tell buf (list :insert :text text)))))

(defun yank-pop-cmd ()
  (let ((client (pine.client:current-client)))
    (when (and (member (pine.client:last-command client) '("yank" "yank-pop")
                       :test #'equal)
               *last-yank*
               (rest (pine.client:kill-ring client)))
      (let ((top (pop (pine.client:kill-ring client))))
        (setf (pine.client:kill-ring client)
              (append (pine.client:kill-ring client) (list top))))
      (destructuring-bind (buf sl sc old-text) *last-yank*
        (let ((new-text (kill-ring-top)))
          (multiple-value-bind (el ec) (%advance-pos sl sc old-text)
            (sento.actor:tell buf (list :delete-region :start-line sl :start-col sc
                                        :end-line el :end-col ec)))
          (sento.actor:tell buf (list :insert :text new-text))
          (setf *last-yank* (list buf sl sc new-text)))))))

(defun copy-region-cmd ()
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client)))
    (when buf
      (let ((state (sento.actor:ask-s buf '(:get-state) :time-out 5)))
        (multiple-value-bind (sl sc el ec) (pine.buffer:region-bounds state)
          (when sl
            (let ((text (pine.buffer:region-string state sl sc el ec)))
              (kill-ring-push text)
              (pine.echo:message "copied"))))))))
