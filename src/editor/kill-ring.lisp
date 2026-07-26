(defpackage #:pine.editor.kill-ring
  (:use #:cl)
  (:export #:*last-yank* #:copy-region-cmd #:kill-line-cmd #:kill-region-cmd #:kill-ring-push #:kill-ring-top #:kill-words-cmd #:set-mark #:yank-cmd #:yank-pop-cmd))

(in-package #:pine.editor.kill-ring)

(defun kill-ring-push (text)
  (let ((client (pine.editor.frame:current-client)))
    (when (and text (plusp (length text)))
      (if (and (member (pine.editor.frame:last-command client)
                       '("kill-line" "kill-region" "kill-word" "backward-kill-word")
                       :test #'equal)
               (pine.editor.frame:kill-ring client))
          (setf (first (pine.editor.frame:kill-ring client))
                (concatenate 'string (first (pine.editor.frame:kill-ring client)) text))
          (progn
            (push text (pine.editor.frame:kill-ring client))
            (when (> (length (pine.editor.frame:kill-ring client))
                     (pine.editor.frame:kill-ring-max client))
              (setf (pine.editor.frame:kill-ring client)
                    (subseq (pine.editor.frame:kill-ring client) 0
                            (pine.editor.frame:kill-ring-max client))))))
      (setf (pine.state.store:store :kill-ring) (pine.editor.frame:kill-ring client)))))

(defun kill-ring-top ()
  (first (pine.editor.frame:kill-ring (pine.editor.frame:current-client))))


;;;; Mark and region

(defun set-mark ()
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer client)))
    (when buf
      (let ((snap (pine.editor.frame:current-buffer-snapshot)))
        (when snap
          (sento.actor:tell buf
            (list :set-meta :key :mark-line :value (pine.text.buffer:point-line snap)))
          (sento.actor:tell buf
            (list :set-meta :key :mark-col :value (pine.text.buffer:point-col snap)))
          (pine.editor.echo:message "mark set"))))))

;;;; Kill commands

(defun kill-region-cmd ()
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer client)))
    (when buf
      (let ((state (pine.core.actor:ask buf '(:get-state) :timeout 5)))
        (multiple-value-bind (sl sc el ec) (pine.text.buffer:region-bounds state)
          (when sl
            (let ((text (pine.text.buffer:region-string state sl sc el ec)))
              (kill-ring-push text)
              (sento.actor:tell buf
                (list :delete-region :start-line sl :start-col sc
                      :end-line el :end-col ec)))))))))

(defun kill-line-cmd ()
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer client)))
    (when buf
      (let* ((snap (pine.editor.frame:current-buffer-snapshot))
             (pl (pine.text.buffer:point-line snap))
             (pc (pine.text.buffer:point-col snap))
             (line (fset:@ (pine.text.buffer:lines snap) pl))
             (len (length line)))
        (if (< pc len)
            (let ((text (subseq line pc)))
              (kill-ring-push text)
              (sento.actor:tell buf
                (list :delete-region :start-line pl :start-col pc
                      :end-line pl :end-col len)))
            (when (< (1+ pl) (pine.text.buffer:line-count snap))
              (kill-ring-push (string #\Newline))
              (sento.actor:tell buf
                (list :delete-region :start-line pl :start-col pc
                      :end-line (1+ pl) :end-col 0))))))))

(defun kill-words-cmd (n)
  "Kill N words forward (negative = backward): the region from point to where
point-after-move :word lands, into the kill ring."
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer client)))
    (when buf
      (let* ((state (pine.core.actor:ask buf '(:get-state) :timeout 5))
             (snap (pine.text.buffer:state->snapshot state))
             (pl (pine.text.buffer:point-line snap))
             (pc (pine.text.buffer:point-col snap)))
        (multiple-value-bind (tl tc) (pine.text.buffer:point-after-move snap :word n)
          (multiple-value-bind (sl sc el ec)
              (if (or (< tl pl) (and (= tl pl) (< tc pc)))
                  (values tl tc pl pc)
                  (values pl pc tl tc))
            (unless (and (= sl el) (= sc ec))
              (kill-ring-push (pine.text.buffer:region-string state sl sc el ec))
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
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer client))
         (text (kill-ring-top)))
    (when (and text buf)
      (let ((snap (pine.editor.frame:current-buffer-snapshot)))
        (when snap
          (setf *last-yank* (list buf (pine.text.buffer:point-line snap)
                                  (pine.text.buffer:point-col snap) text))))
      (sento.actor:tell buf (list :insert :text text)))))

(defun yank-pop-cmd ()
  (let ((client (pine.editor.frame:current-client)))
    (when (and (member (pine.editor.frame:last-command client) '("yank" "yank-pop")
                       :test #'equal)
               *last-yank*
               (rest (pine.editor.frame:kill-ring client)))
      (let ((top (pop (pine.editor.frame:kill-ring client))))
        (setf (pine.editor.frame:kill-ring client)
              (append (pine.editor.frame:kill-ring client) (list top))))
      (destructuring-bind (buf sl sc old-text) *last-yank*
        (let ((new-text (kill-ring-top)))
          (multiple-value-bind (el ec) (%advance-pos sl sc old-text)
            (sento.actor:tell buf (list :delete-region :start-line sl :start-col sc
                                        :end-line el :end-col ec)))
          (sento.actor:tell buf (list :insert :text new-text))
          (setf *last-yank* (list buf sl sc new-text)))))))

(defun copy-region-cmd ()
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer client)))
    (when buf
      (let ((state (pine.core.actor:ask buf '(:get-state) :timeout 5)))
        (multiple-value-bind (sl sc el ec) (pine.text.buffer:region-bounds state)
          (when sl
            (let ((text (pine.text.buffer:region-string state sl sc el ec)))
              (kill-ring-push text)
              (pine.editor.echo:message "copied"))))))))
