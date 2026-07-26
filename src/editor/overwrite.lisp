(in-package :pine.editor)

;;;; overwrite-mode: transparent augmentation of self-insert. It runs BEFORE
;;;; the base insert (method combination), deleting the char under point so the
;;;; inserted char overwrites it, then falls through to the normal insert.

(defun %overwrite-forward ()
  (let ((buf (pine.editor.frame:current-buffer (pine.editor.frame:current-client))))
    (when buf
      (multiple-value-bind (l c) (pine.editor.ask:ask buf :point)
        (let ((line (pine.editor.ask:ask buf :line l)))
          (when (and line (< c (length line)))
            (pine.editor.ask:tell buf :delete-region
                              :start-line l :start-col c
                              :end-line l :end-col (1+ c))))))))

(defmethod pine.editor.command:execute :before
    ((modes pine.editor.mode:overwrite-mode) command argument)
  (declare (ignore argument))
  (when (string= (pine.editor.command:command-name command) "self-insert-command")
    (%overwrite-forward)))
