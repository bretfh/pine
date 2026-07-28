(defpackage #:pine.editor.kill-ring
  (:use #:cl)
  (:export #:*last-yank* #:copy-region-cmd #:kill-line-cmd #:kill-region-cmd #:kill-ring-push #:kill-ring-top #:kill-words-cmd #:set-mark #:yank-cmd #:yank-pop-cmd))

(in-package #:pine.editor.kill-ring)
(named-readtables:in-readtable pine.path:syntax)

;;;; The kill ring is /kill: a ring is a write option, so pushing is a write,
;;;; the newest is a read, and the whole ring is a pattern read. Nothing here
;;;; holds it, and it survives a restart because a held path does.

(defparameter +kills+ 60 "How many kills /kill remembers.")

(defun kill-ring-push (text)
  (when (and text (plusp (length text)))
    (pine.ns:write /kill text :max +kills+)))

(defun kill-ring-top ()
  (pine.ns:read /kill))

(defun kill-ring-entry (n)
  "The Nth newest kill, or NIL."
  (pine.ns:read (pine.path:path /kill (princ-to-string n))))

;;;; Mark and region

(defun set-mark ()
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer client)))
    (when buf
      (let ((snap (pine.editor.frame:current-buffer-snapshot)))
        (when snap
          ;; the mark is one place, so it is set in one write; two halves
          ;; landing separately is a mark that is briefly nowhere
          (pine.text.buffer:put buf :mark
                                (fset:seq (pine.text.buffer:point-line snap)
                                          (pine.text.buffer:point-col snap)))
          (pine.editor.echo:message "mark set"))))))

;;;; Kill commands

(defun kill-region-cmd ()
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer client)))
    (when buf
      (pine.text.buffer:edit buf (fset:seq :kill)))))

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
              (pine.text.buffer:edit buf (fset:seq :delete (fset:seq pl pc) (fset:seq pl len))))
            (when (< (1+ pl) (pine.text.buffer:line-count snap))
              (kill-ring-push (string #\Newline))
              (pine.text.buffer:edit buf (fset:seq :delete (fset:seq pl pc)
                                                  (fset:seq (1+ pl) 0)))))))))

(defun kill-words-cmd (n)
  "Kill N words forward (negative = backward): the region from point to where
point-after-move :word lands, into the kill ring."
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer client)))
    (when buf
      (let* ((state (pine.text.buffer:state-of buf))
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
              (pine.text.buffer:edit buf (fset:seq :delete (fset:seq sl sc) (fset:seq el ec))))))))))

;;;; The most recent yank, so yank-pop can delete it before inserting the next
;;;; ring entry: (buffer start-line start-col inserted-text how-far-back).
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
                                  (pine.text.buffer:point-col snap) text 0))))
      (pine.text.buffer:edit buf (fset:seq :insert text)))))

(defun yank-pop-cmd ()
  "Replace what the last yank inserted with the next entry back in the ring.
The ring does not move: which entry this is walks back through it."
  (let ((client (pine.editor.frame:current-client)))
    (when (and (member (pine.editor.frame:last-command client) '("yank" "yank-pop")
                       :test #'equal)
               *last-yank*)
      (destructuring-bind (buf sl sc old-text back) *last-yank*
        (let ((new-text (or (kill-ring-entry (1+ back)) (kill-ring-entry 0))))
          (when new-text
            (multiple-value-bind (el ec) (%advance-pos sl sc old-text)
              (pine.text.buffer:edit buf (fset:seq :delete (fset:seq sl sc) (fset:seq el ec))))
            (pine.text.buffer:edit buf (fset:seq :insert new-text))
            (setf *last-yank*
                  (list buf sl sc new-text
                        (if (kill-ring-entry (1+ back)) (1+ back) 0)))))))))

(defun copy-region-cmd ()
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer client)))
    (when buf
      (let ((state (pine.text.buffer:state-of buf)))
        (multiple-value-bind (sl sc el ec) (pine.text.buffer:region-bounds state)
          (when sl
            (let ((text (pine.text.buffer:region-string state sl sc el ec)))
              (kill-ring-push text)
              (pine.editor.echo:message "copied"))))))))
