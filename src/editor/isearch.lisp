(defpackage #:pine.editor.isearch
  (:use #:cl)
  (:export #:*isearch* #:*isearch-last*
           #:isearch #:make-isearch #:isearch-p
           #:isearch-string #:isearch-direction
           #:isearch-origin-line #:isearch-origin-col #:isearch-lines
           #:isearch-match-line #:isearch-match-col #:isearch-wrapped
           #:isearch-start))

(in-package #:pine.editor.isearch)

;;;; Incremental search (C-s / C-r). The pending-key-reader intercept owns every
;;;; key while a search is live, re-installing itself until accept or abort.
;;;; Matching is per-line (a pattern never spans a newline) with smart case: an
;;;; all-lowercase pattern folds case, any capital makes it exact.

(defstruct isearch
  (string "")
  (direction :forward)
  origin-line origin-col
  lines                       ; fset line seq captured at entry
  match-line match-col        ; start of the current match, nil while failing
  wrapped)

(defvar *isearch* nil)
(defvar *isearch-last* ""
  "Last accepted search string; C-s C-s with an empty search repeats it.")

(defun %isearch-fold-p (pattern)
  (notany #'upper-case-p pattern))

(defun %isearch-find (lines pattern from-line from-col direction)
  "(values line col) of the match start: forward finds the first match starting
at or after FROM; backward the last match ending at or before FROM."
  (when (plusp (length pattern))
    (let* ((fold (%isearch-fold-p pattern))
           (pat (if fold (string-downcase pattern) pattern))
           (n (fset:size lines)))
      (flet ((line-hit (i start end from-end)
               (let* ((line (fset:@ lines i))
                      (s (if fold (string-downcase line) line)))
                 (search pat s
                         :start2 (max 0 (min start (length s)))
                         :end2 (max 0 (min end (length s)))
                         :from-end from-end))))
        (ecase direction
          (:forward
           (loop for i from (max 0 from-line) below n
                 for pos = (line-hit i (if (= i from-line) from-col 0)
                                     most-positive-fixnum nil)
                 when pos return (values i pos)))
          (:backward
           (loop for i from (min from-line (1- n)) downto 0
                 for pos = (line-hit i 0 (if (= i from-line) from-col
                                             most-positive-fixnum)
                                     t)
                 when pos return (values i pos))))))))

(defun %isearch-goto (line col)
  (let ((buf (pine.editor.frame:current-buffer (pine.editor.frame:current-client))))
    (when buf
      (pine.text.buffer:put-point buf line col))))

(defun %isearch-echo (st &optional failing)
  (pine.editor.echo:message
   (format nil "~:[~;Failing ~]~:[~;Wrapped ~]I-search~:[~; backward~]: ~a"
           failing (isearch-wrapped st)
           (eq (isearch-direction st) :backward)
           (isearch-string st))))

(defun %isearch-land (st line col)
  "Record the match and put point at its end (forward) or start (backward)."
  (setf (isearch-match-line st) line (isearch-match-col st) col)
  (if (eq (isearch-direction st) :forward)
      (%isearch-goto line (+ col (length (isearch-string st))))
      (%isearch-goto line col))
  (%isearch-echo st))

(defun %isearch-research (st)
  "Search for the current string from the origin (string just changed)."
  (multiple-value-bind (line col)
      (%isearch-find (isearch-lines st) (isearch-string st)
                     (isearch-origin-line st) (isearch-origin-col st)
                     (isearch-direction st))
    (if line
        (%isearch-land st line col)
        (progn (setf (isearch-match-line st) nil)
               (%isearch-echo st t)))))

(defun %isearch-repeat (st)
  "Advance to the next match past the current one, wrapping once at the end."
  (let ((lines (isearch-lines st))
        (dir (isearch-direction st))
        (ml (isearch-match-line st))
        (mc (isearch-match-col st)))
    (multiple-value-bind (from-line from-col)
        (cond ((null ml)                       ; failing: wrap from the far end
               (if (eq dir :forward)
                   (values 0 0)
                   (values (1- (fset:size lines)) most-positive-fixnum)))
              ((eq dir :forward) (values ml (1+ mc)))
              (t (values ml mc)))
      (when (null ml) (setf (isearch-wrapped st) t))
      (multiple-value-bind (line col)
          (%isearch-find lines (isearch-string st) from-line from-col dir)
        (if line
            (%isearch-land st line col)
            (progn (setf (isearch-match-line st) nil)
                   (%isearch-echo st t)))))))

(defun %isearch-printable-p (key)
  (and (= 1 (length (pine.editor.key:key-sym key)))
       (not (pine.editor.key:key-ctrl key))
       (not (pine.editor.key:key-meta key))
       (not (pine.editor.key:key-super key))
       (graphic-char-p (char (pine.editor.key:key-sym key) 0))))

(defun %isearch-exit (&key accept)
  (when (and accept *isearch* (plusp (length (isearch-string *isearch*))))
    (setf *isearch-last* (isearch-string *isearch*)))
  (setf *isearch* nil)
  (pine.editor.echo:message ""))

(defun %isearch-reader (key)
  (let ((client (pine.editor.frame:current-client))
        (st *isearch*)
        (sym nil))
    (when st
      (setf sym (pine.editor.key:key-sym key))
      (cond
        ;; abort: back to where the search began
        ((or (and (pine.editor.key:key-ctrl key) (string= sym "g"))
             (string= sym "Escape"))
         (%isearch-goto (isearch-origin-line st) (isearch-origin-col st))
         (%isearch-exit)
         (pine.editor.echo:message "quit"))
        ;; accept: point stays at the match
        ((string= sym "Return")
         (%isearch-exit :accept t))
        ;; C-s / C-r inside the search: next match / flip direction
        ((and (pine.editor.key:key-ctrl key) (member sym '("s" "r") :test #'string=))
         (setf (isearch-direction st) (if (string= sym "s") :forward :backward))
         (when (and (zerop (length (isearch-string st)))
                    (plusp (length *isearch-last*)))
           (setf (isearch-string st) *isearch-last*))
         (%isearch-repeat st)
         (pine.editor.command:read-next-key client #'%isearch-reader))
        ;; shrink
        ((string= sym "BackSpace")
         (when (plusp (length (isearch-string st)))
           (setf (isearch-string st)
                 (subseq (isearch-string st) 0 (1- (length (isearch-string st))))))
         (setf (isearch-wrapped st) nil)
         (if (plusp (length (isearch-string st)))
             (%isearch-research st)
             (progn (%isearch-goto (isearch-origin-line st) (isearch-origin-col st))
                    (%isearch-echo st)))
         (pine.editor.command:read-next-key client #'%isearch-reader))
        ;; extend
        ((%isearch-printable-p key)
         (setf (isearch-string st)
               (concatenate 'string (isearch-string st) sym))
         (%isearch-research st)
         (pine.editor.command:read-next-key client #'%isearch-reader))
        ;; any other key: accept the search, then let the key do its normal job
        (t
         (%isearch-exit :accept t)
         (pine.editor.command:dispatch client key))))))

(defun isearch-start (direction)
  (let* ((client (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer client)))
    (when buf
      (let* ((state (pine.text.buffer:state-of buf))
             (snap (pine.text.buffer:state->snapshot state)))
        (setf *isearch*
              (make-isearch :direction direction
                            :origin-line (pine.text.buffer:point-line snap)
                            :origin-col (pine.text.buffer:point-col snap)
                            :lines (pine.text.buffer:lines state)))
        (%isearch-echo *isearch*)
        (pine.editor.command:read-next-key client #'%isearch-reader)))))
