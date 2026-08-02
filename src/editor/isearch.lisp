(defpackage #:pine.editor.isearch
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path)
                    (#:frame #:pine.editor.frame) (#:key #:pine.key))
  (:export #:searching #:isearch-start))

(in-package #:pine.editor.isearch)
(named-readtables:in-readtable pine.path:syntax)

;;;; Incremental search (C-s / C-r). The pending-key-reader intercept owns every
;;;; key while a search is live, re-installing itself until accept or abort.
;;;; Matching is per-line -- a pattern never spans a newline -- with smart case:
;;;; an all-lowercase pattern folds case, any capital makes it exact.
;;;;
;;;; What a search is, is at /isearch: the string, which way it is going, where
;;;; it started and where it is now. So it is visible, scriptable and resumable
;;;; like anything else, and a second window searching is a second space rather
;;;; than a fight over one variable. The captured lines are the exception --
;;;; they are the buffer's own seq, held while the search runs.

(defun at (leaf) (p:child /isearch leaf))

(defun searching () (ns:read /isearch/string))

(defun %get (leaf &optional default) (or (ns:read (at leaf)) default))
(defun %put (leaf value) (ns:write (at leaf) value))

(defun %fold-p (pattern) (notany #'upper-case-p pattern))

(defun %find (lines pattern from-line from-col direction)
  "(values line col) of the match start: forward finds the first match starting
at or after FROM, backward the last ending at or before it."
  (when (plusp (length pattern))
    (let* ((fold (%fold-p pattern))
           (pat (if fold (string-downcase pattern) pattern))
           (n (fset:size lines)))
      (flet ((hit (i start end from-end)
               (let* ((line (fset:@ lines i))
                      (s (if fold (string-downcase line) line)))
                 (search pat s
                         :start2 (max 0 (min start (length s)))
                         :end2 (max 0 (min end (length s)))
                         :from-end from-end))))
        (ecase direction
          (:forward
           (loop :for i :from (max 0 from-line) :below n
                 :for pos = (hit i (if (= i from-line) from-col 0)
                                 most-positive-fixnum nil)
                 :when pos :return (values i pos)))
          (:backward
           (loop :for i :from (min from-line (1- n)) :downto 0
                 :for pos = (hit i 0 (if (= i from-line) from-col
                                         most-positive-fixnum)
                                 t)
                 :when pos :return (values i pos))))))))

(defun %goto (line col)
  (let ((buf (frame:current-buffer)))
    (when buf (pine.buf:put-point buf line col))))

(defun %echo (&optional failing)
  (pine.echo:message
   (format nil "~:[~;Failing ~]~:[~;Wrapped ~]I-search~:[~; backward~]: ~a"
           failing (%get :wrapped)
           (eq (%get :direction :forward) :backward)
           (%get :string ""))))

(defun %land (line col)
  "Record the match and put point at its end (forward) or start (backward)."
  (ns:write {(at :match-line) line (at :match-col) col})
  (if (eq (%get :direction :forward) :forward)
      (%goto line (+ col (length (%get :string ""))))
      (%goto line col))
  (%echo))

(defun %failed ()
  (%put :match-line nil)
  (%echo t))

(defun %research (lines)
  "Search for the current string from the origin: the string just changed."
  (multiple-value-bind (line col)
      (%find lines (%get :string "") (%get :origin-line 0) (%get :origin-col 0)
             (%get :direction :forward))
    (if line (%land line col) (%failed))))

(defun %repeat (lines)
  "Advance past the current match, wrapping once at the end."
  (let ((dir (%get :direction :forward))
        (ml (%get :match-line))
        (mc (%get :match-col 0)))
    (multiple-value-bind (from-line from-col)
        (cond ((null ml)
               (if (eq dir :forward)
                   (values 0 0)
                   (values (1- (fset:size lines)) most-positive-fixnum)))
              ((eq dir :forward) (values ml (1+ mc)))
              (t (values ml mc)))
      (when (null ml) (%put :wrapped t))
      (multiple-value-bind (line col)
          (%find lines (%get :string "") from-line from-col dir)
        (if line (%land line col) (%failed))))))

(defun %printable-p (k)
  (and (= 1 (length (key:key-sym k)))
       (not (key:key-ctrl k))
       (not (key:key-meta k))
       (not (key:key-super k))
       (graphic-char-p (char (key:key-sym k) 0))))

(defun %exit (&key accept)
  (let ((string (%get :string "")))
    (when (and accept (plusp (length string)))
      (%put :last string)))
  (ns:write {(at :string) nil (at :match-line) nil (at :match-col) nil
             (at :wrapped) nil (at :direction) nil})
  (pine.echo:message ""))

(defun %reader (lines)
  "The key handler for a live search. LINES is the seq it is searching."
  (lambda (k)
    (let ((sym (key:key-sym k)))
      (flet ((again () (key:read-next-key (%reader lines))))
        (cond
          ;; abort: back to where the search began
          ((or (and (key:key-ctrl k) (string= sym "g")) (string= sym "Escape"))
           (%goto (%get :origin-line 0) (%get :origin-col 0))
           (%exit)
           (pine.echo:message "quit"))
          ((string= sym "Return") (%exit :accept t))
          ;; C-s / C-r inside the search: next match, or flip direction
          ((and (key:key-ctrl k) (member sym '("s" "r") :test #'string=))
           (%put :direction (if (string= sym "s") :forward :backward))
           (when (and (zerop (length (%get :string "")))
                      (plusp (length (%get :last ""))))
             (%put :string (%get :last "")))
           (%repeat lines)
           (again))
          ((string= sym "BackSpace")
           (let ((s (%get :string "")))
             (%put :string (if (plusp (length s)) (subseq s 0 (1- (length s))) s)))
           (%put :wrapped nil)
           (if (plusp (length (%get :string "")))
               (%research lines)
               (progn (%goto (%get :origin-line 0) (%get :origin-col 0)) (%echo)))
           (again))
          ((%printable-p k)
           (%put :string (concatenate 'string (%get :string "") sym))
           (%research lines)
           (again))
          ;; anything else accepts the search, then does its own job
          (t (%exit :accept t)
             (key:dispatch k)))))))

(defun isearch-start (direction)
  (let ((buf (frame:current-buffer)))
    (when buf
      (let* ((state (pine.buf:state-of buf))
             (snap (pine.text:state->snapshot state)))
        (ns:write {(at :string) ""
                   (at :direction) direction
                   (at :origin-line) (pine.text:point-line snap)
                   (at :origin-col) (pine.text:point-col snap)
                   (at :wrapped) nil})
        (%echo)
        (key:read-next-key (%reader (pine.text:lines state)))))))
