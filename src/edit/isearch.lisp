(defpackage #:pine.edit.isearch
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:cmd #:pine.repl.command)
                    (#:mode #:pine.repl.mode) (#:node #:pine/fs/node)
                    (#:text #:pine.edit.text) (#:buffer #:pine.edit.buffer)
                    (#:key #:pine.edit.key) (#:log #:pine/run/log))
  (:export #:install #:searching #:start #:step! #:took #:took-all #:said #:*search*
           #:needle #:forward #:wrapped))

(in-package #:pine.edit.isearch)

(defvar *search* (d:box nil))
(defvar *last* (d:box ""))

(defclass standing ()
  ((of      :initarg :of      :reader of)
   (needle  :initarg :needle  :accessor needle  :initform "")
   (forward :initarg :forward :accessor forward :initform t)
   (from    :initarg :from    :reader from)
   (wrapped :initform nil     :accessor wrapped)))

(defmethod print-object ((s standing) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~:[back~;forward~] ~s" (forward s) (needle s))))

(defun searching () (d:held *search*))

(defun said (&optional (s (searching)))
  (when s
    (format nil "~:[Failing ~;~]I-search~:[ backward~;~]~:[~; [wrapped]~]: ~a"
            (%found s) (forward s) (wrapped s) (needle s))))

(defun %found (s)
  (or (zerop (length (needle s)))
      (multiple-value-bind (line col) (%look s (buffer:point-line (of s))
                                             (buffer:point-col (of s)))
        (declare (ignore col))
        (and line t))))

(defun %look (s line col &key (forward (forward s)))
  (text:search (d:held (buffer:lines (of s))) (needle s) line col :forward forward))

(defun %show (s)
  (log:note "~a" (said s))
  s)

(defun %land (s line col)
  (let ((b (of s)))
    (buffer:goto! b line col)
    (buffer:clear-properties! b)
    (buffer:propertize! b line col (+ col (length (needle s))) '(:face :match))))

(defun %seek (s &key (from-point t))
  "Look from where point is, and wrap once rather than failing at the end."
  (let* ((b (of s))
         (line (if from-point (buffer:point-line b) 0))
         (col (if from-point (buffer:point-col b) 0)))
    (multiple-value-bind (at-line at-col) (%look s line col)
      (cond (at-line (%land s at-line at-col))
            (t (multiple-value-bind (wrap-line wrap-col)
                   (%look s (if (forward s) 0 (1- (buffer:line-count b)))
                          (if (forward s) 0 most-positive-fixnum))
                 (when wrap-line
                   (setf (wrapped s) t)
                   (%land s wrap-line wrap-col))))))
    (%show s)))

(defun %grow (s ch)
  (setf (needle s) (concatenate 'string (needle s) (string ch)))
  (setf (wrapped s) nil)
  (%seek s)
  :again)

(defun %shrink (s)
  (let ((had (needle s)))
    (setf (needle s) (subseq had 0 (max 0 (1- (length had)))))
    (setf (wrapped s) nil)
    (if (zerop (length (needle s)))
        (progn (buffer:clear-properties! (of s)) (%show s))
        (%seek s :from-point nil))
    :again))

(defun step! (s forward)
  "Again, in this direction. With nothing typed yet, the last search comes back."
  (setf (forward s) forward)
  (when (zerop (length (needle s)))
    (setf (needle s) (d:held *last*)))
  (unless (zerop (length (needle s)))
    (let ((b (of s)))
      (buffer:goto! b (buffer:point-line b)
                    (max 0 (+ (buffer:point-col b) (if forward 1 -1))))))
  (%seek s)
  :again)

(defun took (s &key (keep t))
  "End the search: keep where it landed, or go back to where it started."
  (let ((b (of s)))
    (buffer:clear-properties! b)
    (unless keep (buffer:goto! b (first (from s)) (second (from s))))
    (when (plusp (length (needle s))) (d:put! *last* (needle s)))
    (d:put! *search* nil)
    (log:note "~:[quit~;~a~]" keep (needle s))
    b))

(defun %reading (k)
  (let ((s (searching)))
    (cond
      ((null s) nil)
      ((key:key= k (key:parse-key "C-s")) (step! s t))
      ((key:key= k (key:parse-key "C-r")) (step! s nil))
      ((key:key= k (key:parse-key "C-g")) (took s :keep nil) nil)
      ((or (key:key= k (key:parse-key "RET"))
           (key:key= k (key:parse-key "Return"))
           (key:key= k (key:parse-key "Escape")))
       (took s)
       nil)
      ((or (key:key= k (key:parse-key "DEL"))
           (key:key= k (key:parse-key "BackSpace")))
       (%shrink s))
      ((key:self-insert-p k) (%grow s (char (key:key-sym k) 0)))
      (t (took s) (key:dispatch nil k) nil))))

(defun start (&key (forward t))
  "Search as you type. Every key narrows it, C-s and C-r step and turn round,
RET keeps where it landed and C-g goes back."
  (let* ((b (buffer:current))
         (s (make-instance 'standing :of b :forward forward
                                     :from (buffer:point b))))
    (d:put! *search* s)
    (key:take-next #'%reading)
    (%show s)
    s))

(defun took-all ()
  (let ((s (searching)))
    (when s (ignore-errors (took s)))
    (d:put! *search* nil))
  t)

(defun install ()
  (cmd:defcommand "isearch-forward" () (:describes "search as you type")
    (and (start :forward t) :searching))
  (cmd:defcommand "isearch-backward" () (:describes "search back as you type")
    (and (start :forward nil) :searching))
  (mode:bind "text" "C-s" "isearch-forward")
  (mode:bind "text" "C-r" "isearch-backward")
  t)
