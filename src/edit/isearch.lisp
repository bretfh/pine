(defpackage #:pine/edit/isearch
  (:use #:cl)
  (:local-nicknames (#:fault #:pine/run/fault)
                    (#:d #:pine/data) (#:lines #:pine/text/lines)
                    (#:command #:pine/run/command) (#:prompt #:pine/edit/prompt)
                    (#:doc #:pine/text/document) (#:key #:pine/ui/key)
                    (#:keys #:pine/edit/keys) (#:render #:pine/edit/render)
                    (#:log #:pine/run/log))
  (:export #:searching #:start #:step-search #:took #:took-all #:said
           #:needle #:forward #:wrapped))
(in-package #:pine/edit/isearch)

(defvar *search* nil)
(defvar *last* "")

(defclass standing ()
  ((of      :initarg :of      :reader of)
   (needle  :initarg :needle  :accessor needle  :initform "")
   (forward :initarg :forward :accessor forward :initform t)
   (from    :initarg :from    :reader from)
   (wrapped :initform nil     :accessor wrapped))
  (:documentation "A search as it stands: what has been typed, which way it is
going, and where it started so quitting can go back."))

(defmethod print-object ((s standing) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~:[back~;forward~] ~s" (forward s) (needle s))))

(defun searching () *search*)

(defun %look (s line col &key (forward (forward s)))
  (lines:search (doc:lines (of s)) (needle s) line col :forward forward))

(defun %found (s)
  (or (zerop (length (needle s)))
      (and (%look s (doc:at-line (of s)) (doc:at-col (of s))) t)))

(defun said (&optional (s (searching)))
  (when s
    (format nil "~:[Failing ~;~]I-search~:[ backward~;~]~:[~; [wrapped]~]: ~a"
            (%found s) (forward s) (wrapped s) (needle s))))

(defun %show (s)
  (log:note "~a" (said s))
  s)

(defun %land (s line col)
  (let ((document (of s)))
    (doc:goto document line col)
    (doc:forget-spans document)
    (doc:span document line col (+ col (length (needle s))) :match)))

(defun %seek (s &key (from-point t))
  "Look from where point is, and wrap once rather than failing at the end."
  (let* ((document (of s))
         (line (if from-point (doc:at-line document) 0))
         (col (if from-point (doc:at-col document) 0)))
    (multiple-value-bind (at-line at-col) (%look s line col)
      (cond (at-line (%land s at-line at-col))
            (t (multiple-value-bind (wrap-line wrap-col)
                   (%look s (if (forward s) 0 (1- (doc:line-count document)))
                          (if (forward s) 0 most-positive-fixnum))
                 (when wrap-line
                   (setf (wrapped s) t)
                   (%land s wrap-line wrap-col))))))
    (%show s)))

(defun %grow (s ch)
  (setf (needle s) (concatenate 'string (needle s) (string ch))
        (wrapped s) nil)
  (%seek s)
  :again)

(defun %shrink (s)
  (let ((had (needle s)))
    (setf (needle s) (subseq had 0 (max 0 (1- (length had))))
          (wrapped s) nil)
    (if (zerop (length (needle s)))
        (progn (doc:forget-spans (of s)) (%show s))
        (%seek s :from-point nil))
    :again))

(defun step-search (s forward)
  "Again, in this direction. With nothing typed yet, the last search comes back."
  (setf (forward s) forward)
  (when (zerop (length (needle s)))
    (setf (needle s) *last*))
  (unless (zerop (length (needle s)))
    (let ((document (of s)))
      (doc:goto document (doc:at-line document)
                (max 0 (+ (doc:at-col document) (if forward 1 -1))))))
  (%seek s)
  :again)

(defun took (s &key (keep t))
  "End the search: keep where it landed, or go back to where it started."
  (let ((document (of s)))
    (doc:forget-spans document)
    (unless keep (doc:goto document (first (from s)) (second (from s))))
    (when (plusp (length (needle s))) (setf *last* (needle s)))
    (setf *search* nil)
    (log:note "~:[quit~;~a~]" keep (needle s))
    document))

(defun %reading (k)
  (let ((s (searching)))
    (cond
      ((null s) nil)
      ((key:key= k (key:parse "C-s")) (step-search s t))
      ((key:key= k (key:parse "C-r")) (step-search s nil))
      ((key:key= k (key:parse "C-g")) (took s :keep nil) nil)
      ((or (key:key= k (key:parse "RET")) (key:key= k (key:parse "Escape")))
       (took s)
       nil)
      ((key:key= k (key:parse "DEL")) (%shrink s))
      ((key:selfp k) (%grow s (char (key:sym k) 0)))
      (t (took s) (keys:dispatch k) nil))))

(defun start (&key (forward t))
  "Search as you type. Every key narrows it, C-s and C-r step and turn round, RET
keeps where it landed and C-g goes back."
  (let* ((document (doc:current))
         (s (make-instance 'standing :of document :forward forward
                                     :from (doc:point document))))
    (setf *search* s)
    (key:take-next #'%reading)
    (%show s)
    s))

(defun took-all ()
  (let ((s (searching)))
    (when s (fault:or-nothing "a search already given up on is given up on"
              (took s)))
    (setf *search* nil))
  t)

(command:defcommand "isearch-forward" ()
    (:describes "search as you type" :on '(text "C-s"))
  (and (start :forward t) :searching))

(command:defcommand "isearch-backward" ()
    (:describes "search back as you type" :on '(text "C-r"))
  (and (start :forward nil) :searching))

(command:defcommand "query-replace" (from)
    (:describes "replace one string with another"
     :asks '((:prompt "Replace: "))
     :on '(text "M-%"))
  (let ((document (doc:current))
        (from (princ-to-string from)))
    (prompt:ask (format nil "Replace ~a with: " from)
                :then (lambda (to)
                        (let ((n 0))
                          (loop
                            (multiple-value-bind (line col)
                                (lines:search (doc:lines document) from
                                              (doc:at-line document)
                                              (doc:at-col document))
                              (unless line (return))
                              (doc:goto document line col)
                              (doc:delete-region document line col line
                                                 (+ col (length from)))
                              (doc:insert document to)
                              (incf n)))
                          (log:note "replaced ~d" n)
                          n)))
    :asking))
