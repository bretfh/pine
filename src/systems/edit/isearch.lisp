(in-package #:pine/edit)

(defvar *search* nil)
(defvar *last* "")

(defclass a-search ()
  ((of      :initarg :of      :reader of)
   (needle  :initarg :needle  :accessor needle  :initform "")
   (forward :initarg :forward :accessor forward :initform t)
   (from    :initarg :from    :reader from)
   (wrapped :initform nil     :accessor wrapped)))

(defmethod print-object ((s a-search) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~:[back~;forward~] ~s" (forward s) (needle s))))

(defun searching () *search*)

(defun %look (s line col &key (forward (forward s)))
  (text:find-in (text:lines (of s)) (needle s) line col :forward forward))

(defun %found (s)
  (or (zerop (length (needle s)))
      (and (%look s (text:at-line (of s)) (text:at-col (of s))) t)))

(defun banner (&optional (s (searching)))
  (when s
    (format nil "~:[Failing ~;~]I-search~:[ backward~;~]~:[~; [wrapped]~]: ~a"
            (%found s) (forward s) (wrapped s) (needle s))))

(defun %show (s)
  (log:note "~a" (banner s))
  s)

(defun %land (s line col)
  (let ((buffer (of s)))
    (text:goto buffer line col)
    (text:forget-spans buffer)
    (text:span buffer line col (+ col (length (needle s))) :match)))

(defun %seek (s &key (from-point t))
  (let* ((buffer (of s))
         (line (if from-point (text:at-line buffer) 0))
         (col (if from-point (text:at-col buffer) 0)))
    (multiple-value-bind (at-line at-col) (%look s line col)
      (cond (at-line (%land s at-line at-col))
            (t (multiple-value-bind (wrap-line wrap-col)
                   (%look s (if (forward s) 0 (1- (text:line-count buffer)))
                          (if (forward s) 0 most-positive-fixnum))
                 (when wrap-line
                   (setf (wrapped s) t)
                   (%land s wrap-line wrap-col))))))
    (%show s)))

(defun %grow (s said)
  (setf (needle s) (concatenate 'string (needle s) said)
        (wrapped s) nil)
  (%seek s)
  :again)

(defun %shrink (s)
  (let ((had (needle s)))
    (setf (needle s) (subseq had 0 (max 0 (1- (length had))))
          (wrapped s) nil)
    (if (zerop (length (needle s)))
        (progn (text:forget-spans (of s)) (%show s))
        (%seek s :from-point nil))
    :again))

(defun step-search (s forward)
  (setf (forward s) forward)
  (when (zerop (length (needle s)))
    (setf (needle s) *last*))
  (unless (zerop (length (needle s)))
    (let ((buffer (of s)))
      (text:goto buffer (text:at-line buffer)
                (max 0 (+ (text:at-col buffer) (if forward 1 -1))))))
  (%seek s)
  :again)

(defun took (s &key (keep t))
  (let ((buffer (of s)))
    (text:forget-spans buffer)
    (unless keep (text:goto buffer (first (from s)) (second (from s))))
    (when (plusp (length (needle s))) (setf *last* (needle s)))
    (setf *search* nil)
    (log:note "~:[quit~;~a~]" keep (needle s))
    buffer))

(defun %reading (k)
  (let ((s (searching)))
    (cond
      ((null s) nil)
      ((ui:key= k (ui:parse "C-s")) (step-search s t))
      ((ui:key= k (ui:parse "C-r")) (step-search s nil))
      ((ui:key= k (ui:parse "C-g")) (took s :keep nil) nil)
      ((or (ui:key= k (ui:parse "RET")) (ui:key= k (ui:parse "Escape")))
       (took s)
       nil)
      ((ui:key= k (ui:parse "DEL")) (%shrink s))
      ((ui:selfp k) (%grow s (ui:typed k)))
      (t (took s) (dispatch k) nil))))

(defun start (&key (forward t))
  (let* ((buffer (text:current))
         (s (make-instance 'a-search :of buffer :forward forward
                                     :from (text:point buffer))))
    (setf *search* s)
    (ui:take-next #'%reading)
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
  (let ((buffer (text:current))
        (from (princ-to-string from)))
    (if (zerop (length from))
        (log:note "there is nothing to replace")
        (progn
          (ask (format nil "Replace ~a with: " from)
               :then (lambda (to)
                       (let ((n 0))
                         (loop
                           (multiple-value-bind (line col)
                               (text:find-in (text:lines buffer) from
                                             (text:at-line buffer)
                                             (text:at-col buffer))
                             (unless line (return))
                             (text:goto buffer line col)
                             (text:delete-region buffer line col line
                                                 (+ col (length from)))
                             (text:insert buffer to)
                             (incf n)))
                         (log:note "replaced ~d" n)
                         n)))
          :asking))))

