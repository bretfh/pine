(in-package #:pine/text)

(defvar *runtime* nil)
(defvar *counter* 0)

(defclass parser ()
  ((buffer-of :initarg :buffer :reader buffer-of)
   (language-of :initarg :language :reader language-of)
   (state-of    :initarg :state    :reader state-of)
   (running     :initarg :running  :reader running)
   (found       :initform (d:no-map) :reader found)
   (banded      :initform nil :accessor banded)
   (parsed      :initform -1  :accessor parsed)))

(defmethod print-object ((p parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a ~(~a~) at ~d" (fs:name (buffer-of p)) (language-of p)
            (parsed p))))

(defun parsers () (remove nil (mapcar #'parser (buffers))))

(defgeneric band (buffer)
  (:method (buffer) (declare (ignore buffer)) nil))

(defgeneric reparsed (buffer)
  (:method (buffer) (declare (ignore buffer)) nil))

(defun %kept (had edit)
  (cond ((null had) (d:no-map))
        ((null edit) had)
        (t (destructuring-bind (at old new bytes) (first edit)
             (declare (ignore bytes))
             (let ((delta (- new old))
                   (out (d:no-map)))
               (d:do-map (line runs had out)
                 (cond ((< line at) (setf out (d:with out line runs)))
                       ((< line (+ at old)))
                       (t (setf out (d:with out (+ line delta) runs))))))))))

(defun %merged (had runs band)
  (let ((out had))
    (when band
      (dolist (at (d:keys had))
        (when (and (>= at (car band)) (<= at (cdr band)))
          (setf out (d:without out at)))))
    (dolist (run runs out)
      (destructuring-bind (line from to face) run
        (setf out (d:with out line (cons (list from to face)
                                         (or (d:lookup out line) nil))))))))

(defun %flat (map)
  (let ((out nil))
    (d:do-map (line runs map out)
      (dolist (run runs)
        (push (cons line run) out)))))

(defun %parse (p tick)
  (let* ((buffer (buffer-of p))
         (ps (state-of p))
         (lines (lines buffer))
         (edit (edit-of buffer))
         (shown (band buffer)))
    (setf (ps-package ps) (package-of buffer))
    (parse-lines! ps lines :edit (first edit) :from (second edit)
                                   :viewport shown)
    (setf (edit-of buffer) nil)
    (let ((runs (if shown
                    (parse-highlights ps :from-line (car shown)
                                            :to-line (cdr shown))
                    (parse-highlights ps))))
      (sb-ext:atomic-update (slot-value p 'found)
               (lambda (had) (%merged (%kept had edit) runs shown))))
    (meter:counted :parse-lines (if shown (- (cdr shown) (car shown))
                                   (line-count buffer)))
    (setf (banded p) shown)
    (setf (parsed p) tick)
    (reparsed buffer)
    (%flat (found p))))

(defun freshp (p)
  (and (= (parsed p) (tick (buffer-of p)))
       (equal (banded p) (band (buffer-of p)))))

(defun %current (p tick)
  (unless (freshp p) (%parse p tick))
  p)

(defun %receive (p message)
  (destructuring-bind (tag &rest more) message
    (case tag
      (:parse (meter:timing (:parse) (%parse p (first more))))
      (:indent
       (destructuring-bind (tick from to width then) more
         (%current p tick)
         (funcall then
                  (loop :for line :from from :to to
                        :for at := (parse-indent (state-of p) line :width width)
                        :when at :collect (cons line at)))))
      (:motion
       (destructuring-bind (tick kind at col then) more
         (%current p tick)
         (multiple-value-bind (line where)
             (parse-motion (state-of p) kind at col)
           (when line (funcall then line where)))))
      (:stop (free-parse-state (state-of p)))
      (t (error "A parser has no handler for ~s." message)))))

(defun %grammar (buffer)
  (or (for-readtable (readtable-of buffer))
      (mode:says (mode-of buffer) :grammar nil)))

(defun %make (buffer language)
  (multiple-value-bind (lib fn) (grammar-of language)
    (let ((ps (and lib (make-parse-state *runtime* language lib fn
                                                 :syntax (for language)))))
      (when ps
        (let ((p (make-instance 'parser :buffer buffer :language language
                                        :state ps :running nil)))
          (setf (slot-value p 'running)
                (job:start
                 (make-instance 'job:actor
                                :name (format nil "parse-~a-~d"
                                              (fs:name buffer)
                                              (sb-ext:atomic-update *counter* (lambda (old) (1+ old))))
                                :dispatcher :pinned
                                :receive (lambda (message) (%receive p message)))))
          p)))))

(defun %dispose (p)
  (job:stop (running p))
  (free-parse-state (state-of p))
  p)

(defun parser-for (buffer)
  (let* ((language (%grammar buffer))
         (had (parser buffer)))
    (when (and had (not (eq language (language-of had))))
      (forget buffer)
      (setf had nil))
    (when (and language *runtime*)
      (or had
          (let ((mine (%make buffer language)))
            (when mine
              (cond ((d:cas-p (slot-value buffer 'parser) nil mine)
                     (job:tell (running mine) (list :parse (tick buffer)))
                     mine)
                    (t (%dispose mine) (parser buffer)))))))))

(defun note (buffer)
  (let ((p (parser-for buffer)))
    (when (and p (or (/= (parsed p) (tick buffer))
                     (not (equal (banded p) (band buffer)))))
      (job:tell (running p) (list :parse (tick buffer))))
    p))

(defun highlights (buffer)
  (meter:timing (:highlights)
    (let ((p (note buffer)))
      (when p (%flat (found p))))))

(defun indent (buffer from to &key (width 2) then)
  (let ((p (note buffer)))
    (when p
      (job:tell (running p)
                (list :indent (tick buffer) from to width then))
      p)))

(defun motion (buffer kind then)
  (let ((p (note buffer)))
    (when p
      (job:tell (running p)
                (list :motion (tick buffer) kind
                      (at-line buffer) (at-col buffer) then))
      p)))

(defun forget (buffer)
  (let* ((doc (if (stringp buffer) (fs:at "/text" buffer) buffer))
         (p (and doc (parser doc))))
    (when p
      (setf (parser doc) nil)
      (%dispose p)
      (job:forget (job:name (running p))))
    p))

(defmethod killing :after ((buffer buffer))
  (forget buffer))

(defun forget-all ()
  (dolist (doc (buffers) t) (forget doc)))
