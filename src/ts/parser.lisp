(defpackage #:pine.ts.parser
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell) (#:task #:pine.run.task)
                    (#:node #:pine.fs.node) (#:mode #:pine.repl.mode)
                    (#:runtime #:pine.ts.runtime) (#:syntax #:pine.ts.syntax)
                    (#:hl #:pine.ts.highlight) (#:d #:pine.data))
  (:export #:parser #:parser-for #:parsers #:highlights #:note #:wait #:forget
           #:forget-all #:state-of #:language-of #:buffer-of #:*runtime*
           #:*on-parse*
           #:parsed #:indent))

(in-package #:pine.ts.parser)

(defvar *parsers* (c:cell (d:no-map)))
(defvar *runtime* nil)
(defvar *on-parse* nil)
(defparameter +settle+ 2)

(defclass parser ()
  ((buffer-of   :initarg :buffer   :reader buffer-of)
   (language-of :initarg :language :reader language-of)
   (state-of    :initarg :state    :reader state-of)
   (running     :initarg :task     :reader running)
   (found       :initform (c:cell nil) :reader found)
   (parsed      :initform (c:cell -1)  :reader parsed)))

(defmethod print-object ((p parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a ~(~a~) at ~d" (node:name (buffer-of p)) (language-of p)
            (c:held (parsed p)))))

(defun parsers () (d:vals (c:held *parsers*)))

(defun %lines (b)
  (d:as :list (c:held (pine.edit.buffer:lines b))))

(defun %recompute (p tick)
  (let ((ps (state-of p)))
    (runtime:parse-lines! ps (%lines (buffer-of p)))
    (c:put (found p) (hl:parse-highlights ps))
    (c:put (parsed p) tick)
    (when *on-parse* (funcall *on-parse* (buffer-of p)))
    (c:held (found p))))

(defun %receive (p message)
  (case (first message)
    (:parse (%recompute p (second message)))
    (:stop (runtime:free-parse-state (state-of p)))
    (t nil)))

(defun %grammar (b)
  (mode:setting (pine.edit.buffer:mode-of b) :grammar))

(defun %make (b language)
  (multiple-value-bind (lib fn) (syntax:grammar-of language)
   (let ((ps (and lib (runtime:make-parse-state *runtime* language lib fn
                                                :syntax (syntax:for language)))))
    (when ps
      (let ((p (make-instance 'parser :buffer b :language language :state ps
                                      :task nil)))
        (setf (slot-value p 'running)
              (task:actor (format nil "parse ~a" (node:name b))
                          (lambda (message) (%receive p message))))
        p)))))

(defun parser-for (b)
  (let ((language (%grammar b)))
    (when (and language *runtime*)
      (or (d:at (c:held *parsers*) (node:name b))
          (let ((p (%make b language)))
            (when p
              (c:swap *parsers* (lambda (all) (d:with all (node:name b) p)))
              (task:tell (running p) (list :parse (pine.edit.buffer:tick b)))
              p))))))

(defun note (b)
  (let ((p (parser-for b)))
    (when (and p (/= (c:held (parsed p)) (pine.edit.buffer:tick b)))
      (task:tell (running p) (list :parse (pine.edit.buffer:tick b))))
    p))

(defun highlights (b)
  (let ((p (note b)))
    (when p (c:held (found p)))))

(defun wait (b &key (seconds +settle+))
  (let ((p (note b)))
    (when p
      (loop :repeat (round (/ seconds 0.01))
            :until (= (c:held (parsed p)) (pine.edit.buffer:tick b))
            :do (sleep 0.01))
      (c:held (found p)))))

(defun indent (b line &key (width 2))
  (let ((p (note b)))
    (when p
      (wait b)
      (hl:parse-indent (state-of p) line :width width))))

(defun forget (b)
  (let* ((name (if (stringp b) b (node:name b)))
         (p (d:at (c:held *parsers*) name)))
    (when p
      (task:tell (running p) (list :stop))
      (task:stop (running p))
      (c:swap *parsers* (lambda (all) (d:without all name))))
    p))

(defun forget-all ()
  (dolist (p (parsers) (c:put *parsers* (d:no-map)))
    (task:tell (running p) (list :stop))
    (task:stop (running p))))
