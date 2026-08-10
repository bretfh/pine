(defpackage #:pine.ts.parser
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell) (#:task #:pine.run.task)
                    (#:node #:pine.fs.node) (#:mode #:pine.repl.mode)
                    (#:runtime #:pine.ts.runtime) (#:syntax #:pine.ts.syntax)
                    (#:hl #:pine.ts.highlight) (#:d #:pine.data))
  (:export #:parser #:parser-for #:parsers #:highlights #:note #:wait #:forget
           #:forget-all #:state-of #:language-of #:buffer-of #:showing #:banded #:*runtime*
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
   (banded      :initform (c:cell nil) :reader banded)
   (parsed      :initform (c:cell -1)  :reader parsed)))

(defmethod print-object ((p parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a ~(~a~) at ~d" (node:name (buffer-of p)) (language-of p)
            (c:held (parsed p)))))

(defun parsers () (d:vals (c:held *parsers*)))

(defun %lines (b) (c:held (pine.edit.buffer:lines b)))

(defun showing (b)
  "The band some window shows of B, or nil when none does. Past a few thousand
lines only that band is given to tree-sitter at all."
  (let ((w (find b (pine.edit.window:windows) :key #'pine.edit.window:buffer-of)))
    (when w
      (let ((from (pine.edit.window:scroll-of w)))
        (cons from (+ from (max 1 (pine.edit.window:height-of w))))))))

(defun %recompute (p tick)
  (let* ((b (buffer-of p))
         (ps (state-of p))
         (lines (%lines b))
         (edit (pine.edit.buffer:edit-of b))
         (band (showing b)))
    (runtime:parse-lines! ps lines :edit (first edit) :from (second edit)
                                   :viewport band)
    (setf (pine.edit.buffer:edit-of b) nil)
    (c:put (banded p) band)
    (c:put (found p)
           (if band
               (hl:parse-highlights ps :from-line (car band) :to-line (cdr band))
               (hl:parse-highlights ps)))
    (c:put (parsed p) tick)
    (when *on-parse* (funcall *on-parse* b))
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
  "Tell the parser it has fallen behind: the buffer moved, or the window is
showing lines it has not been asked about."
  (let ((p (parser-for b)))
    (when (and p (or (/= (c:held (parsed p)) (pine.edit.buffer:tick b))
                     (not (equal (c:held (banded p)) (showing b)))))
      (task:tell (running p) (list :parse (pine.edit.buffer:tick b))))
    p))

(defun highlights (b)
  (let ((p (note b)))
    (when p (c:held (found p)))))

(defun wait (b &key (seconds +settle+))
  (let ((p (note b)))
    (when p
      (loop :repeat (round (/ seconds 0.01))
            :until (and (= (c:held (parsed p)) (pine.edit.buffer:tick b))
                        (equal (c:held (banded p)) (showing b)))
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
