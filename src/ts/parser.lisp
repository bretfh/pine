(defpackage #:pine.ts.parser
  (:use #:cl)
  (:local-nicknames (#:agent #:pine.run.agent)
                    (#:node #:pine.fs.node) (#:mode #:pine.repl.mode)
                    (#:runtime #:pine.ts.runtime) (#:syntax #:pine.ts.syntax)
                    (#:hl #:pine.ts.highlight) (#:d #:pine.data))
  (:export #:parser #:parser-for #:parsers #:highlights #:note #:wait #:forget
           #:forget-all #:state-of #:language-of #:buffer-of #:showing #:banded #:*runtime*
           #:*on-parse*
           #:parsed #:indent))

(in-package #:pine.ts.parser)

(defvar *parsers* (d:table))
(defvar *runtime* nil)
(defvar *on-parse* nil)
(defparameter +settle+ 2)

(defclass parser ()
  ((buffer-of   :initarg :buffer   :reader buffer-of)
   (language-of :initarg :language :reader language-of)
   (state-of    :initarg :state    :reader state-of)
   (running     :initarg :task     :reader running)
   (found       :initform (d:box nil) :reader found)
   (banded      :initform (d:box nil) :reader banded)
   (parsed      :initform (d:box -1)  :reader parsed)))

(defmethod print-object ((p parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a ~(~a~) at ~d" (node:name (buffer-of p)) (language-of p)
            (d:held (parsed p)))))

(defun parsers () (d:vals (d:all *parsers*)))

(defun %lines (b) (d:held (pine.edit.buffer:lines b)))

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
    (setf (runtime:ps-package ps) (pine.edit.buffer:package-of b))
    (runtime:parse-lines! ps lines :edit (first edit) :from (second edit)
                                   :viewport band)
    (setf (pine.edit.buffer:edit-of b) nil)
    (d:put! (found p)
           (if band
               (hl:parse-highlights ps :from-line (car band) :to-line (cdr band))
               (hl:parse-highlights ps)))
    (d:put! (banded p) band)
    (d:put! (parsed p) tick)
    (when *on-parse* (funcall *on-parse* b))
    (d:held (found p))))

(defun %receive (p message)
  (case (first message)
    (:parse (%recompute p (second message)))
    (:stop (runtime:free-parse-state (state-of p)))
    (t nil)))

(defun %grammar (b)
  "Which language a buffer is parsed as: what it says it is written in, else
what its mode says."
  (or (syntax:for-readtable (pine.edit.buffer:readtable-of b))
      (mode:setting (pine.edit.buffer:mode-of b) :grammar)))

(defun %make (b language)
  (multiple-value-bind (lib fn) (syntax:grammar-of language)
   (let ((ps (and lib (runtime:make-parse-state *runtime* language lib fn
                                                :syntax (syntax:for language)))))
    (when ps
      (let ((p (make-instance 'parser :buffer b :language language :state ps
                                      :task nil)))
        (setf (slot-value p 'running)
              (agent:agent (format nil "parse-~a" (node:name b))
                           (lambda (message) (%receive p message))
                           :dispatcher :parse))
        p)))))

(defun parser-for (b)
  (let* ((language (%grammar b))
         (had (d:at (d:all *parsers*) (node:name b))))
    (when (and had (not (eq language (language-of had))))
      (forget b)
      (setf had nil))
    (when (and language *runtime*)
      (or had
          (let ((p (%make b language)))
            (when p
              (d:keep! *parsers* (node:name b) p)
              (agent:tell (running p) (list :parse (pine.edit.buffer:tick b)))
              p))))))

(defun note (b)
  "Tell the parser it has fallen behind: the buffer moved, or the window is
showing lines it has not been asked about."
  (let ((p (parser-for b)))
    (when (and p (or (/= (d:held (parsed p)) (pine.edit.buffer:tick b))
                     (not (equal (d:held (banded p)) (showing b)))))
      (agent:tell (running p) (list :parse (pine.edit.buffer:tick b))))
    p))

(defun highlights (b)
  (let ((p (note b)))
    (when p (d:held (found p)))))

(defun wait (b &key (seconds +settle+))
  (let ((p (note b)))
    (when p
      (loop :repeat (round (/ seconds 0.01))
            :until (and (= (d:held (parsed p)) (pine.edit.buffer:tick b))
                        (equal (d:held (banded p)) (showing b)))
            :do (sleep 0.01))
      (d:held (found p)))))

(defun indent (b line &key (width 2))
  (let ((p (note b)))
    (when p
      (wait b)
      (hl:parse-indent (state-of p) line :width width))))

(defun forget (b)
  (let* ((name (if (stringp b) b (node:name b)))
         (p (d:at (d:all *parsers*) name)))
    (when p
      (agent:tell (running p) (list :stop))
      (agent:stop (running p))
      (d:drop! *parsers* name))
    p))

(defun forget-all ()
  (dolist (p (parsers) (d:put! *parsers* (d:no-map)))
    (agent:tell (running p) (list :stop))
    (agent:stop (running p))))
