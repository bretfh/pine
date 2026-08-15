(defpackage #:pine/ts/parser
  (:use #:cl)
  (:local-nicknames (#:meter #:pine/run/meter) (#:language #:pine/edit/language) (#:endpoint #:pine/run/endpoint)
                    (#:node #:pine/fs/node) (#:mode #:pine/repl/mode)
                    (#:runtime #:pine/ts/runtime) (#:syntax #:pine/ts/syntax)
                    (#:hl #:pine/ts/highlight) (#:d #:pine/data))
  (:export #:parser #:parser-for #:parsers #:highlights #:note #:wait #:forget
           #:forget-all #:state-of #:language-of #:buffer-of #:showing #:banded #:*runtime*
           #:*on-parse*
           #:parsed #:indent))
(in-package #:pine/ts/parser)

(defvar *parsers* (d:table))
(defvar *runtime* nil)
(defvar *on-parse* nil)
(defvar *counter* 0)
(defparameter +settle+ 2)

(defclass parser ()
  ((buffer-of   :initarg :buffer   :reader buffer-of)
   (language-of :initarg :language :reader language-of)
   (state-of    :initarg :state    :reader state-of)
   (running     :initarg :task     :reader running)
   (found       :initform (d:box (d:no-map)) :reader found)
   (banded      :initform (d:box nil) :reader banded)
   (parsed      :initform (d:box -1)  :reader parsed)))

(defmethod print-object ((p parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a ~(~a~) at ~d" (node:name (buffer-of p)) (language-of p)
            (d:held (parsed p)))))

(defun parsers () (d:vals (d:all *parsers*)))

(defun %lines (b) (d:held (pine/edit/buffer:lines b)))

(defun showing (b)
  "The band some window shows of B, or nil when none does. Past a few thousand
lines only that band is given to tree-sitter at all.

A screen either side of what is on screen, so paging lands on lines that were
walked already instead of on plain text waiting to be coloured."
  (let ((w (find b (pine/edit/window:windows) :key #'pine/edit/window:buffer-of)))
    (when w
      (let* ((from (pine/edit/window:scroll-of w))
             (height (max 1 (pine/edit/window:height-of w))))
        (cons (max 0 (- from height)) (+ from (* 2 height)))))))

(defun %kept (had edit)
  "What stays of the runs already walked. An edit moves every line under it, so
those go; a scroll moves nothing, so they all stay."
  (cond ((null had) (d:no-map))
        ((null edit) had)
        (t (let ((line (first (first edit)))
                 (out (d:no-map)))
             (d:do-map (at runs had out)
               (when (and line (< at line)) (setf out (d:with out at runs))))))))

(defun %merged (had runs band)
  "HAD with the band walked again: the lines in it are what the walk says now,
and the lines outside it are what they were."
  (let ((out had))
    (when band
      (d:do-map (at runs had)
        (declare (ignore runs))
        (when (and (>= at (car band)) (< at (cdr band)))
          (setf out (d:without out at)))))
    (dolist (run runs out)
      (destructuring-bind (line from to face) run
        (setf out (d:with out line (cons (list from to face)
                                         (or (d:at out line) nil))))))))

(defun %flat (map)
  (let ((out nil))
    (d:do-map (line runs map out)
      (dolist (run runs)
        (push (cons line run) out)))))

(defun %recompute (p tick)
  (meter:timing (:parse) (%parse p tick)))

(defun %parse (p tick)
  (let* ((b (buffer-of p))
         (ps (state-of p))
         (lines (%lines b))
         (edit (pine/edit/buffer:edit-of b))
         (band (showing b)))
    (setf (runtime:ps-package ps) (pine/edit/language:package-of b))
    (runtime:parse-lines! ps lines :edit (first edit) :from (second edit)
                                   :viewport band)
    (setf (pine/edit/buffer:edit-of b) nil)
    (let ((runs (if band
                    (hl:parse-highlights ps :from-line (car band) :to-line (cdr band))
                    (hl:parse-highlights ps))))
      (d:swap! (found p)
               (lambda (had) (%merged (%kept had edit) runs band))))
    (meter:counted :parse-lines (if band (- (cdr band) (car band))
                                   (pine/edit/buffer:line-count b)))
    (d:put! (banded p) band)
    (d:put! (parsed p) tick)
    (when *on-parse* (funcall *on-parse* b))
    (%flat (d:held (found p)))))

(defun %receive (p message)
  (case (first message)
    (:parse (%recompute p (second message)))
    (:stop (runtime:free-parse-state (state-of p)))
    (t nil)))

(defun %grammar (b)
  "Which language a buffer is parsed as: what it says it is written in, else
what its mode says."
  (or (syntax:for-readtable (pine/edit/language:readtable-of b))
      (mode:setting (pine/edit/buffer:mode-of b) :grammar)))

(defun %name-for (b)
  (format nil "parse-~a-~d" (node:name b) (incf *counter*)))

(defun %make (b language)
  (multiple-value-bind (lib fn) (syntax:grammar-of language)
   (let ((ps (and lib (runtime:make-parse-state *runtime* language lib fn
                                                :syntax (syntax:for language)))))
    (when ps
      (let ((p (make-instance 'parser :buffer b :language language :state ps
                                      :task nil)))
        (setf (slot-value p 'running)
              (endpoint:endpoint (%name-for b)
                           (lambda (message) (%receive p message))
                           :dispatcher :parse))
        p)))))

(defun %dispose (p)
  (endpoint:stop (running p))
  (runtime:free-parse-state (state-of p))
  p)

(defun parser-for (b)
  "The parser for B, made once. The thread drawing a frame and the one that
just finished a parse both ask, so the one that lands is the one everybody
gets and the other is freed rather than left holding a foreign parser."
  (let* ((language (%grammar b))
         (had (d:at (d:all *parsers*) (node:name b))))
    (when (and had (not (eq language (language-of had))))
      (forget b)
      (setf had nil))
    (when (and language *runtime*)
      (or had
          (let ((mine (%make b language)))
            (when mine
              (let ((kept (d:claim *parsers* (node:name b) mine)))
                (cond ((eq kept mine)
                       (endpoint:tell (running mine)
                                   (list :parse (pine/edit/buffer:tick b)))
                       mine)
                      (t (%dispose mine) kept)))))))))

(defun note (b)
  "Tell the parser it has fallen behind: the buffer moved, or the window is
showing lines it has not been asked about."
  (let ((p (parser-for b)))
    (when (and p (or (/= (d:held (parsed p)) (pine/edit/buffer:tick b))
                     (not (equal (d:held (banded p)) (showing b)))))
      (endpoint:tell (running p) (list :parse (pine/edit/buffer:tick b))))
    p))

(defun highlights (b)
  "Every run walked so far, the band just walked included. What was coloured
before stays coloured while the next band is being walked, so paging does not
blink through plain text."
  (meter:timing (:highlights)
    (let ((p (note b)))
      (when p (%flat (d:held (found p)))))))

(defun wait (b &key (seconds +settle+))
  (let ((p (note b)))
    (when p
      (loop :repeat (round (/ seconds 0.01))
            :until (and (= (d:held (parsed p)) (pine/edit/buffer:tick b))
                        (equal (d:held (banded p)) (showing b)))
            :do (sleep 0.01))
      (%flat (d:held (found p))))))

(defun indent (b line &key (width 2))
  (let ((p (note b)))
    (when p
      (wait b)
      (hl:parse-indent (state-of p) line :width width))))

(defun forget (b)
  (let* ((name (if (stringp b) b (node:name b)))
         (p (d:at (d:all *parsers*) name)))
    (when p
      (endpoint:tell (running p) (list :stop))
      (endpoint:stop (running p))
      (d:drop! *parsers* name))
    p))

(defun forget-all ()
  (dolist (p (parsers) (d:put! *parsers* (d:no-map)))
    (endpoint:tell (running p) (list :stop))
    (endpoint:stop (running p))))
