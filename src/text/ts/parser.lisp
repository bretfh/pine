(defpackage #:pine/text/ts/parser
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:meter #:pine/run/meter) (#:job #:pine/run/job)
                    (#:actors #:pine/run/actors)
                    (#:mode #:pine/text/mode) (#:doc #:pine/text/document)
                    (#:language #:pine/text/language)
                    (#:runtime #:pine/text/ts/runtime)
                    (#:syntax #:pine/text/ts/syntax)
                    (#:hl #:pine/text/ts/highlight))
  (:export #:parser #:parser-for #:parsers #:highlights #:note #:forget
           #:forget-all #:state-of #:language-of #:document-of #:showing #:banded
           #:parsed #:indent #:motion #:currentp #:running
           #:*runtime* #:*on-parse* #:*showing*))
(in-package #:pine/text/ts/parser)

(defvar *parsers* (d:table))
(defvar *runtime* nil)
(defvar *on-parse* nil)
(defvar *showing* nil
  "A function answering the band of lines something is showing of a document, as
(FROM . TO), or nothing. The editor puts its windows here; the parse itself has no
business knowing what a window is.")
(defvar *counter* 0)


(defclass parser ()
  ((document-of :initarg :document :reader document-of)
   (language-of :initarg :language :reader language-of)
   (state-of    :initarg :state    :reader state-of)
   (running     :initarg :running  :reader running)
   (found       :initform (d:box (d:no-map)) :reader found)
   (banded      :initform (d:box nil) :reader banded)
   (parsed      :initform (d:box -1)  :reader parsed)))

(defmethod print-object ((p parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a ~(~a~) at ~d" (node:name (document-of p)) (language-of p)
            (d:held (parsed p)))))

(defun parsers () (d:vals (d:all *parsers*)))

(defun showing (document)
  "The band something shows of DOCUMENT. Past a few thousand lines only that band
is given to tree-sitter at all."
  (when *showing* (funcall *showing* document)))

(defun %kept (had edit)
  "What stays of the runs already walked. The lines the edit replaced go; the ones
above it are where they were, and the ones below it move by as many lines as the
edit added or took away."
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
  "HAD with the band walked again: the lines in it are what the walk says now, and
the lines outside it are what they were."
  (let ((out had))
    (when band
      (dolist (at (d:keys had))
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

(defun %parse (p tick)
  (let* ((document (document-of p))
         (ps (state-of p))
         (lines (d:held (doc:lines document)))
         (edit (doc:edit-of document))
         (band (showing document)))
    (setf (runtime:ps-package ps) (language:package-of document))
    (runtime:parse-lines! ps lines :edit (first edit) :from (second edit)
                                   :viewport band)
    (setf (doc:edit-of document) nil)
    (let ((runs (if band
                    (hl:parse-highlights ps :from-line (car band) :to-line (cdr band))
                    (hl:parse-highlights ps))))
      (d:swap! (found p)
               (lambda (had) (%merged (%kept had edit) runs band))))
    (meter:counted :parse-lines (if band (- (cdr band) (car band))
                                   (doc:line-count document)))
    (d:put! (banded p) band)
    (d:put! (parsed p) tick)
    (when *on-parse* (funcall *on-parse* document))
    (%flat (d:held (found p)))))

(defun currentp (p)
  (and (= (d:held (parsed p)) (doc:tick (document-of p)))
       (equal (d:held (banded p)) (showing (document-of p)))))

(defun %current (p tick)
  (unless (currentp p) (%parse p tick))
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
                        :for at := (hl:parse-indent (state-of p) line :width width)
                        :when at :collect (cons line at)))))
      (:motion
       (destructuring-bind (tick kind at col then) more
         (%current p tick)
         (multiple-value-bind (line where)
             (runtime:parse-motion (state-of p) kind at col)
           (when line (funcall then line where)))))
      (:stop (runtime:free-parse-state (state-of p)))
      (t (error "A parser has no handler for ~s." message)))))

(defun %grammar (document)
  "Which language a document is parsed as: what it says it is written in, else what
its mode says."
  (or (syntax:for-readtable (language:readtable-of document))
      (mode:setting (doc:mode-of document) :grammar)))

(defun %make (document language)
  (multiple-value-bind (lib fn) (syntax:grammar-of language)
    (let ((ps (and lib (runtime:make-parse-state *runtime* language lib fn
                                                 :syntax (syntax:for language)))))
      (when ps
        (let ((p (make-instance 'parser :document document :language language
                                        :state ps :running nil)))
          (setf (slot-value p 'running)
                (job:start
                 (make-instance 'job:actor
                                :name (format nil "parse-~a-~d"
                                              (node:name document) (incf *counter*))
                                :dispatcher :pinned
                                :receive (lambda (message) (%receive p message)))))
          p)))))

(defun %dispose (p)
  (job:stop (running p))
  (runtime:free-parse-state (state-of p))
  p)

(defun parser-for (document)
  "The parser for DOCUMENT, made once. The thread drawing a frame and the one that
just finished a parse both ask, so the one that lands is the one everybody gets and
the other is freed rather than left holding a foreign parser."
  (let* ((language (%grammar document))
         (had (d:at (d:all *parsers*) (node:name document))))
    (when (and had (not (eq language (language-of had))))
      (forget document)
      (setf had nil))
    (when (and language *runtime*)
      (or had
          (let ((mine (%make document language)))
            (when mine
              (let ((kept (d:claim *parsers* (node:name document) mine)))
                (cond ((eq kept mine)
                       (job:tell (running mine) (list :parse (doc:tick document)))
                       mine)
                      (t (%dispose mine) kept)))))))))

(defun note (document)
  "Tell the parser it has fallen behind: the document moved, or what shows it is
showing lines it has not been asked about."
  (let ((p (parser-for document)))
    (when (and p (or (/= (d:held (parsed p)) (doc:tick document))
                     (not (equal (d:held (banded p)) (showing document)))))
      (job:tell (running p) (list :parse (doc:tick document))))
    p))

(defun highlights (document)
  "Every run walked so far, the band just walked included. What was coloured before
stays coloured while the next band is being walked, so paging does not blink through
plain text."
  (meter:timing (:highlights)
    (let ((p (note document)))
      (when p (%flat (d:held (found p)))))))

(defun indent (document from to &key (width 2) then)
  (let ((p (note document)))
    (when p
      (job:tell (running p)
                (list :indent (doc:tick document) from to width then))
      p)))

(defun motion (document kind then)
  (let ((p (note document)))
    (when p
      (job:tell (running p)
                (list :motion (doc:tick document) kind
                      (doc:at-line document) (doc:at-col document) then))
      p)))

(defun forget (document)
  (let* ((name (if (stringp document) document (node:name document)))
         (p (d:at (d:all *parsers*) name)))
    (when p
      (job:tell (running p) (list :stop))
      (job:stop (running p))
      (d:drop! *parsers* name))
    p))

(defun forget-all ()
  (dolist (p (parsers) (d:clear! *parsers*))
    (job:tell (running p) (list :stop))
    (job:stop (running p))))
