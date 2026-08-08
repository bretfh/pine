(defpackage #:pine.edit.render
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:node #:pine.fs.node) (#:build #:pine.ui.build)
                    (#:cells #:pine.ui.cells) (#:layout #:pine.ui.layout)
                    (#:buffer #:pine.edit.buffer) (#:window #:pine.edit.window)
                    (#:mode #:pine.repl.mode) (#:syntax #:pine.ts.syntax)
                    (#:hl #:pine.ts.highlight))
  (:export #:buffer-tree #:window-tree #:frame-tree #:rows #:modeline
           #:visible-lines #:scroll-to-point #:*runtime* #:highlights-for
           #:indent-for #:parse-state-for #:forget-parse))

(in-package #:pine.edit.render)

(defvar *states* (make-hash-table :test 'equal))
(defvar *runtime* nil)

(defun visible-lines (b from height)
  (loop :for n :from from :below (min (buffer:line-count b) (+ from height))
        :collect (buffer:line b n)))

(defun scroll-to-point (w)
  (let* ((b (window:buffer-of w))
         (line (buffer:point-line b))
         (from (window:scroll-of w))
         (height (max 1 (window:height-of w))))
    (setf (window:scroll-of w)
          (cond ((< line from) line)
                ((>= line (+ from height)) (1+ (- line height)))
                (t from)))))

(defun highlights-for (b)
  (let ((grammar (mode:setting (buffer:mode-of b) :grammar)))
    (when (and grammar *runtime*)
      (ignore-errors
       (syntax:compute-highlights *runtime* grammar (buffer:text-of b))))))

(defun %face-at (highlights line col)
  (loop :for (at from to face) :in highlights
        :when (and (= at line) (>= col from) (< col to))
          :do (return face)))

(defun buffer-tree (w)
  (let* ((b (window:buffer-of w))
         (from (window:scroll-of w))
         (height (max 1 (window:height-of w)))
         (highlights (highlights-for b)))
    (apply #'build:column
           :align :stretch
           (loop :for text :in (visible-lines b from height)
                 :for line :from from
                 :collect (if highlights
                              (apply #'build:row
                                     (%runs text line highlights))
                              (build:label text))))))

(defun %runs (text line highlights)
  (let ((out nil) (col 0))
    (loop :while (< col (length text))
          :for face := (%face-at highlights line col)
          :for end := (or (loop :for i :from (1+ col) :below (length text)
                                :unless (eq face (%face-at highlights line i))
                                  :do (return i))
                          (length text))
          :do (push (build:label (subseq text col end) :face face) out)
              (setf col end))
    (or (nreverse out) (list (build:label "")))))

(defun modeline (w)
  (let ((b (window:buffer-of w)))
    (build:label
     (format nil " ~a  ~a  L~d C~d"
             (node:name b)
             (or (mode:setting (buffer:mode-of b) :indicator) (buffer:mode-of b))
             (1+ (buffer:point-line b))
             (buffer:point-col b))
     :face :modeline)))

(defun window-tree (w)
  (scroll-to-point w)
  (build:column :align :stretch
                (buffer-tree w)
                (modeline w)))

(defun frame-tree (&key (echo ""))
  (let ((windows (window:windows)))
    (apply #'build:column :align :stretch
           (append (mapcar #'window-tree windows)
                   (list (build:label echo))))))

(defun rows (&key (width 80) (height 24) (echo ""))
  (let ((tree (frame-tree :echo echo)))
    (cells:render tree width :height height)))

(defun forget-parse (b)
  (let ((ps (gethash (node:name b) *states*)))
    (when ps
      (pine.ts.runtime:free-parse-state ps)
      (remhash (node:name b) *states*))))

(defun parse-state-for (b)
  (let ((grammar (mode:setting (buffer:mode-of b) :grammar)))
    (when (and grammar *runtime*)
      (let ((ps (gethash (node:name b) *states*)))
        (unless ps
          (multiple-value-bind (lib fn) (syntax:grammar-of grammar)
            (when lib
              (setf ps (pine.ts.runtime:make-parse-state
                        *runtime* grammar lib fn :syntax (syntax:for grammar))
                    (gethash (node:name b) *states*) ps))))
        (when ps
          (pine.ts.runtime:parse-lines! ps (d:as :list (pine.run.cell:held
                                                        (buffer:lines b))))
          ps)))))

(defun indent-for (b line)
  (let ((width (or (mode:setting (buffer:mode-of b) :indent) 2))
        (ps (parse-state-for b)))
    (or (and ps (hl:parse-indent ps line :width width))
        (and (plusp line) (buffer:indent-of b (1- line)))
        0)))
