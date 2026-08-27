(defpackage #:pine/text/lines
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:shadow #:delete #:search)
  (:export #:of #:joined #:line #:line-count #:clamp #:insert #:delete #:region
           #:move-by #:wordp #:indent #:search #:foldp
           #:*word-characters*))
(in-package #:pine/text/lines)

(defvar *word-characters* "-_*+/<>=?!%&")

(defun of (text)
  "TEXT as lines. Immutable with structural sharing, so an edit shares every line it
did not touch and two readers never see one change underneath them."
  (let ((split (uiop:split-string (or text "") :separator '(#\Newline))))
    (d:as :seq (or split (list "")))))

(defun joined (lines) (format nil "~{~a~^~%~}" (d:as :list lines)))

(defun line-count (lines) (d:size lines))

(defun line (lines n) (d:lookup lines n ""))

(defun clamp (lines at col)
  (let* ((n (max 0 (min at (max 0 (1- (d:size lines))))))
         (text (line lines n)))
    (values n (max 0 (min col (length text))))))

(defun %replace (lines from to fresh)
  (let ((n (d:size lines)))
    (d:append (d:append (d:subseq lines 0 (min from n)) fresh)
              (d:subseq lines (min (max from to) n) n))))

(defun insert (lines at col string)
  (multiple-value-bind (at col) (clamp lines at col)
    (let* ((text (line lines at))
           (before (subseq text 0 col))
           (after (subseq text col))
           (pieces (uiop:split-string string :separator '(#\Newline))))
      (if (null (cl:rest pieces))
          (values (d:with-at lines at
                             (concatenate 'string before (first pieces) after))
                  at (+ col (length (first pieces))))
          (let ((fresh (d:as :seq
                             (cons (concatenate 'string before (first pieces))
                                   (loop :for piece :in (cl:rest pieces)
                                         :for i :from 1
                                         :collect (if (= i (1- (length pieces)))
                                                      (concatenate 'string piece after)
                                                      piece))))))
            (values (%replace lines at (1+ at) fresh)
                    (+ at (1- (length pieces)))
                    (length (car (cl:last pieces)))))))))

(defun region (lines from-line from-col to-line to-col)
  (multiple-value-bind (from-line from-col) (clamp lines from-line from-col)
    (multiple-value-bind (to-line to-col) (clamp lines to-line to-col)
      (when (or (> from-line to-line)
                (and (= from-line to-line) (> from-col to-col)))
        (rotatef from-line to-line)
        (rotatef from-col to-col))
      (if (= from-line to-line)
          (subseq (line lines from-line) from-col to-col)
          (format nil "~a~%~{~a~%~}~a"
                  (subseq (line lines from-line) from-col)
                  (d:as :list (d:subseq lines (1+ from-line) to-line))
                  (subseq (line lines to-line) 0 to-col))))))

(defun delete (lines from-line from-col to-line to-col)
  (multiple-value-bind (from-line from-col) (clamp lines from-line from-col)
    (multiple-value-bind (to-line to-col) (clamp lines to-line to-col)
      (when (or (> from-line to-line)
                (and (= from-line to-line) (> from-col to-col)))
        (rotatef from-line to-line)
        (rotatef from-col to-col))
      (let ((taken (region lines from-line from-col to-line to-col))
            (joined (concatenate 'string
                                 (subseq (line lines from-line) 0 from-col)
                                 (subseq (line lines to-line) to-col))))
        (values (if (= from-line to-line)
                    (d:with-at lines from-line joined)
                    (%replace lines from-line (1+ to-line) (d:seq joined)))
                from-line from-col taken)))))

(defun wordp (ch) (or (alphanumericp ch) (find ch *word-characters*)))

(defun %step-char (lines at col n)
  (loop :repeat (abs n)
        :do (if (plusp n)
                (if (< col (length (line lines at)))
                    (incf col)
                    (when (< at (1- (d:size lines))) (incf at) (setf col 0)))
                (if (plusp col)
                    (decf col)
                    (when (plusp at)
                      (decf at)
                      (setf col (length (line lines at)))))))
  (values at col))

(defun %step-word (lines at col n)
  (loop :repeat (abs n)
        :do (let ((text (line lines at)))
              (if (plusp n)
                  (progn
                    (loop :while (and (< col (length text))
                                      (not (wordp (char text col))))
                          :do (incf col))
                    (loop :while (and (< col (length text))
                                      (wordp (char text col)))
                          :do (incf col)))
                  (progn
                    (loop :while (and (plusp col) (not (wordp (char text (1- col)))))
                          :do (decf col))
                    (loop :while (and (plusp col) (wordp (char text (1- col))))
                          :do (decf col))))))
  (values at col))

(defun move-by (unit lines at col n)
  (multiple-value-bind (at col) (clamp lines at col)
    (ecase unit
      (:char (%step-char lines at col n))
      (:word (%step-word lines at col n))
      (:line (clamp lines (+ at n) col))
      (:text (if (plusp n)
                 (clamp lines (1- (d:size lines)) most-positive-fixnum)
                 (values 0 0))))))

(defun indent (text)
  (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) text)
      (length text)))

(defun foldp (needle)
  "Whether a search ignores case: it does until you type a capital, which is how you
ask for an exact one."
  (notany #'upper-case-p needle))

(defun search (lines needle at col &key (forward t) (test nil test-p))
  (let ((test (if test-p test (if (foldp needle) #'char-equal #'char=))))
    (loop :with n := (d:size lines)
          :for i := at :then (if forward (1+ i) (1- i))
          :while (and (>= i 0) (< i n))
          :for text := (line lines i)
          :for from := (if (= i at) col (if forward 0 (length text)))
          :for found := (if forward
                            (cl:search needle text :test test
                                       :start2 (min from (length text)))
                            (cl:search needle text :test test :from-end t
                                       :end2 (min from (length text))))
          :when found :do (return (values i found)))))
