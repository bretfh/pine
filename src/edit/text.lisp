(defpackage #:pine.edit.text
  (:use #:cl)
  (:shadow #:delete #:search)
  (:export #:lines-of #:text-of #:line-at #:line-count #:clamp
           #:insert #:delete #:region #:newline
           #:move-by #:word-char-p #:indent-width #:search
           #:beginning-of-line #:end-of-line #:*word-characters*))

(in-package #:pine.edit.text)

(defvar *word-characters* "-_*+/<>=?!%&")

(defun lines-of (text)
  (let ((split (uiop:split-string (or text "") :separator '(#\Newline))))
    (coerce (or split (list "")) 'vector)))

(defun text-of (lines)
  (format nil "~{~a~^~%~}" (coerce lines 'list)))

(defun line-count (lines) (length lines))

(defun line-at (lines n)
  (if (and (>= n 0) (< n (length lines))) (aref lines n) ""))

(defun clamp (lines line col)
  (let* ((n (max 0 (min line (max 0 (1- (length lines))))))
         (text (line-at lines n)))
    (values n (max 0 (min col (length text))))))

(defun %replace (lines from to fresh)
  (let ((n (length lines)))
    (concatenate 'vector (subseq lines 0 (min from n)) fresh
                 (subseq lines (min (max from to) n)))))

(defun insert (lines line col string)
  (multiple-value-bind (line col) (clamp lines line col)
    (let* ((text (line-at lines line))
           (before (subseq text 0 col))
           (after (subseq text col))
           (pieces (uiop:split-string string :separator '(#\Newline))))
      (if (null (rest pieces))
          (values (%replace lines line (1+ line)
                            (vector (concatenate 'string before (first pieces) after)))
                  line
                  (+ col (length (first pieces))))
          (let ((fresh (make-array (length pieces))))
            (setf (aref fresh 0) (concatenate 'string before (first pieces)))
            (loop :for piece :in (rest pieces)
                  :for i :from 1
                  :do (setf (aref fresh i)
                            (if (= i (1- (length pieces)))
                                (concatenate 'string piece after)
                                piece)))
            (values (%replace lines line (1+ line) fresh)
                    (+ line (1- (length pieces)))
                    (length (car (last pieces)))))))))

(defun newline (lines line col) (insert lines line col (string #\Newline)))

(defun region (lines from-line from-col to-line to-col)
  (multiple-value-bind (from-line from-col) (clamp lines from-line from-col)
    (multiple-value-bind (to-line to-col) (clamp lines to-line to-col)
      (when (or (> from-line to-line)
                (and (= from-line to-line) (> from-col to-col)))
        (rotatef from-line to-line)
        (rotatef from-col to-col))
      (if (= from-line to-line)
          (subseq (line-at lines from-line) from-col to-col)
          (format nil "~a~%~{~a~%~}~a"
                  (subseq (line-at lines from-line) from-col)
                  (coerce (subseq lines (1+ from-line) to-line) 'list)
                  (subseq (line-at lines to-line) 0 to-col))))))

(defun delete (lines from-line from-col to-line to-col)
  (multiple-value-bind (from-line from-col) (clamp lines from-line from-col)
    (multiple-value-bind (to-line to-col) (clamp lines to-line to-col)
      (when (or (> from-line to-line)
                (and (= from-line to-line) (> from-col to-col)))
        (rotatef from-line to-line)
        (rotatef from-col to-col))
      (let ((taken (region lines from-line from-col to-line to-col))
            (joined (concatenate 'string
                                 (subseq (line-at lines from-line) 0 from-col)
                                 (subseq (line-at lines to-line) to-col))))
        (values (%replace lines from-line (1+ to-line) (vector joined))
                from-line from-col taken)))))

(defun word-char-p (ch)
  (or (alphanumericp ch) (find ch *word-characters*)))

(defun beginning-of-line (lines line col)
  (declare (ignore lines col))
  (values line 0))

(defun end-of-line (lines line col)
  (declare (ignore col))
  (values line (length (line-at lines line))))

(defun %step-char (lines line col n)
  (loop :repeat (abs n)
        :do (if (plusp n)
                (if (< col (length (line-at lines line)))
                    (incf col)
                    (when (< line (1- (length lines)))
                      (incf line) (setf col 0)))
                (if (plusp col)
                    (decf col)
                    (when (plusp line)
                      (decf line)
                      (setf col (length (line-at lines line)))))))
  (values line col))

(defun %step-word (lines line col n)
  (loop :repeat (abs n)
        :do (let ((text (line-at lines line)))
              (if (plusp n)
                  (progn
                    (loop :while (and (< col (length text))
                                      (not (word-char-p (char text col))))
                          :do (incf col))
                    (loop :while (and (< col (length text))
                                      (word-char-p (char text col)))
                          :do (incf col)))
                  (progn
                    (loop :while (and (plusp col)
                                      (not (word-char-p (char text (1- col)))))
                          :do (decf col))
                    (loop :while (and (plusp col)
                                      (word-char-p (char text (1- col))))
                          :do (decf col))))))
  (values line col))

(defun move-by (unit lines line col n)
  (multiple-value-bind (line col) (clamp lines line col)
    (ecase unit
      (:char (%step-char lines line col n))
      (:word (%step-word lines line col n))
      (:line (clamp lines (+ line n) col))
      (:buffer (if (plusp n)
                   (clamp lines (1- (length lines)) most-positive-fixnum)
                   (values 0 0))))))

(defun indent-width (text)
  (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) text)
      (length text)))

(defun search (lines needle line col &key (forward t))
  (loop :with n := (length lines)
        :for i := line :then (if forward (1+ i) (1- i))
        :while (and (>= i 0) (< i n))
        :for text := (line-at lines i)
        :for from := (if (= i line) col (if forward 0 (length text)))
        :for at := (if forward
                       (cl:search needle text :start2 (min from (length text)))
                       (cl:search needle text :from-end t
                                              :end2 (min from (length text))))
        :when at :do (return (values i at))))
