(in-package #:pine/text)

(defparameter +bodies+ '("def" "with-" "do-" "when" "unless" "let" "loop" "lambda"
                         "case" "cond" "dolist" "dotimes" "handler" "unwind"
                         "if" "progn" "block" "flet" "labels" "eval-when"
                         "multiple-value-bind" "destructuring-bind"))

(defun bodyp (name)
  (and name (some (lambda (p)
                    (and (>= (length name) (length p))
                         (string-equal p name :end2 (length p))))
                  +bodies+)))

(defun %word-at (text from)
  (let* ((n (length text))
         (start (or (position-if-not (lambda (c) (member c '(#\Space #\Tab)))
                                     text :start (min from n))
                    n))
         (end (or (position-if (lambda (c) (member c '(#\Space #\Tab #\( #\) #\")))
                               text :start start)
                  n)))
    (when (< start end) (values (subseq text start end) end))))

(defun head (text)
  (let ((open (position #\( text)))
    (when open
      (multiple-value-bind (word after) (%word-at text (1+ open))
        (when word
          (values word (%word-at text after)))))))

(defun %depth (text depth in-string)
  (let ((i 0) (n (length text)))
    (loop :while (< i n)
          :do (let ((ch (char text i)))
                (cond (in-string
                       (cond ((char= ch #\\) (incf i))
                             ((char= ch #\") (setf in-string nil))))
                      ((char= ch #\;) (return))
                      ((char= ch #\") (setf in-string t))
                      ((and (char= ch #\#) (< (1+ i) n)
                            (char= (char text (1+ i)) #\\))
                       (incf i 2))
                      ((find ch "([") (incf depth))
                      ((find ch ")]") (decf depth))))
              (incf i))
    (values depth in-string)))

(defun forms (buffer)
  (let ((depth 0) (in-string nil) (start nil) (head nil) (name nil) (found nil))
    (dotimes (at (line-count buffer) (nreverse found))
      (let ((text (line buffer at)))
        (when (and (null start) (zerop depth) (plusp (length text))
                   (char= #\( (char text 0)))
          (setf start at)
          (multiple-value-bind (h n) (head text)
            (setf head h name n)))
        (multiple-value-setq (depth in-string) (%depth text depth in-string))
        (when (and start (<= depth 0))
          (push (list head name start at) found)
          (setf start nil depth 0))))))

(defmethod mode:regions ((m mode:lisp) buffer)
  (let ((by-head (d:no-map)))
    (dolist (form (forms buffer))
      (destructuring-bind (head name from to) form
        (let* ((head (or head "form"))
               (name (or name (princ-to-string from)))
               (had (or (d:lookup by-head head) nil)))
          (setf by-head
                (d:with by-head head
                        (append had
                                (list (mode:covering
                                       name (cons from 0)
                                       (cons to (length (line buffer to)))))))))))
    (let (out)
      (d:do-map (head kids by-head (nreverse out))
        (push (mode:covering head
                             (mode:from-of (first kids))
                             (mode:to-of (car (cl:last kids)))
                             kids)
              out)))))

(defun %opens (buffer at)
  (let ((depth 0) (in-string nil) (stack nil))
    (dotimes (line at)
      (let ((text (line buffer line))
            (i 0))
        (let ((n (length text)))
          (loop :while (< i n)
                :do (let ((ch (char text i)))
                      (cond (in-string
                             (cond ((char= ch #\\) (incf i))
                                   ((char= ch #\") (setf in-string nil))))
                            ((char= ch #\;) (return))
                            ((char= ch #\") (setf in-string t))
                            ((and (char= ch #\#) (< (1+ i) n)
                                  (char= (char text (1+ i)) #\\))
                             (incf i 2))
                            ((find ch "([")
                             (incf depth)
                             (push (list i (nth-value 0 (%word-at text (1+ i)))) stack))
                            ((find ch ")]")
                             (decf depth)
                             (pop stack))))
                    (incf i)))))
    (first stack)))

(defmethod mode:indent ((m mode:lisp) buffer at)
  (let ((open (%opens buffer at)))
    (if (null open)
        0
        (destructuring-bind (col head) open
          (+ col (if (bodyp head) (mode:says m :indent 2) 1))))))
