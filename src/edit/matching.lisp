(defpackage #:pine/edit/matching
  (:use #:cl)
  (:local-nicknames (#:fault #:pine/run/fault))
  (:export
   #:name-of #:annotation #:as-row #:matches #:common-prefix
   #:expanded #:split-path #:entries #:files))
(in-package #:pine/edit/matching)

(defvar *separator* #\Space)

(defun name-of (each) (princ-to-string (if (consp each) (first each) each)))

(defun annotation (each)
  (if (consp each) (princ-to-string (rest each)) ""))

(defun as-row (each width)
  (let* ((name (name-of each))
         (note (annotation each))
         (gap (- width (length name) (length note))))
    (if (and (plusp (length note)) (> gap 1))
        (concatenate 'string name (make-string gap :initial-element #\space) note)
        name)))

(defun %tokens (text)
  (remove "" (uiop:split-string (or text "") :separator (list *separator*))
          :test #'string=))

(defun %scored (tokens name)
  "How well NAME answers TOKENS: every token must be somewhere in it, in any order
and in any case. The score is where they were found, so what matches earliest is
offered first, and a name that begins with the first token beats one that merely
contains it. Nothing when a token is missing."
  (let ((score 0) (from-the-start nil))
    (loop :for token :in tokens
          :for first := t :then nil
          :for at := (search token name :test #'char-equal)
          :do (unless at (return-from %scored (values nil nil)))
              (when (and first (zerop at)) (setf from-the-start t))
              (incf score at))
    (values (if from-the-start score (+ score 1000)) t)))

(defun matches (text all)
  (let ((tokens (%tokens text)))
    (if (null tokens)
        (stable-sort (copy-list all) #'string-lessp :key #'name-of)
        (let (scored)
          (dolist (each all)
            (multiple-value-bind (score hit) (%scored tokens (name-of each))
              (when hit (push (cons score each) scored))))
          (mapcar #'cdr
                  (stable-sort (nreverse scored)
                               (lambda (a b)
                                 (let ((sa (car a)) (sb (car b))
                                       (na (name-of (cdr a))) (nb (name-of (cdr b))))
                                   (cond ((/= sa sb) (< sa sb))
                                         ((/= (length na) (length nb))
                                          (< (length na) (length nb)))
                                         (t (string-lessp na nb)))))))))))

(defun common-prefix (strings)
  (if (null strings)
      ""
      (let ((shortest (first strings)))
        (dolist (each (rest strings))
          (let ((at (or (mismatch shortest each :test #'char=) (length each))))
            (setf shortest (subseq shortest 0 (min at (length shortest))))))
        shortest)))

(defun expanded (text)
  "TEXT as a path a person typed: ~ is home, and a second / starts again from the
root, so an absolute path typed over a directory means that path."
  (let* ((text (or text ""))
         (over (search "//" text :from-end t))
         (text (if over (subseq text (1+ over)) text)))
    (cond ((equal text "~") (namestring (user-homedir-pathname)))
          ((and (> (length text) 1) (string= "~/" (subseq text 0 2)))
           (namestring (merge-pathnames (subseq text 2) (user-homedir-pathname))))
          (t text))))

(defun split-path (text)
  (let ((slash (position #\/ text :from-end t)))
    (if slash
        (values (subseq text 0 (1+ slash)) (subseq text (1+ slash)))
        (values "" text))))

(defun entries (where)
  (fault:or-nothing "a directory that is not there has nothing in it"
    (append (sort (mapcar (lambda (p)
                            (concatenate 'string
                                         (car (last (pathname-directory p))) "/"))
                          (uiop:subdirectories where))
                  #'string<)
            (sort (mapcar #'file-namestring (uiop:directory-files where))
                  #'string<))))

(defun files (typed)
  (multiple-value-bind (where base) (split-path (expanded typed))
    (let ((all (entries (if (equal where "") "./" where))))
      (if (equal base "")
          all
          (remove-if-not (lambda (name)
                           (and (>= (length name) (length base))
                                (string= base name :end2 (length base))))
                         all)))))
