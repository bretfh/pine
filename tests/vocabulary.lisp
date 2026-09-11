(in-package :pine/test)

(def-suite* :pine/vocabulary :in :pine)

(defun %retired ()
  "Spelled at run time so this file cannot be caught by its own rename."
  (list (cons (concatenate 'string "stand" "ing") "the tree's entries are nodes")
        (cons (concatenate 'string "+do" "ing+") "the wire's table is +methods+")
        (cons (concatenate 'string "hold" "ing") "what stat says is KIND")
        (cons (concatenate 'string "fs:mo" "ved") "fs:touch")
        (cons (concatenate 'string "d:sw" "ap") "sb-ext:atomic-update")
        (cons (concatenate 'string "defback" "ing") "defdriver")
        (cons (concatenate 'string "first" "p") "first-line")
        (cons (concatenate 'string "wide-" "of") "width-of")
        (cons (concatenate 'string "tall-" "of") "height-of")
        (cons (concatenate 'string "fs:entr" "ies") "fs:children")
        (cons (concatenate 'string "said:s" "aid") "serial:encode")))

(defun %source-files ()
  (let (out)
    (uiop:collect-sub*directories
     (merge-pathnames "src/" (asdf:system-source-directory :pine))
     (constantly t) (constantly t)
     (lambda (d) (dolist (f (uiop:directory-files d "*.lisp")) (push f out))))
    out))

(test a-retired-word-does-not-come-back
  "One concept, one word. Every entry here was two words for one thing, or one
word for two, and the second spelling is what let three vocabularies live at once."
  (let ((retired (%retired)))
    (dolist (file (%source-files))
      (let ((text (uiop:read-file-string file)))
        (loop :for (word . instead) :in retired
              :do (is (null (search word text))
                      "~a says ~a; it is ~a" (file-namestring file) word instead))))))

(test what-is-exported-is-written-down
  "doc/symbols.md is generated from the image, so it cannot describe a pine that
is not there. It is pine's whole written record."
  (is (probe-file (%where)))
  (is (equal (table) (uiop:read-file-string (%where)))))

(defun %quoted-on-line (text at)
  (let ((open (position #\" text :start at))
        (eol (position #\Newline text :start at)))
    (when (and open (or (null eol) (< open eol)))
      (let ((close (position #\" text :start (1+ open))))
        (when close (subseq text (1+ open) close))))))

(defun %foreign-names (text)
  (let (out)
    (dolist (word '("defcfun" "foreign-funcall") (nreverse out))
      (loop :with at := 0
            :for found := (search word text :start2 at)
            :while found
            :do (setf at (+ found (length word)))
                (let ((name (%quoted-on-line text at)))
                  (when name (push name out)))))))

(defun %c-namep (name)
  (and (plusp (length name))
       (not (digit-char-p (char name 0)))
       (every (lambda (c) (or (alphanumericp c) (char= #\_ c))) name)))

(defun %past-feature (text at)
  (let ((i at))
    (loop :while (and (< i (length text))
                      (or (alphanumericp (char text i))
                          (find (char text i) "-_.")))
          :do (incf i))
    i))

(defun %dangling-conditional (text)
  (loop :with at := 0
        :for found := (search "#+" text :start2 at)
        :while found
        :do (setf at (+ found 2))
            (let ((i (%past-feature text (+ found 2))))
              (loop :while (and (< i (length text))
                                (member (char text i)
                                        '(#\Space #\Tab #\Newline #\Return)))
                    :do (incf i))
              (when (and (< (1+ i) (length text))
                         (char= #\# (char text i))
                         (find (char text (1+ i)) "+-"))
                (return (subseq text found (min (length text) (+ found 32))))))))

(test a-foreign-name-is-spelled-the-way-c-spells-it
  (dolist (file (%source-files))
    (let ((text (uiop:read-file-string file)))
      (dolist (name (%foreign-names text))
        (is (%c-namep name) "~a asks for ~s" (file-namestring file) name)))))

(test a-reader-conditional-has-a-form-under-it
  (dolist (file (%source-files))
    (let ((said (%dangling-conditional (uiop:read-file-string file))))
      (is (null said) "~a has ~s" (file-namestring file) said))))
