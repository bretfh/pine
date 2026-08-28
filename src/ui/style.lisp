(in-package #:pine/ui)

(defvar *sheet* nil
  "The stylesheet as (SELECTOR PROPS) pairs, in the order they were written. What a
config puts at /style is appended to what pine ships.")

(defvar *properties* (d:table)
  "What a resolved style may hold: a key, and how to work it out from the css props
that matched. Adding a property is one entry here, not an edit to RESOLVE.")

(defun property (key parser)
  "Say that a style may hold KEY, worked out by PARSER from the matched props."
  (d:keep! *properties* key parser)
  key)

(defun properties () (d:keys (d:all *properties*)))

(defun %words (s)
  (remove "" (uiop:split-string s :separator '(#\space #\tab)) :test #'string=))

(defun %segment (s)
  (cond ((string= s "*") (list :any t :classes nil :pseudo nil))
        ((find #\. s)
         (let* ((colon (position #\: s))
                (pseudo (and colon (subseq s (1+ colon))))
                (body (subseq s 0 (or colon (length s)))))
           (list :any nil
                 :classes (remove "" (uiop:split-string body :separator ".")
                                  :test #'string=)
                 :pseudo pseudo)))
        (t nil)))

(defun segments (s)
  "A selector string as its segments, or nothing when it names an element. Pine
draws widgets, not elements: a selector that names one matches nothing, and saying
so here is better than a rule that silently never fires."
  (let ((segments (mapcar #'%segment (%words s))))
    (unless (member nil segments) segments)))

(defun %group (text)
  (remove nil (mapcar (lambda (g) (segments (string-trim " " g)))
                      (uiop:split-string text :separator '(#\,)))))

(defun specificity (segments)
  "How particular a selector is: how many segments it constrains, then how many
classes across them. A rule that says more wins over one that says less, whatever
order they were written in."
  (list (length segments)
        (reduce #'+ segments :key (lambda (s) (length (getf s :classes)))
                             :initial-value 0)))

(defun %beats (a b)
  (destructuring-bind (as ac) a
    (destructuring-bind (bs bc) b
      (or (> as bs) (and (= as bs) (> ac bc))))))

(defun sheet (&optional pairs)
  (if pairs (setf *sheet* pairs) *sheet*))

(defun rules ()
  "Every rule, compiled, worked out once and kept until the tree moves."
  (memo
   :rules
   (lambda ()
     (loop :for (text props) :in (sheet)
           :for at :from 0
           :append (loop :for segments :in (%group text)
                         :collect (list segments props (specificity segments) at))))))

(defun forget-rules ()
  (let ((n (and (pine/fs/tree:root) (pine/fs/tree:at nil "memo" "rules"))))
    (when n (pine/fs/node:stir n)))
  nil)

(defun %fits (segment classes hover)
  (let ((pseudo (getf segment :pseudo)))
    (and (or (null pseudo) (and (string= pseudo "hover") hover))
         (or (getf segment :any)
             (subsetp (getf segment :classes) classes :test #'string=)))))

(defun %above (segments chain)
  "Each segment matches some ancestor, left to right, root first."
  (if (null segments)
      t
      (loop :for tail :on chain
            :when (%fits (first segments) (first tail) nil)
              :do (return (%above (rest segments) (rest tail)))
            :finally (return nil))))

(defun %matches (segments chain hover)
  (and segments chain
       (%fits (car (last segments)) (car (last chain)) hover)
       (%above (butlast segments) (butlast chain))))

(defun %props (chain hover)
  "The props of every rule matching CHAIN, merged least particular first."
  (let ((found (loop :for rule :in (rules)
                     :when (%matches (first rule) chain hover) :collect rule)))
    (let ((merged nil))
      (dolist (rule (stable-sort found (lambda (a b)
                                         (cond ((%beats (third a) (third b)) nil)
                                               ((%beats (third b) (third a)) t)
                                               (t (< (fourth a) (fourth b))))))
                    merged)
        (loop :for (k v) :on (second rule) :by #'cddr
              :do (setf (getf merged k) v))))))

(defun resolve (chain &key hover)
  "The style for a widget with this chain of class-sets, root first. A map: what it
holds is what the properties table says a style may hold, so nothing here has to be
edited to carry something new."
  (let ((props (%props chain hover))
        (out (d:no-map)))
    (d:do-map (key parser (d:all *properties*) out)
      (let ((v (funcall parser props)))
        (when v (setf out (d:with out key v)))))))

(defun classes (it)
  "A :class value as a list of class names."
  (typecase it
    (null nil)
    (symbol (list (string-downcase (symbol-name it))))
    (string (remove "" (uiop:split-string it :separator '(#\space)) :test #'string=))
    (cons (mapcan #'classes it))))

(defun %lengths (v)
  (typecase v
    (real (list (round v)))
    (cons (mapcar #'round v))
    (string (loop :for tok :in (%words v)
                  :for n := (and (plusp (length tok))
                                 (digit-char-p (char tok 0))
                                 (not (search "rem" tok)) (not (find #\% tok))
                                 (parse-integer tok :junk-allowed t))
                  :when n :collect n))
    (t nil)))

(defun %rgba (s n)
  (let* ((open (position #\( s)) (close (position #\) s))
         (parts (uiop:split-string (subseq s (1+ open) close) :separator '(#\,))))
    (when (>= (length parts) n)
      (flet ((num (i) (read-from-string (string-trim " " (nth i parts)))))
        (list (num 0) (num 1) (num 2) (if (= n 4) (float (num 3)) 1.0))))))

(defun %color (s)
  "(r g b a): the three numbers the rest of pine paints with, and how much of it
shows. Nothing for transparent, none, or unparsable."
  (cond ((or (null s) (equal s "transparent") (equal s "none")) nil)
        ((and (stringp s) (plusp (length s)) (char= (char s 0) #\#))
         (let ((rgb (unhex s)))
           (when rgb (append rgb (list 1.0)))))
        ((and (stringp s) (eql 0 (search "rgba(" s))) (%rgba s 4))
        ((and (stringp s) (eql 0 (search "rgb(" s)))  (%rgba s 3))
        (t nil)))

(defun %box (v)
  "The four sides of a css box shorthand, expanded per css rules."
  (let ((l (%lengths v)))
    (case (length l)
      (0 nil)
      (1 (list (first l) (first l) (first l) (first l)))
      (2 (list (first l) (second l) (first l) (second l)))
      (3 (list (first l) (second l) (third l) (second l)))
      (t (subseq l 0 4)))))

(property :bg (lambda (p) (%color (getf p :background-color))))
(property :fg (lambda (p) (let ((c (%color (getf p :color))))
                            (and c (subseq c 0 3)))))
(property :bold (lambda (p) (equal (getf p :font-weight) "bold")))
(property :font (lambda (p) (first (%lengths (getf p :font-size)))))
(property :min-w (lambda (p) (first (%lengths (getf p :min-width)))))
(property :min-h (lambda (p) (first (%lengths (getf p :min-height)))))

(property :radius
          (lambda (p)
            (let ((v (getf p :border-radius)))
              (cond ((null v) nil)
                    ((realp v) (round v))
                    ((and (stringp v) (search "100%" v)) :round)
                    (t (first (%lengths v)))))))

(property :pad
          (lambda (p)
            (let ((l (%lengths (getf p :padding))))
              (case (length l)
                (0 nil)
                (1 (cons (first l) (first l)))
                (2 (cons (second l) (first l)))
                (3 (cons (second l) (round (+ (first l) (third l)) 2)))
                (t (cons (round (+ (second l) (fourth l)) 2)
                         (round (+ (first l) (third l)) 2)))))))

(property :margin
          (lambda (p)
            (let ((m (or (%box (getf p :margin)) (list 0 0 0 0))))
              (loop :for key :in '(:margin-top :margin-right :margin-bottom
                                   :margin-left)
                    :for i :from 0
                    :for v := (first (%lengths (getf p key)))
                    :when v :do (setf (nth i m) v))
              (unless (every #'zerop m) m))))

(property :grad
          (lambda (p)
            (let ((v (getf p :background-image)))
              (when (and v (search "linear-gradient" v))
                (let* ((open (position #\( v))
                       (close (position #\) v :from-end t))
                       (stops (loop :for part :in (uiop:split-string
                                                   (subseq v (1+ open) close)
                                                   :separator '(#\,))
                                    :for c := (%color (string-trim " " part))
                                    :when c :collect (subseq c 0 3))))
                  (when (>= (length stops) 2) (subseq stops 0 2)))))))

(property :border
          (lambda (p)
            (when (equal (getf p :border-style) "solid")
              (let ((c (%color (getf p :border-color))))
                (list (or (first (%lengths (getf p :border-width))) 1)
                      (and c (subseq c 0 3)))))))

(property :inset
          (lambda (p)
            (let ((v (getf p :box-shadow)))
              (when (and v (search "inset" v))
                (let ((c (%color (car (last (%words v))))))
                  (when c (list (subseq c 0 3)
                                (or (first (%lengths v)) 3))))))))

(property :shadow
          (lambda (p)
            (let ((v (getf p :box-shadow)))
              (when (and v (not (search "inset" v)) (search "px" v))
                (let ((l (%lengths v))
                      (c (%color (car (last (%words v))))))
                  (when (and (>= (length l) 3) c)
                    (list (first l) (second l) (third l) c)))))))

(property :opacity
          (lambda (p)
            (let* ((v (getf p :opacity))
                   (n (if (realp v)
                          v
                          (and (stringp v)
                               (fault:or-nothing
                                   "a style value that is not a form is a string"
                                 (with-standard-io-syntax
                                   (let ((*read-eval* nil))
                                     (read-from-string v))))))))
              (when (realp n) (float (max 0 (min 1 n)) 1.0)))))
