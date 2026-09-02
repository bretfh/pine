(defpackage #:pine/serve/json
  (:use #:cl)
  (:local-nicknames (#:said #:pine/said))
  (:export
   #:as-json #:from-json #:as-verb #:render #:parse))
(in-package #:pine/serve/json)

(defparameter +kinds+ '(("map" . :map) ("seq" . :seq) ("set" . :set))
  "The collections that need a word of their own. A list does not: an array says
what it is, which is why nothing is quoted on this side.")

(defun %keyword-text (k) (format nil ":~a" (string-downcase (symbol-name k))))

(defun %keywordp (text)
  (and (stringp text) (plusp (length text)) (char= #\: (char text 0))))

(defun %as-keyword (text)
  (intern (string-upcase (subseq text 1)) :keyword))

(defun as-json (form)
  "The shape SAID gives a value, as what jzon writes.

A list is an array, because an array is already unambiguous: the :QUOTED the
printed form needs, to tell a list that begins with `map' from a map, is not
needed here and is dropped. A keyword is a string that begins with a colon, so a
plist reads as the run of words it is rather than as a wrapper round each one.

What :QUOTED holds is walked as a list and not looked at again. Looking again is
what reads its first element as the word it only happens to be, and then the list
that was carefully escaped on the way in comes back out as the collection it was
escaped to not be."
  (cond ((null form) 'null)
        ((eq form t) t)
        ((keywordp form) (%keyword-text form))
        ((symbolp form) (error "~s is a symbol; there is no spelling for one here."
                               form))
        ((characterp form) (string form))
        ((or (numberp form) (stringp form)) form)
        ((and (consp form) (eq :map (car form)))
         (let ((out (make-hash-table :test 'equal)))
           (setf (gethash "map" out)
                 (coerce (loop :for (k v) :on (rest form) :by #'cddr
                               :collect (vector (as-json k) (as-json v)))
                         'vector))
           out))
        ((and (consp form) (member (car form) '(:seq :set)))
         (let ((out (make-hash-table :test 'equal)))
           (setf (gethash (string-downcase (symbol-name (car form))) out)
                 (coerce (mapcar #'as-json (rest form)) 'vector))
           out))
        ((and (consp form) (eq :quoted (car form)))
         (coerce (mapcar #'as-json (second form)) 'vector))
        ((consp form) (coerce (mapcar #'as-json form) 'vector))
        (t (error "~s has no spelling here." form))))

(defun %tagged (it)
  "The kind a one-word object names, and what it holds."
  (when (and (hash-table-p it) (= 1 (hash-table-count it)))
    (loop :for (word . kind) :in +kinds+
          :for found := (nth-value 1 (gethash word it))
          :when found :do (return (values kind (gethash word it))))))

(defun from-json (it)
  "What jzon read, as the shape TOOK takes.

An array is a list, and a list that begins with one of the words above is quoted
on the way back, so that TOOK answers the list rather than the collection it looks
like. That is the one place the two spellings differ, and it is handled here so
neither side has to know."
  (cond ((eq it 'null) nil)
        ((null it) nil)
        ((eq it t) t)
        ((%keywordp it) (%as-keyword it))
        ((or (numberp it) (stringp it)) it)
        ((hash-table-p it)
         (multiple-value-bind (kind held) (%tagged it)
           (unless kind
             (error "~s names no kind: a map, a seq or a set is an object of one ~
                     word, and anything else is an array."
                    (loop :for k :being :the :hash-keys :of it :collect k)))
           (case kind
             (:map (list* :map (loop :for pair :across held
                                     :append (list (from-json (aref pair 0))
                                                   (from-json (aref pair 1))))))
             (t (list* kind (map 'list #'from-json held))))))
        ((vectorp it)
         (let ((all (map 'list #'from-json it)))
           (if (member (car all) (said:tags))
               (list :quoted all)
               all)))
        (t (error "~s is not something this speaks." it))))

(defun as-verb (word)
  "The word a verb is named by. A verb is a keyword whatever it was spelled as,
so `stop' and `:stop' are the one verb."
  (when word
    (let ((text (string-downcase (princ-to-string word))))
      (intern (string-upcase (string-left-trim ":" text)) :keyword))))

(defun render (value)
  "VALUE as one line of json."
  (com.inuoe.jzon:stringify (as-json (said:said value))))

(defun parse (text)
  "One line of json, as the value it spells."
  (said:took (from-json (com.inuoe.jzon:parse text))))
