(defpackage #:pine/serve/wire
  (:use #:cl)
  (:local-nicknames (#:json #:pine/serve/json) (#:serial #:pine/serial)
                    (#:peer #:pine/run/peer))
  (:export
   #:decode-request #:encode-reply #:encode-event #:serve #:encode-request #:decode-reply #:eventp))
(in-package #:pine/serve/wire)

(defparameter +methods+
  '(("read"  . :contents)
    ("write" . :write)
    ("ls"    . :entries)
    ("verb"  . :verb)
    ("watch" . :watch)
    ("eval"  . :evaluate)
    ("ping"  . :ping)))

(defun %word (it) (and it (string-downcase (princ-to-string it))))

(defun decode-request (line)
  (let* ((it (com.inuoe.jzon:parse line))
         (id (gethash "id" it))
         (doing (%word (gethash "do" it)))
         (path (gethash "path" it))
         (kind (cdr (assoc doing +methods+ :test #'equal))))
    (values
     (cond ((null doing) (list :no "a line says what to do"))
           ((and (equal doing "eval") (not (peer:evaluatingp)))
            (list :no "this way in does not evaluate"))
           ((null kind) (list :no (format nil "~a is not something to do; there ~
                                              is ~{~a~^, ~}"
                                          doing (mapcar #'car +methods+))))
           ((eq kind :ping) (list :ping))
           ((eq kind :evaluate)
            (let ((form (gethash "form" it)))
              (if (stringp form)
                  (list :evaluate
                        (let ((*read-eval* nil)
                              (*readtable* (named-readtables:find-readtable
                                            'pine/fs/reader:syntax)))
                          (read-from-string form)))
                  (list :no "eval is given a form, as a string"))))
           ((null path) (list :no (format nil "~a names a place" doing)))
           ((eq kind :write)
            (list :write path (json:from-json (gethash "value" it))))
           ((eq kind :verb)
            (list* :verb path (json:as-verb (gethash "verb" it))
                   (map 'list #'json:from-json (or (gethash "with" it) #()))))
           (t (list kind path)))
     id)))

(defun %object (&rest pairs)
  (let ((out (make-hash-table :test 'equal)))
    (loop :for (k v) :on pairs :by #'cddr :do (setf (gethash k out) v))
    out))

(defun %held (said)
  (if (or (= 2 (length said)) (%placep said)) (second said) (rest said)))

(defun %placep (said)
  (and (= 4 (length said)) (eq (third said) :kind)))

(defun encode-reply (id said)
  (com.inuoe.jzon:stringify
   (if (and (consp said) (eq :ok (first said)))
       (handler-case (let ((kind (and (%placep said) (fourth said))))
                       (if kind
                           (%object "id" (or id (quote null))
                                    "ok" (json:as-json (%held said))
                                    "kind" (%word kind))
                           (%object "id" (or id (quote null))
                                    "ok" (json:as-json (%held said)))))
         (error (c) (%object "id" (or id (quote null))
                             "no" (princ-to-string c))))
       (%object "id" (or id (quote null)) "no"
                (if (consp said) (princ-to-string (second said)) "no decode-reply")))))

(defun encode-request (id message)
  (let ((word (car (rassoc (first message) +methods+))))
    (unless word (error "~s is not something this speaks." (first message)))
    (com.inuoe.jzon:stringify
     (case (first message)
       (:ping (%object "id" id "do" word))
       (:evaluate (%object "id" id "do" word
                           "form" (let ((*print-readably* nil))
                                    (prin1-to-string (second message)))))
       (:write (%object "id" id "do" word "path" (second message)
                        "value" (json:as-json (serial:encode (third message)))))
       (:verb (%object "id" id "do" word "path" (second message)
                       "verb" (%word (third message))
                       "with" (coerce (mapcar (lambda (a)
                                                (json:as-json (serial:encode a)))
                                              (cdddr message))
                                      'vector)))
       (t (%object "id" id "do" word "path" (second message)))))))

(defun eventp (it) (and (hash-table-p it) (nth-value 1 (gethash "event" it))))

(defun decode-reply (line)
  (let ((it (com.inuoe.jzon:parse line)))
    (cond ((eventp it)
           (values (list :moved (gethash "path" it)) nil t))
          ((nth-value 1 (gethash "ok" it))
           (values (list :ok (serial:decode (json:from-json (gethash "ok" it))))
                   (gethash "id" it) nil))
          (t (values (list :no (princ-to-string (gethash "no" it)))
                     (gethash "id" it) nil)))))

(defun encode-event (said)
  (com.inuoe.jzon:stringify
   (%object "event" (%word (first said)) "path" (or (second said) (quote null)))))

(defun serve (in ask say &key done)
  (unwind-protect
       (loop :for line := (read-line in nil nil)
             :while line
             :unless (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))
               :do (multiple-value-bind (message id)
                       (handler-case (decode-request line)
                         (error (c) (values (list :no (princ-to-string c)) nil)))
                     (let ((said (if (eq :no (first message))
                                     message
                                     (handler-case (funcall ask message)
                                       (error (c) (list :no (princ-to-string c)))))))
                       (funcall say (encode-reply id said)))))
    (when done (funcall done))))
