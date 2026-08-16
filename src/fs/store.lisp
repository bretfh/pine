(defpackage #:pine/fs/store
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree))
  (:export #:store #:open-store #:close-store #:snapshot #:restore #:file-of
           #:storablep #:written #:read-back #:keep #:forget #:keeping #:*store*
           #:attach #:*on-trouble*))
(in-package #:pine/fs/store)

(defvar *schema*
  "create table if not exists node (path text primary key, value text not null,
                                    at integer)")
(defvar *store* nil)
(defvar *on-trouble* nil
  "Told when a write or a read-back would otherwise have failed in silence. A
value that will not go down, and one that will not come back, are both things the
person running pine has to be able to find out about.")

(defclass store ()
  ((file-of :initarg :file :reader file-of)
   (db      :initarg :db   :reader db)))

(defclass store-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))

(defmethod print-object ((s store) stream)
  (print-unreadable-object (s stream :type t)
    (write-string (princ-to-string (file-of s)) stream)))

(defun %trouble (what)
  (if *on-trouble* (funcall *on-trouble* what) (warn "~a" what))
  nil)

(defun open-store (file)
  (ensure-directories-exist file)
  (let ((db (sqlite:connect file)))
    (sqlite:execute-non-query db "pragma journal_mode = wal")
    (sqlite:execute-non-query db "pragma busy_timeout = 2000")
    (sqlite:execute-non-query db *schema*)
    (setf *store* (make-instance 'store :file file :db db))))

(defun close-store (s)
  (sqlite:disconnect (db s))
  (when (eq s *store*) (setf *store* nil))
  s)

(defun storablep (value)
  (typecase value
    ((or null number string character keyword) t)
    (symbol t)
    (cons (and (storablep (car value)) (storablep (cdr value))))
    ((and vector (not string)) (every #'storablep value))
    (t (and (d:collectionp value)
            (every #'storablep (d:as :list (d:keys value)))
            (every #'storablep (d:as :list (d:vals value)))))))

(defun said (value)
  "VALUE as something that reads back. A map, a seq and a set are written as what
they are, because prin1 cannot say them and read cannot take them."
  (cond ((d:mapp value)
         (list* :map (loop :for (k . v) :in (d:pairs value)
                           :append (list (said k) (said v)))))
        ((d:setp value) (list* :set (mapcar #'said (d:as :list value))))
        ((d:seqp value) (list* :seq (mapcar #'said (d:as :list value))))
        ((consp value) (cons (said (car value)) (said (cdr value))))
        (t value)))

(defun took (form)
  (cond ((and (consp form) (eq :map (car form)))
         (loop :with m := (d:no-map)
               :for (k v) :on (rest form) :by #'cddr
               :do (setf m (d:with m (took k) (took v)))
               :finally (return m)))
        ((and (consp form) (eq :seq (car form)))
         (d:as :seq (mapcar #'took (rest form))))
        ((and (consp form) (eq :set (car form)))
         (d:as :set (mapcar #'took (rest form))))
        ((consp form) (cons (took (car form)) (took (cdr form))))
        (t form)))

(defun written (value)
  (let ((*print-readably* nil) (*print-circle* nil))
    (prin1-to-string (said value))))

(defun read-back (text)
  (let ((*read-eval* nil))
    (handler-case (values (took (read-from-string text)) t)
      (error (c)
        (%trouble (format nil "a value in the store will not read back: ~a" c))
        (values nil nil)))))

(defun snapshot (s &optional (root (tree:root)))
  (let ((n 0))
    (sqlite:with-transaction (db s)
      (sqlite:execute-non-query (db s) "delete from node")
      (tree:walk root
                 (lambda (each)
                   (when (and (node:savedp each)
                              (node:contents each)
                              (storablep (node:contents each)))
                     (sqlite:execute-non-query
                      (db s)
                      "insert or replace into node (path, value, at) values (?, ?, ?)"
                      (node:full-name each) (written (node:contents each))
                      (get-universal-time))
                     (incf n)))))
    n))

(defun keep (n)
  "Write this node where it will be found again, now rather than at shutdown. A
crash must not cost what was written before it, and a write that will not go down
says so rather than vanishing."
  (let ((s *store*))
    (when (and s (node:savedp n) (storablep (node:contents n)))
      (handler-case
          (sqlite:execute-non-query
           (db s) "insert or replace into node (path, value, at) values (?, ?, ?)"
           (node:full-name n) (written (node:contents n)) (get-universal-time))
        (error (c)
          (%trouble (format nil "~a did not reach the store: ~a"
                            (node:full-name n) c))))
      n)))

(defun forget (n)
  "Take N out of the store as it goes out of the tree, and whatever stood under it
with it."
  (let ((s *store*))
    (when (and s (node:savedp n))
      (let ((path (node:full-name n)))
        (sqlite:execute-non-query
         (db s) "delete from node where path = ? or path like ?"
         path (concatenate 'string path "/%"))))
    n))

(defun keeping (&optional (s *store*))
  (setf node:*on-write* (when s #'keep)
        node:*on-erase* (when s #'forget))
  s)

(defun restore (s &optional (root (tree:root)))
  "Put back what the leaves held. A path already standing is written where it
stands, so a buffer's point comes back on the buffer and not on a value beside it.

Nothing is written through while this runs: the store is where these came from."
  (let ((n 0)
        (node:*on-write* nil))
    (loop :for (path text) :in (sqlite:execute-to-list
                                (db s) "select path, value from node")
          :do (multiple-value-bind (value read) (read-back text)
                (let ((names (tree:split-name path)))
                  (when (and names read)
                    (tree:put root names value)
                    (incf n)))))
    n))

(defmethod node:contents ((n store-node))
  (let ((s *store*))
    (and s (princ-to-string (file-of s)))))

(defmethod node:verb ((n store-node) name arguments)
  (declare (ignore arguments))
  (case name
    (:snapshot (and *store* (snapshot *store*)))
    (t (error "/store takes :snapshot."))))

(defun attach (root)
  (node:attach (make-instance 'store-node :name "store"
                                          :describes "where this pine persists")
               root))
