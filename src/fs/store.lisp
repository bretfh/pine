(defpackage #:pine/fs/store
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:commit #:pine/fs/commit) (#:tree #:pine/fs/tree)
                    (#:said #:pine/said) (#:log #:pine/fs/log))
  (:export
   #:open-store #:close-store #:snapshot #:restore #:stale
   #:keeping #:*store*))
(in-package #:pine/fs/store)

(defvar *schema*
  "create table if not exists node (path text primary key, value text not null,
                                    at integer)")
(defvar *store* nil)
(defvar *putting-back* nil
  "Whether this thread is putting values back out of the store.

Bound rather than turned off. Turning the listener off turns it off for every
thread, so a write from anywhere else while a restore ran was a write the store
never heard.")

(defclass store ()
  ((file-of :initarg :file :reader file-of)
   (db      :initarg :db   :reader db)))

(defmethod print-object ((s store) stream)
  (print-unreadable-object (s stream :type t)
    (write-string (princ-to-string (file-of s)) stream)))

(defun %trouble (what)
  "A value that will not go down, and one that will not come back, are both things
the person running pine has to be able to find out about, so they are said where
everything else pine says is said."
  (log:note "~a" what)
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

(defun storablep (value) (said:sayablep value))

(defun written (value)
  "What goes in the file: the shape SAID gives a value, printed. The printing is
the store's; the shape is not, because a value leaving this image for a socket
takes the same one.

Printed as if from the keyword package, so every symbol that is not one carries
the package it is in. Printed from wherever the writing thread happened to stand,
a symbol in that package came back a symbol in whatever package the reading thread
happened to stand in, which is a different symbol with the same name."
  (let ((*print-readably* nil) (*print-circle* nil)
        (*package* (find-package :keyword)))
    (prin1-to-string (said:said value))))

(defun read-back (text)
  (let ((*read-eval* nil) (*package* (find-package :keyword)))
    (handler-case (values (said:took (read-from-string text)) t)
      (error (c)
        (%trouble (format nil "a value in the store will not read back: ~a" c))
        (values nil nil)))))

(defun snapshot (s &optional (root (tree:root)))
  "Write down what stands now. Belt and braces over the write-through, so nothing
is taken out here: a node that really went was taken out as it went, and a system
that has just been stopped has taken its nodes off the tree without meaning that
what they held is to be forgotten.

A branch is not written and a leaf holding NIL is, which is HOLDING's answer and not
a test on the value. Asking whether the value was NIL did both jobs at once, so a
node written NIL survived a live write and went at the next clean shutdown."
  (let ((n 0))
    (sqlite:with-transaction (db s)
      (tree:walk root
                 (lambda (each)
                   (when (and (node:savedp each)
                              (eq :held (node:holding each))
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

(defun %like (text)
  "TEXT as a LIKE pattern that matches itself. Per cent and underscore are the two
characters a pattern is written in, and a node may be named either: /a_b would take
/axb down with it."
  (with-output-to-string (out)
    (loop :for ch :across text
          :do (when (find ch "%_\\") (write-char #\\ out))
              (write-char ch out))))

(defun forget (path)
  "Take PATH out of the store as it goes out of the tree, and whatever stood under
it with it."
  (let ((s *store*))
    (when s
      (sqlite:execute-non-query
       (db s)
       "delete from node where path = ? or path like ? escape '\\'"
       path (concatenate 'string (%like path) "/%")))
    path))

(defun kept (moved)
  "Write down what moved, on the thread that moved it.

Measured at a hundred and thirty-six microseconds a write, almost all of it
sqlite, and it stays here: a slot is kept the moment it is written or what a crash
costs is everything since the image came up. Handing it to a dispatcher would buy
back that time and sell that promise, which is not a trade to make quietly.

What buys the time without selling anything is an append instead of a transaction."
  (let ((s *store*))
    (when (and s (not *putting-back*))
      (sqlite:with-transaction (db s)
        (loop :for n :in moved
              :when (node:nodep n) :do (keep n))))))

(defun keeping (&optional (s *store*))
  "Follow the tree: what moved is written down and what went is taken out. Two
listeners, because a write and an erasure are two things that happen to a place."
  (setf (commit:on-commit :store) (when s #'kept)
        (commit:on-forget :store) (when s #'forget))
  s)

(defun restore (s &optional (root (tree:root)))
  "Put values back into the nodes that already stand. Loading the code builds the
shape; this only fills it in. A path with no node behind it any more is left in the
store rather than conjured as a plain value: what it stood for is what knew how to
read it, and a value node standing in its place would be saved forever after.

Nothing is written through while this runs, on this thread: the store is where
these came from. Bound and not turned off, because turning it off turns it off for
everybody, and a write from another thread while a restore ran was one the store
never heard.

What is put back is put back as a value. A seq beginning with a keyword is an
instruction when it is written, so /tags holding [:urgent] came out of the store as
an instruction to CONJ and what was kept was lost on the way in."
  (let ((n 0)
        (*putting-back* t))
    (loop :for (path text) :in (sqlite:execute-to-list
                                (db s) "select path, value from node")
          :do (multiple-value-bind (value read) (read-back text)
                (let* ((names (tree:split-name path))
                       (at (and names read (apply #'tree:at root names))))
                  (when (and at (node:savedp at))
                    (setf (node:contents at) (node:as-value value))
                    (incf n))))
          :finally (return n))))

(defun stale (s &optional (root (tree:root)))
  "Every path the store holds that nothing stands at any more."
  (loop :for (path) :in (sqlite:execute-to-list (db s) "select path from node")
        :for names := (tree:split-name path)
        :unless (and names (apply #'tree:at root names))
          :collect path))

(defun %attach (root)
  (node:attach (node:answers "store"
                           :reads (lambda ()
                                    (let ((s *store*))
                                      (and s (princ-to-string (file-of s)))))
                           :writes (lambda (value)
                                     (declare (ignore value))
                                     (and *store* (snapshot *store*)))
                           :describes "where this pine persists, and writing it
writes the tree down")
               root))

(pine/fs/tree:builder #'%attach)
