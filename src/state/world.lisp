(defpackage #:pine.state.world
  (:use #:cl)
  (:local-nicknames (#:store #:pine.state.store))
  (:shadow #:push #:open #:close)
  (:export #:*enabled* #:open #:close #:id
           #:value #:push #:items #:clear #:forget
           #:snapshot #:revive #:names #:save #:restore)
  (:documentation "How anything in pine persists.

One facility, whatever the value is and whoever owns it: a mode's state, a
command's scrap, a subsystem's history.

  (value NAME)          a durable value; (setf (value NAME) V) writes
  (push NAME V)         a durable bounded list, newest first
  (snapshot NAME)       a method a subsystem answers; state computed at save
  (revive NAME DATA)    its other half"))

(in-package #:pine.state.world)

(defvar *enabled* t
  "When true, SAVE and RESTORE run their contributors.

A plain special rather than an editor variable: this layer sits under the
editor, and the editor's own variables persist through here.

Durable values and lists are not gated; writing through as they change is what
they are for.")

;;;; Every durable thing lives under (:world NAME), so there is one namespace to
;;;; inspect or clear rather than one per feature.

(defun %key (name) (list :world name))

(defun open (&optional path)
  "Open the store everything persists into. Returns the path opened."
  (store:open path))

(defun close ()
  "Close it. Safe with none open."
  (store:close))

(defun id ()
  "A fresh identity for something that has to be nameable from another image."
  (store:id))

(defun value (name &optional default)
  "The durable value under NAME, or DEFAULT when it has none. setf-able."
  (store:value (%key name) default))

(defun (setf value) (new name &optional default)
  (declare (ignore default))
  (setf (store:value (%key name)) new))

(defun forget (name)
  "Drop the durable value under NAME."
  (store:forget (%key name)))

;;;; Bounded lists: histories, recents, rings. Newest first, trimmed to MAX, an
;;;; equal entry moved to the front rather than duplicated under UNIQUE.

(defun push (name new &key (max 100) (unique t))
  "Append NEW to the durable list NAME, trimmed to its newest MAX entries."
  (store:push (%key name) new :max max :unique unique))

(defun items (name &key limit)
  "The durable list NAME, newest first, LIMIT entries at most."
  (store:items (%key name) :limit limit))

(defun clear (name)
  "Drop the whole durable list NAME."
  (store:clear (%key name)))

;;;; State computed from what is live rather than written as it changes -- the
;;;; window arrangement, the open buffers. A subsystem answers SNAPSHOT for its
;;;; own name and applies REVIVE; writing the method is the whole registration,
;;;; and this file names none of them.

(defgeneric snapshot (name)
  (:documentation "NAME's durable form, computed from live state. NIL when
there is nothing to save from here, which leaves the stored entry standing.")
  (:method (name) (declare (ignore name)) nil))

(defgeneric revive (name data)
  (:documentation "Apply DATA as NAME.")
  (:method (name data) (declare (ignore name data)) nil))

(defun names ()
  "Every name a subsystem answers SNAPSHOT for, in definition order."
  (loop :for method :in (c2mop:generic-function-methods #'snapshot)
        :for spec = (first (c2mop:method-specializers method))
        :when (typep spec 'c2mop:eql-specializer)
          :collect (c2mop:eql-specializer-object spec)))

(defun save (&optional name)
  "Store NAME's snapshot, or every name's. No-op when *ENABLED* is off or
nothing is open."
  (when *enabled*
    (dolist (n (if name (list name) (names)))
      (handler-case
          (let ((data (snapshot n)))
            (when data (setf (value n) data)))
        (error (c)
          (format *error-output* "world save ~a failed: ~a~%" n c))))))

(defun restore (&optional name)
  "Revive NAME from the store, or every name. A failure logs and skips, so the
daemon always comes up."
  (when *enabled*
    (dolist (n (if name (list name) (names)))
      (let ((data (value n)))
        (when data
          (handler-case (revive n data)
            (error (c)
              (format *error-output* "world restore ~a failed: ~a~%" n c))))))))
