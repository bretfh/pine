(in-package #:pine.ref)

;;;; Reactive refs: the substrate's answer to eww vars + the broker's state.
;;;; A ref holds a value, DEDUPS on write (per its test) so identical values
;;;; never re-render, and notifies subscribers on real change. Reads inside a
;;;; tracked render record a dependency automatically, so a widget just derefs a
;;;; ref and re-renders when it changes -- no manual wiring. (Distinct from
;;;; display cells, the char grid; a ref is reactive state, not a screen cell.)

(defstruct (ref (:constructor %make-ref) (:copier nil))
  (name        nil)
  (value       nil)
  (test        #'equal :type function)
  (subscribers nil)
  (lock (bordeaux-threads:make-lock)))

(defvar *refs* (make-hash-table :test 'eq)
  "Named ref registry, so widgets bind by name and sources update by name.")

(defun make-ref (&key value (test #'equal) name)
  (let ((r (%make-ref :value value :test test :name name)))
    (when name (setf (gethash name *refs*) r))
    r))

(defun find-ref (name) (gethash name *refs*))

(defmacro defref (name &optional value &key (test '#'equal))
  "Get or create the named ref (idempotent, so redefining a config file is safe)."
  `(or (find-ref ,name) (make-ref :name ,name :value ,value :test ,test)))

;;;; Dependency tracking: when bound, collects the refs read during a render.

(defvar *tracking* nil)

(defun deref (ref)
  (when *tracking* (setf (gethash ref *tracking*) t))
  (bordeaux-threads:with-lock-held ((ref-lock ref)) (ref-value ref)))

(defun set-ref (ref new)
  "Set REF to NEW; notify subscribers only when it actually changed. Thread
safe; subscribers run outside the lock. Returns non-nil if it changed."
  (let (changed subs)
    (bordeaux-threads:with-lock-held ((ref-lock ref))
      (unless (funcall (ref-test ref) (ref-value ref) new)
        (setf (ref-value ref) new changed t
              subs (copy-list (ref-subscribers ref)))))
    (when changed
      (dolist (s subs) (ignore-errors (funcall (cdr s)))))
    changed))

(defun update-ref (ref fn)
  "Set REF to (FN old-value)."
  (set-ref ref (funcall fn (deref ref))))

(defun ref-subscribe (ref thunk)
  (let ((id (list :sub)))
    (bordeaux-threads:with-lock-held ((ref-lock ref))
      (push (cons id thunk) (ref-subscribers ref)))
    id))

(defun ref-unsubscribe (ref id)
  (bordeaux-threads:with-lock-held ((ref-lock ref))
    (setf (ref-subscribers ref)
          (remove id (ref-subscribers ref) :key #'car :test #'eq))))

(defun call-with-tracking (thunk)
  "Run THUNK; return (values result list-of-refs-read)."
  (let ((tbl (make-hash-table :test 'eq)))
    (let ((*tracking* tbl))
      (let ((result (funcall thunk)))
        (values result (loop for k being the hash-keys of tbl collect k))))))

;;;; A reactive view: a THUNK plus an ON-CHANGE callback. Rendering runs the
;;;; thunk with tracking, then subscribes ON-CHANGE to exactly the refs it read;
;;;; the next render re-subscribes to the (possibly different) new set.

(defstruct (reactive-view (:constructor %make-view))
  thunk on-change (subs nil))

(defun make-view (thunk on-change)
  (%make-view :thunk thunk :on-change on-change))

(defun %clear-subs (view)
  (mapc #'funcall (reactive-view-subs view))
  (setf (reactive-view-subs view) nil))

(defun render-view (view)
  "Run the view's thunk, (re)subscribe to the refs it depends on, return its
result."
  (%clear-subs view)
  (multiple-value-bind (result deps)
      (call-with-tracking (reactive-view-thunk view))
    (setf (reactive-view-subs view)
          (mapcar (lambda (r)
                    (let ((id (ref-subscribe r (reactive-view-on-change view))))
                      (lambda () (ref-unsubscribe r id))))
                  deps))
    result))

(defun dispose-view (view)
  (%clear-subs view))
