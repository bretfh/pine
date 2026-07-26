(defpackage #:pine.state.world
  (:use #:cl)
  (:export #:register #:save-world #:restore-world)
  (:documentation "What comes back after a restart. Subsystems register
:save/:restore function pairs; the data rides (:world NAME) store keys.
Gated by the :world-save editor variable."))

(in-package #:pine.state.world)

;;;; The world: what comes back after a restart. Subsystems register a pair
;;;; of functions. :save computes readable data from live state when asked and
;;;; :restore applies it; the data rides ordinary (:world NAME) store keys.
;;;; This file knows no contributor; each lives with its own subsystem.
;;;; The :world-save editor variable gates the whole thing.

(pine.state.var:defonce :world-save :default t
  :documentation "When non-nil, the world (buffers, arrangement, scratch)
saves to the store and restores on the next start.")

(defvar *contributors* nil
  "Ordered alist of (NAME . (SAVE-FN . RESTORE-FN)); re-register replaces.")

(defun register (name &key save restore)
  "Register world contributor NAME. SAVE is () -> readable data (nil =
nothing to save); RESTORE is (data) -> applies it. Registration order is
restore order."
  (setf *contributors*
        (append (remove name *contributors* :key #'first)
                (list (cons name (cons save restore)))))
  name)

(defun %enabled-p ()
  (ignore-errors (pine.state.var:var :world-save nil)))

(defun save-world (&optional name)
  "Run NAME's (or every) contributor's :save and store the result under
(:world NAME). Silent no-op when :world-save is off or no store is open."
  (when (%enabled-p)
    (loop :for (n save . nil) :in *contributors*
          :when (and save (or (null name) (eq n name)))
            :do (handler-case
                   (let ((data (funcall save)))
                     ;; nil means nothing to save from this context, so the
                     ;; stored entry stands rather than being erased
                     (when data
                       (setf (pine.state.store:store (list :world n)) data)))
                 (error (c)
                   (format *error-output* "world save ~a failed: ~a~%" n c))))))

(defun restore-world (&optional name)
  "Read NAME's (or every) contributor's (:world NAME) entry and apply it. A
failing contributor logs and skips, so the daemon always comes up."
  (when (%enabled-p)
    (loop :for (n nil . restore) :in *contributors*
          :when (and restore (or (null name) (eq n name)))
            :do (let ((data (pine.state.store:store (list :world n))))
                 (when data
                   (handler-case (funcall restore data)
                     (error (c)
                       (format *error-output*
                               "world restore ~a failed: ~a~%" n c))))))))
