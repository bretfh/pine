(defpackage #:pine/kernel/log
  (:use #:cl)
  (:shadow #:append)
  (:local-nicknames (#:d #:pine/data) (#:said #:pine/said)
                    (#:place #:pine/kernel/place) (#:tell #:pine/kernel/tell)
                    (#:tree #:pine/kernel/tree) (#:bt #:bordeaux-threads))
  (:export
   #:keeping #:forget-keeping #:where #:entries #:append #:replay #:compact
   #:at-time #:written #:wholep #:*where* #:*later* #:settled))
(in-package #:pine/kernel/log)

(defvar *where* nil
  "The file the log is written to, or nothing where nothing outlives the image.")

(defvar *pen* (bt:make-lock "pine/log")
  "Who is writing. One writer, so entries land in the order they were told and a
half-written line is impossible.")

(defvar *owed* nil
  "Entries handed over and not yet on the disk.")

(defvar *broke* nil)

(defvar *later* nil
  "How to put an entry down without waiting for the disk. Filled in by whatever
keeps the image's dispatchers; until it is, the writing thread writes it.")

(defun where () *where*)

(defun %entry (moved)
  "One telling, as what to write down: the time, and every place that moved whose
value outlives the image.

What is worked out is not written. It is worked out again from what it read, and
what it read is written -- so keeping it would be keeping the same fact twice and
giving it two chances to disagree with itself."
  (let ((rows (loop :for each :in moved
                    :when (and (place:keptp each) (place:under each))
                      :collect (let ((v (ignore-errors (place:held each))))
                                 (when (said:sayablep v)
                                   (cons (place:full-name each) (said:said v)))))))
    (let ((rows (remove nil rows)))
      (when rows (list (get-universal-time) rows)))))

(defun append (entry)
  "Put one entry at the end of the log.

Sequential, and off whatever thread wrote. A write is finished when the news is
handed over; it does not wait for a disk. What that costs is a window at the very
end -- entries handed over and not yet down -- and SETTLED is how somebody who
cannot afford it waits."
  (when (and *where* entry)
    (let ((line (let ((*print-readably* nil) (*print-circle* nil)
                      (*print-pretty* nil))
                  (format nil "~s~%" entry))))
      (bt:with-lock-held (*pen*)
        (with-open-file (out *where* :direction :output :external-format :utf-8
                                     :if-exists :append :if-does-not-exist :create)
          (write-string line out)
          (finish-output out)))))
  entry)

(defun %told (moved)
  (let ((entry (%entry moved)))
    (when entry
      (d:swap *owed* (lambda (all) (cons entry all)))
      (flet ((down ()
               (let ((mine (d:emptied *owed*)))
                 (dolist (each (reverse mine)) (append each)))))
        (if *later* (funcall *later* #'down) (down)))))
  moved)

(defun settled (&key (seconds 2))
  "Wait for what has been handed over to be down. For whoever is about to pull
the plug on purpose."
  (loop :repeat (round (/ seconds 0.01))
        :when (null *owed*) :do (return t)
        :do (sleep 0.01)))

(defun keeping (where)
  "Follow the tree: everything that moves and outlives the image is written down.

One entry for each telling, so a group of writes told together is one entry --
which is what makes the log a list of states the tree actually stood in, rather
than a list of the steps between them."
  (setf *where* where)
  (setf (tell:on-move :log) #'%told)
  where)

(defun forget-keeping ()
  (setf (tell:on-move :log) nil *where* nil)
  nil)

(defun wholep (entry)
  "Whether an entry is one somebody finished writing."
  (and (consp entry) (integerp (first entry)) (listp (second entry))
       (null (cddr entry))
       (every (lambda (row) (and (consp row) (stringp (car row))))
              (second entry))))

(defun entries (&optional (where *where*))
  "Every whole entry in the log, and nothing else.

A machine that stops does it somewhere, and where it stops may be halfway through
a line. That line is not an entry -- nothing was ever told it -- so it is dropped
rather than read, and the log ends at the last thing that really happened. This is
the whole of why the tree comes back at a boundary and not partway into one."
  (when (and where (probe-file where))
    (with-open-file (in where :external-format :utf-8)
      (loop :for form := (handler-case (cl:read in nil :done)
                           (error () :done))
            :until (eq form :done)
            :when (wholep form) :collect form))))

(defun written (&optional (where *where*))
  "What the log says stands, as a name-to-value map. The tree is a fold over the
log, and this is the fold: the last thing said about a name is what stands there."
  (let ((out (d:no-map)))
    (dolist (entry (entries where) out)
      (loop :for (name . value) :in (second entry)
            :do (setf out (d:with out name (said:took value)))))))

(defun at-time (when &optional (where *where*))
  "What the log says stood at a time. The same fold, stopped early."
  (let ((out (d:no-map)))
    (dolist (entry (entries where) out)
      (when (> (first entry) when) (return out))
      (loop :for (name . value) :in (second entry)
            :do (setf out (d:with out name (said:took value)))))))

(defun replay (&optional (where *where*))
  "Put what the log says back, without saying any of it moved again.

Held together, so an image coming up is one piece of news and not one for every
line ever written. A name nothing stands at is made, because the log is what
stood and what stood is what should stand again."
  (let ((standing (written where)) (n 0))
    (tell:together
      (d:do-pairs (name value standing)
        (let ((p (or (tree:reach name) (tree:make-under name :value))))
          (when (typep p 'place:value)
            (setf (place:holds p) value (place:written p) t)
            (incf n)))))
    n))

(defun compact (&optional (where *where*))
  "Write the fold down and throw the steps away.

The log is the truth and this does not change what it says; it says the same
thing in fewer lines. Written beside and moved into place, so a machine that
stops in the middle of this still has a log."
  (when (and where (probe-file where))
    (let ((standing (written where))
          (next (make-pathname :type "compacting" :defaults where)))
      (bt:with-lock-held (*pen*)
        (with-open-file (out next :direction :output :external-format :utf-8
                                  :if-exists :supersede :if-does-not-exist :create)
          (let ((*print-readably* nil) (*print-circle* nil) (*print-pretty* nil))
            (prin1 (list (get-universal-time)
                         (loop :for (name . value) :in (d:pairs standing)
                               :collect (cons name (said:said value))))
                   out)
            (terpri out))
          (finish-output out))
        (rename-file next where))
      (d:size standing))))
