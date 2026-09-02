(in-package #:pine/fs/node)

(defun child (n name builder)
  "The child N keeps under NAME, made once. Two threads asking at once both answer
the one that landed, which is what lets anything reading it be worked out again.

A builder that answers nothing leaves nothing behind: a name nobody has put
anything at is asked about once per ask, rather than filling the memo with an
entry that says so and that every walk of the children then has to step over."
  (let ((name (princ-to-string name)))
    (or (d:lookup (d:all (memo n)) name)
        (let ((made (funcall builder)))
          (when made (d:claim (memo n) name made))))))

(defgeneric attach (node into)
  (:documentation "Put NODE under INTO, in place of whatever stood at its name.

Whatever stood there is let go of first. A node taken out of the tree and left in
the reader sets of what it read is worked out for ever after, every time any of
that moves, and holds all of it for as long as the image runs -- so replacing one
without detaching it is the leak DETACH is written to stop, spelled the other way
round. A surface declared twice leaked the first one, and it went on being worked
out from every device it had ever read.

The order it was attached in and the name it answers to move together, in one
replacement, so the two cannot disagree.

INTO moved: what a branch holds is what is under it, so putting something there is
a write and whatever listed it is worked out again.")
  (:method ((n node) (into node))
    (let* ((said (%said (name n)))
           (had (d:lookup (by-name into) said)))
      (when (and had (not (eq had n)))
        (detach into said)
        (when (eq had (d:lookup (d:all (memo into)) said))
          (d:drop! (memo into) said)))
      (setf (parent n) into)
      (d:swap (slot-value into 'under)
              (lambda (all)
                (%under (d:with (d:as :seq (cl:remove said
                                                      (d:as :list (under-order all))
                                                      :key #'name :test #'equal))
                                n)
                        (d:with (under-by-name all) said n))))
      (moved into))
    n))

(defgeneric detach (node name)
  (:documentation "Take NAME off NODE, and stop it reading anything.

What it read has to be given up here. A node taken off the tree and left in the
reader sets of what it read is worked out for ever after, every time any of that
moves, and holds all of it for as long as the image runs. A surface that was
erased goes on being worked out from /dev/cpu.

What is kept in the memo is not dropped here, so a name taken off and put back
answers the same node it did before: that is what lets a region survive its
document being read again. ERASE-CHILD is the one that means it has gone.")
  (:method ((n node) name)
    (let ((gone (resolve n name)))
      (when gone
        (d:swap (slot-value n 'under)
                (lambda (all)
                  (%under (d:remove gone (under-order all))
                          (d:without (under-by-name all) (%said (name gone))))))
        (dolist (on (saw gone)) (undepend gone on))
        (setf (saw gone) nil)
        (setf (parent gone) nil)
        (moved n))
      gone)))

(defgeneric make-child (node name)
  (:documentation "A fresh child of NODE named NAME, made in whatever stands behind
it: a plain node keeps it here, a mounted directory makes a file on the disk. A name
that ends in / asks for a branch.")
  (:method ((n node) name)
    (attach (make-instance 'value :name (string-right-trim "/" (princ-to-string name)))
            n)))

(defgeneric erase-child (node name)
  (:documentation "Take NAME out of NODE, and out of whatever stands behind it.

Saying the path went is COMMIT:FORGET's, not this one's: a store keeping a copy
of the tree hears an erasure the same way it hears a write, and this layer names
nothing that is listening.")
  (:method ((n node) name)
    "What was kept is let go after it is detached, not before. Dropped first, the
detach asks for it again, a worked-out child is built again to be taken off, and
what is left behind in the memo is that second one with nothing over it."
    (let ((gone (resolve n name)))
      (when gone (commit:forget (full-name gone)))
      (let ((it (detach n name)))
        (d:drop! (memo n) (princ-to-string name))
        (or it gone)))))

(defun slots (object into &rest pairs)
  "One node per slot of OBJECT, under INTO."
  (loop :for (name slot) :on pairs :by #'cddr
        :collect (attach (make-instance 'slot
                                        :name (string-downcase (string name))
                                        :object object :slot slot :into into)
                         into)))
