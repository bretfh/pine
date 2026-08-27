(defpackage #:notes
  (:use #:pine/user)
  (:shadow #:note)
  (:export #:notes #:note #:entry #:journal #:sticky))
(in-package #:notes)

;;; An app, written the way anything is written here: classes and methods in a
;;; package of its own. Nothing under src/ names this file, and this file names
;;; nothing private. It is the editor's equal, and that is the whole point.

(defvar *entries* (map)
  "What has been written down, by title. A value, so two threads reading it never
see one change underneath them.")

;;; A kind of node. Its children are the entries, so an entry is a place: read it
;;; for what it says, write it to say something else, and anything watching one
;;; hears about it -- from this image or from another machine.

(defclass entry (node) ()
  (:documentation "One thing written down, at a place."))

(defclass journal (node) ()
  (:documentation "Everything written down. /notes is one of these."))

(defun titles () (sort (keys *entries*) #'string<))

(defmethod contents ((n journal)) (titles))
(defmethod nodes ((n journal))
  (loop :for title :in (titles)
        :collect (child n title
                        (lambda () (make-instance 'entry :name title :over n)))))

(defmethod resolve ((n journal) title)
  "Every title is a place, whether or not anything has been written there yet.
Writing one is how an entry comes to exist."
  (let ((title (princ-to-string title)))
    (child n title (lambda () (make-instance 'entry :name title :over n)))))

(defmethod contents ((n entry)) (pine/data:at *entries* (name n)))
(defmethod (setf contents) (said (n entry))
  (setf *entries* (if said
                      (with *entries* (name n) (princ-to-string said))
                      (without *entries* (name n))))
  (stir (over n))
  said)

;;; A mode. The chain is class inheritance, so this is prose with one thing of
;;; its own to say: what its text divides into.
;;;
;;; NOTE is a word pine already uses -- it is how you say something in the log --
;;; so this package shadows it. Nothing is special about pine's words: they are
;;; symbols in a package, and this is what Common Lisp does about that.

(defclass note (prose) ()
  (:documentation "A note: lines, and the headings that divide them."))

(defmethod claims ((m note)) '("*.note"))

(defmethod setting ((m note) key)
  (case key (:comment "#") (T (call-next-method))))

(defun %headingp (line)
  (and (plusp (length line)) (char= #\* (char line 0))))

(defmethod structure ((m note) document)
  "Every heading, and the lines under it. What comes back is put in the namespace
under the document, so /text/x.note/heading/Today is a place you can read, write
and watch."
  (let ((found NIL)
        (n (pine/text/document:line-count document)))
    (dotimes (at n)
      (let ((line (pine/text/document:line document at)))
        (when (%headingp line)
          (push (list (string-trim " *" line) at) found))))
    (flet ((ends (at)
             (cons at (length (pine/text/document:line document at)))))
      (let ((all (nreverse found)))
        (when all
          (list (list* "heading"
                       (cons (second (first all)) 0)
                       (ends (1- n))
                       (loop :for ((title from) . more) :on all
                             :for to := (if more
                                            (1- (second (first more)))
                                            (1- n))
                             :collect (list title (cons from 0)
                                            (ends (max from to)))))))))))

;;; A role, and a surface on it. One ANCHOR method puts a new kind of surface on
;;; screen; nothing showing it needs knowledge of it, because the role crosses the
;;; wire with the surface.

(defclass sticky (overlay) ()
  (:documentation "A note stuck to the corner of the screen."))

(defmethod anchor ((r sticky) width height)
  (map :edges '(:top :right) :wide width :tall height :keeps 0
       :margin '(16 16 0 0)))

(defun %latest ()
  "The last thing written down, read through the namespace rather than out of the
variable behind it. That is what makes the surface follow it: what a surface reads
is what it is worked out again for, and a place is what it can read."
  (let ((title (first (last (read "/notes")))))
    (when title (list title (read (format NIL "/notes/~a" title))))))

;;; The system. It starts and stops like anything else that runs, which is what
;;; puts it at /system/notes and lets you take it away again.

(defclass notes (system) ()
  (:documentation "Notes: a place to write things down, and a note stuck to the
corner of the screen showing the last one."))

(offers 'notes)

(defmethod start ((s notes))
  (attach (make-instance 'journal :name "notes"
                                  :describes "what has been written down")
          (root))
  (defcommand "note" (title said)
    (:describes "write something down"
     :asks '((:prompt "Note: ")))
    (setf (pine/fs/node:contents (at NIL (format NIL "notes/~a" title)))
          (or said ""))
    title)
  (defcommand "notes" () (:describes "everything written down")
    (titles))
  (defcommand "forget-note" (title) (:describes "take one back off")
    (setf (pine/fs/node:contents (at NIL (format NIL "notes/~a" title))) NIL)
    T)
  (bind 'text "C-c n" "note")
  (defsurface sticky (:as 'sticky)
    (let ((latest (%latest)))
      (column :class "sticky"
              (label (or (first latest) "nothing written down"))
              (label (or (second latest) "")))))
  s)

(defmethod stop ((s notes))
  (dolist (name '("note" "notes" "forget-note")) (pine/run/command:forget name))
  (erase NIL "surface/sticky")
  (erase NIL "notes")
  s)
