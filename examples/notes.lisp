(defpackage #:notes
  (:use #:pine/user)
  (:shadow #:note)
  (:export #:notes #:note #:journal #:sticky))
(in-package #:notes)

;;; An app, written the way anything is written here: classes and methods in a
;;; package of its own. Nothing under src/ names this file, and this file names
;;; nothing private. It is the editor's equal, and that is the whole point.

;;; A kind of node. Its children are the entries, so an entry is a place: read it
;;; for what it says, write it to say something else, and anything watching one
;;; hears about it -- from this image or from another machine.

(defclass journal (node) ()
  (:documentation "Everything written down. /notes is one of these.

There is no map of entries here. An entry is a node under this one, which is a
place, is saved, and is watched, and keeping the text in a variable beside the tree
would be keeping it twice -- one of them the copy that persists and one of them the
copy anything else can reach."))

(defmethod contents ((n journal)) (sort (listing n) #'string<))

(defmethod make-child ((n journal) name)
  "What is under it is what it says, so putting one there moves it. A mounted
directory says the same thing the same way."
  (let ((made (call-next-method))) (moved n) made))

(defmethod erase-child ((n journal) name)
  (let ((gone (call-next-method))) (moved n) gone))

;;; A mode. The chain is class inheritance, so this is prose with one thing of
;;; its own to say: what its text divides into.
;;;
;;; NOTE is a word pine already uses -- it is how you say something in the log --
;;; so this package shadows it. Nothing is special about pine's words: they are
;;; symbols in a package, and this is what Common Lisp does about that.

(defclass note (prose) ()
  (:documentation "A note: lines, and the headings that divide them."))

(defmethod handles ((m note)) '("*.note"))

(defmethod setting ((m note) key)
  (case key (:comment "#") (T (call-next-method))))

(defun %headingp (line)
  (and (plusp (length line)) (char= #\* (char line 0))))

(defmethod structure ((m note) document)
  "Every heading, and the lines under it, as spans. What comes back is put
in the namespace under the document, so /text/x.note/heading/Today is a place you
can read, write and watch."
  (let ((found NIL)
        (n (line-count document)))
    (dotimes (at n)
      (let ((said (line document at)))
        (when (%headingp said)
          (push (list (string-trim " *" said) at) found))))
    (flet ((ends (at) (cons at (length (line document at)))))
      (let ((all (nreverse found)))
        (when all
          (list (covering "heading"
                          (cons (second (first all)) 0)
                          (ends (1- n))
                          (loop :for ((title from) . more) :on all
                                :for to := (if more
                                               (1- (second (first more)))
                                               (1- n))
                                :collect (covering title (cons from 0)
                                                   (ends (max from to)))))))))))

;;; A role, and a surface on it. One ANCHOR method puts a new kind of surface on
;;; screen; nothing showing it needs knowledge of it, because the role crosses the
;;; wire with the surface.

(defclass sticky (overlay) ()
  (:documentation "A note stuck to the corner of the screen."))

(defmethod anchor ((r sticky) width height)
  (placing :edges '(:top :right) :wide width :tall height
           :margin (inset :top 16 :right 16)))

(defun %latest ()
  "The last thing written down, read through the namespace rather than out of the
node behind it. That is what makes the surface follow it: what a surface reads is
what it is worked out again for, and a place is what it can read."
  (let ((title (first (last (read "/notes" :else (list))))))
    (when title (list title (read (format NIL "/notes/~a" title) :else "")))))

;;; The system. It starts like anything else that runs, which is what puts it at
;;; /system/notes. What it puts up while it starts is its, so there is no STOP:
;;; the place, the surface and the chord all go when it does.

(defclass notes (system) ()
  (:documentation "Notes: a place to write things down, and a note stuck to the
corner of the screen showing the last one."))

(offers 'notes)

(defmethod start ((s notes))
  (puts (make-instance 'journal :name "notes"
                                :describes "what has been written down"))
  (defcommand "note" (title said)
    (:describes "write something down"
     :asks '((:prompt "Note: ")))
    (write (format NIL "/notes/~a" title) (or said ""))
    title)
  (defcommand "notes" () (:describes "everything written down")
    (read "/notes" :else (list)))
  (defcommand "forget-note" (title) (:describes "take one back off")
    (erase (format NIL "/notes/~a" title))
    T)
  (bind 'text "C-c n" "note")
  (defsurface sticky (:as 'sticky)
    (let ((latest (%latest)))
      (column :class "sticky"
              (label (or (first latest) "nothing written down"))
              (label (or (second latest) "")))))
  s)
