(defpackage #:notes
  (:use #:cl #:pine)
  (:shadowing-import-from #:pine #:read #:write #:map #:set)
  (:import-from #:pine/mode #:prose #:handles #:setting #:regions #:covering #:bind)
  (:import-from #:pine/text #:line #:line-count)
  (:import-from #:pine/ui
   #:overlay #:anchor #:placing #:inset #:defsurface #:column #:label)
  (:local-nicknames (#:fs #:pine/fs))
  (:shadow #:note)
  (:export #:notes #:note #:sticky))
(in-package #:notes)

(named-readtables:in-readtable pine/fs/reader:syntax)

;;; An app, written the way anything is written here: classes and methods in a
;;; package of its own. Nothing under src/ names this file, and this file names
;;; nothing private. It is the editor's equal, and that is the whole point.

;;; A place. /notes is a mount and each note a value under it: read one for what
;;; it says, write it to say something else, and anything watching it hears --
;;; from this image or from another machine. There is no map of notes beside the
;;; tree: keeping the text in a variable would be keeping it twice, one copy that
;;; persists and one anything else can reach.

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

(defmethod regions ((m note) buffer)
  "Every heading, and the lines under it, as spans. What comes back is put
in the namespace under the buffer, so /text/x.note/heading/Today is a place you
can read, write and watch."
  (let ((found NIL)
        (n (line-count buffer)))
    (dotimes (at n)
      (let ((said (line buffer at)))
        (when (%headingp said)
          (push (list (string-trim " *" said) at) found))))
    (flet ((ends (at) (cons at (length (line buffer at)))))
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
  (placing :edges '(:top :right) :width width :height height
           :margin (inset :top 16 :right 16)))

(defun %latest ()
  "The last thing written height, read through the namespace rather than out of the
node behind it. That is what makes the surface follow it: what a surface reads is
what it is worked out again for, and a place is what it can read."
  (let ((title (first (last (read "/notes" :else (list))))))
    (when title (list title (read (format NIL "/notes/~a" title) :else "")))))

;;; The system. It starts like anything else that runs, which is what puts it at
;;; /proc/notes. What it puts up while it starts is its, so there is no STOP:
;;; the place, the surface and the chord all go when it does.

(defclass notes (module) ()
  (:documentation "Notes: a place to write things height, and a note stuck to the
corner of the screen showing the last one."))


(defmethod start ((s notes))
  (mount (make-instance 'mount :describes "what has been written height") "/notes")
  (defcommand "note" (title said)
    (:describes "write something height"
     :asks '((:prompt "Note: ")))
    (write (format NIL "/notes/~a" title) (or said ""))
    title)
  (defcommand "notes" () (:describes "everything written height")
    (read "/notes" :else (list)))
  (defcommand "forget-note" (title) (:describes "take one back off")
    (erase (format NIL "/notes/~a" title))
    T)
  (bind 'text "C-c n" "note")
  (defsurface sticky (:as 'sticky)
    (let ((latest (%latest)))
      (column :class "sticky"
              (label (or (first latest) "nothing written height"))
              (label (or (second latest) "")))))
  s)
