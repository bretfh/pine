(defpackage #:vcs
  (:use #:pine/user)
  (:export #:vcs #:board))
(in-package #:vcs)

;;; An app that brings a device of its own. Nothing under src/ knows what a
;;; checkout is, and this file names nothing private: a device is a declaration,
;;; so a package that uses PINE/USER and nothing else can add one to /dev.

;;; The place everything here is about. A value node, so pointing it somewhere
;;; else is a write and every reading below is worked out again -- the device rows
;;; read it, so the graph says so and nothing here has to say it twice.

(defun %at ()
  (read "/work" :else (namestring *default-pathname-defaults*)))

(defun %how-many (said)
  "How many lines a program printed. LINES is not a word the language has: it means
a document's lines in one package and splitting text in another, and pine will not
let one word be both."
  (if (plusp (length said)) (1+ (count #\N ewline said)) 0))

;;; The device. What it is, said once; how this machine answers it, said once per
;;; way of answering. The first backing whose programs are all on the path wins,
;;; and where none of them is there every reading says :ABSENT rather than NIL --
;;; so a board on a machine with neither shows a dash, not a branch called nothing.

(defdevice vcs
  :describes "the checkout at /work: its branch, what is uncommitted, and the
last thing done"
  :refreshes 5)

(defbacking vcs (:needs "git")
  (branch :reads (sh "git -C ~a rev-parse --abbrev-ref HEAD 2>/dev/null" (%at))
          :writes (lambda (said) (sh "git -C ~a switch ~a" (%at) said) T))
  (dirty :reads (%how-many (sh "git -C ~a status --porcelain 2>/dev/null" (%at))))
  (head :reads (sh "git -C ~a log -1 --format=%s 2>/dev/null" (%at))))

(defbacking vcs (:needs "jj")
  (branch :reads (sh "jj -R ~a log -r @ --no-graph -T bookmarks 2>/dev/null" (%at))
          :writes (lambda (said) (sh "jj -R ~a bookmark set ~a" (%at) said) T))
  (dirty :reads (%how-many (sh "jj -R ~a diff --name-only 2>/dev/null" (%at))))
  (head :reads (sh "jj -R ~a log -r @ --no-graph -T description 2>/dev/null"
                   (%at))))

;;; A role of its own, and a surface on it. The role says where it goes and
;;; nothing showing it needs to know what a board is.

(defclass board (overlay) ()
  (:documentation "A strip in the corner saying where the work is."))

(defmethod anchor ((r board) width height)
  (placing :edges '(:bottom :left) :wide width :tall height
           :margin (inset :bottom 12 :left 12)))

;;; The system. It declares a place, a device, a surface, two commands and a
;;; chord, and defines no STOP: what it put up while it started is its, and goes
;;; when it does.

(defclass vcs (system) ()
  (:documentation "What is being worked on: the checkout at /work, and a board
in the corner saying where it is and what is uncommitted."))

(offers 'vcs)

(defmethod start ((s vcs))
  (puts (make "work" :held (namestring *default-pathname-defaults*)
                     :describes "the checkout everything here is about"))
  (device "vcs")
  (defcommand "work" (&optional where)
    (:describes "the checkout, or where to point it")
    (when where (write "/work" (princ-to-string where)))
    (read "/work"))
  (defcommand "branch" () (:describes "what branch the checkout is on")
    (read "/dev/vcs/branch"))
  (bind 'text "C-c v" "branch")
  (defsurface board (:as 'board)
    (column :class "board"
            (label (read "/dev/vcs/branch" :else "no checkout"))
            (label (format NIL "~a uncommitted"
                           (read "/dev/vcs/dirty" :else 0)))
            (label (read "/dev/vcs/head" :else ""))))
  s)
