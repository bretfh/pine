(defpackage #:vcs
  (:use #:cl #:pine #:pine/ui #:pine/host)
  (:shadowing-import-from #:pine #:read #:write #:map #:set)
  (:import-from #:pine/mode #:bind)
  (:import-from #:pine/edit #:completes)
  (:export #:vcs #:board))
(in-package #:vcs)

(named-readtables:in-readtable pine/fs/reader:syntax)

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
  (if (plusp (length said)) (1+ (count #\Newline said)) 0))

(defun %split (said)
  "What a program printed, a line at a time."
  (let ((out NIL) (from 0))
    (dotimes (at (length said))
      (when (char= #\Newline (char said at))
        (when (> at from) (push (subseq said from at) out))
        (setf from (1+ at))))
    (when (> (length said) from) (push (subseq said from) out))
    (nreverse out)))

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


(defmethod start ((s vcs))
  (puts (make-instance 'value :name "work"
                       :held (namestring *default-pathname-defaults*)
                       :describes "the checkout everything here is about"))
  (device "vcs")
  (defcommand "work" (&optional where)
    (:describes "the checkout, or where to point it")
    (when where (write "/work" (princ-to-string where)))
    (read "/work"))
  (defcommand "branch" () (:describes "what branch the checkout is on")
    (read "/dev/vcs/branch"))
  (bind 'text "C-c v" "branch")

  ;; A kind of question of its own, and the words that answer it. Nothing pine
  ;; ships knows what a branch is; COMPLETES is how a package says what its own
  ;; category offers, so the prompt narrows over branches the way it does over
  ;; commands.
  (completes :vcs-branch
             (lambda (text)
               (declare (ignore text))
               (%split (sh "git -C ~a for-each-ref --format=%(refname:short) refs/heads 2>/dev/null"
                           (%at)))))

  (defcommand "switch-branch" (name)
    (:describes "check another branch out"
     :asks '((:prompt "Branch: " :category :vcs-branch :must-match T)))
    (write "/dev/vcs/branch" (princ-to-string name))
    (read "/dev/vcs/branch"))
  (defsurface board (:as 'board)
    (column :class "board"
            (label (read "/dev/vcs/branch" :else "no checkout"))
            (label (format NIL "~a uncommitted"
                           (read "/dev/vcs/dirty" :else 0)))
            (label (read "/dev/vcs/head" :else ""))))
  s)
