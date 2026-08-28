(defpackage #:pine/term/terminal
  (:use #:cl)
  (:local-nicknames (#:text #:pine/text)
                    (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:job #:pine/run/job) (#:log #:pine/fs/log)
                    (#:mode #:pine/mode) (#:fault #:pine/run/fault)
                    (#:vt #:pine/vt))
  (:export #:terminal #:shell #:open-terminal #:terminals #:named #:send #:resize
           #:screen #:vt-of #:fd-of #:pid-of #:runs #:wide #:tall
           #:*shell* #:*chunk*))
(in-package #:pine/term/terminal)

(defvar *shell* nil
  "What a terminal runs when nobody says. The login shell, as the environment
names it.")

(defparameter *chunk* 65536)
(defparameter +waiting+ 100)

(defparameter +named+
  '(("Up" . :up) ("Down" . :down) ("Left" . :left) ("Right" . :right)
    ("Home" . :home) ("End" . :end)
    ("PageUp" . :page-up) ("PageDown" . :page-down)
    ("Delete" . :delete) ("Insert" . :insert)
    ("RET" . #\Return) ("TAB" . #\Tab) ("DEL" . #\Rubout) ("SPC" . #\Space)
    ("Escape" . #\Escape))
  "What a terminal calls the keys pine names.")

(defclass terminal (text:document job:thread)
  ((vt-of  :initarg :vt   :reader vt-of)
   (fd-of  :initform nil  :accessor fd-of)
   (pid-of :initform nil  :accessor pid-of)
   (runs   :initarg :runs :reader runs)
   (wide   :initarg :wide :accessor wide :initform 80)
   (tall   :initarg :tall :accessor tall :initform 24))
  (:documentation "A program's screen, as a document you can read, and the thread
blocked on its pty.

Both, because it is both: a DOCUMENT is what a window shows and what a command
acts on, and a THREAD is where something blocks. The same reason a PEER is an
IMAGE and a MOUNT."))

(defmethod print-object ((term terminal) stream)
  (print-unreadable-object (term stream :type t)
    (format stream "~a ~dx~d~:[ (ended)~;~]" (node:name term)
            (wide term) (tall term) (fd-of term))))

(defun terminals ()
  (remove-if-not (lambda (n) (typep n 'terminal)) (node:nodes (text:root))))

(defun named (name)
  (let ((it (text:named (princ-to-string name))))
    (and (typep it 'terminal) it)))

(defun %rgb (colour)
  "A colour the program asked for, as the three numbers a cell is painted with.
An index is one of the 256 a terminal has; anything else is already the numbers."
  (let ((said (if (integerp colour) (vt:color-index-to-rgb colour) colour)))
    (when said (coerce said 'list))))

(defun %face (props)
  "A run of the program's colour, as a face: what a cell is painted with, and
nothing else. The same three numbers a theme's face works out to, so the grid
paints one the way it paints the other."
  (list (%rgb (getf props :fg))
        (%rgb (getf props :bg))
        (logior (if (getf props :bold) 1 0)
                (if (getf props :italic) 2 0)
                (if (getf props :underline) 4 0))))

(defun screen (term)
  "What is on the screen now: the text, and the runs of colour over it.

The colour is spans on the document, which is what a search that has just landed
says and what a parse says. One kind of thing, painted one way."
  (let ((vt (vt-of term))
        (rows nil)
        (spans nil))
    (dotimes (y (vt:term-height vt))
      (multiple-value-bind (text changes) (vt:term-render-line vt y)
        (push text rows)
        (loop :for (run . more) :on changes
              :do (destructuring-bind (from props) run
                    (let ((to (if more (first (first more)) (length text))))
                      (when (and (> to from) props)
                        (push (list y from to (%face props)) spans)))))))
    (values (format nil "~{~a~^~%~}" (nreverse rows)) (nreverse spans))))

(defun %shown (term)
  "Put what the screen says into the document, and say so. Point follows the
program's cursor: what you are looking at is where it is writing."
  (multiple-value-bind (text spans) (screen term)
    (setf (text:lines term) (text:of text))
    (setf (text:spans term) spans))
  (text:goto term (vt:term-cursor-y (vt-of term)) (vt:term-cursor-x (vt-of term)))
  (setf (text:modified term) nil)
  (node:stir term)
  term)

(defun %escape (term k)
  "The bytes a terminal sends for a key. Control and meta are what the terminal
protocol says they are, not what an editor would do with them."
  (let* ((sym (ui:sym k))
         (said (cdr (assoc sym +named+ :test #'equal)))
         (mods (append (when (ui:ctrl k) '(:ctrl))
                       (when (ui:meta k) '(:meta))
                       (when (ui:shift k) '(:shift)))))
    (cond ((and said (keywordp said))
           (vt:key-event-to-escape-sequence (vt-of term) (cons said mods)))
          ((and said (null mods)) (string said))
          (said (vt:key-event-to-escape-sequence (vt-of term) (cons said mods)))
          ((and (= 1 (length sym)) (ui:ctrl k))
           (let ((c (char-upcase (char sym 0))))
             (when (<= 64 (char-code c) 95)
               (string (code-char (- (char-code c) 64))))))
          ((and (= 1 (length sym)) (ui:meta k))
           (format nil "~c~a" #\Escape sym))
          (t (ui:typed k)))))

(defun send (term said)
  "Give a program what was typed at it. A key becomes what a terminal sends for
it; anything else goes as it stands."
  (let ((fd (fd-of term)))
    (when fd
      (let ((text (etypecase said
                    (string said)
                    (character (string said))
                    (ui:key (%escape term said)))))
        (when (and text (plusp (length text)))
          (fault:attempt (lambda () (vt:pty-write-string fd text))
                         "giving a program what was typed at it")))))
  term)

(defclass shell (mode:text) ()
  (:documentation "Text a program is writing. A key is not an edit: it goes to the
program, and what comes back is the text.

This is what a mode is for. Nothing about the document is special -- PRESS and
INSERT are methods, and the keymap this class inherits is still in force for what
they do not take."))

(defmethod mode:setting ((m shell) key)
  (case key
    (:aside t)
    (:tab-width 8)
    (t (call-next-method))))

(defmethod mode:press ((m shell) (term terminal) key)
  (send term key))

(defmethod mode:insert ((m shell) (term terminal) string)
  (send term string))

(defun resize (term wide tall)
  (when (and (plusp wide) (plusp tall)
             (or (/= wide (wide term)) (/= tall (tall term))))
    (setf (wide term) wide (tall term) tall)
    (vt:term-resize (vt-of term) wide tall)
    (when (fd-of term) (vt:pty-set-size (fd-of term) tall wide))
    (%shown term))
  term)

(defun %ended (term)
  (let ((fd (fd-of term)) (pid (pid-of term)))
    (setf (fd-of term) nil (pid-of term) nil)
    (when fd (fault:or-nothing "the program may have closed it first"
               (vt:pty-close fd)))
    (when pid
      (fault:or-nothing "one that has already ended cannot be killed"
        (vt:pty-kill pid))
      (fault:or-nothing "one already reaped has no status left to take"
        (vt:pty-reap pid))))
  (log:note "~a ended" (node:name term))
  term)

(defun %reading (term)
  "Read what the program wrote until it stops writing or we are asked to stop.
This is what a thread is for: a pty read blocks, and nothing else here does."
  (lambda ()
    (loop :until (job:stopping term)
          :do (when (vt:pty-wait (fd-of term) +waiting+)
                (let ((said (fault:or-nothing "the program ending closes the pty"
                              (vt:pty-read-string (fd-of term) *chunk*))))
                  (if (and said (plusp (length said)))
                      (progn (vt:term-process-output (vt-of term) said)
                             (%shown term))
                      (return)))))
    (%ended term)))

(defmethod (setf node:contents) (value (term terminal))
  "Writing a terminal is typing at it. There is nothing else a write could mean:
the text is the program's, not yours."
  (send term (princ-to-string value))
  value)

(defmethod job:stop :before ((term terminal))
  (when (pid-of term)
    (fault:or-nothing "one that has already ended cannot be killed"
      (vt:pty-kill (pid-of term)))))

(defun shell ()
  (or *shell* (uiop:getenv "SHELL") "/bin/sh"))

(defun open-terminal (name &key runs (wide 80) (tall 24))
  "A program with a screen of its own, in a document. Reading it is reading the
screen; writing it is typing at the program."
  (let* ((runs (or runs (shell)))
         (vt (vt:make-term :width wide :height tall))
         (term (text:make-document (princ-to-string name)
                                  :class 'terminal
                                  :mode (make-instance 'shell)
                                  :vt vt :runs runs :wide wide :tall tall
                                  :restarts nil
                                  :describes runs)))
    (multiple-value-bind (fd pid) (vt:spawn-pty-process runs :rows tall
                                                             :cols wide)
      (unless fd (error "no pty for ~a" runs))
      (setf (fd-of term) fd (pid-of term) pid))
    (setf (vt:term-input-fn vt) (lambda (said) (send term said))
          (job:thunk term) (%reading term))
    (node:slots term term "wide" 'wide "tall" 'tall)
    (job:supervise term)
    (job:start term)
    (%shown term)
    term))

(pine/word:lends "open-terminal")
