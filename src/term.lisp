(defpackage #:pine.term
  (:use :cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:proc #:pine.proc))
  (:export
   #:terminal #:terminal-term #:terminal-fd #:terminal-pid #:terminal-buffer
   #:terminal-on-output
   #:open-terminal #:close-terminal
   #:terminal-for-buffer
   #:term-write
   #:gterm-text
   #:drain-terminals
   #:resize-active-terminal
   #:terminal-dispatch
  ))

(in-package #:pine.term)
(named-readtables:in-readtable pine.path:syntax)

;;;; Terminal buffers: a pine.vt emulator driven by a pty. The reader thread
;;;; only appends bytes to PENDING under a lock; the render tick drains PENDING
;;;; and feeds the emulator, so the term is mutated from one thread. Keys go
;;;; straight to the pty as escape sequences.
;;;;
;;;; A terminal is a /proc runnable: it holds a descriptor, a pid and a reader
;;;; thread. So it is at /proc/term:?buffer, killing it is a write, and closing
;;;; it gives all three back.

(defstruct (terminal (:constructor %make-terminal))
  buffer term fd pid on-output
  ;; PENDING is raw output chunks (newest first) pushed by the reader under
  ;; LOCK. CARRY is what a drain deferred (oldest first, one thread). Chunks
  ;; rather than one growing string keep appends O(1) under a flood.
  (pending nil)
  (carry nil)
  (lock (bordeaux-threads:make-lock))
  reader
  (alive t))

(defparameter *drain-budget* 8192
  "Output bytes fed to an emulator per tick, so a flood does not block the UI
thread.")

(defparameter *carry-cap* (* 4 1024 1024)
  "Deferred output bytes kept. Past this a flood is dropped oldest first.")

(defparameter *wait-ms* 200
  "How long a reader waits for output before looking at its own flag.")

(defparameter *named-keys*
  '(("Up" . :up) ("Down" . :down) ("Left" . :left) ("Right" . :right)
    ("Home" . :home) ("End" . :end) ("Prior" . :page-up) ("Next" . :page-down)
    ("Insert" . :insert) ("Delete" . :delete)
    ("Return" . :enter) ("Tab" . :tab) ("Escape" . :escape)
    ("BackSpace" . :backspace)))

(defun at (name)
  "Where the terminal on buffer NAME is declared."
  (p:path /proc (format nil "term:~a" name)))

(defun terminal-for-buffer (buffer)
  (let ((name (pine.buf:name-of buffer)))
    (when name
      (let ((entry (ns:read (at name))))
        (and (fset:map? entry) (fset:lookup entry :terminal))))))

(defun terminals ()
  "Every terminal this space has open."
  (let ((acc nil))
    (dolist (path (ns:matches /proc/*) (nreverse acc))
      (let ((declaration (ns:read path)))
        (when (and (fset:map? declaration) (fset:lookup declaration :terminal))
          (push (fset:lookup declaration :terminal) acc))))))

(defun gterm-text (terminal)
  (if terminal (pine.vt:term-dump-to-string (terminal-term terminal)) ""))

;;;; Reader thread and draining

(defun start-reader (tobj)
  "Read TOBJ's pty until it is closed or told to stop.

It waits with a timeout rather than blocking: a blocked foreign read cannot be
woken by closing the descriptor, and killing a thread inside one wedges the
image at the next GC."
  (flet ((reader ()
           (loop :while (terminal-alive tobj)
                 :do (when (pine.vt:pty-wait (terminal-fd tobj) *wait-ms*)
                       (let ((s (handler-case
                                    (pine.vt:pty-read-string (terminal-fd tobj) 8192)
                                  (error () nil))))
                         (if (null s)
                             (progn (setf (terminal-alive tobj) nil) (return))
                             (progn
                               (bordeaux-threads:with-lock-held ((terminal-lock tobj))
                                 (push s (terminal-pending tobj)))
                               (alexandria:when-let
                                   ((on-output (terminal-on-output tobj)))
                                 (funcall on-output)))))))))
    (setf (terminal-reader tobj)
          (bordeaux-threads:make-thread #'reader :name "pine-pty-reader"))))

(defun %drain-one (tobj)
  "Feed up to *drain-budget* bytes of TOBJ's pending output into its emulator."
  (let* ((fresh (bordeaux-threads:with-lock-held ((terminal-lock tobj))
                  (prog1 (nreverse (terminal-pending tobj))
                    (setf (terminal-pending tobj) nil))))
         (chunks (nconc (terminal-carry tobj) fresh))
         (budget *drain-budget*)
         (leftover nil)
         (processed nil))
    (setf (terminal-carry tobj) nil)
    (dolist (chunk chunks)
      (if (and (null leftover) (plusp budget))
          (progn
            (handler-case (pine.vt:term-process-output (terminal-term tobj) chunk)
              (error () nil))
            (decf budget (length chunk))
            (setf processed t))
          (push chunk leftover)))
    (when leftover
      (let* ((carry (nreverse leftover))
             (total (reduce #'+ carry :key #'length :initial-value 0)))
        (loop :while (and (> total *carry-cap*) (cdr carry))
              :do (decf total (length (pop carry))))
        (setf (terminal-carry tobj) carry)))
    processed))

(defun drain-terminals (&optional client)
  (declare (ignore client))
  (let ((any nil))
    (dolist (tobj (terminals) any)
      (when (%drain-one tobj) (setf any t)))))

;;;; The runnable

(defun %spawn (name rows cols command on-output)
  (multiple-value-bind (fd pid)
      (pine.vt:spawn-pty-process command :rows rows :cols cols)
    (when (minusp fd) (error "pty spawn failed"))
    (let* ((term (pine.vt:make-term
                  :width cols :height rows
                  ;; the emulator calls input-fn as (term bytes) when a program
                  ;; queries the terminal (device attrs, cursor pos, colours)
                  :input-fn (lambda (term bytes)
                              (declare (ignore term))
                              (pine.vt:pty-write-string fd bytes))))
           (tobj (%make-terminal :buffer name :term term :fd fd :pid pid
                                 :on-output on-output)))
      (start-reader tobj)
      tobj)))

(defmethod proc:start ((kind (eql :terminal)) entry &key &allow-other-keys)
  (let ((declaration (fset:lookup entry :declaration)))
    (fset:map-union
     entry
     {:took (fset:lookup declaration :terminal)
      :state :running})))

(defmethod proc:halt ((kind (eql :terminal)) entry)
  "Give back the descriptor, the pid and the reader thread. Nothing else knows
what a terminal took, so nothing else can."
  (let ((tobj (proc:took entry)))
    (when (terminal-p tobj)
      ;; the flag first: the reader is between waits, so it stops on its own
      ;; and nothing has to kill a thread inside a foreign call
      (setf (terminal-alive tobj) nil)
      (let ((reader (terminal-reader tobj)))
        (when reader
          (ignore-errors (bordeaux-threads:join-thread reader))))
      (ignore-errors (pine.vt:pty-close (terminal-fd tobj)))
      (ignore-errors (pine.vt:pty-reap (terminal-pid tobj)))))
  (fset:map-union entry {:took nil :state :stopped}))

(defmethod proc:alive-p ((kind (eql :terminal)) entry)
  (let ((tobj (proc:took entry)))
    (and (terminal-p tobj) (terminal-alive tobj) t)))

(proc:runnable :terminal)

(defun open-terminal (buffer &key (rows 24) (cols 80) (command "/bin/sh")
                                  on-output)
  "Open a terminal on BUFFER and declare it at /proc, which is what keeps it.
ON-OUTPUT is called with no arguments whenever the pty has said something, so
whoever paces the repaint hears about it instead of polling for it."
  (let* ((name (pine.buf:name-of buffer))
         (tobj (%spawn name rows cols command on-output)))
    (ns:write (at name) (fset:map (:terminal tobj) (:buffer name)))
    tobj))

(defun close-terminal (buffer)
  "Close BUFFER's terminal. Writing the path nil is what stops it."
  (let ((name (pine.buf:name-of buffer)))
    (when name (ns:write (at name) nil))))

(defun resize-active-terminal (cols rows)
  (let* ((buf (ns:read /buf/current))
         (tobj (and buf (terminal-for-buffer buf))))
    (when tobj
      (pine.vt:term-resize (terminal-term tobj) cols rows)
      (pine.vt:pty-set-size (terminal-fd tobj) rows cols))))

;;;; Input: a key to pty bytes

(defun %key-mods (key)
  (append (when (pine.key:key-ctrl key) '(:ctrl))
          (when (pine.key:key-meta key) '(:meta))
          (when (pine.key:key-shift key) '(:shift))))

(defun key->pty-bytes (term key)
  (let* ((sym (pine.key:key-sym key))
         (named (cdr (assoc sym *named-keys* :test #'string=))))
    (cond
      (named (pine.vt:key-event-to-escape-sequence term (cons named (%key-mods key))))
      ((= 1 (length sym))
       (let ((ch (char sym 0)))
         (cond
           ((pine.key:key-ctrl key)
            (string (code-char (logand #x1f (char-code (char-upcase ch))))))
           ((pine.key:key-meta key) (format nil "~C~C" #\Escape ch))
           (t sym))))
      (t nil))))

(defun terminal-dispatch (client key)
  "In a terminal buffer send KEY to the pty, except C-x which stays an editor
prefix so the user can switch away."
  (declare (ignore client))
  (let ((tobj (terminal-for-buffer (ns:read /buf/current))))
    (when tobj
      (if (and (pine.key:key-ctrl key) (string= (pine.key:key-sym key) "x"))
          nil
          (progn
            (let ((bytes (key->pty-bytes (terminal-term tobj) key)))
              (when bytes (pine.vt:pty-write-string (terminal-fd tobj) bytes)))
            t)))))

(defun term-write (buffer text)
  (let ((tobj (terminal-for-buffer buffer)))
    (when tobj (pine.vt:pty-write-string (terminal-fd tobj) text))))

;;;; Terminal mode: the text verbs go to the pty and the shell answers.

(ns:serve :terminal
  {:at [(p:parse "/mode/terminal")]
   :after [:mode]
   :doc "terminal mode: the text verbs go to the pty and the shell answers"
   :up (lambda ()
         (ns:write (p:parse "/mode/terminal")
                   {:parent :text
                    :indicator "Term"
                    :on {:insert (lambda (buf text)
                                   (term-write buf text)
                                   {})
                         :newline (lambda (buf)
                                    (term-write buf (string #\Newline))
                                    {})
                         :delete (lambda (buf from to)
                                   (declare (ignore from to))
                                   (term-write buf (string (code-char 127)))
                                   {})}}))})