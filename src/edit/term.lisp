(defpackage #:pine.edit.term
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:c #:pine.run.cell)
                    (#:node #:pine.fs.node) (#:buffer #:pine.edit.buffer)
                    (#:mode #:pine.repl.mode) (#:cmd #:pine.repl.command)
                    (#:process #:pine.proc.process) (#:task #:pine.run.task)
                    (#:super #:pine.proc.supervisor) (#:fault #:pine.run.fault))
  (:export #:terminal #:terminal-for #:open-terminal #:close-terminal
           #:terminals #:send #:screen #:resize #:fd #:pid #:term-of
           #:install #:*shell* #:key->bytes))

(in-package #:pine.edit.term)

(defvar *shell* (or (uiop:getenv "SHELL") "/bin/sh"))
(defvar *terminals* (c:cell (d:no-map)))

(defparameter +named+
  '(("RET" . :enter) ("TAB" . :tab) ("DEL" . :backspace) ("ESC" . :escape)
    ("Up" . :up) ("Down" . :down) ("Left" . :left) ("Right" . :right)
    ("Home" . :home) ("End" . :end) ("PageUp" . :page-up)
    ("PageDown" . :page-down) ("Insert" . :insert) ("Delete" . :delete)))

(defclass terminal ()
  ((name    :initarg :name    :reader name)
   (fd      :initarg :fd      :reader fd)
   (pid     :initarg :pid     :reader pid)
   (term-of :initarg :term    :reader term-of)
   (reader  :initform nil     :accessor reader)))

(defmethod print-object ((tm terminal) stream)
  (print-unreadable-object (tm stream :type t)
    (format stream "~a pid ~a" (name tm) (pid tm))))

(defun terminals () (d:vals (c:held *terminals*)))

(defun terminal-for (name) (d:at (c:held *terminals*) name))

(defun screen (tm)
  (let ((term (term-of tm)))
    (loop :for row :below (pine.vt:term-height term)
          :collect (with-output-to-string (s)
                     (loop :for cell :across (pine.vt:term-grid-row term row)
                           :do (write-char (or (pine.vt:cell-char cell) #\Space) s))))))

(defun %refresh (tm b)
  (setf (node:contents b) (format nil "~{~a~^~%~}"
                                  (mapcar (lambda (line) (string-right-trim " " line))
                                          (screen tm))))
  (buffer:goto! b (pine.vt:term-cursor-y (term-of tm))
                (pine.vt:term-cursor-x (term-of tm))))

(defun send (tm text)
  (pine.vt:pty-write-string (fd tm) text)
  text)

(defun resize (tm cols rows)
  (pine.vt:term-resize (term-of tm) cols rows)
  tm)

(defun %event (k)
  (let ((named (cdr (assoc (pine.edit.key:key-sym k) +named+ :test #'equal)))
        (mods (append (when (pine.edit.key:key-ctrl k) '(:ctrl))
                      (when (pine.edit.key:key-meta k) '(:meta))
                      (when (pine.edit.key:key-shift k) '(:shift)))))
    (cond (named (cons named mods))
          ((and (= 1 (length (pine.edit.key:key-sym k)))
                (pine.edit.key:key-ctrl k))
           (let ((code (logand 31 (char-code
                                   (char-upcase
                                    (char (pine.edit.key:key-sym k) 0))))))
             (code-char code)))
          ((= 1 (length (pine.edit.key:key-sym k)))
           (char (pine.edit.key:key-sym k) 0))
          (t nil))))

(defun key->bytes (tm k)
  (let ((event (%event k)))
    (when event
      (pine.vt:key-event-to-escape-sequence (term-of tm) event))))

(defun open-terminal (name &key (command *shell*) (cols 80) (rows 24)
                                supervisor)
  (multiple-value-bind (fd pid) (pine.vt:spawn-pty-process command :rows rows :cols cols)
    (let* ((term (pine.vt:make-term :width cols :height rows))
           (tm (make-instance 'terminal :name name :fd fd :pid pid :term term))
           (b (or (buffer:buffer-named name) (buffer:make-buffer name :mode "term"))))
      (c:swap *terminals* (lambda (all) (d:with all name tm)))
      (setf (reader tm)
            (task:spawn (format nil "term ~a" name)
                        (lambda ()
                          (loop
                            (when (pine.vt:pty-wait fd 50)
                              (let ((text (pine.vt:pty-read-string fd 8192)))
                                (when (or (null text) (zerop (length text)))
                                  (return))
                                (fault:attempt
                                 (lambda ()
                                   (pine.vt:term-process-output term text)
                                   (%refresh tm b))
                                 "terminal output")))))))
      (when supervisor
        (super:supervise supervisor
                         (make-instance 'process:thread-process
                                        :name (format nil "term ~a" name)
                                        :restarts nil
                                        :thunk (lambda () nil))))
      (setf (buffer:current) b)
      tm)))

(defun close-terminal (name)
  (let ((tm (terminal-for name)))
    (when tm
      (when (reader tm) (task:stop (reader tm)))
      (ignore-errors (pine.vt:pty-kill (pid tm)))
      (ignore-errors (pine.vt:pty-close (fd tm)))
      (ignore-errors (pine.vt:pty-reap (pid tm)))
      (c:swap *terminals* (lambda (all) (d:without all name))))
    tm))

(defun %typed (k)
  (let* ((b (buffer:current))
         (tm (and b (terminal-for (node:name b)))))
    (when tm
      (let ((bytes (key->bytes tm k)))
        (when bytes (send tm bytes))
        t))))

(defun install ()
  (mode:mode "term" :parent "text" :settings '(:indicator "Term"))
  (mode:handle "term" :insert (lambda (b text)
                                (let ((tm (terminal-for (node:name b))))
                                  (when tm (send tm text)))))
  (cmd:defcommand "terminal" (&optional name)
      (:describes "a shell in a buffer")
    (let ((it (or name (format nil "term-~d" (1+ (length (terminals)))))))
      (open-terminal (princ-to-string it))
      (princ-to-string it)))
  (cmd:defcommand "terminals" () (:describes "every terminal there is")
    (mapcar #'name (terminals)))
  (cmd:defcommand "close-terminal" (name) (:describes "stop a terminal"
                                           :asks '((:prompt "Terminal: ")))
    (and (close-terminal (princ-to-string name)) t))
  t)
