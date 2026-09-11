(defpackage #:pine/term
  (:use #:cl)
  (:local-nicknames (#:text #:pine/text) (#:edit #:pine/edit)
                    (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:job #:pine/run/job) (#:log #:pine/fs/log)
                    (#:module #:pine/run/module) (#:command #:pine/run/command)
                    (#:mode #:pine/mode) (#:fault #:pine/run/fault)
                    (#:vt #:pine/vt))
  (:export
   #:current #:terminal #:shell #:open-terminal #:terminals #:send
   #:resize #:runs #:width))
(in-package #:pine/term)

(defvar *shell* nil)

(defparameter *chunk* 65536)
(defparameter +waiting+ 100)

(defparameter +named+
  '(("Up" . :up) ("Down" . :down) ("Left" . :left) ("Right" . :right)
    ("Home" . :home) ("End" . :end)
    ("PageUp" . :page-up) ("PageDown" . :page-down)
    ("Delete" . :delete) ("Insert" . :insert)
    ("RET" . #\Return) ("TAB" . #\Tab) ("DEL" . #\Rubout) ("SPC" . #\Space)
    ("Escape" . #\Escape)))

(defclass terminal (text:buffer job:thread)
  ((vt-of  :initarg :vt   :reader vt-of)
   (fd-of  :initform nil  :accessor fd-of)
   (pid-of :initform nil  :accessor pid-of)
   (runs   :initarg :runs :reader runs)
   (width   :initarg :width :accessor width :initform 80)
   (height   :initarg :height :accessor height :initform 24)))

(defmethod print-object ((term terminal) stream)
  (print-unreadable-object (term stream :type t)
    (format stream "~a ~dx~d~:[ (ended)~;~]" (fs:name term)
            (width term) (height term) (fd-of term))))

(defun terminals ()
  (remove-if-not (lambda (n) (typep n 'terminal)) (fs:children (fs:at "/text"))))

(defun %rgb (colour)
  (let ((said (if (integerp colour) (vt:color-index-to-rgb colour) colour)))
    (when said (coerce said 'list))))

(defun %face (props)
  (list (%rgb (getf props :fg))
        (%rgb (getf props :bg))
        (logior (if (getf props :bold) 1 0)
                (if (getf props :italic) 2 0)
                (if (getf props :underline) 4 0))))

(defun screen (term)
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
  (multiple-value-bind (text spans) (screen term)
    (setf (text:lines term) (text:of text))
    (setf (text:spans term) spans))
  (text:goto term (vt:term-cursor-y (vt-of term)) (vt:term-cursor-x (vt-of term)))
  (setf (text:modified term) nil)
  (fs:touch term)
  term)

(defun %escape (term k)
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

(defclass shell (mode:text) ())

(defmethod mode:setting ((m shell) key)
  (case key
    (:aside t)
    (:tab-width 8)
    (t (call-next-method))))

(defmethod mode:press ((m shell) (term terminal) key)
  (send term key))

(defmethod mode:typing ((m shell) (term terminal) string)
  (send term string))

(defun resize (term width height)
  (when (and (plusp width) (plusp height)
             (or (/= width (width term)) (/= height (height term))))
    (setf (width term) width (height term) height)
    (vt:term-resize (vt-of term) width height)
    (when (fd-of term) (vt:pty-set-size (fd-of term) height width))
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
  (log:note "~a ended" (fs:name term))
  term)

(defun %reading (term)
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

(defmethod (setf text:text) (value (term terminal))
  (send term (princ-to-string value))
  value)

(defmethod job:stop :before ((term terminal))
  (when (pid-of term)
    (fault:or-nothing "one that has already ended cannot be killed"
      (vt:pty-kill (pid-of term)))))

(defun shell ()
  (or *shell* (uiop:getenv "SHELL") "/bin/sh"))

(defun open-terminal (name &key runs (width 80) (height 24))
  (let* ((runs (or runs (shell)))
         (vt (vt:make-term :width width :height height))
         (term (text:make-buffer (princ-to-string name)
                                  :class 'terminal
                                  :mode (make-instance 'shell)
                                  :vt vt :runs runs :width width :height height
                                  :on-fault :leave
                                  :describes runs)))
    (multiple-value-bind (fd pid) (vt:spawn-pty-process runs :rows height
                                                             :cols width)
      (unless fd (error "no pty for ~a" runs))
      (setf (fd-of term) fd (pid-of term) pid))
    (setf (vt:term-input-fn vt) (lambda (said) (send term said))
          (job:body term) (%reading term))
    (job:supervise term)
    (job:start term)
    (%shown term)
    term))

(defmethod fs:names ((tm terminal))
  '((:width  . "how many columns it has")
    (:height . "how many lines it has")))

(defmethod fs:read ((tm terminal) (name (eql :width)))
  (width tm))

(defmethod fs:write ((tm terminal) (name (eql :width)) value)
  (setf (width tm) value))

(defmethod fs:read ((tm terminal) (name (eql :height)))
  (height tm))

(defmethod fs:write ((tm terminal) (name (eql :height)) value)
  (setf (height tm) value))
