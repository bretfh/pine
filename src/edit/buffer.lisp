(defpackage #:pine/edit/buffer
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node) (#:mount #:pine/fs/mount)
                    (#:tree #:pine/fs/tree) (#:world #:pine/world/world)
                    (#:mode #:pine/repl/mode) (#:text #:pine/edit/text)
                    (#:d #:pine/data) (#:history #:pine/edit/history))
  (:export #:buffer #:make-buffer #:buffers #:buffer-named
           #:kill-buffer #:scratch #:current #:current-buffer #:asidep #:*on-current* #:lines #:point #:mark
           #:mode-of #:minors-of #:file-of #:source #:origin #:tick #:properties
           #:source-node #:*visiting*
           #:line #:line-count #:text-of #:insert! #:delete-back! #:newline!
           #:delete-region! #:goto! #:move! #:region-of #:mark!
           #:point-line #:point-col #:changed #:modified #:settings #:setting
           #:visited #:leaving!
           #:past #:undo! #:redo! #:undoable #:redoable
           #:marks #:mark-at #:put-mark! #:drop-mark!
           #:propertize! #:properties-at #:clear-properties! #:edit-of
           #:overlay! #:overlays-at #:clear-overlays!
           #:indent-line! #:indent-of))
(in-package #:pine/edit/buffer)

(defvar *current* nil)
(defvar *on-current* nil)
(defvar *visiting* nil)
(defvar *places* (d:no-map))

(defclass buffer (node:node)
  ((lines      :initform (d:box (text:lines-of "")) :reader lines)
   (point-line :initform 0   :accessor point-line)
   (point-col  :initform 0   :accessor point-col)
   (mark       :initform nil :accessor mark)
   (mode-of    :initarg :mode   :accessor mode-of   :initform "text")
   (minors-of  :initarg :minors :accessor minors-of :initform nil)
   (source     :initarg :source :reader source     :initform nil)
   (file-of    :initarg :file   :reader file-of    :initform nil)
   (tick       :initform 0   :accessor tick)
   (past       :initform (history:history) :reader past)
   (marks      :initform (d:no-map) :accessor marks)
   (properties :initform (d:no-seq) :accessor properties)
   (edit-of    :initform nil :accessor edit-of)
   (modified   :initform nil :accessor modified)
   (settings   :initform (d:no-map) :accessor settings)))

(defclass source-node (node:node) ())

(defmethod print-object ((b buffer) stream)
  (print-unreadable-object (b stream :type t)
    (format stream "~a ~d:~d" (node:name b) (point-line b) (point-col b))))

(defun point (b) (list (point-line b) (point-col b)))

(defmethod node:contents ((b buffer)) (text:text-of (d:held (lines b))))

(defmethod (setf node:contents) (value (b buffer))
  (%note b)
  (setf (edit-of b) nil)
  (d:put! (lines b) (text:lines-of (princ-to-string value)))
  (changed b)
  value)

(defmethod node:persistp ((b buffer)) nil)

(defmethod mode:in-force ((b buffer))
  (append (sort (remove nil (mapcar #'mode:mode-named (minors-of b)))
                #'> :key #'mode:precedence)
          (mode:chain (mode:mode-named (mode-of b)))))

(defmethod mode:setting ((b buffer) key &optional default)
  (mode:setting (mode:mode-named (mode-of b)) key default))

(defun %bytes (text)
  (length (sb-ext:string-to-octets (or text "") :external-format :utf-8)))

(defun %edited (b was line old-lines new-lines bytes)
  "What the last edit did, for whoever parses this buffer: at LINE, OLD-LINES
lines became NEW-LINES and it grew by BYTES, counted from WAS. What was marked
on the text moves with it."
  (setf (edit-of b) (list (list line old-lines new-lines bytes) was))
  (%properties-after b line old-lines new-lines))

(defun changed (b)
  (setf (modified b) t)
  (incf (tick b))
  (node:invalidate b)
  b)

(defun %attach-slots (b)
  (node:slots b b "point-line" 'point-line "point-col" 'point-col
                  "mode" 'mode-of "tick" 'tick)
  (node:attach (make-instance 'source-node :name "file"
                                           :describes "where this buffer reads and writes")
               b)
  b)

(defun %buffers-node (&optional (w world:*world*))
  (world:ensure w "buffer"))

(defun asidep (b)
  "Whether B is shown somewhere other than a window: the prompt is drawn on the
echo line, so a window goes on showing what it was showing while one is up."
  (and (typep b 'buffer) (setting b :aside) t))

(defun make-buffer (name &rest initargs &key &allow-other-keys)
  (let ((b (apply #'make-instance 'buffer :name name initargs)))
    (node:attach b (%buffers-node))
    (%attach-slots b)
    (world:identify world:*world* b)
    b))

(defun buffers (&optional (w world:*world*))
  (remove-if-not (lambda (n) (typep n 'buffer)) (node:nodes (%buffers-node w))))

(defun buffer-named (name &optional (w world:*world*))
  (let ((n (tree:at (%buffers-node w) (princ-to-string name))))
    (and (typep n 'buffer) n)))

(defun scratch ()
  "The buffer there is always one of. Killing the last one makes a fresh one
rather than leaving the editor with nothing to type into."
  (or (buffer-named "scratch") (make-buffer "scratch" :mode "lisp")))

(defun kill-buffer (name)
  "Forget a buffer, and leave something current. A buffer that is gone must not
still be what every command reads."
  (let ((b (buffer-named name)))
    (when b
      (node:detach (%buffers-node) (node:name b))
      (when (eq b *current*)
        (setf *current* (or (first (buffers)) (scratch)))))
    b))

(defun current () *current*)

(defun current-buffer () *current*)

(defun (setf current) (b)
  "What is being typed into. The window with the keyboard follows it, so the
buffer a command switches to is the buffer on the screen."
  (when *current* (leaving! *current*))
  (setf *current* (if (stringp b) (buffer-named b) b))
  (when (and *current* *on-current*) (funcall *on-current* *current*))
  *current*)

(defun line (b n) (text:line-at (d:held (lines b)) n))

(defun line-count (b) (text:line-count (d:held (lines b))))

(defun text-of (b) (text:text-of (d:held (lines b))))

(defun goto! (b line col)
  (multiple-value-bind (line col) (text:clamp (d:held (lines b)) line col)
    (setf (point-line b) line (point-col b) col)
    (node:invalidate b)
    (point b)))

(defun move! (b unit n)
  (multiple-value-bind (line col)
      (text:move-by unit (d:held (lines b)) (point-line b) (point-col b) n)
    (goto! b line col)))

(defun %note (b)
  (history:remember (past b) (d:held (lines b)) (point-line b) (point-col b))
  b)

(defun undoable (b) (history:undoable (past b)))
(defun redoable (b) (history:redoable (past b)))

(defun %restore (b was)
  (when was
    (d:put! (lines b) (history:state-lines was))
    (setf (point-line b) (history:state-line was)
          (point-col b) (history:state-col was))
    (changed b))
  (and was (point b)))

(defun undo! (b)
  (%restore b (history:undo (past b)
                            (history:state (d:held (lines b))
                                           (point-line b) (point-col b)))))

(defun redo! (b)
  (%restore b (history:redo (past b)
                            (history:state (d:held (lines b))
                                           (point-line b) (point-col b)))))

(defun mark-at (b name) (d:at (marks b) name))

(defun put-mark! (b name &optional (line (point-line b)) (col (point-col b)))
  (setf (marks b) (d:with (marks b) name (list line col)))
  (list line col))

(defun drop-mark! (b name)
  (setf (marks b) (d:without (marks b) name))
  name)

(defun propertize! (b line from to props)
  (setf (properties b) (d:with (properties b) (list line from to props)))
  props)

(defun %adjusted (span line old-lines new-lines)
  "Where a span sits after an edit at LINE turned OLD-LINES into NEW-LINES.
A span above is untouched, one below shifts, and one the edit ran through is
dropped rather than left describing the wrong text."
  (destructuring-bind (at from to props) span
    (let ((delta (- new-lines old-lines))
          (through (+ line old-lines)))
      (cond ((< at line) span)
            ((and (>= at line) (< at through))
             (if (and (= at line) (zerop delta)) span nil))
            (t (list (+ at delta) from to props))))))

(defun %properties-after (b line old-lines new-lines)
  (setf (properties b)
        (d:as :seq
              (remove nil (mapcar (lambda (span)
                                    (%adjusted span line old-lines new-lines))
                                  (d:as :list (properties b)))))))

(defun overlay! (b line text &optional (face :comment))
  "Text drawn after a line rather than in it: what an evaluation answered, what
a checker said."
  (setf (properties b)
        (d:with (properties b) (list line 0 0 (list :after text :face face))))
  text)

(defun overlays-at (b line)
  (let (acc)
    (d:do-seq (i each (properties b) (nreverse acc))
      (declare (ignore i))
      (destructuring-bind (at from to props) each
        (declare (ignore from to))
        (when (and (= at line) (getf props :after)) (push props acc))))))

(defun clear-overlays! (b)
  (setf (properties b)
        (d:as :seq (remove-if (lambda (span) (getf (fourth span) :after))
                              (d:as :list (properties b)))))
  b)

(defun properties-at (b line col)
  (let (acc)
    (d:do-seq (i each (properties b) (nreverse acc))
      (declare (ignore i))
      (destructuring-bind (at from to props) each
        (when (and (= at line) (>= col from) (< col to))
          (push props acc))))))

(defun clear-properties! (b)
  (setf (properties b) (d:no-seq))
  b)

(defun indent-of (b line)
  (text:indent-width (line b line)))

(defun indent-line! (b line target)
  (let* ((text (line b line))
         (had (text:indent-width text))
         (body (subseq text had))
         (fresh (concatenate 'string (make-string target :initial-element #\Space)
                             body)))
    (unless (equal text fresh)
      (%note b)
      (let ((was (d:held (lines b))))
        (%edited b was line 1 1 (- (%bytes fresh) (%bytes text)))
        (d:put! (lines b) (d:with-at was line fresh)))
      (when (= line (point-line b))
        (setf (point-col b) (max 0 (+ (point-col b) (- target had)))))
      (changed b))
    target))

(defun insert! (b string)
  (%note b)
  (let ((was (d:held (lines b)))
        (at (point-line b)))
    (multiple-value-bind (fresh line col)
        (text:insert was (point-line b) (point-col b) string)
      (%edited b was at 1 (1+ (count #\Newline string)) (%bytes string))
      (d:put! (lines b) fresh)
      (setf (point-line b) line (point-col b) col)
      (changed b)
      (point b))))

(defun newline! (b) (insert! b (string #\Newline)))

(defun delete-back! (b &optional (n 1))
  (%note b)
  (multiple-value-bind (line col)
      (text:move-by :char (d:held (lines b)) (point-line b) (point-col b) (- n))
    (multiple-value-bind (fresh at-line at-col taken)
        (text:delete (d:held (lines b)) line col (point-line b) (point-col b))
      (d:put! (lines b) fresh)
      (setf (point-line b) at-line (point-col b) at-col)
      (changed b)
      taken)))

(defun delete-region! (b from-line from-col to-line to-col)
  (%note b)
  (let ((was (d:held (lines b))))
    (multiple-value-bind (fresh at-line at-col taken)
        (text:delete was from-line from-col to-line to-col)
      (%edited b was from-line (1+ (- to-line from-line)) 1 (- (%bytes taken)))
        (d:put! (lines b) fresh)
      (goto! b at-line at-col)
      (changed b)
      taken)))

(defun mark! (b &optional (line (point-line b)) (col (point-col b)))
  (setf (mark b) (list line col)))

(defun region-of (b)
  (when (mark b)
    (destructuring-bind (line col) (mark b)
      (text:region (d:held (lines b)) line col (point-line b) (point-col b)))))

(defun origin (b)
  "What this buffer is remembered under: the host path when it is on one, and
the path in the tree otherwise."
  (or (file-of b) (let ((n (source b))) (and n (node:full-name n)))))

(defun visited (b)
  "Where point was the last time this place was open."
  (d:at *places* (origin b)))

(defun (setf source) (n b)
  "What this buffer is a view onto. A buffer on a file carries the host path
too, because compiling and loading one is said in paths."
  (setf (slot-value b 'source) n
        (slot-value b 'file-of) (and (typep n 'mount:file-node)
                                     (namestring (mount:truename-of n))))
  (node:invalidate b)
  n)

(defmethod node:contents ((n source-node)) (origin (node:parent n)))

(defmethod (setf node:contents) (value (n source-node))
  "Writing where a buffer reads from opens it there."
  (when *visiting* (funcall *visiting* (node:parent n) (princ-to-string value)))
  value)

(defmethod node:leafp ((n source-node)) t)

(defmethod node:persistp ((n source-node)) nil)

(defun leaving! (b)
  "Remember where point was, so coming back lands where you left."
  (let ((where (origin b)))
    (when where (setf *places* (d:with *places* where (point b)))))
  b)

(defun setting (b key &optional default)
  "What this buffer reads for KEY: its own, then its modes', then the default.
A buffer overrides a setting by holding one of the same name."
  (multiple-value-bind (held there) (d:at (settings b) key)
    (declare (ignore there))
    (or held (mode:setting (mode-of b) key default))))

(defun (setf setting) (value b key)
  (setf (settings b) (d:with (settings b) key value))
  value)

