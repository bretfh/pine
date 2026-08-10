(defpackage #:pine.edit.buffer
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell) (#:node #:pine.fs.node)
                    (#:tree #:pine.fs.tree) (#:world #:pine.world.world)
                    (#:mode #:pine.repl.mode) (#:text #:pine.edit.text)
                    (#:d #:pine.data) (#:history #:pine.edit.history))
  (:export #:buffer #:make-buffer #:buffers #:buffer-named
           #:kill-buffer #:scratch #:current #:current-buffer #:*on-current* #:lines #:point #:mark
           #:mode-of #:minors-of #:file-of #:tick #:properties
           #:line #:line-count #:text-of #:insert! #:delete-back! #:newline!
           #:delete-region! #:goto! #:move! #:region-of #:mark! #:visit! #:save!
           #:point-line #:point-col #:changed
           #:past #:undo! #:redo! #:undoable #:redoable
           #:marks #:mark-at #:put-mark! #:drop-mark!
           #:propertize! #:properties-at #:clear-properties! #:edit-of
           #:indent-line! #:indent-of))

(in-package #:pine.edit.buffer)

(defvar *current* nil)
(defvar *on-current* nil)

(defclass buffer (node:node)
  ((lines      :initform (c:cell (text:lines-of "")) :reader lines)
   (point-line :initform 0   :accessor point-line)
   (point-col  :initform 0   :accessor point-col)
   (mark       :initform nil :accessor mark)
   (mode-of    :initarg :mode   :accessor mode-of   :initform "text")
   (minors-of  :initarg :minors :accessor minors-of :initform nil)
   (file-of    :initarg :file   :accessor file-of   :initform nil)
   (tick       :initform 0   :accessor tick)
   (past       :initform (history:history) :reader past)
   (marks      :initform (d:no-map) :accessor marks)
   (properties :initform (d:no-seq) :accessor properties)
   (edit-of    :initform nil :accessor edit-of)))

(defmethod print-object ((b buffer) stream)
  (print-unreadable-object (b stream :type t)
    (format stream "~a ~d:~d" (node:name b) (point-line b) (point-col b))))

(defun point (b) (list (point-line b) (point-col b)))

(defmethod node:contents ((b buffer)) (text:text-of (c:held (lines b))))

(defmethod (setf node:contents) (value (b buffer))
  (%note b)
  (setf (edit-of b) nil)
  (c:put (lines b) (text:lines-of (princ-to-string value)))
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
lines became NEW-LINES and it grew by BYTES, counted from WAS."
  (setf (edit-of b) (list (list line old-lines new-lines bytes) was)))

(defun changed (b)
  (incf (tick b))
  (node:invalidate b)
  b)

(defun %attach-slots (b)
  (node:slots b b "point-line" 'point-line "point-col" 'point-col
                  "mode" 'mode-of "file" 'file-of "tick" 'tick)
  b)

(defun %buffers-node (&optional (w world:*world*))
  (world:ensure w "buf"))

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
  (setf *current* (if (stringp b) (buffer-named b) b))
  (when (and *current* *on-current*) (funcall *on-current* *current*))
  *current*)

(defun line (b n) (text:line-at (c:held (lines b)) n))

(defun line-count (b) (text:line-count (c:held (lines b))))

(defun text-of (b) (text:text-of (c:held (lines b))))

(defun goto! (b line col)
  (multiple-value-bind (line col) (text:clamp (c:held (lines b)) line col)
    (setf (point-line b) line (point-col b) col)
    (node:invalidate b)
    (point b)))

(defun move! (b unit n)
  (multiple-value-bind (line col)
      (text:move-by unit (c:held (lines b)) (point-line b) (point-col b) n)
    (goto! b line col)))

(defun %note (b)
  (history:remember (past b) (c:held (lines b)) (point-line b) (point-col b))
  b)

(defun undoable (b) (history:undoable (past b)))
(defun redoable (b) (history:redoable (past b)))

(defun %restore (b was)
  (when was
    (c:put (lines b) (history:state-lines was))
    (setf (point-line b) (history:state-line was)
          (point-col b) (history:state-col was))
    (changed b))
  (and was (point b)))

(defun undo! (b)
  (%restore b (history:undo (past b)
                            (history:state (c:held (lines b))
                                           (point-line b) (point-col b)))))

(defun redo! (b)
  (%restore b (history:redo (past b)
                            (history:state (c:held (lines b))
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
      (let ((was (c:held (lines b))))
        (%edited b was line 1 1 (- (%bytes fresh) (%bytes text)))
        (c:put (lines b) (d:with-at was line fresh)))
      (when (= line (point-line b))
        (setf (point-col b) (max 0 (+ (point-col b) (- target had)))))
      (changed b))
    target))

(defun insert! (b string)
  (%note b)
  (let ((was (c:held (lines b)))
        (at (point-line b)))
    (multiple-value-bind (fresh line col)
        (text:insert was (point-line b) (point-col b) string)
      (%edited b was at 1 (1+ (count #\Newline string)) (%bytes string))
      (c:put (lines b) fresh)
      (setf (point-line b) line (point-col b) col)
      (changed b)
      (point b))))

(defun newline! (b) (insert! b (string #\Newline)))

(defun delete-back! (b &optional (n 1))
  (%note b)
  (multiple-value-bind (line col)
      (text:move-by :char (c:held (lines b)) (point-line b) (point-col b) (- n))
    (multiple-value-bind (fresh at-line at-col taken)
        (text:delete (c:held (lines b)) line col (point-line b) (point-col b))
      (c:put (lines b) fresh)
      (setf (point-line b) at-line (point-col b) at-col)
      (changed b)
      taken)))

(defun delete-region! (b from-line from-col to-line to-col)
  (%note b)
  (let ((was (c:held (lines b))))
    (multiple-value-bind (fresh at-line at-col taken)
        (text:delete was from-line from-col to-line to-col)
      (%edited b was from-line (1+ (- to-line from-line)) 1 (- (%bytes taken)))
        (c:put (lines b) fresh)
      (goto! b at-line at-col)
      (changed b)
      taken)))

(defun mark! (b &optional (line (point-line b)) (col (point-col b)))
  (setf (mark b) (list line col)))

(defun region-of (b)
  (when (mark b)
    (destructuring-bind (line col) (mark b)
      (text:region (c:held (lines b)) line col (point-line b) (point-col b)))))

(defun visit! (b path)
  (setf (file-of b) (namestring path))
  (setf (node:contents b)
        (if (probe-file path) (uiop:read-file-string path) ""))
  (let ((m (mode:mode-for path)))
    (when m (setf (mode-of b) (mode:name m))))
  (goto! b 0 0)
  b)

(defun save! (b &optional (path (file-of b)))
  (when path
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string (text-of b) out))
    (setf (file-of b) (namestring path))
    path))
