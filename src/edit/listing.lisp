(defpackage #:pine/edit/listing
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:cmd #:pine/repl/command) (#:mode #:pine/repl/mode)
                    (#:node #:pine/fs/node) (#:key #:pine/edit/key)
                    (#:text #:pine/edit/text) (#:buffer #:pine/edit/buffer)
                    (#:window #:pine/edit/window) (#:prompt #:pine/edit/prompt)
                    (#:log #:pine/run/log))
  (:export #:install #:into #:activate #:listings #:listing #:rows #:acts
           #:place #:row-at #:step! #:said))
(in-package #:pine/edit/listing)

(defvar *listings* (d:table))

(defclass listing ()
  ((rows :initarg :rows :accessor rows :initform nil)
   (acts :initarg :acts :accessor acts :initform nil)))

(defmethod print-object ((l listing) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~d row~:p~:[~; you can act on~]" (length (rows l)) (acts l))))

(defun listings () (d:all *listings*))

(defun %of (b) (d:at (d:all *listings*) (node:name b)))

(defun said (row) (if (consp row) (car row) (princ-to-string row)))

(defun %place-of (row) (and (consp row) (cdr row)))

(defun row-at (b &optional (line (buffer:point-line b)))
  "The row point is on: what it says, and what it stands for."
  (let ((l (%of b)))
    (when l (nth line (rows l)))))

(defun place (&optional (b (buffer:current)))
  "The place the row point is on stands for. This is what a key acts on: a row
names a buffer, a node or a path, so nothing has to read the line back out of
the text to know what it was about."
  (%place-of (row-at b)))

(defun %mark (b)
  (setf (buffer:setting b :selection) (place b))
  b)

(defun into (name rows &optional acts)
  "Put ROWS in a buffer of its own and show it. A row is a string, or (TEXT .
PLACE) where PLACE is what that row stands for. With ACTS, the buffer is a
listing: RET on a row hands ACTS the place it stands for."
  (let ((b (or (buffer:buffer-named name) (buffer:make-buffer name)))
        (rows (if (stringp rows)
                  (uiop:split-string rows :separator '(#\Newline))
                  rows)))
    (setf (node:contents b)
          (format nil "~{~a~^~%~}" (mapcar #'said rows)))
    (buffer:goto! b 0 0)
    (setf (buffer:minors-of b)
          (if acts
              (adjoin "list" (buffer:minors-of b) :test #'equal)
              (remove "list" (buffer:minors-of b) :test #'equal)))
    (d:keep! *listings* name (make-instance 'listing :rows rows :acts acts))
    (setf (buffer:current) b)
    (%mark b)
    (node:name b)))

(defun activate ()
  (let* ((b (buffer:current))
         (l (%of b)))
    (when (and l (acts l))
      (funcall (acts l) (place b)))))

(defun step! (delta &optional (b (buffer:current)))
  "The row after this one, or before it, wrapping round the ends."
  (let* ((l (%of b))
         (n (length (and l (rows l)))))
    (when (plusp n)
      (buffer:goto! b (mod (+ (buffer:point-line b) delta) n) 0)
      (%mark b)
      (buffer:point-line b))))

(defun %replacing (b from to)
  (let ((n 0))
    (loop
      (multiple-value-bind (line col)
          (text:search (pine/data:held (buffer:lines b)) from
                       (buffer:point-line b) (buffer:point-col b))
        (unless line (return))
        (buffer:goto! b line col)
        (buffer:delete-region! b line col line (+ col (length from)))
        (buffer:insert! b to)
        (incf n)))
    (log:note "replaced ~d" n)
    n))

(defun %b () (buffer:current))

(defun %buffer-rows ()
  (mapcar (lambda (b)
            (cons (format nil "~a~30t~a" (node:name b) (or (buffer:file-of b) ""))
                  b))
          (buffer:buffers)))

(defun install ()
  (cmd:defcommand "jobs" () (:describes "what this image is running")
    (into "*jobs*"
          (append
           (list (format nil "~16a ~10a ~a" "what" "state" "name") "")
           (loop :for p :in (pine/proc/supervisor:processes
                             pine/repl/shell:*supervisor*)
                 :collect (cons (format nil "~16a ~10a ~a" "process"
                                        (pine/proc/process:state p)
                                        (pine/proc/process:name p))
                                p))
           (loop :for tk :in (pine/run/task:tasks)
                 :collect (cons (format nil "~16a ~10a ~a" "thread"
                                        (if (pine/run/task:alivep tk) :running :done)
                                        (pine/run/task:name tk))
                                tk))
           (loop :for a :in (pine/run/endpoint:endpoints)
                 :collect (cons (format nil "~16a ~10a ~a" "endpoint" :running
                                        (pine/run/endpoint:name a))
                                a))
           (loop :for name :in (pine/run/timer:names)
                 :collect (format nil "~16a ~10a ~a" "tick" :running name)))
          (lambda (it)
            (log:note "~a" (or it "that row is a heading")))))
  (cmd:defcommand "list-buffers" () (:describes "every buffer there is")
    (into "*buffers*" (%buffer-rows)
          (lambda (b) (when (node:nodep b) (setf (buffer:current) b)))))
  (cmd:defcommand "list-activate" () (:describes "open what this row stands for")
    (activate))
  (cmd:defcommand "list-next" () (:describes "the row after this one")
    (step! 1))
  (cmd:defcommand "list-prev" () (:describes "the row before this one")
    (step! -1))
  (cmd:defcommand "list-place" () (:describes "what the row point is on stands for")
    (let ((it (place)))
      (log:note "~a" (cond ((null it) "this row stands for nothing")
                           ((node:nodep it) (node:full-name it))
                           (t it)))
      it))
  (cmd:defcommand "query-replace" (from)
      (:describes "replace one string with another"
       :asks '((:prompt "Replace: ")))
    (let ((b (%b))
          (from (princ-to-string from)))
      (prompt:ask (format nil "Replace ~a with: " from)
                  :then (lambda (to) (%replacing b from to)))
      :asking))
  (cmd:defcommand "describe-key" () (:describes "what a chord runs")
    (prompt:ask "Describe key: "
                :then (lambda (chord)
                        (let ((c (mode:binding (%b) chord)))
                          (log:note "~a: ~a" chord
                                    (if c (format nil "~a, ~a" (cmd:name c)
                                                  (cmd:describes c))
                                        "undefined"))))))
  (cmd:defcommand "describe-bindings" () (:describes "every chord in force here")
    (let ((of (%b)))
      (into "*help*"
            (cons (format nil "chords in force in ~a" (node:name of))
                  (cons ""
                        (loop :for (chord . name) :in (sort (key:bindings-of of)
                                                            #'string< :key #'car)
                              :collect (cons (format nil "~16a ~a" chord name)
                                             (cmd:command-named name)))))
            (lambda (c) (when c (log:note "~a" (or (cmd:describes c) (cmd:name c))))))))
  (cmd:defcommand "describe-mode" () (:describes "what this buffer's mode is")
    (let ((m (mode:mode-named (buffer:mode-of (%b)))))
      (into "*help*"
            (list (buffer:mode-of (%b))
                  ""
                  (format nil "chain     ~{~a~^ -> ~}" (mapcar #'mode:name (mode:chain m)))
                  (format nil "settings  ~a" (mode:settings m))
                  (format nil "minors    ~a" (buffer:minors-of (%b)))))))
  t)
