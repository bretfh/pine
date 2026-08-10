(defpackage #:pine.edit.listing
  (:use #:cl)
  (:local-nicknames (#:cmd #:pine.repl.command) (#:mode #:pine.repl.mode)
                    (#:node #:pine.fs.node) (#:key #:pine.edit.key)
                    (#:text #:pine.edit.text) (#:buffer #:pine.edit.buffer)
                    (#:window #:pine.edit.window) (#:prompt #:pine.edit.prompt)
                    (#:log #:pine.run.log))
  (:export #:install #:into #:activate #:listings))

(in-package #:pine.edit.listing)


(defvar *listings* (make-hash-table :test 'equal))

(defun into (name text &optional activate)
  "Put TEXT in a buffer of its own and show it. With ACTIVATE, the buffer is a
listing: RET on a line hands that line to it."
  (let ((b (or (buffer:buffer-named name) (buffer:make-buffer name))))
    (setf (node:contents b) text)
    (buffer:goto! b 0 0)
    (setf (buffer:minors-of b)
          (if activate
              (adjoin "list" (buffer:minors-of b) :test #'equal)
              (remove "list" (buffer:minors-of b) :test #'equal)))
    (setf (gethash name *listings*) activate)
    (setf (buffer:current) b)
    (node:name b)))

(defun activate ()
  (let* ((b (%b))
         (fn (gethash (node:name b) *listings*)))
    (when fn
      (funcall fn (buffer:line b (buffer:point-line b))))))

(defun %replacing (b from to)
  (let ((n 0))
    (loop
      (multiple-value-bind (line col)
          (text:search (pine.run.cell:held (buffer:lines b)) from
                       (buffer:point-line b) (buffer:point-col b))
        (unless line (return))
        (buffer:goto! b line col)
        (buffer:delete-region! b line col line (+ col (length from)))
        (buffer:insert! b to)
        (incf n)))
    (log:note "replaced ~d" n)
    n))

(defun %b () (buffer:current))

(defun install ()
  (cmd:defcommand "list-buffers" () (:describes "every buffer there is")
    (into "*buffers*"
           (with-output-to-string (out)
             (dolist (b (buffer:buffers))
               (format out "~a~30t~a~%" (node:name b) (or (buffer:file-of b) ""))))
           (lambda (line)
             (let ((name (string-right-trim " " (subseq line 0 (min 30 (length line))))))
               (when (buffer:buffer-named name)
                 (setf (buffer:current) (buffer:buffer-named name)))))))
  (cmd:defcommand "list-activate" () (:describes "open what this line names")
    (activate))
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
    (into "*help*"
           (with-output-to-string (out)
             (format out "chords in force in ~a~%~%" (node:name (%b)))
             (loop :for (chord . name) :in (sort (key:bindings-of (%b)) #'string<
                                                 :key #'car)
                   :do (format out "~16a ~a~%" chord name)))))
  (cmd:defcommand "describe-mode" () (:describes "what this buffer's mode is")
    (let ((m (mode:mode-named (buffer:mode-of (%b)))))
      (into "*help*"
             (with-output-to-string (out)
               (format out "~a~%~%" (buffer:mode-of (%b)))
               (format out "chain     ~{~a~^ -> ~}~%"
                       (mapcar #'mode:name (mode:chain m)))
               (format out "settings  ~a~%" (mode:settings m))
               (format out "minors    ~a~%" (buffer:minors-of (%b)))))))
  t)
