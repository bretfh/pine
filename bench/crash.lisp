(require :asdf)
(asdf:load-system :pine/kernel)

(defpackage #:pine/bench/crash
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:tree #:pine/kernel/tree)
                    (#:log #:pine/kernel/log) (#:k #:pine/kernel/call)))
(in-package #:pine/bench/crash)

(defvar *where* (or (uiop:getenv "LOG") "/tmp/pine-crash.log"))

(defun writing ()
  "Write in a loop until somebody pulls the plug.

Every group of three must be all there or none there. A machine that stops in the
middle of one and comes back holding two of them is a machine whose log is a list
of steps rather than a list of states."
  (log:keeping *where*)
  (let ((n 0))
    (loop
      (incf n)
      (k:together
        (k:write "/three/a" n)
        (k:write "/three/b" n)
        (k:write "/three/c" n)))))

(defun three (entry)
  (loop :for (name . value) :in (second entry)
        :when (member name '("/three/a" "/three/b" "/three/c") :test #'equal)
          :collect value))

(defun neither-p (entry)
  "Whether an entry names some of the three and not all of them at one number."
  (let ((said (three entry)))
    (and said
         (not (and (eql 3 (length said))
                   (every (lambda (each) (eql each (first said))) said))))))

(defun looking ()
  (let* ((entries (log:entries *where*))
         (neither (count-if #'neither-p entries)))
    (format t "~&~:d entries in the log~%" (length entries))
    (format t "~&~30@a ~a~%" "every entry all or none:"
            (if (zerop neither) "yes"
                (format nil "NO ~d of them are neither" neither)))
    (setf tree:*root* (tree:make-root))
    (log:replay *where*)
    (let ((a (k:read "/three/a")) (b (k:read "/three/b")) (c (k:read "/three/c")))
      (format t "~&~30@a a ~a, b ~a, c ~a~%" "what came back:" a b c)
      (format t "~&~30@a ~a~%" "at a boundary:"
              (if (and (eql a b) (eql b c)) "yes" "NO"))
      (if (and (zerop neither) (eql a b) (eql b c))
          (format t "~&~%nothing partial survived.~%")
          (progn (format t "~&~%something partial survived.~%")
                 (sb-ext:exit :code 1))))))

(if (equal "look" (uiop:getenv "DO")) (looking) (writing))
(sb-ext:exit)
