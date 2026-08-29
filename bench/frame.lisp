(require :asdf)
(require :sb-sprof)
(asdf:load-system :pine/test)

(defpackage #:pine/bench/frame
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:text #:pine/text) (#:edit #:pine/edit)
                    (#:ui #:pine/ui) (#:mode #:pine/mode)
                    (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)))
(in-package #:pine/bench/frame)

(defvar *lines* (or (ignore-errors (parse-integer (uiop:getenv "LINES"))) 20000))
(defparameter +cols+ 100)
(defparameter +rows+ 42)

(defun text-of (lines)
  (format nil "~{~a~^~%~}"
          (loop :for i :below lines
                :collect (format nil "(defun f~d (x) (+ x ~d))" i i))))

(defun waited (d &key (seconds 120))
  (let ((p (text:parser-for d)))
    (when p
      (loop :repeat (round (/ seconds 0.02))
            :until (text:currentp p)
            :do (sleep 0.02)))
    p))

(defun cost (label n thunk)
  (sb-ext:gc :full t)
  (let ((before (sb-ext:get-bytes-consed))
        (at (get-internal-real-time)))
    (dotimes (i n) (funcall thunk))
    (let* ((secs (max 1d-6 (/ (- (get-internal-real-time) at)
                              (float internal-time-units-per-second))))
           (bytes (- (sb-ext:get-bytes-consed) before)))
      (format t "~&~40@a ~9,3f ms ~12:d bytes~%"
              label (/ (* secs 1000) n) (round bytes n))
      (force-output))))

(defun main ()
  (pine:start)
  (pine:use :text)
  (pine:use :edit)
  (let* ((d (text:make-document "frame" :mode (make-instance 'mode:lisp)))
         (w (edit:focused)))
    (setf (node:contents d) (text-of *lines*))
    (setf (edit:across w) +cols+ (edit:down w) +rows+)
    (edit:show w d)
    (setf (text:current) d)
    (text:goto d 10 0)
    (setf (node:contents (tree:at nil "surface/editor/size"))
          (list :wide (* 9 +cols+) :tall (* 18 +rows+)
                :cols +cols+ :lines +rows+ :font 15))
    (waited d)

    (format t "~&~%a document of ~:d lines, a window of ~d rows~%~%" *lines* +rows+)

    (let ((runs (text:highlights d)))
      (format t "~&~40@a ~:d~%" "highlight runs the parser has walked"
              (length runs))
      (format t "~&~40@a ~:d~%" "runs a screenful could possibly want"
              (* +rows+ 8)))

    (let ((p (text:parser-for d)))
      (format t "~&~40@a ~:d~%" "lines the found map holds"
              (pine/data:size (slot-value p 'pine/text::found))))
    (format t "~&~%as it is while somebody types -- a key before each~%~%")
    (let ((n 0))
      (cost "a key, then the whole wire" 60
            (lambda ()
              (edit:dispatch (ui:make-key (string (code-char (+ 97 (mod (incf n) 26))))))
              (node:contents (tree:at nil "surface/editor/wire"))))
      (cost "a key, then text:highlights" 60
            (lambda ()
              (edit:dispatch (ui:make-key (string (code-char (+ 97 (mod (incf n) 26))))))
              (text:highlights d)))
      (cost "a key, then note alone" 60
            (lambda ()
              (edit:dispatch (ui:make-key (string (code-char (+ 97 (mod (incf n) 26))))))
              (pine/text::note d)))
      (cost "a key, and nothing else" 60
            (lambda ()
              (edit:dispatch (ui:make-key (string (code-char (+ 97 (mod (incf n) 26)))))))))
    (waited d)

    (format t "~&~%and standing still~%~%")
    (let ((p (text:parser-for d)))
      (cost "text:highlights, both halves" 20 (lambda () (text:highlights d)))
      (cost "  the note half alone" 20 (lambda () (pine/text::note d)))
      (cost "  the flatten half alone" 20
            (lambda () (pine/text::%flat (slot-value p 'pine/text::found))))
      (cost "  band alone" 20 (lambda () (text:band d)))
      (cost "  parser-for alone" 20 (lambda () (text:parser-for d))))
    (cost "text:spans" 20 (lambda () (text:spans d)))
    (cost "the whole wire the screen is handed" 20
          (lambda () (node:contents (tree:at nil "surface/editor/wire"))))

    (format t "~&~%and paging to the end, then again~%~%")
    (setf (edit:scrolled w) (max 0 (- *lines* +rows+)))
    (text:goto d (edit:scrolled w) 0)
    (node:contents (tree:at nil "surface/editor/wire"))
    (waited d)
    (let ((runs (text:highlights d)))
      (format t "~&~40@a ~:d~%" "highlight runs now walked" (length runs)))
    (cost "text:highlights" 20 (lambda () (text:highlights d)))
    (cost "the whole wire" 20
          (lambda () (node:contents (tree:at nil "surface/editor/wire"))))
    (pine:stop)))

(main)
(sb-ext:exit)
