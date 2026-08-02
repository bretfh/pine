(defpackage #:pine.kill
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:b #:pine.text))
  (:export #:push-text #:top #:entry #:set-mark #:kill-region #:kill-line
           #:kill-words #:yank #:yank-pop #:copy-region #:*kept*))

(in-package #:pine.kill)
(named-readtables:in-readtable pine.path:syntax)

;;;; The ring at /kill, and where the last yank landed at /kill/at, so a second
;;;; window on the same ring has its own of the second.

(defparameter *kept* 60)

(defun push-text (text)
  (when (and text (plusp (length text)))
    (ns:write /kill text :max *kept* :keep t)))

(defun top () (ns:read /kill))

(defun entry (n) (ns:read (p:path /kill (princ-to-string n))))

(defun %current ()
  (let ((at (ns:read /buf/current)))
    (and at (p:leaf at))))

(defun set-mark ()
  (let ((name (%current)))
    (when name
      (multiple-value-bind (line col) (pine.buf:point name)
        (ns:write (pine.buf:at name :mark) (fset:seq line col))
        (pine.echo:message "mark set")))))

(defun kill-region ()
  (let ((name (%current)))
    (when name (ns:write (pine.buf:at name :text) (fset:seq :kill)))))

(defun kill-line ()
  (let ((name (%current)))
    (when name
      (let* ((state (pine.buf:state name))
             (snap (b:state->snapshot state))
             (pl (b:point-line snap))
             (pc (b:point-col snap))
             (line (fset:@ (b:lines snap) pl))
             (len (length line)))
        (if (< pc len)
            (progn
              (push-text (subseq line pc))
              (ns:write (pine.buf:at name :text)
                        (fset:seq :delete (fset:seq pl pc) (fset:seq pl len))))
            (when (< (1+ pl) (b:line-count snap))
              (push-text (string #\Newline))
              (ns:write (pine.buf:at name :text)
                        (fset:seq :delete (fset:seq pl pc) (fset:seq (1+ pl) 0)))))))))

(defun kill-words (n)
  (let ((name (%current)))
    (when name
      (let* ((state (pine.buf:state name))
             (snap (b:state->snapshot state))
             (pl (b:point-line snap))
             (pc (b:point-col snap)))
        (multiple-value-bind (tl tc) (b:point-after-move snap :word n)
          (multiple-value-bind (sl sc el ec)
              (if (or (< tl pl) (and (= tl pl) (< tc pc)))
                  (values tl tc pl pc)
                  (values pl pc tl tc))
            (unless (and (= sl el) (= sc ec))
              (push-text (b:region-string state sl sc el ec))
              (ns:write (pine.buf:at name :text)
                        (fset:seq :delete (fset:seq sl sc) (fset:seq el ec))))))))))

(defun yanked ()
  (ns:read /kill/at))

(defun (setf yanked) (value)
  (ns:write /kill/at value))

(defun %after (line col text)
  (loop :for ch :across text
        :do (if (char= ch #\Newline)
                (setf line (1+ line) col 0)
                (incf col)))
  (values line col))

(defun yank ()
  (let ((name (%current))
        (text (top)))
    (when (and name text)
      (multiple-value-bind (line col) (pine.buf:point name)
        (setf (yanked) (fset:seq name line col text 0)))
      (ns:write (pine.buf:at name :text) (fset:seq :insert text)))))

(defun yank-pop ()
  (let ((was (yanked)))
    (when (and (member (pine.key:last) (list "yank" "yank-pop") :test #'equal)
               (fset:seq? was))
      (let ((name (fset:lookup was 0)) (sl (fset:lookup was 1))
            (sc (fset:lookup was 2)) (old (fset:lookup was 3))
            (back (fset:lookup was 4)))
        (let ((new (or (entry (1+ back)) (entry 0))))
          (when new
            (multiple-value-bind (el ec) (%after sl sc old)
              (ns:write (pine.buf:at name :text)
                        (fset:seq :delete (fset:seq sl sc) (fset:seq el ec))))
            (ns:write (pine.buf:at name :text) (fset:seq :insert new))
            (setf (yanked)
                  (fset:seq name sl sc new
                            (if (entry (1+ back)) (1+ back) 0)))))))))

(defun copy-region ()
  (let ((name (%current)))
    (when name
      (let ((state (pine.buf:state name)))
        (multiple-value-bind (sl sc el ec) (b:region-bounds state)
          (when sl
            (push-text (b:region-string state sl sc el ec))
            (pine.echo:message "copied")))))))
