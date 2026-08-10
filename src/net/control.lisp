(defpackage #:pine.net.control
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell) (#:node #:pine.fs.node)
                    (#:tree #:pine.fs.tree) (#:watch #:pine.fs.watch)
                    (#:world #:pine.world.world) (#:server #:pine.net.server)
                    (#:cmd #:pine.repl.command) (#:session #:pine.repl.session)
                    (#:log #:pine.run.log) (#:fault #:pine.run.fault))
  (:export #:serve #:received #:*on-quit* #:*on-reload* #:*agents*
           #:said #:at))

(in-package #:pine.net.control)

(defvar *on-quit* nil)
(defvar *on-reload* (lambda () nil))
(defvar *agents* (lambda () nil))
(defvar *watches* (c:cell nil))

(defun said (value)
  (typecase value
    (null "")
    (string value)
    (list (format nil "~{~a~^~%~}" (mapcar #'said value)))
    (t (princ-to-string value))))

(defun at (where)
  (let ((root (world:root world:*world*)))
    (if (or (null where) (equal where "/"))
        root
        (tree:at root (string-left-trim "/" (princ-to-string where))))))

(defun %read (where)
  (let ((n (at where)))
    (if n
        (list :ok (said (node:contents n)))
        (list :no (format nil "no node at ~a" where)))))

(defun %write (where text)
  (let ((n (tree:ensure (world:root world:*world*)
                        (string-left-trim "/" (princ-to-string where)))))
    (setf (node:contents n)
          (let ((*package* (or (find-package :pine.user) *package*)))
            (handler-case (read-from-string text) (error () text))))
    (list :ok (said (node:contents n)))))

(defun %ls (where)
  (let ((n (at where)))
    (if n (list :ok (said (tree:listing n)))
        (list :no (format nil "no node at ~a" where)))))

(defun %evaluate (text)
  (let* ((s (session:open-session
             :name "control"
             :package (or (find-package :pine.user) (find-package :cl-user))))
         (e (session:evaluate s (session:read s text))))
    (session:close s)
    (if (session:fault e)
        (list :no (princ-to-string (session:fault e)))
        (list :ok (format nil "~a~{~s~^~%~}" (session:said e)
                          (session:answered e))))))

(defun %status ()
  (list :ok (format nil "~a: pid ~d, remoting ~d, ~d command~:p, ~d watching"
                    (world:name world:*world*)
                    (sb-posix:getpid)
                    (and server:*server* (server:remoting-port server:*server*))
                    (length (cmd:commands))
                    (length (watch:watchers)))))

(defun %pid () (list :ok (princ-to-string (sb-posix:getpid))))

(defun %watch (where uri)
  (let ((n (at where))
        (sys (and server:*server* (server:actor-system server:*server*))))
    (cond ((null n) (list :no (format nil "no node at ~a" where)))
          ((null sys) (list :no "this pine has no remoting"))
          (t
           (let* ((there (sento.remoting:make-remote-ref sys uri))
                  (w (watch:watch n (lambda (of value)
                                      (sento.actor:tell
                                       there (list :moved (node:full-name of)
                                                   (said value)))))))
             (c:swap *watches* (lambda (all) (cons (cons uri w) all)))
             (list :ok (format nil "watching ~a" (node:full-name n))))))))

(defun %unwatch (uri)
  (let ((held (assoc uri (c:held *watches*) :test #'equal)))
    (when held
      (watch:unwatch (cdr held))
      (c:swap *watches* (lambda (all) (remove held all))))
    (list :ok "")))

(defun %listing (where)
  "Every path under WHERE that holds something, with what it holds."
  (let ((n (at where))
        (acc nil))
    (when n
      (tree:walk n (lambda (each)
                     (let ((held (ignore-errors (node:contents each))))
                       (when (and held (node:leafp each))
                         (push (cons (node:full-name each) (said held)) acc))))
                 :into (complement #'node:livep))
      (nreverse acc))))

(defun %diff (where against)
  "What is under WHERE that is not the same under AGAINST."
  (let ((here (%listing where))
        (there (%listing against)))
    (list :ok
          (with-output-to-string (out)
            (dolist (each here)
              (let ((match (assoc (%rebase (car each) where against)
                                  there :test #'equal)))
                (cond ((null match) (format out "+ ~a ~a~%" (car each) (cdr each)))
                      ((not (equal (cdr match) (cdr each)))
                       (format out "~~ ~a ~a -> ~a~%" (car each)
                               (cdr match) (cdr each))))))
            (dolist (each there)
              (unless (assoc (%rebase (car each) against where)
                             here :test #'equal)
                (format out "- ~a ~a~%" (car each) (cdr each))))))))

(defun %rebase (path from to)
  (let ((from (string-right-trim "/" (princ-to-string from)))
        (to (string-right-trim "/" (princ-to-string to))))
    (if (and (plusp (length from))
             (>= (length path) (length from))
             (string= from path :end2 (length from)))
        (concatenate 'string to (subseq path (length from)))
        path)))

(defun %run (name arguments)
  (let ((c (cmd:command-named name)))
    (if c
        (list :ok (said (cmd:run c arguments)))
        (list :no (format nil "no command named ~a" name)))))

(defun received (message)
  (fault:attempt
   (lambda ()
     (destructuring-bind (verb &rest arguments) message
       (case verb
         (:read    (%read (first arguments)))
         (:write   (%write (first arguments) (second arguments)))
         (:ls      (%ls (first arguments)))
         (:eval    (%evaluate (first arguments)))
         (:run     (%run (first arguments) (rest arguments)))
         (:status  (%status))
         (:diff    (%diff (first arguments) (second arguments)))
         (:reload  (list :ok (princ-to-string (funcall *on-reload*))))
         (:agents  (list :ok (said (funcall *agents*))))
         (:watch   (%watch (first arguments) (second arguments)))
         (:unwatch (%unwatch (first arguments)))
         (:pid     (%pid))
         (:ping    (list :ok "pong"))
         (:quit    (log:note "asked to stop")
                   (when *on-quit* (funcall *on-quit*))
                   (list :ok "stopping"))
         (t (list :no (format nil "no verb ~a" verb))))))
   "a control message"))

(defun serve (s &key on-quit)
  (when on-quit (setf *on-quit* on-quit))
  (sento.actor-context:actor-of (server:actor-system s)
    :name "control"
    :dispatcher :pinned
    :receive (lambda (message)
               (or (received message) (list :no "that faulted")))))
