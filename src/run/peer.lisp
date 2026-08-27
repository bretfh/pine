(defpackage #:pine/run/peer
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:mount #:pine/fs/mount)
                    (#:job #:pine/run/job) (#:image #:pine/run/image)
                    (#:actors #:pine/run/actors) (#:watch #:pine/run/watch)
                    (#:fault #:pine/run/fault)
                    (#:log #:pine/run/log))
  (:export #:peer #:remote #:reach #:serve #:peers #:named #:uri #:ref
           #:listen-to #:local-uri #:*timeout*))
(in-package #:pine/run/peer)

(defvar *timeout* 30)
(defvar *asked* (d:table)
  "The faults this image is standing in on somebody else's behalf, by token.")
(defvar *counter* 0)

(defclass peer (image:image mount:mount)
  ((uri :initarg :uri :accessor uri)
   (ref :initform nil :accessor ref))
  (:documentation "Another pine: an image you can evaluate in and a namespace you
can graft. One class because it is one thing, and that is why a read of
/host/laptop/dev/audio/volume and a read of /dev/audio/volume are the same act."))


(defun peers () (remove-if-not (lambda (j) (typep j 'peer)) (job:jobs)))

(defun named (name)
  (let ((j (job:named name)))
    (and (typep j 'peer) j)))

(defun %uri (host port actor)
  (format nil "sento://~a:~d/user/~a" host port actor))

(defun %ask (p message &key (timeout *timeout*))
  (let ((to (ref p)))
    (unless to (error "~a is not reached." (job:name p)))
    (sento.actor:ask-s to message :time-out timeout)))

(defmethod job:alivep ((p peer)) (and (ref p) t))

(defmethod job:start ((p peer))
  (setf (ref p) (sento.remoting:make-remote-ref (actors:actors) (uri p)))
  (let ((said (ignore-errors (%ask p (list :ping) :timeout 5))))
    (unless (and (consp said) (eq :ok (first said)))
      (setf (ref p) nil)
      (error "no pine answering at ~a" (uri p))))
  p)

(defmethod job:stop ((p peer))
  (setf (ref p) nil)
  p)

(defun reach (name &key host port (actor "tree"))
  "Get to another pine. What comes back is a job you can start and stop and a
namespace you can mount."
  (let ((p (make-instance 'peer :name name :restarts nil
                                :uri (%uri (or host actors:*host*) port actor)
                                :describes (%uri (or host actors:*host*) port
                                                 actor))))
    (job:start p)
    p))

(defmethod image:evaluate ((p peer) form &key (timeout *timeout*))
  "Work in a pine reached over the network. The far side stands in its fault rather
than unwinding, so what comes back carries the restarts it is still offering."
  (let ((said (%ask p (list :evaluate form) :timeout timeout)))
    (cond ((and (consp said) (eq :ok (first said)))
           (let ((answer (rest said)))
             (cond ((getf answer :said-broke)
                    (image:borrowing p (getf answer :said-broke) (getf answer :offers)
                                     :token (getf answer :token))
                    (values nil (getf answer :said-broke) (getf answer :offers)
                            (or (getf answer :said) "")))
                   (t (values (getf answer :answered) nil nil
                              (or (getf answer :said) ""))))))
          (t (values nil (format nil "~a" said) nil "")))))

(defmethod fault:resume ((p peer) f restart)
  (%ask p (list :take restart (fault:token f))))

(defun %crossed (p where message)
  (let ((said (%ask p (list* (first message) where (rest message)))))
    (when (and (consp said) (eq :ok (first said))) (second said))))

(defun %under (where name)
  (format nil "~a/~a" (string-right-trim "/" where) name))

(defun remote (p where name)
  "A place in another pine's namespace: four closures over which pine and which
path. A read is a read and a write is a write, and what is under it is what that
pine says is under it."
  (node:place name
              :describes (uri p)
              :reads  (lambda () (%crossed p where (list :contents)))
              :writes (lambda (value) (%crossed p where (list :write value)))
              :names  (lambda () (%crossed p where (list :nodes)))
              :each   (lambda (child)
                        (let ((child (princ-to-string child)))
                          (when (member child (%crossed p where (list :nodes))
                                        :test #'equal)
                            (remote p (%under where child) child))))))

(defmethod mount:mount ((what peer) into name)
  "Graft another pine's namespace here. From now on a read of a path under it is a
read, and a write is a write."
  (node:attach (remote what "/" name) into))

(defun local-uri (name)
  "Where an actor in this image is reached from another one."
  (%uri actors:*host* (or (actors:remoting) 0) name))

(defun listen-to (p where tells &key name)
  "Ask another pine to say so whenever a place in it moves. What comes back is a
job: stopping it is how you stop listening."
  (let* ((name (or name (format nil "watch-~d" (d:swap *counter* #'1+))))
         (j (make-instance 'job:actor
                           :name name :restarts nil :dispatcher :pinned
                           :describes (format nil "~a of ~a" where (job:name p))
                           :receive (lambda (message)
                                      (when (eq :moved (first message))
                                        (funcall tells (second message)
                                                 (third message)))))))
    (job:start j)
    (%ask p (list :watch where (local-uri name)))
    j))

(defun %watching (where uri)
  (let ((n (tree:at (tree:root) (string-left-trim "/" (princ-to-string where))))
        (to (sento.remoting:make-remote-ref (actors:actors) uri)))
    (if (null n)
        (list :no (format nil "nothing at ~a" where))
        (progn
          (watch:watch n (lambda (of said)
                           (declare (ignore said))
                           (sento.actor:tell
                            to (list :moved (node:full-name of) t)))
                       :name (format nil "~a->~a" (node:full-name n) uri))
          (list :ok (node:full-name n))))))

(defun %place (where message)
  "A place another pine is asking about. A write makes it if nothing stands there,
the way a write does here; a read and a verb do not, because there is nothing to
read and nothing to tell."
  (let* ((name (string-left-trim "/" (princ-to-string where)))
         (n (if (eq :write (first message))
                (tree:ensure (tree:root) name)
                (tree:at (tree:root) name))))
    (if (null n)
        (list :no (format nil "nothing at ~a" where))
        (case (first message)
          (:contents (list :ok (node:contents n)))
          (:write    (setf (node:contents n) (second message))
                     (list :ok (node:contents n)))
          (:verb     (node:verb n (second message) (cddr message))
                     (list :ok (node:contents n)))
          (:nodes    (list :ok (mapcar #'node:name (node:nodes n))))
          (t (list :no "no such question about a place"))))))

(defun %work (form)
  "Evaluate FORM on a thread that stands in whatever breaks, so the restarts it
offers are the ones still there, and answer as soon as it has a value or a fault."
  (let ((answered nil)
        (said (make-string-output-stream))
        (was (fault:standing)))
    (actors:blocking
     "answering a peer"
     (lambda ()
       (let ((*standard-output* said))
         (fault:with-debugger
           (fault:attempt (lambda () (setf answered (multiple-value-list (eval form))))
                          "answering a peer")))))
    (let ((broke nil))
      (loop :repeat (round (/ *timeout* 0.01))
            :until (or answered broke)
            :do (setf broke (find-if-not (lambda (f) (member f was))
                                         (fault:standing)))
                (unless broke (sleep 0.01)))
      (cond (broke
             (let ((token (d:swap *counter* #'1+)))
               (d:keep! *asked* token broke)
               (list :ok :said-broke (princ-to-string (fault:condition-of broke))
                     :offers (fault:offers broke) :token token
                     :said (get-output-stream-string said))))
            (t (list :ok :answered answered
                     :said (get-output-stream-string said)))))))

(defun %received (message)
  (case (first message)
    (:ping (list :ok :pong))
    (:evaluate (%work (second message)))
    (:watch (%watching (second message) (third message)))
    (:take (let ((f (d:at (d:all *asked*) (third message))))
             (list :ok (and f (fault:take f (second message))))))
    ((:contents :write :verb :nodes)
     (%place (second message) (list* (first message) (cddr message))))
    (t (list :no "no such question"))))

(defun serve (&key (name "tree"))
  "Answer another pine's questions about this one: reads and writes of places, and
work to do in this image. Pinned, because a fault it stands in would otherwise take
a shared worker with it."
  (let ((j (make-instance 'job:actor :name name :restarts nil
                                     :dispatcher :pinned
                                     :describes "what another pine may ask here"
                                     :receive #'%received)))
    (job:start j)
    (log:note "answering peers at ~a"
              (%uri actors:*host* (or (actors:remoting) 0) name))
    j))
