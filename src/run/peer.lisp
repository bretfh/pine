(defpackage #:pine/run/peer
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:mount #:pine/fs/mount)
                    (#:job #:pine/run/job) (#:image #:pine/run/image)
                    (#:actors #:pine/run/actors) (#:watch #:pine/run/watch)
                    (#:fault #:pine/run/fault) (#:said #:pine/said)
                    (#:commit #:pine/fs/commit) (#:log #:pine/fs/log))
  (:export
   #:reach #:serve #:named #:received #:telling #:forget-watches #:watches))
(in-package #:pine/run/peer)

(defvar *timeout* 30)
(defvar *asked* (d:table)
  "The faults this image is standing in on somebody else's behalf, by token.")
(defvar *counter* 0)
(defvar *telling* nil
  "How to reach whoever is asking, where they left a way to be reached. A watch
fires on another thread long after the question that asked for it, so what answers
a question and what carries an event are two different things, and only the second
needs this.")
(defvar *watching* nil
  "Where the watches made while answering are collected, so whoever opened the
connection can let them go when it closes. A watch outlives the question that
asked for it and must not outlive the asker.")
(defvar *by-uri* (d:table)
  "The watches made for somebody with an address of their own, by that address.
Each of their questions arrives as its own message, so there is no one call to
collect them in; this is where they are kept until that address says it is done
or goes away.")

(defclass peer (image:image)
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
  (let ((said (fault:or-nothing "there may be no pine at that address"
                (%ask p (list :ping) :timeout 5))))
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
  (let ((p (make-instance 'peer :name name :on-fault :leave
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
                   (t (values (mapcar #'said:took (getf answer :answered))
                              nil nil
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
  (node:lists name
              :describes (uri p)
              :reads  (lambda () (said:took (%crossed p where (list :contents))))
              :writes (lambda (value)
                        (%crossed p where (list :write (said:said value))))
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
                           :name name :on-fault :leave :dispatcher :pinned
                           :describes (format nil "~a of ~a" where (job:name p))
                           :receive (lambda (message)
                                      (when (eq :moved (first message))
                                        (funcall tells (second message)
                                                 (third message)))))))
    (job:start j)
    (%ask p (list :watch where (local-uri name)))
    j))

(defun %to-uri (uri)
  "Reaching somebody who is an actor with an address of their own."
  (let ((to (sento.remoting:make-remote-ref (actors:actors) uri)))
    (lambda (said) (sento.actor:tell to said))))

(defmacro telling ((how &optional watches) &body body)
  "Answer with HOW as the way back to whoever is asking, collecting the watches
they make into WATCHES. What a transport binds around its dispatch."
  `(let ((*telling* ,how)
         (*watching* (or ,watches *watching*)))
     ,@body))

(defun watches () (and *watching* (cdr *watching*)))

(defun forget-watches (&optional (held *watching*))
  "Let go of the watches made through one connection. Whoever opened it calls
this when it closes: a watch telling somebody who has gone is one that fires for
the life of the image and reaches nobody."
  (dolist (w (cdr held) t)
    (fault:or-nothing "a watch already let go of is let go of" (watch:unwatch w))))

(defun %watching (where &optional uri)
  "Say so whenever a place moves, to whoever asked.

Where they are an actor with an address, that address; otherwise back the way the
question came, which is the only way there is for anything on a stream."
  (let ((n (tree:at (tree:root) (string-left-trim "/" (princ-to-string where))))
        (to (if uri (%to-uri uri) *telling*)))
    (cond ((null n) (list :no (format nil "nothing at ~a" where)))
          ((null to) (list :no "there is no way back to whoever asked"))
          (t (let ((w (watch:watch n (lambda (of said)
                                       (declare (ignore said))
                                       (funcall to (list :moved (node:full-name of) t)))
                                   :name (format nil "~a->~a" (node:full-name n)
                                                 (or uri "the connection")))))
               (when *watching* (push w (cdr *watching*)))
               (when uri (d:update! *by-uri* uri (lambda (had) (cons w had))))
               (list :ok (node:full-name n)))))))

(defun %done (uri)
  "Somebody with an address says they are finished. Their watches go with them:
one telling an address nobody is listening at fires for the life of the image and
reaches no one."
  (let ((held (d:lookup (d:all *by-uri*) uri)))
    (dolist (w held) (fault:or-nothing "a watch already let go of is let go of"
                       (watch:unwatch w)))
    (d:drop! *by-uri* uri)
    (list :ok (length held))))

(defun %said (n)
  "What stands at N, spelled. A value that has no spelling is an object standing
for itself -- a widget, a document, a compositor -- and the answer is to say so
and name the place, rather than a nil that reads as an empty one."
  (let ((value (node:contents n)))
    (if (said:sayablep value)
        (list :ok (said:said value))
        (list :no (format nil "~a holds a ~(~a~), which has no spelling; what is ~
                               under it may"
                          (node:full-name n)
                          (class-name (class-of value)))))))

(defun %place (where message)
  "A place another pine is asking about. A write makes it if nothing stands there,
the way a write does here; a read and a verb do not, because there is nothing to
read and nothing to tell.

What crosses is spelled rather than handed over: whoever is asking may be a lisp
with fset loaded, and may just as well be a shell."
  (let* ((name (string-left-trim "/" (princ-to-string where)))
         (n (if (eq :write (first message))
                (tree:ensure (tree:root) name)
                (tree:at (tree:root) name))))
    (if (null n)
        (list :no (format nil "nothing at ~a" where))
        (case (first message)
          (:contents (%said n))
          (:write    (setf (node:contents n) (said:took (second message)))
                     (%said n))
          (:verb     (node:verb n (second message)
                                (mapcar #'said:took (cddr message)))
                     (%said n))
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
       (unwind-protect
            (let ((*standard-output* said))
              (fault:with-debugger
                (fault:attempt
                 (lambda () (setf answered (multiple-value-list (eval form))))
                 "answering a peer")))
         (fault:changed))))
    (let ((broke (fault:wait-until
                  (lambda ()
                    (or (find-if-not (lambda (f) (member f was)) (fault:standing))
                        (and answered :answered)))
                  *timeout*)))
      (when (eq broke :answered) (setf broke nil))
      (cond (broke
             (let ((token (d:swap *counter* #'1+)))
               (d:keep! *asked* token broke)
               (list :ok :said-broke (princ-to-string (fault:condition-of broke))
                     :offers (fault:offers broke) :token token
                     :said (get-output-stream-string said))))
            (t (list :ok :answered (mapcar #'said:said answered)
                     :said (get-output-stream-string said)))))))

(defun received (message)
  "What somebody asking gets back. Whatever breaks in here is answered rather than
thrown: this is the edge of the image, and on the other side of it is somebody who
can do nothing with a dropped connection but can do something with a sentence.

It is kept as a fault too, from where it was signalled, so what broke answering is
in the debugger with everything else that broke.

One table for every way in. What differs between a peer over sento and a shell on
a socket is how the words arrive and how an event goes back, which is TELLING, and
nothing else.

One line is one piece of news, however many places it moves. A line that writes
four of them tells whoever is listening once, at the end, with all four -- so a
store writes once and a watcher is woken once, and nothing is worked out from a
half-done line."
  (block answering
    (handler-bind ((error (lambda (c)
                            (fault:report c "answering what was asked here")
                            (return-from answering
                              (list :no (princ-to-string c))))))
      (commit:writing (%answer message)))))

(defun %answer (message)
  (case (first message)
    (:ping (list :ok :pong))
    (:evaluate (%work (second message)))
    (:watch (%watching (second message) (third message)))
    (:done (%done (second message)))
    (:take (let ((f (d:lookup (d:all *asked*) (third message))))
             (list :ok (and f (fault:take f (second message))))))
    ((:contents :write :verb :nodes)
     (%place (second message) (list* (first message) (cddr message))))
    (t (list :no "no such question"))))

(defun serve (&key (name "tree"))
  "Answer another pine's questions about this one: reads and writes of places, and
work to do in this image. Pinned, because a fault it stands in would otherwise take
a shared worker with it."
  (let ((j (make-instance 'job:actor :name name :on-fault :leave
                                     :dispatcher :pinned
                                     :describes "what another pine may ask here"
                                     :receive #'received)))
    (job:start j)
    (log:note "answering peers at ~a"
              (%uri actors:*host* (or (actors:remoting) 0) name))
    j))
