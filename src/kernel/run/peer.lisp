(defpackage #:pine/run/peer
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:mount #:pine/fs/mount)
                    (#:job #:pine/run/job) (#:image #:pine/run/image)
                    (#:actors #:pine/run/actors) (#:watch #:pine/run/watch)
                    (#:fault #:pine/run/fault) (#:said #:pine/said)
                    (#:commit #:pine/fs/commit) (#:log #:pine/fs/log))
  (:export
   #:reach #:serve #:named #:received #:telling #:forget-watches #:watches
   #:evaluatingp #:*trusted* #:*evaluates*))
(in-package #:pine/run/peer)

(defvar *timeout* 30)
(defvar *trusted* nil
  "Whether this way in is one somebody asked for.

Off unless a transport says otherwise, and a transport says so by existing at all:
the socket sits under the runtime directory and cannot be opened by anybody who
could not already start a lisp as this user, and a port is one nothing opens
unless the person running pine wrote it down. Both are a decision somebody made.

The default is the point. A way in added later that has not thought about this
gets a namespace it can read and write, and not a lisp it can evaluate in --
rather than the other way round, which is what a daemon that turned a port on for
you was.")
(defvar *evaluates* t
  "Whether a way in this image trusts may be given a form to evaluate.

On, because the other end of one could already run a lisp as this user: refusing
would protect nothing and would take the one verb a person at a terminal most
wants. A pine answering somewhere it is less sure of turns this off and still
answers reads and writes.

Here and not in the file that reads a line off a socket, because there is more
than one way in and what may be asked must not depend on which one somebody
used.")
(defvar *waits* nil
  "Whether this way in waits for work it asked for, on its own thread.")
(defvar *answers* (d:table)
  "Work a peer asked for and has not yet been told the end of, by token.")
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

(defun %took (p said)
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
        (t (values nil (format nil "~a" said) nil ""))))

(defmethod image:evaluate ((p peer) form &key (timeout *timeout*))
  "Work in a pine reached over the network. The far side answers at once with a
token and goes on; this side asks after it until it is done or TIMEOUT is up. The
far side never waits, so nothing else asking it is held behind this."
  (let ((said (%ask p (list :evaluate form) :timeout timeout)))
    (if (and (consp said) (eq :ok (first said)) (eq :working (second said)))
        (let ((token (getf (cddr said) :token))
              (due (+ (get-internal-real-time)
                      (* timeout internal-time-units-per-second))))
          (loop
            (let ((again (%ask p (list :answer token) :timeout timeout)))
              (cond ((not (and (consp again) (eq :ok (first again))))
                     (return (values nil (format nil "~a" again) nil "")))
                    ((eq (second again) :working)
                     (when (> (get-internal-real-time) due)
                       (return (values nil
                                       (format nil "no answer within ~d second~:p"
                                               timeout)
                                       nil "")))
                     (sleep 0.05))
                    (t (return (%took p again)))))))
        (%took p said))))

(defmethod fault:resume ((p peer) f restart)
  (%ask p (list :take restart (fault:token f))))

(defun %crossed (p where message)
  (let ((said (%ask p (list* (first message) where (rest message)))))
    (when (and (consp said) (eq :ok (first said))) (second said))))

(defun %under (where name)
  (format nil "~a/~a" (string-right-trim "/" where) name))

(defclass remote (node:place)
  ((peer  :initarg :peer  :reader peer-of)
   (where :initarg :where :reader where-of))
  (:documentation "A place in another pine's namespace. A class rather than a plain
one, so watching it is a method: what it answers is asking that pine to say when it
moves, which is what makes a read, a write and a watch of /host/laptop/dev/audio all
the same three acts they are here."))

(defun remote (p where name)
  "A place in another pine's namespace: four closures over which pine and which
path. A read is a read and a write is a write, and what is under it is what that
pine says is under it."
  (make-instance 'remote :name name
              :peer p :where where
              :describes (uri p)
              :reads  (lambda () (said:took (%crossed p where (list :contents))))
              :writes (lambda (value)
                        (%crossed p where (list :write (said:said value))))
              :names  (lambda () (%crossed p where (list :nodes)))
              :each   (lambda (child)
                        (let ((child (princ-to-string child)))
                          (when (%crossed p where (list :node child))
                            (remote p (%under where child) child))))))

(defmethod watch:watch ((n remote) tells &key every name tells-when poll)
  "Watching a place in another pine is asking that pine to say when it moves. The
near side and the far side were two paths; this is the near one, and it is the verb
rather than something beside it."
  (declare (ignore every tells-when poll))
  (listen-to (peer-of n) (where-of n)
             (lambda (where said) (declare (ignore where)) (funcall tells n said))
             :name name))

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

(defmacro telling ((how &optional watches trusted waits) &body body)
  "Answer with HOW as the way back to whoever is asking, collecting the watches
they make into WATCHES. What a transport binds around its dispatch.

TRUSTED is the transport saying whether the other end carries this user's own
name. It is said here, where everything else about the connection is said, so
that what may be asked is one question with one answer however the words arrived."
  `(let ((*telling* ,how)
         (*watching* (or ,watches *watching*))
         (*trusted* ,trusted)
         (*waits* ,waits))
     ,@body))

(defun evaluatingp ()
  "Whether this way in may be given a form to evaluate."
  (and *trusted* *evaluates*))

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
        (list :ok (said:said value) :kind (node:holding n))
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
          (:write    (setf (node:contents n)
                           (node:as-value (said:took (second message))))
                     (%said n))
          (:verb     (node:verb n (second message)
                                (mapcar #'said:took (cddr message)))
                     (%said n))
          (:nodes    (list :ok (mapcar #'node:name (node:nodes n))))
          (:node     (let ((it (node:resolve n (second message))))
                       (if it
                           (list :ok (node:name it))
                           (list :no (format nil "nothing at ~a under ~a"
                                             (second message) where)))))
          (t (list :no "no such question about a place"))))))

(defvar *brief-eval* 0.02
  "How long the peer answers an :EVALUATE inline before it hands back a token.
Fast work finishes inside this and is one round trip; slow work detaches, and the
one actor that answers every peer is held no longer than this by any of them.")

(defun %work (form &optional (budget 0))
  "Start FORM on a thread that stands in whatever breaks, and answer within BUDGET.
What finishes in time comes back as its value; what does not comes back as a token
to ask after, so nothing waits on the far side for work it started."
  (let ((token (d:swap *counter* #'1+))
        (mine (list nil))
        (done (list nil))
        (answered (list nil))
        (said (make-string-output-stream))
        (ready (bordeaux-threads:make-semaphore)))
    (d:keep! *answers* token (list mine done answered said))
    (actors:blocking
     "answering a peer"
     (lambda ()
       (unwind-protect
            (let ((*standard-output* said)
                  (fault:*keeping* mine))
              (fault:with-debugger
                (fault:attempt
                 (lambda () (setf (car answered) (multiple-value-list (eval form))
                                  (car done) t))
                 "answering a peer")))
         (fault:changed)
         (bordeaux-threads:signal-semaphore ready))))
    (when (plusp budget)
      (bordeaux-threads:wait-on-semaphore ready :timeout budget))
    (let ((now (%answered token)))
      (if (eq (second now) :working)
          (list :ok :working :token token)
          now))))

(defun %answered (token)
  "What the work asked for under TOKEN came to, or :WORKING while it has not."
  (let ((it (d:lookup (d:all *answers*) token)))
    (if (null it)
        (list :no (format nil "nothing was asked under ~a" token))
        (destructuring-bind (mine done answered said) it
          (cond ((car mine)
                 (d:drop! *answers* token)
                 (d:keep! *asked* token (car mine))
                 (list :ok :said-broke (princ-to-string (fault:condition-of (car mine)))
                       :offers (fault:offers (car mine)) :token token
                       :said (get-output-stream-string said)))
                ((car done)
                 (d:drop! *answers* token)
                 (list :ok :answered (mapcar #'said:said (car answered))
                       :said (get-output-stream-string said)))
                (t (list :ok :working)))))))

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
    (:evaluate (cond ((not (evaluatingp)) (list :no "this way in does not evaluate"))
                     (*waits* (%work (second message) *timeout*))
                     (t (%work (second message) *brief-eval*))))
    (:watch (%watching (second message) (third message)))
    (:done (%done (second message)))
    (:answer (%answered (second message)))
    (:take (let* ((token (third message))
                  (f (d:lookup (d:all *asked*) token)))
             (d:drop! *asked* token)
             (list :ok (and f (fault:take f (second message))))))
    ((:contents :write :verb :nodes :node)
     (%place (second message) (list* (first message) (cddr message))))
    (t (list :no "no such question"))))

(defun serve (&key (name "tree"))
  "Answer another pine's questions about this one: reads and writes of places, and
work to do in this image. Pinned, because a fault it stands in would otherwise take
a shared worker with it.

Trusted, because there is no port to reach this on until somebody asked for one.
Opening it is the decision; this is only where the decision is carried."
  (let ((j (make-instance 'job:actor :name name :on-fault :leave
                                     :dispatcher :pinned
                                     :describes "what another pine may ask here"
                                     :receive (lambda (message)
                                                (telling (nil nil t)
                                                  (received message))))))
    (job:start j)
    (log:note "answering peers at ~a"
              (%uri actors:*host* (or (actors:remoting) 0) name))
    j))
