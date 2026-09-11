(defpackage #:pine/run/peer
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:job #:pine/run/job) (#:image #:pine/run/image)
                    (#:actors #:pine/run/actors) (#:watch #:pine/run/watch)
                    (#:fault #:pine/run/fault) (#:serial #:pine/serial) (#:log #:pine/fs/log))
  (:export
   #:reach #:serve #:named #:received #:telling #:forget-watches #:watches
   #:evaluatingp #:*trusted* #:*evaluates*))
(in-package #:pine/run/peer)

(defvar *timeout* 30)
(defvar *trusted* nil)
(defvar *evaluates* t)
(defvar *waits-inline* nil)
(defclass asked (job:thread)
  ((answered    :initform nil :accessor answered)
   (done        :initform nil :accessor done)
   (output      :initform (make-string-output-stream) :reader output)
   (ready       :initform (bordeaux-threads:make-semaphore) :reader ready)
   (suspended-in :initform (list nil) :reader suspended-in)))

(defvar *counter* 0)
(defvar *telling* nil)
(defvar *watching* nil)

(defclass peer (image:image)
  ((uri :initarg :uri :accessor uri)
   (ref :initform nil :accessor ref)))

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
                 (t (values (mapcar #'serial:decode (getf answer :answered))
                            nil nil
                            (or (getf answer :said) ""))))))
        (t (values nil (format nil "~a" said) nil ""))))

(defmethod image:evaluate ((p peer) form &key (timeout *timeout*))
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

(defclass remote ()
  ((peer  :initarg :peer  :reader peer-of)
   (where :initarg :where :reader where-of)))

(defclass remote-dir (remote fs:mount) ())
(defclass remote-leaf (remote fs:derived) ())

(defun remote (p where name &optional (kind :dir))
  (make-instance (if (eq kind :dir) 'remote-dir 'remote-leaf)
                 :name name :peer p :where where :describes (uri p)))

(defmethod fs:volatile-p ((n remote-dir) &optional name) (declare (ignore name)) t)
(defmethod fs:volatile-p ((n remote-leaf) &optional name) (declare (ignore name)) t)

(defmethod fs:children ((n remote-dir))
  (remove nil (mapcar (lambda (child) (fs:child n child))
                      (%crossed (peer-of n) (where-of n) (list :entries)))))

(defmethod fs:child ((n remote-dir) name)
  (let ((child (princ-to-string name)))
    (fs:ensure-child n child
              (lambda ()
                (let ((kind (%crossed (peer-of n) (where-of n) (list :entry child))))
                  (when kind
                    (let ((it (remote (peer-of n) (%under (where-of n) child) child kind)))
                      (setf (fs:parent it) n)
                      it)))))))

(defmethod fs:works ((n remote-leaf))
  (serial:decode (%crossed (peer-of n) (where-of n) (list :contents))))

(defmethod fs:takes ((n remote-leaf) value)
  (%crossed (peer-of n) (where-of n) (list :write (serial:encode value))))

(defmethod watch:watch ((n remote) tells &key every name tells-when poll for)
  (declare (ignore every tells-when poll for))
  (listen-to (peer-of n) (where-of n)
             (lambda (where said) (declare (ignore where)) (funcall tells n said))
             :name name))

(defmethod fs:mount ((what peer) where)
  (fs:mount (remote what "/" (job:name what) :dir) where))

(defun local-uri (name)
  (%uri actors:*host* (or (actors:remoting) 0) name))

(defun listen-to (p where tells &key name)
  (let* ((name (or name (format nil "watch-~d" (sb-ext:atomic-update *counter* (lambda (old) (1+ old))))))
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
  (let ((to (sento.remoting:make-remote-ref (actors:actors) uri)))
    (lambda (said) (sento.actor:tell to said))))

(defmacro telling ((how &optional watches trusted waits) &body body)
  `(let ((*telling* ,how)
         (*watching* (or ,watches *watching*))
         (*trusted* ,trusted)
         (*waits-inline* ,waits))
     ,@body))

(defun evaluatingp ()
  (and *trusted* *evaluates*))

(defun watches () (and *watching* (cdr *watching*)))

(defun forget-watches (&optional (held *watching*))
  (dolist (w (cdr held) t)
    (fault:or-nothing "a watch already let go of is let go of" (watch:unwatch w))))

(defun %watching (where &optional uri)
  (let ((n (fs:at (fs:root) (string-left-trim "/" (princ-to-string where))))
        (to (if uri (%to-uri uri) *telling*)))
    (cond ((null n) (list :no (format nil "nothing at ~a" where)))
          ((null to) (list :no "there is no way back to whoever asked"))
          (t (let ((w (watch:watch n (lambda (of said)
                                       (declare (ignore said))
                                       (funcall to (list :moved (fs:full-name of) t)))
                                   :name (format nil "~a->~a" (fs:full-name n)
                                                 (or uri "the connection"))
                                   :for uri)))
               (when *watching* (push w (cdr *watching*)))
               (list :ok (fs:full-name n)))))))

(defun %done (uri)
  (let ((held (remove uri (watch:watchers) :key #'watch:for :test-not #'equal)))
    (dolist (w held) (fault:or-nothing "a watch already let go of is let go of"
                       (watch:unwatch w)))
    (list :ok (length held))))

(defun %said (n)
  (let ((value (fs:contents n)))
    (if (serial:encodablep value)
        (list :ok (serial:encode value) :kind (fs:kind n))
        (list :no (format nil "~a holds a ~(~a~), which has no spelling; what is ~
                               under it may"
                          (fs:full-name n)
                          (class-name (class-of value)))))))

(defun %place (where message)
  (let* ((name (string-left-trim "/" (princ-to-string where)))
         (n (if (eq :write (first message))
                (fs:mount (make-instance 'fs:value) (concatenate 'string "/" name))
                (fs:at (fs:root) name))))
    (if (null n)
        (list :no (format nil "nothing at ~a" where))
        (case (first message)
          (:contents (%said n))
          (:write    (setf (fs:contents n)
                           (fs:as-value (serial:decode (second message))))
                     (%said n))
          (:verb     (fs:verb n (second message)
                                (mapcar #'serial:decode (cddr message)))
                     (%said n))
          (:entries  (list :ok (mapcar #'fs:name (fs:children n))))
          (:entry    (let ((it (fs:child n (second message))))
                       (if it
                           (list :ok (if (typep it 'fs:mount) :dir :leaf))
                           (list :no (format nil "nothing at ~a under ~a"
                                             (second message) where)))))
          (t (list :no "no such question about a place"))))))

(defvar *brief-eval-seconds* 0.02)

(defun %work (form &optional (budget 0))
  (let* ((token (sb-ext:atomic-update *counter* (lambda (old) (1+ old))))
         (j (make-instance 'asked :name (%ask-name token) :on-fault :leave
                                  :describes "work another pine asked for")))
    (setf (job:body j)
          (lambda ()
            (unwind-protect
                 (let ((*standard-output* (output j))
                       (fault:*keeping* (suspended-in j)))
                   (fault:with-debugger
                     (fault:attempt
                      (lambda () (setf (answered j) (multiple-value-list (eval form))
                                       (done j) t))
                      "answering a peer")))
              (fault:wake)
              (setf (job:stopping j) t)
              (bordeaux-threads:signal-semaphore (ready j)))))
    (job:start j)
    (when (plusp budget)
      (bordeaux-threads:wait-on-semaphore (ready j) :timeout budget))
    (let ((now (%answered token)))
      (if (eq (second now) :working)
          (list :ok :working :token token)
          now))))

(defun %ask-name (token) (format nil "ask-~d" token))

(defun %answered (token)
  (let ((j (job:named (%ask-name token))))
    (if (null j)
        (list :no (format nil "nothing was asked under ~a" token))
        (let ((f (car (suspended-in j))))
          (cond (f (list :ok :said-broke (princ-to-string (fault:condition-of f))
                         :offers (fault:offers f) :token token
                         :said (get-output-stream-string (output j))))
                ((done j)
                 (let ((said (list :ok :answered (mapcar #'serial:encode (answered j))
                                   :said (get-output-stream-string (output j)))))
                   (job:forget (job:name j))
                   said))
                (t (list :ok :working)))))))

(defun received (message)
  (block answering
    (handler-bind ((error (lambda (c)
                            (fault:report c "answering what was asked here")
                            (return-from answering
                              (list :no (princ-to-string c))))))
      (fs:writing (%answer message)))))

(defun %answer (message)
  (case (first message)
    (:ping (list :ok :pong))
    (:evaluate (cond ((not (evaluatingp)) (list :no "this way in does not evaluate"))
                     (*waits-inline* (%work (second message) *timeout*))
                     (t (%work (second message) *brief-eval-seconds*))))
    (:watch (%watching (second message) (third message)))
    (:done (%done (second message)))
    (:answer (%answered (second message)))
    (:take (let* ((j (job:named (%ask-name (third message))))
                  (f (and j (car (suspended-in j))))
                  (taken (and f (fault:take f (second message)))))
             (when j (job:forget (job:name j)))
             (list :ok taken)))
    ((:contents :write :verb :entries :entry)
     (%place (second message) (list* (first message) (cddr message))))
    (t (list :no "no such question"))))

(defun serve (&key (name "tree"))
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
