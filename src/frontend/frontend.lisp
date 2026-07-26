(in-package #:pine.frontend)

;;;; What every frontend is, whatever it draws on. Two things: the way it
;;;; joins the daemon, and the queue the daemon's threads hand work across.
;;;; The backing owns the loop that drains that queue, because only the
;;;; backing knows what else it must wait on.

(defstruct (pump (:constructor %make-pump))
  "The queue between the daemon's threads and the frontend's own.

Work crosses on the queue and the pipe carries the news that it did, so a
backing waiting on descriptors can wait on this one too."
  wake-in
  wake-out
  (queue nil)
  (lock (bordeaux-threads:make-lock)))

(defun make-pump ()
  (multiple-value-bind (wake-in wake-out) (sb-unix:unix-pipe)
    (unless wake-in
      (error "cannot open the wake pipe"))
    (%make-pump :wake-in wake-in :wake-out wake-out)))

(defun wake (pump)
  "Wake PUMP's thread. Safe from any thread; it touches nothing but the pipe."
  (cffi:with-foreign-object (byte :unsigned-char)
    (setf (cffi:mem-ref byte :unsigned-char) 1)
    (cffi:foreign-funcall "write"
                          :int (pump-wake-out pump) :pointer byte :long 1
                          :long)))

(defun enqueue (pump thunk)
  "Hand THUNK to the frontend's own thread, and say so."
  (bordeaux-threads:with-lock-held ((pump-lock pump))
    (push thunk (pump-queue pump)))
  (wake pump))

(defun pump-queued-p (pump)
  (bordeaux-threads:with-lock-held ((pump-lock pump))
    (and (pump-queue pump) t)))

(defun drain (pump)
  "Run what the other threads queued. A thunk that signals is reported, and the
remaining ones still run."
  (let (thunks)
    (bordeaux-threads:with-lock-held ((pump-lock pump))
      (setf thunks (nreverse (pump-queue pump))
            (pump-queue pump) nil))
    (dolist (thunk thunks)
      (handler-case (funcall thunk)
        (error (e)
          (format *error-output* "pine: ~a~%" e)
          (finish-output *error-output*))))))

(defun drain-wake (pump)
  "Empty the pipe, so news already acted on does not wake us again."
  (cffi:with-foreign-object (buf :unsigned-char 64)
    (cffi:foreign-funcall "read"
                          :int (pump-wake-in pump) :pointer buf :long 64
                          :long)))

(defun close-pump (pump)
  (sb-unix:unix-close (pump-wake-in pump))
  (sb-unix:unix-close (pump-wake-out pump)))

;;;; The backing: whatever a frontend draws on and waits for. The loop is here
;;;; because it is the same everywhere; only the waiting differs.

(defclass backing ()
  ()
  (:documentation "What a frontend draws on. A backing waits for work and
delivers what arrived; the loop around it is not its business."))

(defgeneric wait-for-work (backing pump timeout)
  (:documentation "Block until BACKING or PUMP has something, or TIMEOUT
milliseconds pass. TIMEOUT of -1 waits for as long as it takes. Returns true
when the pump was what woke us, so the loop knows to drain the pipe."))

(defgeneric dispatch-pending (backing)
  (:documentation "Handle everything BACKING has already delivered, without
blocking."))

(defgeneric shutdown (backing)
  (:documentation "Release what the backing holds.")
  (:method ((b backing)) (declare (ignore b)) nil))

(defun run (backing pump &key done ready pending deadline)
  "Serve BACKING and the daemon until DONE returns true.

READY runs once before each wait, for work the frontend owes itself: a
repaint, a key it is repeating. PENDING is true while there is more to do now,
which keeps the loop from settling down on a surface it has just dirtied.
DEADLINE gives the milliseconds until the frontend's next deadline, or nil
when it has none."
  (loop :until (funcall done)
        :do (drain pump)
            (when ready (funcall ready))
            (dispatch-pending backing)
            (unless (or (pump-queued-p pump)
                        (and pending (funcall pending)))
              (when (wait-for-work backing pump
                                   (or (and deadline (funcall deadline)) -1))
                (drain-wake pump)))))

;;;; Joining the daemon.

(defparameter *attach-attempts* 60
  "How many times a frontend re-announces itself while the daemon is silent.")

(defparameter *attach-interval* 2
  "Seconds between those attempts.")

(defun %keep-attaching (sys daemon self kind ready)
  (bordeaux-threads:make-thread
   (lambda ()
     (loop :repeat *attach-attempts*
           :until (funcall ready)
           :do (pine.attach:attach-to-daemon sys daemon self :kind kind)
               (sleep *attach-interval*))
     (unless (funcall ready)
       (format *error-output* "pine ~(~a~): no daemon answered at ~a~%" kind daemon)
       (finish-output *error-output*)))
   :name "pine-attach"))

(defun attach (kind handler &key (host pine.server:*host*) (port pine.server:*port*)
                                 ready)
  "Bring this image up as a frontend of KIND and join the daemon at HOST:PORT.

HANDLER takes the daemon's messages, on an actor thread; anything it does to
the backing belongs on the queue. Returns the actor system, which also serves
this image as an agent named for its kind, so the daemon can evaluate in it.

READY, when given, says whether the daemon has answered. A frontend the
compositor starts before the session is up passes it, and the attach is
repeated until it does."
  (unless pine.server:*server*
    (setf pine.server:*server* (make-instance 'pine.server:server)))
  (pine.buffer:install-default-faces)
  (let* ((name (string-downcase (symbol-name kind)))
         (sys (sento.actor-system:make-actor-system pine.server:*app-actor-config*)))
    (sento.remoting:enable-remoting sys :host pine.server:*host* :port 0)
    (sento.actor-context:actor-of sys :name "display"
      :receive (lambda (msg) (funcall handler msg) nil))
    (let* ((self-port (sento.remoting:remoting-port sys))
           (daemon (pine.server:daemon-uri "attach" :host host :port port))
           (self (pine.server:local-uri "display" self-port)))
      (if ready
          (%keep-attaching sys daemon self kind ready)
          (pine.attach:attach-to-daemon sys daemon self :kind kind))
      (pine.agent:serve sys :name name :master-host host :master-port port
                            :self-port self-port))
    sys))
