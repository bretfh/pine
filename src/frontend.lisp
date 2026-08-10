(defpackage #:pine.frontend
  (:use #:cl)
  (:export #:pump #:make-pump #:close-pump #:pump-wake-in
           #:enqueue #:wake #:pump-queued-p #:drain #:drain-wake
           #:backing #:wait-for-work #:dispatch-pending #:shutdown #:run
           #:attach #:answering-p #:*watch*))

(in-package #:pine.frontend)

(defvar *attempts* 60)

(defvar *interval* 2)

(defvar *watch* 3)

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

(defun %keep-attaching (sys daemon self kind ready)
  (loop :repeat *attempts*
        :until (funcall ready)
        :do (pine.net.attach:attach-to sys daemon self :kind kind)
            (sleep *interval*))
  (unless (funcall ready)
    (format *error-output* "pine: no daemon after ~d tries, giving up~%" *attempts*)
    (finish-output *error-output*)
    (uiop:quit 0)))

(defun answering-p (host port)
  (let ((socket (ignore-errors (usocket:socket-connect host port :timeout 1))))
    (when socket (usocket:socket-close socket) t)))

(defun %watch-daemon (host port ready)
  "A frontend outlives nothing: when the daemon it draws for is gone, so is it.
Without this a killed daemon leaves its windows on the screen for good."
  (bordeaux-threads:make-thread
   (lambda ()
     (loop :until (funcall ready) :do (sleep *interval*))
     (loop :with missed := 0
           :do (sleep *watch*)
               (if (answering-p host port)
                   (setf missed 0)
                   (incf missed))
               (when (>= missed 2)
                 (format *error-output* "pine: the daemon is gone~%")
                 (finish-output *error-output*)
                 (uiop:quit 0))))
   :name "pine daemon watch"))

(defun attach (kind handler &key (host pine.net.server:*host*)
                                 (port pine.net.server:*port*)
                                 (name "display"))
  (let ((sys (sento.actor-system:make-actor-system))
        (joined (pine.data:box nil)))
    (sento.remoting:enable-remoting sys :host pine.net.server:*host* :port 0)
    (sento.actor-context:actor-of sys
      :name name
      :dispatcher :pinned
      :receive (lambda (message)
                 (when (member (first message) '(:attached :refused))
                   (pine.data:put! joined t))
                 (funcall handler message)))
    (let* ((self-port (sento.remoting:remoting-port sys))
           (daemon (pine.net.server:daemon-uri "attach" :host host :port port))
           (self (pine.net.server:local-uri name self-port :host host)))
      (bordeaux-threads:make-thread
       (lambda () (%keep-attaching sys daemon self kind
                                   (lambda () (pine.data:held joined))))
       :name "pine attach")
      (%watch-daemon host port (lambda () (pine.data:held joined)))
      sys)))
