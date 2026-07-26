(in-package #:pine.wayland)

;;;; The compositor connection and the wait.
;;;;
;;;; A frontend waits on two things: this connection, and the work its own
;;;; threads queue for it, since only this thread may touch a proxy. Both are
;;;; descriptors, so the wait is one poll(2) over the pair and an idle
;;;; frontend costs nothing.

(defconstant +af-unix+ 1 "AF_UNIX.")
(defconstant +sock-stream+ 1 "SOCK_STREAM.")
(defconstant +pollin+ 1 "POLLIN.")

(cffi:defcstruct pollfd
  (fd :int)
  (events :short)
  (revents :short))

(defun display-path ()
  "The compositor's socket, as the environment names it."
  (let ((name (uiop:getenv "WAYLAND_DISPLAY"))
        (dir (uiop:getenv "XDG_RUNTIME_DIR")))
    (unless (and name (plusp (length name)))
      (error "WAYLAND_DISPLAY is unset"))
    (cond ((char= (char name 0) #\/) name)
          ((and dir (plusp (length dir))) (format nil "~a/~a" dir name))
          (t (error "XDG_RUNTIME_DIR is unset")))))

(defun connect-display ()
  "Open the compositor connection. Returns the display and its descriptor.

The socket is opened here, rather than by `wl-display-connect', because a
display built by that function keeps its descriptor where a client cannot read
it and there is then nothing to wait on."
  (let ((path (display-path))
        (fd (cffi:foreign-funcall "socket"
                                  :int +af-unix+ :int +sock-stream+ :int 0
                                  :int)))
    (when (minusp fd)
      (error "cannot open a socket for ~a" path))
    (let ((socket (make-instance 'wire:data-socket :fd fd)))
      (wire:connect socket path)
      (values (wl-display-connect socket) fd))))

(defun dispatch-pending (display)
  "Dispatch every event already read from the connection."
  (loop :while (wl-display-listen display)
        :do (wl-display-dispatch-event display)))

(defun wait-for-work (fd pump timeout)
  "Block until the compositor or another thread has something for us.

TIMEOUT is in milliseconds, or -1 to wait for as long as it takes. Returns
true when the queue was what woke us."
  (cffi:with-foreign-object (fds '(:struct pollfd) 2)
    (let ((connection (cffi:mem-aptr fds '(:struct pollfd) 0))
          (queue (cffi:mem-aptr fds '(:struct pollfd) 1)))
      (setf (cffi:foreign-slot-value connection '(:struct pollfd) 'fd) fd
            (cffi:foreign-slot-value connection '(:struct pollfd) 'events) +pollin+
            (cffi:foreign-slot-value connection '(:struct pollfd) 'revents) 0
            (cffi:foreign-slot-value queue '(:struct pollfd) 'fd)
            (pine.frontend:pump-wake-in pump)
            (cffi:foreign-slot-value queue '(:struct pollfd) 'events) +pollin+
            (cffi:foreign-slot-value queue '(:struct pollfd) 'revents) 0)
      (let ((ready (cffi:foreign-funcall "poll"
                                         :pointer fds :unsigned-long 2
                                         :int timeout :int)))
        (and (plusp ready)
             (plusp (logand +pollin+
                            (cffi:foreign-slot-value queue '(:struct pollfd)
                                                     'revents))))))))

(defun run-loop (display fd pump &key done ready pending deadline)
  "Serve the compositor and the daemon until DONE returns true.

READY runs once before each wait, for work the frontend owes itself: a
repaint, a key it is repeating. PENDING is true while there is more to do now,
which keeps the loop from settling down on a surface it has just dirtied.
DEADLINE gives the milliseconds until the frontend's next deadline, or nil when
it has none."
  (loop :until (funcall done)
        :do (pine.frontend:drain pump)
            (when ready (funcall ready))
            (dispatch-pending display)
            (unless (or (pine.frontend:pump-queued-p pump)
                        (and pending (funcall pending)))
              (when (wait-for-work fd pump
                                   (or (and deadline (funcall deadline)) -1))
                (pine.frontend:drain-wake pump)))))
