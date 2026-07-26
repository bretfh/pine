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

(defclass backing (pine.frontend:backing)
  ((display :initarg :display :reader display)
   (fd :initarg :fd :reader fd
       :documentation "The connection's descriptor, to wait on."))
  (:documentation "A compositor connection, as something a frontend waits on."))

(defun connect-display ()
  "Open the compositor connection and return it as a backing.

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
      (make-instance 'backing :display (wl-display-connect socket) :fd fd))))

(defmethod pine.frontend:dispatch-pending ((b backing))
  (let ((display (display b)))
    (loop :while (wl-display-listen display)
          :do (wl-display-dispatch-event display))))

(defmethod pine.frontend:shutdown ((b backing))
  (wl-display-disconnect (display b)))

(defmethod pine.frontend:wait-for-work ((b backing) pump timeout)
  "Wait on the connection and the queue at once, which is the whole reason the
descriptor is held."
  (cffi:with-foreign-object (fds '(:struct pollfd) 2)
    (let ((connection (cffi:mem-aptr fds '(:struct pollfd) 0))
          (queue (cffi:mem-aptr fds '(:struct pollfd) 1)))
      (setf (cffi:foreign-slot-value connection '(:struct pollfd) 'fd) (fd b)
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
