(defpackage #:pine/wayland/display
  (:use #:cl #:wayflan-client)
  (:local-nicknames (#:wayflan #:xyz.shunter.wayflan.wire) (#:pump #:pine/wayland/pump))
  (:export
   #:connect #:of #:dispatch #:disconnect #:wait))
(in-package #:pine/wayland/display)

(defconstant +af-unix+ 1)
(defconstant +sock-stream+ 1)
(defconstant +pollin+ 1)

(cffi:defcstruct pollfd
  (fd :int)
  (events :short)
  (revents :short))

(defclass display ()
  ((of :initarg :of :reader of)
   (fd :initarg :fd :reader fd))
  (:documentation "A compositor connection, and the descriptor to wait on. The
socket is opened here rather than by WL-DISPLAY-CONNECT because a display built
by that keeps its descriptor where a client cannot read it, and there is then
nothing to wait on."))

(defun path ()
  "The compositor's socket, as the environment names it."
  (let ((name (uiop:getenv "WAYLAND_DISPLAY"))
        (dir (uiop:getenv "XDG_RUNTIME_DIR")))
    (unless (and name (plusp (length name)))
      (error "WAYLAND_DISPLAY is unset"))
    (cond ((char= (char name 0) #\/) name)
          ((and dir (plusp (length dir))) (format nil "~a/~a" dir name))
          (t (error "XDG_RUNTIME_DIR is unset")))))

(defun connect ()
  (let ((where (path))
        (fd (cffi:foreign-funcall "socket"
                                  :int +af-unix+ :int +sock-stream+ :int 0 :int)))
    (when (minusp fd) (error "cannot open a socket for ~a" where))
    (let ((socket (make-instance 'wayflan:data-socket :fd fd)))
      (wayflan:connect socket where)
      (make-instance 'display :of (wl-display-connect socket) :fd fd))))

(defun dispatch (d)
  (let ((it (of d)))
    (loop :while (wl-display-listen it)
          :do (wl-display-dispatch-event it))))

(defun disconnect (d) (wl-display-disconnect (of d)))

(defun wait (d p timeout)
  "Wait on the connection and the pump at once, which is the whole reason the
descriptor is held. Answers whether the pump is what woke us."
  (cffi:with-foreign-object (fds '(:struct pollfd) 2)
    (let ((connection (cffi:mem-aptr fds '(:struct pollfd) 0))
          (queue (cffi:mem-aptr fds '(:struct pollfd) 1)))
      (setf (cffi:foreign-slot-value connection '(:struct pollfd) 'fd) (fd d)
            (cffi:foreign-slot-value connection '(:struct pollfd) 'events) +pollin+
            (cffi:foreign-slot-value connection '(:struct pollfd) 'revents) 0
            (cffi:foreign-slot-value queue '(:struct pollfd) 'fd) (pump:wake-in p)
            (cffi:foreign-slot-value queue '(:struct pollfd) 'events) +pollin+
            (cffi:foreign-slot-value queue '(:struct pollfd) 'revents) 0)
      (let ((ready (cffi:foreign-funcall "poll"
                                         :pointer fds :unsigned-long 2
                                         :int timeout :int)))
        (and (plusp ready)
             (plusp (logand +pollin+
                            (cffi:foreign-slot-value queue '(:struct pollfd)
                                                     'revents))))))))
