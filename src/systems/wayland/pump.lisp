(in-package #:pine/wayland)

(defparameter +nonblock+ 2048)

(defstruct (pump (:constructor %pump))
  wake-in
  wake-out
  (queue nil))

(defun make-pump ()
  (multiple-value-bind (in out) (sb-unix:unix-pipe)
    (unless in (error "cannot open the wake pipe"))
    (cffi:foreign-funcall "fcntl" :int in :int 4 :int +nonblock+ :int)
    (%pump :wake-in in :wake-out out)))

(defun close-pump (p)
  (fault:or-nothing "a descriptor already closed is closed"
    (sb-unix:unix-close (pump-wake-in p)))
  (fault:or-nothing "and so is the other end"
    (sb-unix:unix-close (pump-wake-out p)))
  p)

(defun wake-in (p) (pump-wake-in p))

(defun wake (p)
  (cffi:with-foreign-object (byte :unsigned-char)
    (setf (cffi:mem-ref byte :unsigned-char) 1)
    (cffi:foreign-funcall "write"
                          :int (pump-wake-out p) :pointer byte :long 1 :long)))

(defun hand (p thunk)
  (sb-ext:atomic-update (pump-queue p) (lambda (had) (cons thunk had)))
  (wake p))

(defun queuedp (p) (and (pump-queue p) t))

(defun drain (p)
  (dolist (thunk (nreverse (d:emptied (pump-queue p))))
    (fault:attempt thunk "something handed to the compositor thread")))

(defun drain-wake (p)
  (cffi:with-foreign-object (buffer :unsigned-char 256)
    (cffi:foreign-funcall "read" :int (pump-wake-in p)
                                 :pointer buffer :long 256 :long)))
