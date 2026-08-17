(defpackage #:pine/wayland/pump
  (:use #:cl)
  (:export #:pump #:make-pump #:close-pump #:wake-in #:wake #:hand #:queuedp
           #:drain #:drain-wake))
(in-package #:pine/wayland/pump)

(defparameter +nonblock+ 2048
  "O_NONBLOCK. The read end must never block: the thread that reads it is the one
holding the compositor connection, and a read that waits is a screen that has
stopped drawing.")

(defstruct (pump (:constructor %pump))
  "The queue between whatever thread had the news and the one holding the
compositor connection. Work crosses on the queue and a pipe carries the news that
it did, so a thread waiting on descriptors can wait on this one too."
  wake-in
  wake-out
  (queue nil)
  (lock (bordeaux-threads:make-lock)))

(defun make-pump ()
  (multiple-value-bind (in out) (sb-unix:unix-pipe)
    (unless in (error "cannot open the wake pipe"))
    (cffi:foreign-funcall "fcntl" :int in :int 4 :int +nonblock+ :int)
    (%pump :wake-in in :wake-out out)))

(defun close-pump (p)
  (ignore-errors (sb-unix:unix-close (pump-wake-in p)))
  (ignore-errors (sb-unix:unix-close (pump-wake-out p)))
  p)

(defun wake-in (p) (pump-wake-in p))

(defun wake (p)
  "Wake the thread waiting on this pump. Safe from anywhere: it touches nothing
but the pipe."
  (cffi:with-foreign-object (byte :unsigned-char)
    (setf (cffi:mem-ref byte :unsigned-char) 1)
    (cffi:foreign-funcall "write"
                          :int (pump-wake-out p) :pointer byte :long 1 :long)))

(defun hand (p thunk)
  "Hand THUNK to the thread on the other end, and say so."
  (bordeaux-threads:with-lock-held ((pump-lock p))
    (push thunk (pump-queue p)))
  (wake p))

(defun queuedp (p)
  (bordeaux-threads:with-lock-held ((pump-lock p))
    (and (pump-queue p) t)))

(defun drain (p)
  "Run what was handed over. One that breaks is said so and the rest still run."
  (let (thunks)
    (bordeaux-threads:with-lock-held ((pump-lock p))
      (setf thunks (nreverse (pump-queue p))
            (pump-queue p) nil))
    (dolist (thunk thunks)
      (handler-case (funcall thunk)
        (error (broke)
          (format *error-output* "pine: ~a~%" broke)
          (finish-output *error-output*))))))

(defun drain-wake (p)
  "Read what the wakes wrote, so the pipe does not stay readable. Once: the read
end does not block, and whatever is left makes it readable again."
  (cffi:with-foreign-object (buffer :unsigned-char 256)
    (cffi:foreign-funcall "read" :int (pump-wake-in p)
                                 :pointer buffer :long 256 :long)))
