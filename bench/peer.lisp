(require :asdf)
(asdf:load-system :pine/all)

(defpackage #:pine/bench/peer
  (:use #:cl)
  (:local-nicknames (#:fs #:pine/fs)
                    (#:mount #:pine/fs/mount) (#:peer #:pine/run/peer)
                    (#:image #:pine/run/image) (#:actors #:pine/run/actors)
                    (#:said #:pine/said))
  (:export #:main))
(in-package #:pine/bench/peer)

(defvar *runs* (or (ignore-errors (parse-integer (uiop:getenv "RUNS"))) 300))

(defvar *serializer* (make-instance 'sento.remoting.serialization:sexp-serializer))

(defun cost (label n thunk)
  (sb-ext:gc :full t)
  (let ((before (sb-ext:get-bytes-consed))
        (at (get-internal-real-time)))
    (dotimes (i n) (funcall thunk))
    (let ((secs (max 1d-6 (/ (- (get-internal-real-time) at)
                             (float internal-time-units-per-second))))
          (bytes (- (sb-ext:get-bytes-consed) before)))
      (format t "~&~46@a ~10,1f us ~9,0f bytes~%"
              label (/ (* secs 1e6) n) (/ bytes n))
      (force-output))))

(defun granularity ()
  "The smallest step this machine's clock takes, so a number below is known to
be one the clock could have said."
  (let ((steps '()))
    (dotimes (i 1000)
      (let ((a (get-internal-real-time)))
        (loop :for b := (get-internal-real-time)
              :until (/= a b)
              :finally (push (- b a) steps))))
    (format t "~&~46@a ~10,3f us~%" "clock steps by"
            (/ (* 1e6 (reduce #'min steps))
               (float internal-time-units-per-second)))))

(defun main ()
  (granularity)
  (pine:start :remoting 0)
  (pine::write "/dev/audio/volume" 41)
  (pine::write "/dev/net/wifi" "cafe-guest")
  (let ((under (fs:ensure "/many")))
    (dotimes (i 100) (fs:attach (make-instance 'fs:value :name (format nil "kid~d" i)) under)))
  (dolist (wide '(16 256 2048))
    (let ((under (fs:ensure (format nil "/cold~d" wide))))
      (dotimes (i wide) (fs:attach (make-instance 'fs:value :name (format nil "kid~d" i)) under))))
  (peer:serve)

  (format t "~&~%what one message costs to spell~%~%")
  (let* ((message (list :contents "/dev/audio/volume"))
         (bytes (rseri:serialize *serializer* message)))
    (format t "~&~46@a ~10d~%" "the message, in bytes on the wire"
            (length bytes))
    (cost "write-to-string it" *runs*
          (lambda () (write-to-string message :readably t)))
    (cost "and the string to octets" *runs*
          (lambda () (flexi-streams:string-to-octets
                      (write-to-string message :readably t)
                      :external-format :utf-8)))
    (cost "serialize (both of those)" *runs*
          (lambda () (rseri:serialize *serializer* message)))
    (cost "deserialize it again" *runs*
          (lambda () (rseri:deserialize *serializer* bytes)))
    (cost "spell a value pine's own way" *runs*
          (lambda () (said:took (said:said 41)))))

  (let ((p (peer:reach "self" :port (actors:remoting))))
    (mount:mount p (fs:root) "host")
    (assert (equal 41 (fs:contents (fs:at "/host/dev/audio/volume"))))

    (format t "~&~%walking to a child, by how many it stands among~%~%")
    (dolist (wide '(16 256 2048))
      (let ((i -1))
        (cost (format nil "cold walk, one of ~d" wide) wide
              (lambda () (assert (fs:at (format nil "/host/cold~d/kid~d"
                                                  wide (incf i))))))))
    (cost "walk to one already walked" *runs*
          (lambda () (fs:at "/host/cold256/kid1")))

    (format t "~&~%a pine mounted into another: both sides of the trip~%~%")
    (cost "read a value three deep, in this pine" 200000
          (lambda () (fs:contents (fs:at "/dev/audio/volume"))))
    (cost "ping" *runs*
          (lambda () (peer::%ask p (list :ping))))
    (cost "read three deep, through a mount" *runs*
          (lambda () (fs:contents (fs:at "/host/dev/audio/volume"))))
    (cost "write, through a mount" *runs*
          (lambda () (setf (fs:contents (fs:at "/host/dev/audio/volume")) 41)))
    (cost "list 2 children, through a mount" *runs*
          (lambda () (fs:entries (fs:at "/host/dev"))))
    (cost "list 100 children, through a mount" *runs*
          (lambda () (fs:entries (fs:at "/host/many"))))
    (cost "evaluate (+ 2 2) over there" *runs*
          (lambda () (image:evaluate p '(+ 2 2))))
    (format t "~&~%")
    (pine/run/job:stop p)))

(main)
(sb-ext:exit)
