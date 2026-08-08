(defpackage #:pine.provider.audio
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:out #:pine.provider.out))
  (:export #:audio-node #:install #:volume #:muted #:sink #:sinks #:set-volume
           #:toggle-muted #:set-sink))

(in-package #:pine.provider.audio)

(defparameter +leaves+ '("volume" "muted" "sink"))

(defclass audio-node (node:node) ())
(defclass reading-node (node:node) ())
(defclass sinks-node (node:node) ())
(defclass sink-node (node:node)
  ((id :initarg :id :reader id)))

(defun %wpctl () (out:has "wpctl"))

(defun volume ()
  (let ((n (out:number-in (out:sh "wpctl get-volume @DEFAULT_AUDIO_SINK@"))))
    (when n (round (* 100 n)))))

(defun muted ()
  (search "MUTED" (out:sh "wpctl get-volume @DEFAULT_AUDIO_SINK@")))

(defun set-volume (percent)
  (out:sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ ~d%" (max 0 (min 100 percent)))
  percent)

(defun toggle-muted ()
  (out:sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
  (muted))

(defun %sink-lines ()
  (let ((seen nil) (inside nil))
    (dolist (line (out:lines (out:sh "wpctl status")) (nreverse seen))
      (cond ((search "Sinks:" line) (setf inside t))
            ((and inside (search "Sources:" line)) (setf inside nil))
            ((and inside (out:number-in line))
             (push (cons (out:number-in line)
                         (string-trim " *.<>" (subseq line (or (position #\. line) 0))))
                   seen))))))

(defun sinks () (mapcar #'car (%sink-lines)))

(defun sink ()
  (let ((line (find-if (lambda (each) (search "*" (cdr each))) (%sink-lines))))
    (car (or line (first (%sink-lines))))))

(defun set-sink (id)
  (out:sh "wpctl set-default ~a" id)
  id)

(defmethod node:nodes ((n audio-node))
  (append (loop :for name :in +leaves+
                :collect (make-instance 'reading-node :name name :parent n))
          (list (make-instance 'sinks-node :name "sinks" :parent n))))

(defmethod node:resolve ((n audio-node) name)
  (cond ((member name +leaves+ :test #'equal)
         (make-instance 'reading-node :name name :parent n))
        ((equal name "sinks") (make-instance 'sinks-node :name "sinks" :parent n))))

(defmethod node:contents ((n audio-node)) (volume))

(defmethod node:contents ((n reading-node))
  (let ((name (node:name n)))
    (cond ((equal name "volume") (volume))
          ((equal name "muted") (and (muted) t))
          ((equal name "sink") (sink)))))

(defmethod (setf node:contents) (value (n reading-node))
  (let ((name (node:name n)))
    (cond ((equal name "volume") (set-volume value))
          ((equal name "muted") (toggle-muted))
          ((equal name "sink") (set-sink value))))
  value)

(defmethod node:nodes ((n sinks-node))
  (loop :for (id . text) :in (%sink-lines)
        :collect (make-instance 'sink-node :name (princ-to-string id)
                                           :parent n :id id)))

(defmethod node:resolve ((n sinks-node) name)
  (make-instance 'sink-node :name name :parent n :id name))

(defmethod node:contents ((n sinks-node)) (sinks))

(defmethod node:contents ((n sink-node))
  (cdr (assoc (id n) (%sink-lines) :test #'equal)))

(defmethod node:leafp ((n reading-node)) t)
(defmethod node:leafp ((n sink-node)) t)

(defmethod node:livep ((n audio-node)) t)
(defmethod node:livep ((n reading-node)) t)
(defmethod node:livep ((n sinks-node)) t)
(defmethod node:livep ((n sink-node)) t)

(defun install (root &optional (name "audio"))
  (node:attach (make-instance 'audio-node :name name
                                          :describes "the default sink: volume, muted, which one")
               root))
