(defpackage #:pine.provider.media
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:out #:pine.provider.out))
  (:export #:media-node #:install #:status #:title #:artist #:album #:art
           #:position-of #:length-of #:act #:*player*))

(in-package #:pine.provider.media)

(defvar *player* nil)

(defparameter +readings+
  '("status" "title" "artist" "album" "art" "position" "length"))

(defparameter +verbs+ '("play" "pause" "next" "previous" "stop"))

(defclass media-node (node:node) ())
(defclass reading-node (node:node) ())
(defclass verb-node (node:node) ())

(defun %playerctl (format &rest arguments)
  (let ((which (if *player* (format nil "-p ~a " *player*) "")))
    (apply #'out:sh (concatenate 'string "playerctl " which format) arguments)))

(defun status ()
  (let ((said (%playerctl "status 2>/dev/null")))
    (cond ((search "Playing" said) :playing)
          ((search "Paused" said) :paused)
          ((plusp (length said)) :stopped))))

(defun %meta (key)
  (let ((said (%playerctl "metadata ~a 2>/dev/null" key)))
    (when (plusp (length said)) said)))

(defun title () (%meta "xesam:title"))
(defun artist () (%meta "xesam:artist"))
(defun album () (%meta "xesam:album"))
(defun art () (%meta "mpris:artUrl"))

(defun position-of ()
  (out:number-in (%playerctl "position 2>/dev/null")))

(defun length-of ()
  (let ((micro (out:number-in (%meta "mpris:length"))))
    (when micro (round micro 1000000))))

(defun act (what)
  (when (member what +verbs+ :test #'equal)
    (%playerctl "~a" (if (equal what "pause") "play-pause" what))
    t))

(defun %reading (name)
  (cond ((equal name "status") (status))
        ((equal name "title") (title))
        ((equal name "artist") (artist))
        ((equal name "album") (album))
        ((equal name "art") (art))
        ((equal name "position") (position-of))
        ((equal name "length") (length-of))))

(defmethod node:nodes ((n media-node))
  (append (loop :for name :in +readings+
                :collect (make-instance 'reading-node :name name :parent n))
          (loop :for name :in +verbs+
                :collect (make-instance 'verb-node :name name :parent n))))

(defmethod node:resolve ((n media-node) name)
  (cond ((member name +readings+ :test #'equal)
         (make-instance 'reading-node :name name :parent n))
        ((member name +verbs+ :test #'equal)
         (make-instance 'verb-node :name name :parent n))))

(defmethod node:contents ((n media-node)) (status))
(defmethod node:contents ((n reading-node)) (%reading (node:name n)))
(defmethod node:contents ((n verb-node)) (node:name n))

(defmethod (setf node:contents) (value (n reading-node))
  (when (equal "position" (node:name n))
    (%playerctl "position ~a" value))
  value)

(defmethod (setf node:contents) (value (n verb-node))
  (declare (ignore value))
  (act (node:name n)))

(defmethod node:leafp ((n reading-node)) t)
(defmethod node:leafp ((n verb-node)) t)
(defmethod node:livep ((n media-node)) t)
(defmethod node:livep ((n reading-node)) t)
(defmethod node:livep ((n verb-node)) t)

(defun install (root &key (name "media") player)
  (setf *player* player)
  (node:attach (make-instance 'media-node :name name
                                          :describes "what is playing, through mpris")
               root))
