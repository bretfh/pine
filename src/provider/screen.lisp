(defpackage #:pine.provider.screen
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:out #:pine.provider.out))
  (:export #:screen-node #:install #:brightness #:set-brightness #:device))

(in-package #:pine.provider.screen)

(defclass screen-node (node:node) ())
(defclass reading-node (node:node) ())

(defun device ()
  (first (directory "/sys/class/backlight/*/")))

(defun brightness ()
  (let ((where (device)))
    (when where
      (let ((now (out:number-in (out:sh "cat ~abrightness 2>/dev/null"
                                        (namestring where))))
            (most (out:number-in (out:sh "cat ~amax_brightness 2>/dev/null"
                                         (namestring where)))))
        (when (and now most (plusp most))
          (round (* 100 now) most))))))

(defun set-brightness (percent)
  (when (device)
    (out:sh "brightnessctl --class=backlight set ~d%" (max 1 (min 100 percent)))
    percent))

(defmethod node:every-seconds ((n screen-node)) 5)

(defun %kid (n name)
  (node:child n name
              (lambda () (make-instance 'reading-node :name name :parent n))))

(defmethod node:nodes ((n screen-node))
  (list (%kid n "brightness")))

(defmethod node:resolve ((n screen-node) name)
  (when (equal name "brightness") (%kid n name)))

(defmethod node:contents ((n screen-node)) (brightness))
(defmethod node:contents ((n reading-node)) (brightness))

(defmethod (setf node:contents) (value (n reading-node))
  (set-brightness value))

(defmethod node:leafp ((n reading-node)) t)
(defmethod node:livep ((n screen-node)) t)
(defmethod node:livep ((n reading-node)) t)

(defun install (root &optional (name "screen"))
  (node:attach (make-instance 'screen-node :name name
                                           :describes "the backlight, as a percentage")
               root))
