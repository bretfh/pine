(defpackage #:pine.provider.screen
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:out #:pine.provider.out))
  (:export #:screen-node #:install #:brightness #:set-brightness #:device
           #:outputs #:output #:set-output))

(in-package #:pine.provider.screen)

(defclass screen-node (node:node) ())
(defclass reading-node (node:node) ())
(defclass outputs-node (node:node) ())
(defclass output-node (node:node) ())

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

(defun outputs ()
  "What niri says the screens are, by name."
  (let ((said (ignore-errors
               (com.inuoe.jzon:parse (out:sh "niri msg --json outputs")))))
    (when (hash-table-p said)
      (loop :for name :being :the :hash-keys :of said :collect name))))

(defun output (name)
  (let ((said (ignore-errors
               (com.inuoe.jzon:parse (out:sh "niri msg --json outputs")))))
    (when (hash-table-p said)
      (let ((it (gethash name said)))
        (when (hash-table-p it)
          (let ((mode (let ((modes (gethash "modes" it))
                            (at (gethash "current_mode" it)))
                        (when (and (vectorp modes) at (< at (length modes)))
                          (aref modes at)))))
            (list :scale (gethash "scale" it)
                  :mode (when (hash-table-p mode)
                          (format nil "~ax~a@~,3f"
                                  (gethash "width" mode) (gethash "height" mode)
                                  (/ (or (gethash "refresh_rate" mode) 0) 1000.0))))))))))

(defun set-output (name value)
  (let ((scale (getf value :scale)) (mode (getf value :mode)))
    (when scale (out:sh "niri msg output ~s scale ~a" name scale))
    (when mode (out:sh "niri msg output ~s mode ~a" name mode))
    value))

(defmethod node:nodes ((n screen-node))
  (list (%kid n "brightness")
        (node:child n "outputs"
                    (lambda () (make-instance 'outputs-node :name "outputs"
                                                            :parent n)))))

(defmethod node:resolve ((n screen-node) name)
  (cond ((equal name "brightness") (%kid n name))
        ((equal name "outputs")
         (node:child n name (lambda () (make-instance 'outputs-node :name name
                                                                    :parent n))))))

(defmethod node:nodes ((n outputs-node))
  (loop :for name :in (outputs)
        :collect (node:child n name
                             (lambda () (make-instance 'output-node :name name
                                                                    :parent n)))))

(defmethod node:resolve ((n outputs-node) name)
  (node:child n name
              (lambda () (make-instance 'output-node :name name :parent n))))

(defmethod node:contents ((n outputs-node)) (outputs))
(defmethod node:contents ((n output-node)) (output (node:name n)))

(defmethod (setf node:contents) (value (n output-node))
  (set-output (node:name n) value))

(defmethod node:leafp ((n output-node)) t)
(defmethod node:livep ((n outputs-node)) t)
(defmethod node:livep ((n output-node)) t)

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
