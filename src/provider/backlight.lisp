(defpackage #:pine.provider.backlight
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:out #:pine.provider.out))
  (:export #:backlight))

(in-package #:pine.provider.backlight)
(named-readtables:in-readtable pine.path:syntax)

;;;; The screens: how bright the panel is, and what the outputs are set to.
;;;;
;;;;   (write /screen (backlight))
;;;;
;;;; Brightness is sysfs, because it is two integers in two files and shelling
;;;; out to read them would be slower and less certain. The outputs come from
;;;; the compositor, which is the only thing that knows them.

(defun %device ()
  "The backlight sysfs directory, or NIL on a machine with no panel."
  (first (directory "/sys/class/backlight/*/")))

(defun %brightness ()
  (let ((device (%device)))
    (when device
      (let ((now (out:int-file (merge-pathnames "brightness" device)))
            (most (out:int-file (merge-pathnames "max_brightness" device))))
        (when (and now most (plusp most)) (round (* 100 now) most))))))

(defun %set-brightness (percent)
  (out:sh "brightnessctl set ~a%" (max 1 (min 100 (round percent)))))

(defun %outputs ()
  "Every output the compositor reports, name to what it is set to."
  (let ((table (out:json (out:sh "niri msg --json outputs"))))
    (when (hash-table-p table)
      (loop :for name :being :the :hash-keys :of table :using (:hash-value o)
            :collect (cons name o)))))

(defun %mode (output)
  (let* ((modes (and (hash-table-p output) (gethash "modes" output)))
         (current (and (hash-table-p output) (gethash "current_mode" output)))
         (mode (and (vectorp modes) (integerp current) (< current (length modes))
                    (aref modes current))))
    (when (hash-table-p mode)
      (format nil "~ax~a@~,3f" (gethash "width" mode) (gethash "height" mode)
              (* 0.001 (or (gethash "refresh_rate" mode) 0))))))

(defun backlight ()
  "The screens: the panel's brightness, and the outputs there are."
  (ns:provider
   {:doc "the panel through sysfs and brightnessctl, the outputs through niri"}

   (/screen/brightness
    {:read (pine.data:fn [] (%brightness))
     :write (pine.data:fn [v] (%set-brightness v))
     :doc "0..100"})

   (/screen/outputs/?name
    {:read (pine.data:fn []
             (let ((found (assoc name (%outputs) :test #'string=)))
               (when found
                 (fset:map (:mode (%mode (cdr found)))
                           (:scale (gethash "scale" (cdr found) 1))))))
     :write (pine.data:fn [v]
              (let ((mode (and (fset:map? v) (fset:lookup v :mode)))
                    (scale (and (fset:map? v) (fset:lookup v :scale))))
                (when mode (out:sh "niri msg output '~a' mode '~a'" name mode))
                (when scale (out:sh "niri msg output '~a' scale ~a" name scale))
                v))
     :doc "{:mode .. :scale ..}"})

   (/screen/outputs
    {:ls (pine.data:fn [] (mapcar #'car (%outputs)))
     :doc "every output there is"})

   (/screen {:doc "outputs and backlight"})))
