(defpackage #:pine.provider.pipewire
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:out #:pine.provider.out))
  (:export #:pipewire))

(in-package #:pine.provider.pipewire)
(named-readtables:in-readtable pine.path:syntax)


;;;; Sound, through wpctl and pactl.
;;;;
;;;;   (write /audio (pipewire))
;;;;
;;;; pactl subscribe says when anything moved, so nothing here polls: a volume
;;;; key changes the volume and the bar has the new number because the path it
;;;; read moved.

(defparameter *default* "@DEFAULT_AUDIO_SINK@")

(defun %volume ()
  (out:percent (out:sh "wpctl get-volume ~a" *default*)))

(defun %mutedp ()
  (and (search "MUTED" (out:sh "wpctl get-volume ~a" *default*)) t))

(defun %sinks ()
  "Every sink pactl knows, name to description."
  (loop :for line :in (out:lines (out:sh "pactl list short sinks"))
        :for parts = (out:split line #\tab)
        :when (second parts)
          :collect (cons (second parts) (or (third parts) (second parts)))))

(defun pipewire ()
  "Sound: the default sink's volume and mute, and the sinks there are."
  (ns:provider
   {:on /sh/${"pactl subscribe"}
    :doc "pipewire, through wpctl and pactl"}

   (/audio/volume
    {:read (pine.data:fn [] (%volume))
     :write (pine.data:fn [v] (out:sh "wpctl set-volume ~a ~a%" *default* v))
     :doc "0..100"})

   (/audio/muted
    {:read (pine.data:fn [] (%mutedp))
     :write (pine.data:fn [v] (out:sh "wpctl set-mute ~a ~a" *default* (if v 1 0)))
     :verbs {:toggle (pine.data:fn [] (fset:map (/audio/muted (not (%mutedp)))))}
     :doc "whether the default sink is muted; [:toggle] flips it"})

   (/audio/sinks/?name
    {:read (pine.data:fn []
             (let ((found (assoc name (%sinks) :test #'string=)))
               (when found (fset:map (:desc (cdr found))))))
     :doc "what that sink calls itself"})

   (/audio/sinks
    {:ls (pine.data:fn [] (mapcar #'car (%sinks)))
     :doc "every sink there is"})

   (/audio/sink
    {:read (pine.data:fn [] (out:sh "pactl get-default-sink"))
     :write (pine.data:fn [v] (out:sh "pactl set-default-sink '~a'" v))
     :doc "the sink everything plays through"})

   (/audio {:doc "sinks, volume, mute"})))
