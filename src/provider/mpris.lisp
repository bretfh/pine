(defpackage #:pine.provider.mpris
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:out #:pine.provider.out))
  (:export #:mpris))

(in-package #:pine.provider.mpris)
(named-readtables:in-readtable pine.path:syntax)

;;;; What is playing, through playerctl.
;;;;
;;;;   (write /media (mpris :player "emms"))
;;;;
;;;; playerctl --follow says when the track or the state changes, so a bar
;;;; showing the title is right without asking.

(defun %player-flag (player)
  (if player (format nil " --player='~a'" player) ""))

(defun %metadata (player)
  (let ((text (out:sh "playerctl~a metadata --format ~
                       '{{status}}~~{{title}}~~{{artist}}~~{{mpris:artUrl}}~~~
                       {{position}}~~{{mpris:length}}'"
                      (%player-flag player))))
    (when (plusp (length text)) (out:split text #\~))))

(defun %seconds (micros)
  (let ((n (out:number (or micros ""))))
    (when n (round n 1000000))))

(defun %status (text)
  (cond ((null text) nil)
        ((string-equal text "Playing") :playing)
        ((string-equal text "Paused") :paused)
        ((string-equal text "Stopped") :stopped)
        (t nil)))

(defun mpris (&key player)
  "What is playing: the track, where it is, and the transport."
  (ns:provider
   {:on /sh/${"playerctl --follow status"}
    :doc "mpris, through playerctl"}

   (/media/position
    {:read (pine.data:fn [] (%seconds (fifth (%metadata player))))
     :write (pine.data:fn [v]
              (out:sh "playerctl~a position ~a" (%player-flag player) v))
     :doc "seconds; write to seek"})

   (/media
    {:read (pine.data:fn []
             (let ((row (%metadata player)))
               (when row
                 (fset:map (:status (%status (first row)))
                           (:title (second row))
                           (:artist (third row))
                           (:art (fourth row))
                           (:position (%seconds (fifth row)))
                           (:length (%seconds (sixth row)))))))
     :verbs {:play (pine.data:fn [] (out:sh "playerctl~a play" (%player-flag player)) t)
             :pause (pine.data:fn [] (out:sh "playerctl~a pause" (%player-flag player)) t)
             :next (pine.data:fn [] (out:sh "playerctl~a next" (%player-flag player)) t)
             :previous (pine.data:fn []
                         (out:sh "playerctl~a previous" (%player-flag player)) t)}
     :doc "{:status .. :title .. :artist .. :art .. :position .. :length ..}; ~
           [:play] [:pause] [:next] [:previous]"})))
