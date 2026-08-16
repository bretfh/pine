(defpackage #:pine/run/actors
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fault #:pine/run/fault))
  (:export #:boot #:leave #:actors #:runningp #:remoting
           #:pool #:pools #:dispatcher-for #:*pools* #:*workers* #:*soonest* #:*host* #:*port*
           #:repeat #:after #:cancel #:ticks #:blocking))
(in-package #:pine/run/actors)

(defvar *actors* nil)
(defvar *wheel* nil)
(defvar *ticks* (d:table))
(defvar *host* "127.0.0.1")
(defvar *port* 17000)
(defparameter *soonest* 0.05)
(defvar *workers*
  (max 2 (1- (or (ignore-errors
                  (parse-integer (uiop:run-program '("nproc")
                                                   :output '(:string :stripped t))))
                 4))))
(defvar *pools* (d:table)
  "The dispatchers this image will have, by name. A system that wants work of its
own kept off everybody else's asks for one before boot; sento fixes them when the
system is made, so this is read once and not added to after.")

(defun pool (name workers)
  "Ask for a dispatcher of NAME with WORKERS workers. Nothing is reserved: the
parse has one because text asked for it, not because this file knows about text."
  (d:keep! *pools* name workers)
  name)

(defun pools () (d:all *pools*))

(defun dispatcher-for (name)
  "The dispatcher an actor asks for, if this image has it. Sento fixes its pools
when the system is made, so a system loaded after boot that asked for one of its
own runs on the shared pool until the next start rather than refusing to run."
  (if (or (member name '(:shared :pinned)) (d:at (pools) name))
      name
      :shared))

(defun %config ()
  (list :dispatchers
        (list* :shared (list :workers *workers* :strategy :random)
               (loop :for (name . workers) :in (d:pairs (pools))
                     :append (list name (list :workers workers
                                              :strategy :random))))
        :scheduler (list :enabled :true :max-size 1000
                         :resolution (round (* 1000 *soonest*)))))

(defun actors () *actors*)

(defun runningp () (and *actors* t))

(defun boot (&key remoting)
  "One actor system for this image, made whether or not remoting is on. Everything
that runs is on it: the wheel, the pools, every actor. There is no second clock and
no thread that sleeps in a loop."
  (let ((sys (sento.actor-system:make-actor-system (%config))))
    (when remoting
      (sento.remoting:enable-remoting sys :host *host* :port remoting))
    (setf *actors* sys
          *wheel* (sento.actor-system:scheduler sys))
    sys))

(defun remoting ()
  "The port other pines reach this one on, or nothing. One question, one name."
  (and *actors*
       (sento.remoting:remoting-enabled-p *actors*)
       (sento.remoting:remoting-port *actors*)))

(defun leave ()
  (dolist (name (ticks)) (cancel name))
  (let ((sys *actors*))
    (when sys
      (when (sento.remoting:remoting-enabled-p sys)
        (ignore-errors (sento.remoting:disable-remoting sys)))
      (ignore-errors (sento.actor-context:shutdown sys :wait t))))
  (setf *actors* nil *wheel* nil)
  t)

(defun ticks () (d:keys (d:all *ticks*)))

(defun %off-wheel (thunk what)
  "Off the wheel thread. The wheel is one thread for the whole image and these
thunks shell out, read files and paint."
  (lambda ()
    (let ((sys *actors*))
      (flet ((run () (fault:attempt thunk what)))
        (if sys
            (sento.tasks:with-context (sys) (sento.tasks:task-start #'run))
            (run))))))

(defun cancel (name)
  (let ((had (d:at (d:all *ticks*) name)))
    (when (and had *wheel*)
      (ignore-errors (sento.wheel-timer:cancel *wheel* had))
      (d:drop! *ticks* name))
    name))

(defun repeat (seconds thunk &key (as (gensym "REPEAT-")) (what "a tick"))
  "Run THUNK every SECONDS, under the name AS. Asking again under one name replaces
what was there."
  (when *wheel*
    (cancel as)
    (let ((signature (gensym "PINE-REPEAT-"))
          (seconds (max seconds *soonest*)))
      (sento.wheel-timer:schedule-recurring *wheel* seconds seconds
                                            (%off-wheel thunk what) signature)
      (d:keep! *ticks* as signature)
      as)))

(defun after (seconds thunk &key (what "a tick"))
  (when *wheel*
    (sento.wheel-timer:schedule-once *wheel* (max seconds *soonest*)
                                     (%off-wheel thunk what))))

(defun blocking (name thunk)
  "A thread, for something that blocks: a pty read, a child's stdout, a frontend's
own loop. Everything else is an actor or a tick."
  (bordeaux-threads:make-thread thunk :name (format nil "pine ~a" name)))
