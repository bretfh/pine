(defpackage #:pine/run/actors
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:fault #:pine/run/fault))
  (:export
   #:boot #:leave #:actors #:runningp #:remoting
   #:dispatcher-for #:*host* #:*port* #:schedule #:unschedule #:after
   #:later #:blocking #:joined #:*reading-workers* #:*watching-workers*))
(in-package #:pine/run/actors)

(defvar *actors* nil)
(defvar *wheel* nil)
(defvar *host* "127.0.0.1")
(defvar *port* 17000)
(defparameter *soonest* 0.05)
(defvar *workers* nil)
(defvar *reading-workers* 8)
(defvar *watching-workers* 4)

(defun dispatcher-for (name)
  (if (member name '(:shared :pinned :slow :watch)) name :shared))

(defun workers ()
  (or *workers*
      (setf *workers*
            (max 2 (1- (or (fault:or-nothing "a machine that will not say how many cores"
                             (parse-integer
                              (uiop:run-program '("nproc")
                                                :output '(:string :stripped t))))
                           4))))))

(defun %config ()
  (list :dispatchers
        (list :shared (list :workers (workers) :strategy :random)
              :slow (list :workers *reading-workers* :strategy :round-robin)
              :watch (list :workers *watching-workers* :strategy :round-robin))
        :scheduler (list :enabled :true :max-size 1000
                         :resolution (round (* 1000 *soonest*)))))

(defun actors () *actors*)

(defun runningp () (and *actors* t))

(defun boot (&key remoting)
  (let ((sys (sento.actor-system:make-actor-system (%config))))
    (when remoting
      (fault:or-nothing "that port is taken"
        (sento.remoting:enable-remoting sys :host *host* :port remoting))
      (unless (sento.remoting:remoting-enabled-p sys)
        (fault:or-nothing "no port to answer peers on"
          (sento.remoting:enable-remoting sys :host *host* :port 0))))
    (setf *actors* sys
          *wheel* (sento.actor-system:scheduler sys))
    sys))

(defun later (name thunk)
  (let ((sys *actors*))
    (if sys
        (let ((to (or (getf (sento.actor-system:dispatchers sys) name)
                      (getf (sento.actor-system:dispatchers sys) :shared))))
          (sento.dispatcher:dispatch-async
           to (list (lambda () (fault:attempt thunk (format nil "~a work" name)))))
          t)
        (progn (funcall thunk) t))))

(defun %hand-off (n thunk)
  (declare (ignore n))
  (later :slow thunk))

(defun remoting ()
  (and *actors*
       (sento.remoting:remoting-enabled-p *actors*)
       (sento.remoting:remoting-port *actors*)))

(defun leave ()
  (let ((sys *actors*))
    (when sys
      (when (sento.remoting:remoting-enabled-p sys)
        (fault:or-nothing "remoting may already be off"
          (sento.remoting:disable-remoting sys)))
      (fault:or-nothing "a system already down cannot be put down twice"
        (sento.actor-context:shutdown sys :wait t))))
  (setf *actors* nil *wheel* nil)
  t)

(defun %off-wheel (thunk what)
  (lambda ()
    (let ((sys *actors*))
      (flet ((run () (fault:attempt thunk what)))
        (if sys
            (sento.tasks:with-context (sys) (sento.tasks:task-start #'run))
            (run))))))

(defun schedule (seconds thunk what)
  (when *wheel*
    (let ((signature (gensym "PINE-REPEAT-"))
          (seconds (max seconds *soonest*)))
      (sento.wheel-timer:schedule-recurring *wheel* seconds seconds
                                            (%off-wheel thunk what) signature)
      signature)))

(defun unschedule (signature)
  (when (and signature *wheel*)
    (fault:or-nothing "a tick that has already fired is not there to cancel"
      (sento.wheel-timer:cancel *wheel* signature)))
  signature)

(defun after (seconds thunk &key (what "a tick"))
  (when *wheel*
    (sento.wheel-timer:schedule-once *wheel* (max seconds *soonest*)
                                     (%off-wheel thunk what))))

(defun blocking (name thunk)
  (bordeaux-threads:make-thread thunk :name (format nil "pine ~a" name)))

(defun joined (thread)
  (when thread (bordeaux-threads:join-thread thread)))

(setf fs:*slow-pool* #'%hand-off)
