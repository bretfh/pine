(defpackage #:pine.source
  (:use #:cl)
  (:export #:workspaces #:start-niri-source #:stop-niri-source))

(in-package #:pine.source)

;;;; Data sources feed reactive cells. niri pushes a JSON event stream, so a
;;;; thin reader thread (blocking read-line is unavoidable for a push stream)
;;;; forwards each workspace event to a source actor owned by the actor system;
;;;; the actor recomputes and set-cell's. Stopping terminates the subprocess so
;;;; the reader hits EOF and exits on its own -- the actor system disposes the
;;;; actor. Nothing is force-killed. Pine runs inside the Wayland session, so
;;;; unlike the eww broker there is no NIRI_SOCKET discovery.

(defun niri-json (&rest args)
  (com.inuoe.jzon:parse
   (uiop:run-program (list* "niri" "msg" "--json" args) :output :string)))

(defun workspaces ()
  "niri workspaces as (:idx N :focused BOOL :urgent BOOL), sorted by idx."
  (sort (map 'list
             (lambda (w) (list :idx (gethash "idx" w)
                               :focused (gethash "is_focused" w)
                               :urgent (gethash "is_urgent" w)))
             (niri-json "workspaces"))
        #'< :key (lambda (p) (getf p :idx))))

(defstruct niri-source system actor process reader)

(defvar *niri* nil "The running niri source, or nil.")

(defun %source-actor (system cell)
  "An actor that refreshes CELL from niri on a :refresh message."
  (sento.actor-context:actor-of system
    :name "niri-source"
    :receive
    (lambda (msg)
      (case (first msg)
        (:refresh (ignore-errors (pine.cell:set-cell cell (workspaces))))
        (t nil)))))

(defun start-niri-source (system)
  "Keep the :workspaces cell live from niri's event stream, supervised by the
actor SYSTEM. Idempotent."
  (or *niri*
      (let* ((cell (pine.cell:defcell :workspaces nil))
             (actor (%source-actor system cell))
             (proc (uiop:launch-program '("niri" "msg" "--json" "event-stream")
                                        :output :stream)))
        (ignore-errors (pine.cell:set-cell cell (workspaces)))
        (let ((reader
                (bordeaux-threads:make-thread
                 (lambda ()
                   (handler-case
                       (loop with out = (uiop:process-info-output proc)
                             for line = (read-line out nil nil)
                             while line
                             when (search "Workspace" line)
                               do (sento.actor:tell actor '(:refresh)))
                     (error () nil)))
                 :name "pine-niri-reader")))
          (setf *niri* (make-niri-source :system system :actor actor
                                         :process proc :reader reader))))))

(defun stop-niri-source ()
  "Terminate the subprocess (the reader then hits EOF and returns) and stop the
source actor. No thread is force-killed."
  (when *niri*
    (ignore-errors (uiop:terminate-process (niri-source-process *niri*) :urgent t))
    (ignore-errors (sento.actor-context:stop (niri-source-system *niri*)
                                             (niri-source-actor *niri*)))
    (setf *niri* nil)))
