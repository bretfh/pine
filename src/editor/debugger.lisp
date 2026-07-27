(defpackage #:pine.editor.debugger
  (:use #:cl)
  (:export #:agent-debug-surface #:attend-session #:note-attendance #:debugger-quit #:eval-done #:eval-error #:eval-notify #:jobs-builder #:resolve-session #:text-layout #:*attended-session* #:*debugger-sessions* #:dbg-session #:dbg-session-ev #:dbg-session-kind #:dbg-session-restarts #:invoke-pending-restart))

(in-package #:pine.editor.debugger)

;;;; Evaluation runs through pine.err on its own thread, never on the UI
;;;; thread, so a slow/looping/erroring form can't hang or crash the editor.

;;;; Debugger sessions. A fault -- a local evaluation, or an error shipped home
;;;; from a :process agent -- becomes a session and joins a registry, because a
;;;; multi-image substrate faults in more than one place at once (two agents, an
;;;; agent and a buffer). The *debugger* buffer shows the ATTENDED session; Tab
;;;; pages to the next; picking a restart resolves the attended one and advances
;;;; to the next, or dismisses the buffer when none remain. Resolving drives the
;;;; decision back to where the restart is live: pick-restart on the blocked
;;;; local eval, or :resume to the agent.

(defstruct dbg-session
  id                 ; small integer, for the switcher header
  kind               ; :local or :agent
  ev                 ; the pine.err:evaluation (kind :local), resumed by pick-restart
  agent eval-id      ; agent name + eval-id (kind :agent), resumed by :resume
  header             ; one-line title
  condition          ; condition text
  restarts           ; list of (name report)
  backtrace)         ; text, or nil

(defvar *debugger-sessions* nil "Live debugger sessions, most-recent first.")
(defvar *attended-session* nil "The session the *debugger* buffer currently shows.")
(defvar *debugger-session-counter* 0)

(defun eval-notify (text)
  "Show TEXT in the echo area and repaint, safely from the eval thread."
  (pine.editor.echo:message text)
  (let ((r (ignore-errors (pine.editor.frame:renderer (pine.editor.frame:current-client)))))
    (when r (sento.actor:tell r '(:force-render)))))

(defun eval-done (ev &optional at)
  "Surface a finished eval: the result echoes, and lands inline as an overlay
on the form's line when AT is (BUFFER . LINE)."
  (case (pine.err:evaluation-status ev)
    (:ok (let ((txt (format nil "=> ~{~s~^, ~}" (pine.err:evaluation-values ev))))
           (when at
             (ignore-errors
              (sento.actor:tell (car at)
                                (list :overlay :line (cdr at) :text txt))))
           (eval-notify txt)))
    (:aborted (eval-notify "aborted"))))

;;;; The debugger buffer is a layout buffer: restart rows are selectable
;;;; nodes whose activation invokes that restart, so Return / C-n / C-p are the
;;;; ordinary surface interaction and point->node needs no side table. The
;;;; restart stays live on its blocked thread (or in its agent); only the
;;;; decision moves.

(defvar *debugger-return-to* nil
  "Buffer name to return to when the last session is resolved or dismissed.")

(defun %switch-to-buffer (name)
  (let ((client (pine.editor.frame:current-client))
        (buf (pine.editor.frame:buffer name)))
    (when buf
      (pine.editor.frame:switch-buffer name)
      (let ((r (ignore-errors (pine.editor.frame:renderer client))))
        (when r
          (sento.actor:tell r (list :switch-buffer :buffer buf :name name)))))))

(defun %debugger-builder (session ordered)
  "The *debugger* layout for SESSION: switcher row (when several sessions are
live), header, condition, one selectable restart row per restart -- activation
invokes that restart -- and the backtrace."
  (lambda (state)
    (declare (ignore state))
    (apply #'pine.ui.build:column :align :stretch
           (append
            (when (> (length ordered) 1)
              (list (pine.ui.build:label
                     (format nil "session ~d/~d  (Tab: next)   ~{~a~^  |  ~}"
                             (1+ (or (position session ordered) 0)) (length ordered)
                             (mapcar #'dbg-session-header ordered))
                     :class "dbg-switch")
                    (pine.ui.build:label "")))
            (list (pine.ui.build:label (dbg-session-header session) :class "dbg-header")
                  (pine.ui.build:label ""))
            (mapcar (lambda (l) (pine.ui.build:label l :class "dbg-cond"))
                    (pine.text.buffer:split-lines (dbg-session-condition session)))
            (list (pine.ui.build:label "")
                  (pine.ui.build:label "Restarts (Return on a line):" :class "dbg-note"))
            (loop for (name report) in (dbg-session-restarts session) collect
              (pine.ui.build:choice
               :class "restart" :prefix-selected "" :prefix-unselected ""
               :data (let ((nm name)) (lambda () (invoke-pending-restart nm)))
               (pine.ui.build:label (format nil "  [~a]  ~a" (or name "") (or report ""))
                                  :class "restart-lbl")))
            (when (dbg-session-backtrace session)
              (list* (pine.ui.build:label "")
                     (pine.ui.build:label "Backtrace:" :class "dbg-note")
                     (mapcar (lambda (l) (pine.ui.build:label l :class "dbg-bt"))
                             (pine.text.buffer:split-lines (dbg-session-backtrace session)))))))))

(defun attend-session (session)
  "Open SESSION in the *debugger* buffer, make it the attended one, and switch
to it. The eval target follows the attended fault, so C-x C-e / recompile land
in the image that broke -- fix the defun there, then pick retry."
  (setf *attended-session* session
        pine.editor.target:*eval-target* (ecase (dbg-session-kind session)
                        (:agent (dbg-session-agent session))
                        (:local :local)))
  (pine.editor.layout:show-layout "*debugger*"
                (%debugger-builder session (reverse *debugger-sessions*))
                :mode :debugger-mode :selection 0))

(defun %push-session (session)
  "Register SESSION and attend it. The return-to buffer is captured the first
time the debugger opens, so resolving the last session lands you back where you
were before any fault."
  (unless *debugger-sessions*
    (setf *debugger-return-to* (ignore-errors (pine.editor.ask:ask :current :name))
          pine.editor.target:*eval-target-saved* pine.editor.target:*eval-target*))
  (push session *debugger-sessions*)
  (note-attendance session t)
  (attend-session session))

(defun %dismiss-debugger ()
  "Hide the *debugger* buffer and return to the pre-debugger buffer. Does not
resolve anything -- any sessions still in the registry stay parked."
  (when *debugger-return-to*
    (%switch-to-buffer *debugger-return-to*))
  (ignore-errors (pine.editor.frame:kill-buffer "*debugger*")))

(defun resolve-session (session)
  "Drop SESSION from the registry (its thread was just resumed); attend the next
live session, or dismiss the buffer and clear the return-to when none remain."
  (setf *debugger-sessions* (remove session *debugger-sessions*))
  (note-attendance session nil)
  (let ((next (first *debugger-sessions*)))
    (cond (next (attend-session next))
          (t (setf *attended-session* nil
                   pine.editor.target:*eval-target* pine.editor.target:*eval-target-saved*)   ; back to the pre-fault target
             (%dismiss-debugger)
             (setf *debugger-return-to* nil)))))

(defun note-attendance (session open)
  "Say whether SESSION's evaluation is being looked at, so the thread parked in
it knows whether a decision is still coming."
  (let ((ev (dbg-session-ev session)))
    (when ev (setf (pine.err:attended-p ev) open))))

(defun eval-error (ev)
  (%push-session
   (make-dbg-session
    :id (incf *debugger-session-counter*) :kind :local :ev ev
    :header (format nil "Evaluation error: ~a" (pine.err:evaluation-condition-type ev))
    :condition (pine.err:evaluation-condition ev)
    :restarts (pine.err:evaluation-restarts ev)
    :backtrace (pine.err:evaluation-backtrace ev)))
  (eval-notify (format nil "eval error: ~a  (0-9/Return picks a restart, q quits)"
                        (pine.err:evaluation-condition-type ev))))

(defun agent-debug-surface (msg)
  "A process agent's error, surfaced in the editor: show its restarts and drive
the resume back to that agent. Move the decision, not the handler."
  (when (eq (first msg) :agent-debug)
    (destructuring-bind (&key agent eval-id condition restarts &allow-other-keys)
        (rest msg)
      (%push-session
       (make-dbg-session
        :id (incf *debugger-session-counter*) :kind :agent :agent agent :eval-id eval-id
        :header (format nil "Error in agent ~a" agent)
        :condition (or condition "")
        :restarts (mapcar (lambda (r) (list r nil)) (remove nil restarts))
        :backtrace nil))
      (eval-notify (format nil "agent ~a error (0-9/Return picks a restart)" agent)))))

(defun %session-resume (session name)
  "Send NAME to where SESSION's restart is live: pick-restart on the blocked
local eval, or :resume to the agent that shipped its restarts home."
  (ecase (dbg-session-kind session)
    (:local
     (when (and (dbg-session-ev session)
                (eq (pine.err:evaluation-status (dbg-session-ev session)) :error))
       (pine.err:pick-restart (dbg-session-ev session) name)))
    (:agent
     (let ((info (pine.core.actor:find-agent
                  (pine.editor.frame:server-of (pine.editor.frame:current-client))
                  (dbg-session-agent session))))
       (when info
         (sento.actor:tell (pine.core.actor:agent-info-actor info)
                           (list :resume :eval-id (dbg-session-eval-id session)
                                 :restart name)))))))

(defun invoke-pending-restart (name)
  "Invoke restart NAME on the attended session, then resolve it (advance to the
next live session, or dismiss the debugger)."
  (let ((session *attended-session*))
    (cond
      ((null session) (pine.editor.echo:message "no evaluation in the debugger") nil)
      (t (%session-resume session name)
         (resolve-session session)
         (pine.editor.echo:message (format nil "invoked ~a" name))
         t))))

(defun debugger-quit ()
  "Dismiss the *debugger* view without resolving; parked sessions stay in the
registry (M-x debugger reopens the attended one)."
  (%dismiss-debugger))

(defun text-layout (text)
  "A read-only layout from TEXT: the first line styled as the heading, the
rest as entries."
  (lambda (state)
    (declare (ignore state))
    (let ((lines (pine.text.buffer:split-lines text)))
      (apply #'pine.ui.build:column :align :stretch
             (cons (pine.ui.build:label (or (first lines) "") :class "help-head")
                   (mapcar (lambda (l) (pine.ui.build:label l :class "help-entry"))
                           (rest lines)))))))

(defun %attend-job (j)
  "Open the debugger on job J's session when one is parked, else echo its
status."
  (let ((s (find-if
            (lambda (s)
              (ecase (dbg-session-kind s)
                (:agent (and (equal (dbg-session-agent s) (getf j :agent))
                             (eql (dbg-session-eval-id s) (getf j :id))))
                (:local (and (equal (getf j :agent) "local")
                             (dbg-session-ev s)
                             (eql (pine.err:evaluation-id (dbg-session-ev s))
                                  (getf j :id))))))
            *debugger-sessions*)))
    (if s
        (attend-session s)
        (pine.editor.echo:message (format nil "job ~a: ~a" (getf j :id) (getf j :status))))))

(defun jobs-builder ()
  "The *jobs* layout: every live evaluation across the daemon and the agents,
one selectable row each; Return attends an errored one's debugger session."
  (lambda (state)
    (declare (ignore state))
    (apply #'pine.ui.build:column :align :stretch
           (list* (pine.ui.build:label "Jobs  (Return attends an errored one)"
                                     :class "help-head")
                  (pine.ui.build:label (format nil "~4@a  ~10a ~9a ~a"
                                             "id" "agent" "status" "form / condition")
                                     :class "dbg-note")
                  (loop for j in (pine.core.jobs:list-jobs) collect
                    (let ((j j))
                      (pine.ui.build:choice
                       :class "job-row" :prefix-selected "" :prefix-unselected ""
                       :data (lambda () (%attend-job j))
                       (pine.ui.build:label
                        (format nil "~4@a  ~10a ~9a ~a"
                                (getf j :id) (getf j :agent) (getf j :status)
                                (or (getf j :form) (getf j :condition) ""))
                        :class "help-entry"))))))))
