(in-package #:pine.editor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(defun start-editor ()
  (let* ((client (pine.client:current-client))
         (server (pine.client:server-of client)))
    (pine.buffer:start-buffer-registry server)
    (handler-case (pine.ts:ensure-ts (pine.server:ts-runtime server))
      (error () nil))
    (pine.render:start-renderer client)
    (pine.mode:install-default-modes)
    (install-variables)
    (install-commands)
    (install-bindings)
    (ensure-minibuffer client)
    (setf pine.command:*terminal-handler* #'pine.term:terminal-dispatch)
    (setf pine.eval:*on-debug* #'%eval-error)
    (let ((buf (pine.buffer:make-buffer "scratch")))
      (pine.buffer:make-window buf "scratch"
                               :row 0 :col 0 :width 80 :height 29 :focused t)
      (pine.render:subscribe-to-buffer buf)
      (pine.mode:set-buffer-mode buf :text-mode)
      (pine.buffer:tell buf :set-local :key :package :value :pine-user))
    (pine.render:relayout)))

;;;; Motion / eval helpers (command implementations)

(defun focused-snap ()
  "The snapshot commands read for point/line context. While a prompt is active
this is the minibuffer's snapshot -- the minibuffer is the current buffer, so a
command like beginning-of-line must see its input, not the window behind it."
  (let ((c (pine.client:current-client)))
    (if (pine.client:prompt-active c)
        (pine.client:minibuffer-snap c)
        (let ((w (pine.client:focused-window c)))
          (when w (pine.buffer:snap w))))))

(defun cur-buffer () (pine.client:current-buffer (pine.client:current-client)))

(defun %fresh-snap ()
  (let ((buf (cur-buffer))) (when buf (pine.buffer:ask buf :snapshot))))

(defun %buffer-ts-lang ()
  (let ((mode (pine.mode:current-buffer-mode)))
    (and (typep mode 'pine.mode:major-mode) (pine.mode:ts-language mode))))

(defun %ts-runtime ()
  (pine.server:ts-runtime (pine.client:server-of (pine.client:current-client))))

(defun %sexp-move (kind)
  "Move point structurally via the buffer's persistent tree (no reparse). The
buffer walks its own tree from its own point and moves; nothing blocks here."
  (let ((buf (cur-buffer)))
    (when buf (sento.actor:tell buf (list :ts-motion :kind kind)))))

(defun move-chars (n)
  "Move point N characters (negative = left) across line boundaries. The buffer
computes the target from its own state, so this never blocks on a round-trip."
  (let ((buf (cur-buffer)))
    (when buf (sento.actor:tell buf (list :move-by :unit :char :n n)))))

(defun move-lines (n)
  "Move point N lines (negative = up), keeping the column where possible."
  (let ((buf (cur-buffer)))
    (when buf (sento.actor:tell buf (list :move-by :unit :line :n n)))))

(defun move-words (n)
  "Move point N words (negative = backward) across line boundaries."
  (let ((buf (cur-buffer)))
    (when buf (sento.actor:tell buf (list :move-by :unit :word :n n)))))

(defun %point->offset (snap)
  (let ((pl (pine.buffer:point-line snap))
        (pc (pine.buffer:point-col snap))
        (lines (pine.buffer:lines snap)))
    (+ (loop for i from 0 below pl sum (1+ (length (fset:@ lines i)))) pc)))

(defun %whitespace-p (c)
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline) (char= c #\Return)))

(defun %sexp-delim-p (c)
  (or (%whitespace-p c) (char= c #\() (char= c #\)) (char= c #\")))

(defun %match-paren-backward (text close-pos)
  (let ((depth 1) (i (1- close-pos)) (in-string nil))
    (loop while (>= i 0) do
      (let ((c (char text i)))
        (cond
          (in-string
           (when (and (char= c #\") (or (zerop i) (not (char= (char text (1- i)) #\\))))
             (setf in-string nil))
           (decf i))
          ((char= c #\") (setf in-string t) (decf i))
          ((char= c #\)) (incf depth) (decf i))
          ((char= c #\()
           (decf depth)
           (when (zerop depth) (return-from %match-paren-backward i))
           (decf i))
          (t (decf i)))))
    nil))

(defun %match-string-backward (text close-pos)
  (let ((i (1- close-pos)))
    (loop while (>= i 0) do
      (let ((c (char text i)))
        (if (and (char= c #\") (or (zerop i) (not (char= (char text (1- i)) #\\))))
            (return-from %match-string-backward i)
            (decf i))))
    nil))

(defun %atom-start-backward (text end-inclusive)
  (let ((i end-inclusive))
    (loop while (and (>= i 0) (not (%sexp-delim-p (char text i)))) do (decf i))
    (1+ i)))

(defun %preceding-sexp-bounds (text end)
  (let ((i (1- end)))
    (loop while (and (>= i 0) (%whitespace-p (char text i))) do (decf i))
    (when (minusp i) (return-from %preceding-sexp-bounds nil))
    (let ((c (char text i)))
      (cond
        ((char= c #\)) (let ((s (%match-paren-backward text i))) (when s (values s (1+ i)))))
        ((char= c #\") (let ((s (%match-string-backward text i))) (when s (values s (1+ i)))))
        (t (values (%atom-start-backward text i) (1+ i)))))))

(defun %infer-package (text)
  "The package named by the last (in-package ...) in TEXT, like SLIME infers it
from the buffer, or nil."
  (let ((pos 0) (result nil))
    (loop for idx = (search "(in-package" text :start2 pos)
          while idx do
            (multiple-value-bind (form new)
                (ignore-errors (read-from-string text nil nil :start idx))
              (when (and (consp form) (symbolp (first form))
                         (string-equal (symbol-name (first form)) "IN-PACKAGE")
                         (second form))
                (let ((p (find-package (second form)))) (when p (setf result p))))
              (setf pos (if (and new (> new idx)) new (1+ idx)))))
    result))

(defun %buffer-package (state)
  (let ((inferred (%infer-package (pine.buffer:state->string state)))
        (name (pine.buffer:buffer-local state :package nil)))
    (or inferred (and name (find-package name)) (find-package :cl-user))))

(defun %lc->offset (text line col)
  "Character offset of LINE/COL in TEXT."
  (let ((i 0) (l 0) (n (length text)))
    (loop while (and (< i n) (< l line))
          do (when (char= (char text i) #\Newline) (incf l))
             (incf i))
    (min (+ i col) n)))

;;;; Evaluation runs through pine.eval on its own thread, never on the UI
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
  ev                 ; the pine.eval:evaluation (kind :local), resumed by pick-restart
  agent eval-id      ; agent name + eval-id (kind :agent), resumed by :resume
  header             ; one-line title
  condition          ; condition text
  restarts           ; list of (name report)
  backtrace)         ; text, or nil

(defvar *debugger-sessions* nil "Live debugger sessions, most-recent first.")
(defvar *attended-session* nil "The session the *debugger* buffer currently shows.")
(defvar *debugger-session-counter* 0)

(defvar *eval-target* :local
  "Where C-x C-e / eval-defun run: :local (this image), or a registered agent
name (a :process agent's own image). The one eval path, target swappable.")
(defvar *eval-target-saved* :local
  "The eval target from before the debugger opened, restored when it closes:
while attending a fault the target follows the faulted image, so a fix compiles
into the image that broke.")

(defun %eval-notify (text)
  "Show TEXT in the echo area and repaint, safely from the eval thread."
  (pine.echo:message text)
  (let ((r (ignore-errors (pine.client:renderer (pine.client:current-client)))))
    (when r (sento.actor:tell r '(:force-render)))))

(defun %eval-done (ev)
  (case (pine.eval:evaluation-status ev)
    (:ok (%eval-notify (format nil "=> ~{~s~^, ~}" (pine.eval:evaluation-values ev))))
    (:aborted (%eval-notify "aborted"))))

;;;; The debugger buffer is a layout buffer: restart rows are selectable
;;;; nodes whose activation invokes that restart, so Return / C-n / C-p are the
;;;; ordinary surface interaction and point->node needs no side table. The
;;;; restart stays live on its blocked thread (or in its agent); only the
;;;; decision moves.

(defvar *debugger-return-to* nil
  "Buffer name to return to when the last session is resolved or dismissed.")

(defun %switch-to-buffer (name)
  (let ((client (pine.client:current-client))
        (buf (pine.buffer:buffer name)))
    (when buf
      (pine.buffer:switch-buffer name)
      (let ((r (ignore-errors (pine.client:renderer client))))
        (when r
          (sento.actor:tell r (list :switch-buffer :buffer buf :name name)))))))

(defun %debugger-builder (session ordered)
  "The *debugger* layout for SESSION: switcher row (when several sessions are
live), header, condition, one selectable restart row per restart -- activation
invokes that restart -- and the backtrace."
  (lambda (state)
    (declare (ignore state))
    (apply #'pine.layout:column :align :stretch
           (append
            (when (> (length ordered) 1)
              (list (pine.layout:label
                     (format nil "session ~d/~d  (Tab: next)   ~{~a~^  |  ~}"
                             (1+ (or (position session ordered) 0)) (length ordered)
                             (mapcar #'dbg-session-header ordered))
                     :class "dbg-switch")
                    (pine.layout:label "")))
            (list (pine.layout:label (dbg-session-header session) :class "dbg-header")
                  (pine.layout:label ""))
            (mapcar (lambda (l) (pine.layout:label l :class "dbg-cond"))
                    (pine.buffer:split-lines (dbg-session-condition session)))
            (list (pine.layout:label "")
                  (pine.layout:label "Restarts (Return on a line):" :class "dbg-note"))
            (loop for (name report) in (dbg-session-restarts session) collect
              (pine.layout:choice
               :class "restart" :prefix-selected "" :prefix-unselected ""
               :data (let ((nm name)) (lambda () (invoke-pending-restart nm)))
               (pine.layout:label (format nil "  [~a]  ~a" (or name "") (or report ""))
                                  :class "restart-lbl")))
            (when (dbg-session-backtrace session)
              (list* (pine.layout:label "")
                     (pine.layout:label "Backtrace:" :class "dbg-note")
                     (mapcar (lambda (l) (pine.layout:label l :class "dbg-bt"))
                             (pine.buffer:split-lines (dbg-session-backtrace session)))))))))

(defun %attend-session (session)
  "Open SESSION in the *debugger* buffer, make it the attended one, and switch
to it. The eval target follows the attended fault, so C-x C-e / recompile land
in the image that broke -- fix the defun there, then pick retry."
  (setf *attended-session* session
        *eval-target* (ecase (dbg-session-kind session)
                        (:agent (dbg-session-agent session))
                        (:local :local)))
  (show-layout "*debugger*"
                (%debugger-builder session (reverse *debugger-sessions*))
                :mode :debugger-mode :selection 0))

(defun %push-session (session)
  "Register SESSION and attend it. The return-to buffer is captured the first
time the debugger opens, so resolving the last session lands you back where you
were before any fault."
  (unless *debugger-sessions*
    (setf *debugger-return-to* (ignore-errors (pine.buffer:ask :current :name))
          *eval-target-saved* *eval-target*))
  (push session *debugger-sessions*)
  (%attend-session session))

(defun %dismiss-debugger ()
  "Hide the *debugger* buffer and return to the pre-debugger buffer. Does not
resolve anything -- any sessions still in the registry stay parked."
  (when *debugger-return-to*
    (%switch-to-buffer *debugger-return-to*))
  (ignore-errors (pine.buffer:kill-buffer "*debugger*")))

(defun %resolve-session (session)
  "Drop SESSION from the registry (its thread was just resumed); attend the next
live session, or dismiss the buffer and clear the return-to when none remain."
  (setf *debugger-sessions* (remove session *debugger-sessions*))
  (let ((next (first *debugger-sessions*)))
    (cond (next (%attend-session next))
          (t (setf *attended-session* nil
                   *eval-target* *eval-target-saved*)   ; back to the pre-fault target
             (%dismiss-debugger)
             (setf *debugger-return-to* nil)))))

(defun %eval-error (ev)
  (%push-session
   (make-dbg-session
    :id (incf *debugger-session-counter*) :kind :local :ev ev
    :header (format nil "Evaluation error: ~a" (pine.eval:evaluation-condition-type ev))
    :condition (pine.eval:evaluation-condition ev)
    :restarts (pine.eval:evaluation-restarts ev)
    :backtrace (pine.eval:evaluation-backtrace ev)))
  (%eval-notify (format nil "eval error: ~a  (0-9/Return picks a restart, q quits)"
                        (pine.eval:evaluation-condition-type ev))))

(defun %agent-debug-surface (msg)
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
      (%eval-notify (format nil "agent ~a error (0-9/Return picks a restart)" agent)))))

(defun %session-resume (session name)
  "Send NAME to where SESSION's restart is live: pick-restart on the blocked
local eval, or :resume to the agent that shipped its restarts home."
  (ecase (dbg-session-kind session)
    (:local
     (when (and (dbg-session-ev session)
                (eq (pine.eval:evaluation-status (dbg-session-ev session)) :error))
       (pine.eval:pick-restart (dbg-session-ev session) name)))
    (:agent
     (let ((info (pine.actor:find-agent
                  (pine.client:server-of (pine.client:current-client))
                  (dbg-session-agent session))))
       (when info
         (sento.actor:tell (pine.actor:agent-info-actor info)
                           (list :resume :eval-id (dbg-session-eval-id session)
                                 :restart name)))))))

(defun invoke-pending-restart (name)
  "Invoke restart NAME on the attended session, then resolve it (advance to the
next live session, or dismiss the debugger)."
  (let ((session *attended-session*))
    (cond
      ((null session) (pine.echo:message "no evaluation in the debugger") nil)
      (t (%session-resume session name)
         (%resolve-session session)
         (pine.echo:message (format nil "invoked ~a" name))
         t))))

(defun %debugger-quit ()
  "Dismiss the *debugger* view without resolving; parked sessions stay in the
registry (M-x debugger reopens the attended one)."
  (%dismiss-debugger))

(defun %text-layout (text)
  "A read-only layout from TEXT: the first line styled as the heading, the
rest as entries."
  (lambda (state)
    (declare (ignore state))
    (let ((lines (pine.buffer:split-lines text)))
      (apply #'pine.layout:column :align :stretch
             (cons (pine.layout:label (or (first lines) "") :class "help-head")
                   (mapcar (lambda (l) (pine.layout:label l :class "help-entry"))
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
                             (eql (pine.eval:evaluation-id (dbg-session-ev s))
                                  (getf j :id))))))
            *debugger-sessions*)))
    (if s
        (%attend-session s)
        (pine.echo:message (format nil "job ~a: ~a" (getf j :id) (getf j :status))))))

(defun %jobs-builder ()
  "The *jobs* layout: every live evaluation across the daemon and the agents,
one selectable row each; Return attends an errored one's debugger session."
  (lambda (state)
    (declare (ignore state))
    (apply #'pine.layout:column :align :stretch
           (list* (pine.layout:label "Jobs  (Return attends an errored one)"
                                     :class "help-head")
                  (pine.layout:label (format nil "~4@a  ~10a ~9a ~a"
                                             "id" "agent" "status" "form / condition")
                                     :class "dbg-note")
                  (loop for j in (pine.jobs:list-jobs) collect
                    (let ((j j))
                      (pine.layout:choice
                       :class "job-row" :prefix-selected "" :prefix-unselected ""
                       :data (lambda () (%attend-job j))
                       (pine.layout:label
                        (format nil "~4@a  ~10a ~9a ~a"
                                (getf j :id) (getf j :agent) (getf j :status)
                                (or (getf j :form) (getf j :condition) ""))
                        :class "help-entry"))))))))

(defun eval-in-target (str package &key on-done bindings)
  "Evaluate STR in the current *eval-target* image over the one eval path: :local
runs through the local-agent (in-image), a named target through that agent, both
off the caller thread on the shared pine.eval engine. ON-DONE runs on the eval
thread for :local; for a remote agent the result comes home as an :agent-result
(the *jobs* surface) and BINDINGS do not cross the wire. The repl and the editor
eval commands share this, so `set-eval-target' redirects both."
  (if (or (null *eval-target*) (eq *eval-target* :local))
      (if pine.actor:*local-agent*
          (pine.actor:agent-eval nil pine.actor:*local-agent* str
                                 :package package :bindings bindings :on-done on-done)
          (pine.eval:evaluate-string str :package package
                                     :bindings bindings :on-done on-done))
      (pine.actor:agent-eval (pine.client:server-of (pine.client:current-client))
                             *eval-target* str :package package :on-done on-done)))

(defun %eval-form-string (str package)
  ;; errors reach *on-debug* (local) or come home from a process agent via
  ;; agent-debug; the client binding rides along for :local.
  (eval-in-target str package
                  :on-done #'%eval-done
                  :bindings (list (cons 'pine.client:*client*
                                        (pine.client:current-client)))))

(defun eval-defun ()
  "Evaluate the top-level form point is inside (C-M-x), via the buffer's tree."
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (snap (pine.buffer:state->snapshot state))
             (lang (%buffer-ts-lang)))
        (if (null lang)
            (eval-last-sexp)
            (multiple-value-bind (sl sc el ec)
                (pine.ts:defun-bounds-pos (%ts-runtime) lang text
                                          (pine.buffer:point-line snap)
                                          (pine.buffer:point-col snap))
              (if sl
                  (%eval-form-string
                   (subseq text (%lc->offset text sl sc) (%lc->offset text el ec))
                   (%buffer-package state))
                  (eval-last-sexp))))))))

(defun %offset->lc (text offset)
  (let ((line 0) (col 0))
    (dotimes (i (min offset (length text)) (values line col))
      (if (char= (char text i) #\Newline) (setf line (1+ line) col 0) (incf col)))))

(defun %token-at (text offset)
  "The symbol token surrounding OFFSET, bounded by sexp delimiters."
  (let ((s (min offset (length text))) (e (min offset (length text))) (n (length text)))
    (loop while (and (> s 0) (not (%sexp-delim-p (char text (1- s))))) do (decf s))
    (loop while (and (< e n) (not (%sexp-delim-p (char text e)))) do (incf e))
    (when (< s e) (subseq text s e))))

(defun %open-definition (label srcs)
  (let* ((src (first srcs))
         (path (and src (sb-introspect:definition-source-pathname src)))
         (coff (and src (sb-introspect:definition-source-character-offset src))))
    (cond
      ((and path (probe-file path))
       (pine.file:find-file (namestring path))
       (when coff
         (let ((nbuf (cur-buffer)))
           (when nbuf
             (let ((ntext (pine.buffer:state->string
                           (sento.actor:ask-s nbuf '(:get-state) :time-out 5))))
               (multiple-value-bind (l c) (%offset->lc ntext coff)
                 (sento.actor:tell nbuf (list :move-point :line l :col c)))))))
       (pine.echo:message (format nil "~a" (file-namestring path))))
      (t (pine.echo:message (format nil "no source for ~a" label))))))

(defun find-definition ()
  "Jump to the source of the symbol at point (M-.), via sb-introspect."
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (snap (pine.buffer:state->snapshot state))
             (off (min (%point->offset snap) (length text)))
             (tok (%token-at text off))
             (pkg (%buffer-package state)))
        (if (null tok)
            (pine.echo:message "no symbol at point")
            (let* ((sym (let ((*package* pkg)) (ignore-errors (read-from-string tok))))
                   (srcs (and (symbolp sym)
                              (loop for kind in '(:function :macro :generic-function
                                                  :variable :class)
                                    thereis (ignore-errors
                                             (sb-introspect:find-definition-sources-by-name
                                              sym kind))))))
              (%open-definition tok srcs)))))))

(defun %token-start (text offset)
  (let ((s (min offset (length text))))
    (loop while (and (> s 0) (not (%sexp-delim-p (char text (1- s))))) do (decf s))
    s))

(defun %symbol-candidates (prefix pkg)
  "Downcased names of symbols accessible in PKG that start with PREFIX."
  (let ((up (string-upcase prefix)) (out nil))
    (do-symbols (s pkg)
      (let ((name (symbol-name s)))
        (when (and (>= (length name) (length up))
                   (string= up name :end2 (length up)))
          (pushnew (string-downcase name) out :test #'string=))))
    (sort out #'string<)))

(defun complete-symbol ()
  "Complete the symbol at point against the buffer's package; Tab when there is
no symbol to complete."
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (snap (pine.buffer:state->snapshot state))
             (off (min (%point->offset snap) (length text)))
             (start (%token-start text off))
             (prefix (subseq text start off))
             (pkg (%buffer-package state)))
        (if (zerop (length prefix))
            (pine.command:call-command "insert-tab")
            (let ((cands (%symbol-candidates prefix pkg)))
              (cond
                ((null cands) (pine.echo:message "no completions"))
                ((null (rest cands)) (%replace-prefix buf prefix (first cands)))
                (t (completing-read "Complete: " cands
                     (lambda (choice) (%replace-prefix buf prefix choice)))))))))))

(defun %replace-prefix (buf prefix choice)
  (dotimes (i (length prefix)) (sento.actor:tell buf '(:backspace)))
  (pine.buffer:tell buf :insert :text choice))

(defun symbol-arglist ()
  "Echo the lambda list of the function named at point (M-x arglist)."
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (snap (pine.buffer:state->snapshot state))
             (off (min (%point->offset snap) (length text)))
             (tok (%token-at text off))
             (pkg (%buffer-package state)))
        (when tok
          (let ((sym (let ((*package* pkg)) (ignore-errors (read-from-string tok)))))
            (if (and (symbolp sym) (fboundp sym))
                (pine.echo:message
                 (format nil "~a ~(~a~)" tok (sb-introspect:function-lambda-list sym)))
                (pine.echo:message (format nil "~a: not a function" tok)))))))))

(defun load-file ()
  "Compile and load the current buffer's file into the eval target's image."
  (let* ((buf (cur-buffer))
         (state (and buf (sento.actor:ask-s buf '(:get-state) :time-out 5)))
         (path (and state (pine.buffer:buffer-local state :pathname nil))))
    (if path
        (%eval-form-string (format nil "(load (compile-file ~s))" (namestring path))
                           (find-package :cl-user))
        (pine.echo:message "buffer has no file"))))

(defun eval-last-sexp ()
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (snap (pine.buffer:state->snapshot state))
             (offset (min (%point->offset snap) (length text))))
        (multiple-value-bind (start end) (%preceding-sexp-bounds text offset)
          (if start
              (%eval-form-string (subseq text start end) (%buffer-package state))
              (pine.echo:message "no form before point")))))))

(defun eval-buffer ()
  ;; one evaluation on the eval thread reads and runs every form in order, so
  ;; *package* changes (in-package) carry across forms, a looping form can't
  ;; hang the editor, and a reader/eval error reaches the shared debugger
  ;; surface like every other eval path.
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (package (%buffer-package state))
             (cli (pine.client:current-client))
             (thunk (lambda ()
                      (let ((pine.client:*client* cli) (*package* package)
                            (pos 0) (count 0))
                        (loop
                          (multiple-value-bind (form new-pos)
                              (read-from-string text nil :eof :start pos)
                            (when (eq form :eof) (return count))
                            (eval form) (incf count) (setf pos new-pos))))))
             (done (lambda (ev)
                     (%eval-notify
                      (case (pine.eval:evaluation-status ev)
                        (:ok (format nil "eval-buffer: ~a forms"
                                     (first (pine.eval:evaluation-values ev))))
                        (:aborted "eval-buffer aborted")
                        (t "eval-buffer: error"))))))
        ;; one eval path: route the whole-buffer eval through the local agent.
        (if pine.actor:*local-agent*
            (pine.actor:agent-run nil pine.actor:*local-agent* thunk
                                  :package package :on-done done)
            (pine.eval:evaluate-thunk thunk :package package :on-done done))))))

(defun scroll-window (delta)
  (let* ((client (pine.client:current-client))
         (w (pine.client:focused-window client))
         (buf (pine.client:current-buffer client)))
    (when (and w buf)
      (let ((snap (pine.buffer:snap w)))
        (when (and snap (typep snap 'pine.buffer:snapshot))
          (let* ((max-scroll (max 0 (- (pine.buffer:line-count snap)
                                       (pine.buffer:win-height w))))
                 (new-scroll (max 0 (min max-scroll
                                         (+ (pine.buffer:scroll-top w) delta))))
                 (h (pine.buffer:win-height w))
                 (pl (pine.buffer:point-line snap))
                 (pc (pine.buffer:point-col snap)))
            (setf (pine.buffer:scroll-top w) new-scroll)
            (cond
              ((< pl new-scroll)
               (sento.actor:tell buf (list :move-point :line new-scroll
                 :col (min pc (length (fset:@ (pine.buffer:lines snap) new-scroll))))))
              ((>= pl (+ new-scroll h))
               (let ((target (+ new-scroll h -1)))
                 (sento.actor:tell buf (list :move-point :line target
                   :col (min pc (length (fset:@ (pine.buffer:lines snap) target))))))))
            (sento.actor:tell (pine.client:renderer client) '(:force-render))))))))

;;;; Help / self-documentation. Help buffers are read-only layout buffers
;;;; (%text-layout via show-layout); describe-key echoes.

(defun %describe-key-text (key)
  (let ((entry (pine.command:key-binding (pine.client:current-client) key))
        (s (pine.key:key->string key)))
    (cond ((pine.keymap:prefix-p entry) (format nil "~a is a prefix key" s))
          ((stringp entry) (format nil "~a runs the command ~a" s entry))
          ((pine.command:self-insert-key-p key)
           (format nil "~a runs self-insert-command" s))
          (t (format nil "~a is undefined" s)))))

(defun %bindings-text ()
  (let* ((client (pine.client:current-client))
         (rows (loop for km in (pine.mode:active-keymaps client)
                     append (pine.keymap:keymap-bindings km t))))
    (with-output-to-string (out)
      (format out "Active bindings~%~%")
      (loop for (keys . cmd) in (sort (remove-duplicates rows :test #'equal
                                                         :key #'car :from-end t)
                                      #'string< :key #'car)
            do (format out "~16a  ~a~%" keys cmd)))))

(defun install-variables ()
  (pine.var:defonce :tab-width :default 8
    :documentation "Tab stop width for the plain-text indent fallback.")
  (pine.var:defonce :format-on-save :default nil
    :documentation "When non-nil, save-file reindents the whole buffer first.")
  (pine.var:defonce :debug-on-error :default nil
    :documentation "When non-nil, an error in a command opens the *debugger*
restart menu instead of only echoing the message. Same knob as Emacs's
debug-on-error; edit-actor and eval errors always reach the debugger."))

(defun %variables-text ()
  (with-output-to-string (out)
    (format out "Editor variables~%~%")
    (dolist (name (pine.var:all-variable-names))
      (let ((v (pine.var:find-variable name)))
        (format out "~a = ~s [~(~a~)]~%    default ~s~a~%"
                name (pine.var:var name) (pine.var:variable-scope name)
                (pine.var:evar-default v)
                (let ((d (pine.var:evar-documentation v)))
                  (if (plusp (length d)) (format nil "~%    ~a" d) "")))))))

(defun %mode-text ()
  (let* ((client (pine.client:current-client))
         (major (pine.mode:current-buffer-mode))
         (minors (pine.mode:buffer-minor-modes client)))
    (with-output-to-string (out)
      (format out "Major mode: ~a (~a)~%"
              (pine.mode:mode-name major) (pine.mode:mode-indicator major))
      (loop for m = major then (pine.mode:parent-mode m) while m
            do (format out "  ~a~%" (pine.mode:mode-name m)))
      (let ((lang (and (typep major 'pine.mode:major-mode)
                       (pine.mode:ts-language major))))
        (when lang (format out "  tree-sitter language: ~a~%" lang)))
      (format out "~%Minor modes:~%")
      (if minors
          (dolist (m minors)
            (format out "  ~a (~a) precedence ~a~a~%"
                    (pine.mode:mode-name m) (pine.mode:mode-indicator m)
                    (pine.mode:precedence m)
                    (if (pine.mode:transparent m) " transparent" "")))
          (format out "  none~%")))))

;;;; Interaction on layout buffers. A layout buffer's snapshot carries the
;;;; arranged node tree (:layout-tree) and the selection index
;;;; (:layout-selection), so navigation is data + re-render -- the published
;;;; tree is never mutated -- and activation resolves point (or the selection)
;;;; to a node and runs its thunk: an action's callback, or a selectable whose
;;;; :data is a function.

(defun %layout-buffer ()
  (pine.client:current-buffer (pine.client:current-client)))

(defun %layout-snap (&optional (buf (%layout-buffer)))
  (and buf (sento.actor:ask-s buf '(:get-snapshot) :time-out 5)))

(defun layout-tree (&optional (snap (%layout-snap)))
  (and snap (pine.buffer:buffer-local snap :layout-tree)))

(defun layout-node-at-point ()
  "The node under point on the current buffer's layout tree, or nil."
  (let* ((snap (%layout-snap))
         (tree (layout-tree snap)))
    (when tree
      (pine.layout:node-at tree (pine.buffer:point-line snap)
                           (pine.buffer:point-col snap)))))

(defun layout-select (delta)
  "Move the layout selection by DELTA (wrapping), reproject, and land point on
the selected row. The reproject and the snapshot read serialize in the buffer's
mailbox, so the tree we read is the reprojected one."
  (let* ((buf (%layout-buffer))
         (snap (%layout-snap buf))
         (tree (layout-tree snap)))
    (when tree
      (let* ((n (length (pine.layout:collect-selectables tree)))
             (cur (pine.buffer:buffer-local snap :layout-selection 0))
             (new (if (plusp n) (mod (+ cur delta) n) 0)))
        (sento.actor:tell buf (list :reproject :selection new))
        (let* ((snap2 (%layout-snap buf))
               (tree2 (layout-tree snap2))
               (sel (and tree2 (nth new (pine.layout:collect-selectables tree2)))))
          (when sel
            (sento.actor:tell buf (list :move-point
                                        :line (pine.layout:start-line sel)
                                        :col 0))))))))

(defun %node-activation (node)
  "The thunk NODE activates to: an action's callback, a selectable whose data is
a function, or such a node anywhere below."
  (typecase node
    (null nil)
    (pine.layout:action (pine.layout:callback node))
    (pine.layout:selectable
     (let ((d (pine.layout:data node)))
       (if (functionp d) d (some #'%node-activation (pine.layout:nodes-of node)))))
    (t (some #'%node-activation (pine.layout:nodes-of node)))))

(defun layout-activate ()
  "Run the activation under point, else the selected row's."
  (let* ((snap (%layout-snap))
         (tree (layout-tree snap)))
    (when tree
      (let* ((at (pine.layout:node-at tree (pine.buffer:point-line snap)
                                      (pine.buffer:point-col snap)))
             (sel (nth (pine.buffer:buffer-local snap :layout-selection 0)
                       (pine.layout:collect-selectables tree)))
             (thunk (or (%node-activation at) (%node-activation sel))))
        (if thunk
            (funcall thunk)
            (pine.echo:message "nothing to activate here"))))))

;;;; Window commands over the live editor tree. The arrangement is layout
;;;; nodes; a split wraps the focused window leaf in a column/row with a new
;;;; window on the same buffer, delete prunes, other-window cycles the leaves.

(defun %editor-leaves (&optional (client (pine.client:current-client)))
  "The window leaves of CLIENT's live tree, in tree order."
  (let ((tree (pine.client:arrangement client)) (acc nil))
    (when tree
      (labels ((walk (n)
                 (when (and (typep n 'pine.layout:window-node)
                            (eq (pine.layout:window-kind n) :window)
                            (pine.layout:window-of n))
                   (push n acc))
                 (dolist (c (pine.layout:nodes-of n)) (walk c))))
        (walk tree)))
    (nreverse acc)))

(defun %focused-leaf (&optional (client (pine.client:current-client)))
  (find (pine.client:focused-window client) (%editor-leaves client)
        :key #'pine.layout:window-of))

(defun %node-parent (root node)
  "NODE's parent under ROOT, walking nodes-of, or nil for the root itself."
  (labels ((walk (n)
             (let ((kids (pine.layout:nodes-of n)))
               (if (member node kids :test #'eq)
                   n
                   (some #'walk kids)))))
    (unless (eq root node) (walk root))))

(defun %replace-child (parent old new)
  "Swap OLD for NEW among PARENT's stacked children."
  (when (typep parent '(or pine.layout:vstack pine.layout:hstack))
    (setf (pine.layout:nodes parent)
          (substitute new old (pine.layout:nodes parent) :test #'eq))
    t))

(defun %focus-leaf (leaf)
  "Focus LEAF's backing window and follow with the current buffer, so typing
lands in it."
  (let ((client (pine.client:current-client))
        (w (pine.layout:window-of leaf)))
    (pine.buffer:focus-window w)
    (setf (pine.client:current-buffer client) (pine.buffer:buffer-ref w))))

(defun %split-window (orient)
  "Split the focused window along ORIENT (:column below, :row beside): a new
window on the same buffer joins the parent as a flat sibling when the parent
already stacks that way, else the leaf wraps in a fresh stack. A divider sits
between; sizes stay even because siblings share one weight."
  (let* ((client (pine.client:current-client))
         (tree (pine.client:arrangement client))
         (leaf (%focused-leaf client)))
    (unless leaf
      (pine.echo:message "no window to split")
      (return-from %split-window))
    (let* ((w (pine.layout:window-of leaf))
           (buf (pine.buffer:buffer-ref w))
           (weight (max 1 (pine.layout:expand-of leaf)))
           (nw (pine.buffer:make-window buf (pine.buffer:window-name w)))
           (nn (pine.layout:window nil :of nw :kind :window :expand weight
                                   :font-px (pine.layout:font-px leaf)
                                   :opacity (pine.layout:window-opacity leaf)))
           (div (pine.layout:rule :vertical (eq orient :row)
                                  :face :border-inactive))
           (p (%node-parent tree leaf))
           (same (and p (typep p (ecase orient
                                   (:column 'pine.layout:vstack)
                                   (:row 'pine.layout:hstack))))))
      (setf (pine.buffer:snap nw) (pine.buffer:snap w)
            (pine.buffer:scroll-top nw) (pine.buffer:scroll-top w))
      (cond
        (same
         (setf (pine.layout:nodes p)
               (loop for k in (pine.layout:nodes p)
                     append (if (eq k leaf) (list k div nn) (list k)))))
        (t
         (setf (pine.layout:expand-of leaf) weight)
         (let ((container (ecase orient
                            (:column (pine.layout:column :align :stretch
                                       :expand weight leaf div nn))
                            (:row (pine.layout:row :align :stretch :spacing 0
                                       :expand weight leaf div nn)))))
           (cond
             ((eq tree leaf) (setf (pine.client:arrangement client) container))
             ((and p (%replace-child p leaf container)))
             (t (pine.echo:message "cannot split here")
                (return-from %split-window))))))
      (%focus-leaf leaf)
      (pine.render:relayout))))

(defun %remove-with-divider (p leaf)
  "Remove LEAF from P's children along with its adjacent divider (the one
before it, else the one after), so a split's separator leaves with it."
  (let* ((kids (pine.layout:nodes p))
         (i (position leaf kids :test #'eq))
         (prev (and i (plusp i) (nth (1- i) kids)))
         (next (and i (nth (1+ i) kids)))
         (div (cond ((typep prev 'pine.layout:separator) prev)
                    ((typep next 'pine.layout:separator) next))))
    (setf (pine.layout:nodes p)
          (remove-if (lambda (k) (or (eq k leaf) (eq k div))) kids))))

(defun %delete-leaf (leaf)
  "Remove LEAF and its divider from the live tree, splicing out a container
left with one child, and dropping its backing window."
  (let* ((client (pine.client:current-client))
         (tree (pine.client:arrangement client))
         (p (%node-parent tree leaf)))
    (when (and p (typep p '(or pine.layout:vstack pine.layout:hstack)))
      (%remove-with-divider p leaf)
      (let ((kids (pine.layout:nodes p)))
        (when (and (null (rest kids)) (first kids))
          (let ((child (first kids))
                (gp (%node-parent tree p)))
            (setf (pine.layout:expand-of child) (pine.layout:expand-of p))
            (if gp
                (%replace-child gp p child)
                (setf (pine.client:arrangement client) child)))))
      (pine.buffer:remove-window (pine.layout:window-of leaf))
      t)))

(defun delete-window-cmd ()
  (let* ((client (pine.client:current-client))
         (leaves (%editor-leaves client))
         (leaf (%focused-leaf client)))
    (cond
      ((null (rest leaves)) (pine.echo:message "only one window"))
      ((and leaf (%delete-leaf leaf))
       (let ((next (first (%editor-leaves client))))
         (when next (%focus-leaf next)))
       (pine.render:relayout))
      (t (pine.echo:message "cannot delete this window")))))

(defun delete-other-windows-cmd ()
  (let* ((client (pine.client:current-client))
         (leaf (%focused-leaf client)))
    (when leaf
      (dolist (other (remove leaf (%editor-leaves client) :test #'eq))
        (%delete-leaf other))
      (%focus-leaf leaf)
      (pine.render:relayout))))

(defun other-window-cmd ()
  (let* ((client (pine.client:current-client))
         (leaves (%editor-leaves client))
         (pos (position (%focused-leaf client) leaves :test #'eq)))
    (when (and leaves (rest leaves))
      (%focus-leaf (nth (mod (1+ (or pos 0)) (length leaves)) leaves)))))

(defun show-layout (name builder &key (mode :base-mode) (selection 0))
  "Open buffer NAME as a layout buffer showing BUILDER (state -> node tree),
switch to it, and enable layout-mode on it. Returns the buffer."
  (let* ((client (pine.client:current-client))
         (cols (pine.buffer:frame-cols (pine.client:frame client)))
         (buf (pine.buffer:make-buffer name)))
    (pine.mode:set-buffer-mode buf mode)
    (pine.buffer:tell buf :set-layout :builder builder :width cols
                          :selection selection)
    (pine.render:subscribe-to-buffer buf)
    (pine.buffer:switch-buffer name)
    (let ((r (ignore-errors (pine.client:renderer client))))
      (when r (sento.actor:tell r (list :switch-buffer :buffer buf :name name))))
    (ignore-errors (pine.mode:enable-minor-mode client :layout-mode))
    buf))


;;;; Commands

(defmacro defcmd (name (&rest args) &body body)
  `(pine.command:define-command ,name ,args ,@body))

(defun install-commands ()
  (defcmd "keyboard-quit" ()
    (setf (pine.client:pending-keys (pine.client:current-client)) nil)
    ;; C-g while attending a local eval interrupts it into its abort restart
    ;; (kills a runaway loop) and resolves the session.
    (let ((s *attended-session*))
      (when (and s (eq (dbg-session-kind s) :local) (dbg-session-ev s))
        (pine.eval:abort-evaluation (dbg-session-ev s))
        (%resolve-session s)))
    (let ((buf (cur-buffer)))
      (when buf
        (sento.actor:tell buf (list :set-meta :key :mark-line :value nil))
        (sento.actor:tell buf (list :set-meta :key :mark-col :value nil))))
    (cancel-prompt))
  (defcmd "backspace" ()
    (let ((buf (cur-buffer))) (when buf (sento.actor:tell buf '(:backspace)))))
  (defcmd "delete-char" ()
    (let ((buf (cur-buffer)) (snap (focused-snap)))
      (when (and buf snap)
        (sento.actor:tell buf
          (list :delete-region
                :start-line (pine.buffer:point-line snap) :start-col (pine.buffer:point-col snap)
                :end-line (pine.buffer:point-line snap) :end-col (1+ (pine.buffer:point-col snap)))))))
  (defcmd "newline" ()
    (let ((buf (cur-buffer))) (when buf (pine.buffer:tell buf :newline))))
  (defcmd "undo" ()
    (let ((buf (cur-buffer))) (when buf (sento.actor:tell buf '(:undo)))))
  (defcmd "redo" ()
    (let ((buf (cur-buffer))) (when buf (sento.actor:tell buf '(:redo)))))

  (defcmd "forward-char" (n)  (:interactive :number) (move-chars n))
  (defcmd "backward-char" (n) (:interactive :number) (move-chars (- n)))
  (defcmd "next-line" (n)     (:interactive :number) (move-lines n))
  (defcmd "previous-line" (n) (:interactive :number) (move-lines (- n)))
  (defcmd "forward-word" (n)  (:interactive :number) (move-words n))
  (defcmd "backward-word" (n) (:interactive :number) (move-words (- n)))
  (defcmd "kill-word" (n)          (:interactive :number) (kill-words-cmd n))
  (defcmd "backward-kill-word" (n) (:interactive :number) (kill-words-cmd (- n)))
  (defcmd "isearch-forward" ()  (isearch-start :forward))
  (defcmd "isearch-backward" () (isearch-start :backward))
  (defcmd "universal-argument" () (:prefix)
    (let ((cli (pine.client:current-client)))
      (setf (pine.client:prefix-arg cli)
            (list (* 4 (pine.command:prefix-numeric-value
                        (pine.client:prefix-arg cli)))))))
  (defcmd "digit-argument" () (:prefix)
    (let* ((cli (pine.client:current-client))
           (key (pine.client:this-command-key cli))
           (d (and key (digit-char-p (char (pine.key:key-sym key) 0))))
           (cur (pine.client:prefix-arg cli)))
      (when d
        (setf (pine.client:prefix-arg cli)
              (cond ((eq cur '-) (- d))
                    ((and (integerp cur) (minusp cur)) (- (+ (* 10 (- cur)) d)))
                    ((integerp cur) (+ (* 10 cur) d))
                    (t d))))))
  (defcmd "negative-argument" () (:prefix)
    (let* ((cli (pine.client:current-client))
           (cur (pine.client:prefix-arg cli)))
      (setf (pine.client:prefix-arg cli)
            (cond ((eq cur '-) nil)
                  ((integerp cur) (- cur))
                  (t '-)))))

  (defcmd "beginning-of-line" ()
    (let ((buf (cur-buffer)) (snap (focused-snap)))
      (when (and buf snap)
        (sento.actor:tell buf (list :move-point :line (pine.buffer:point-line snap) :col 0)))))
  (defcmd "end-of-line" ()
    (let ((buf (cur-buffer)) (snap (focused-snap)))
      (when (and buf snap)
        (let ((len (length (fset:@ (pine.buffer:lines snap) (pine.buffer:point-line snap)))))
          (sento.actor:tell buf (list :move-point :line (pine.buffer:point-line snap) :col len))))))
  (defcmd "beginning-of-buffer" ()
    (let ((buf (cur-buffer))) (when buf (sento.actor:tell buf (list :move-point :line 0 :col 0)))))
  (defcmd "end-of-buffer" ()
    (let ((buf (cur-buffer)) (snap (focused-snap)))
      (when (and buf snap)
        (let* ((ll (1- (pine.buffer:line-count snap)))
               (lc (length (fset:@ (pine.buffer:lines snap) ll))))
          (sento.actor:tell buf (list :move-point :line ll :col lc))))))

  (defcmd "scroll-down" ()
    (let ((w (pine.client:focused-window (pine.client:current-client))))
      (when w (scroll-window (- (pine.buffer:win-height w) 2)))))
  (defcmd "scroll-up" ()
    (let ((w (pine.client:focused-window (pine.client:current-client))))
      (when w (scroll-window (- 2 (pine.buffer:win-height w))))))

  (defcmd "forward-sexp" ()      (%sexp-move :forward-sexp))
  (defcmd "backward-sexp" ()     (%sexp-move :backward-sexp))
  (defcmd "beginning-of-defun" () (%sexp-move :beginning-of-defun))
  (defcmd "end-of-defun" ()      (%sexp-move :end-of-defun))
  (defcmd "mark-sexp" ()
    (set-mark)
    (%sexp-move :forward-sexp))

  (defcmd "set-mark" ()     (set-mark))
  (defcmd "kill-line" ()    (kill-line-cmd))
  (defcmd "kill-region" ()  (kill-region-cmd))
  (defcmd "copy-region" ()  (copy-region-cmd))
  (defcmd "yank" ()         (yank-cmd))
  (defcmd "yank-pop" ()     (yank-pop-cmd))

  (defcmd "find-file" ()
    (read-file-name "Find file: "
      (lambda (path)
        (handler-case (pine.file:find-file path)
          (error (c) (pine.echo:message (format nil "error: ~a" c)))))))
  (defcmd "save-file" ()
    (handler-case
        (let ((buf (cur-buffer)))
          ;; opt-in apheleia-style reformat before write; the reindent is a tell
          ;; and save's :get-state queues behind it in the actor mailbox, so the
          ;; write sees the formatted text
          (when (and buf (pine.var:var :format-on-save buf))
            (let ((snap (sento.actor:ask-s buf '(:get-snapshot) :time-out 5)))
              (pine.buffer:tell buf :indent-lines
                                :from 0 :to (1- (pine.buffer:line-count snap)))))
          (pine.file:save-current-buffer))
      (error (c) (pine.echo:message (format nil "error: ~a" c)))))
  (defcmd "split-window-below" () (%split-window :column))
  (defcmd "split-window-right" () (%split-window :row))
  (defcmd "delete-window" () (delete-window-cmd))
  (defcmd "delete-other-windows" () (delete-other-windows-cmd))
  (defcmd "other-window" () (other-window-cmd))
  (defcmd "switch-buffer" ()
    (completing-read "Switch to: " (pine.buffer:list-buffers)
      (lambda (name)
        (let* ((client (pine.client:current-client))
               (buf (pine.buffer:switch-buffer name)))
          (when buf
            (sento.actor:tell (pine.client:renderer client)
                              (list :switch-buffer :buffer buf :name name))
            (pine.render:subscribe-to-buffer buf))))))
  (defcmd "list-buffers" ()
    (pine.echo:message (format nil "buffers: ~{~a~^, ~}" (pine.buffer:list-buffers))))
  (defcmd "execute-command" ()
    (completing-read "M-x " (pine.command:all-command-names)
      (lambda (name) (pine.command:call-command name))))
  (defcmd "eval-expression" ()
    (prompt "Eval: "
      (lambda (text)
        (let ((pkg (let ((buf (cur-buffer)))
                     (if buf
                         (%buffer-package (sento.actor:ask-s buf '(:get-state) :time-out 5))
                         (find-package :cl-user)))))
          (%eval-form-string text pkg)))))
  (defcmd "choose-restart" ()
    (let ((names (and *attended-session*
                      (remove nil (mapcar #'first
                                          (dbg-session-restarts *attended-session*))))))
      (if names
          (completing-read "Restart: " names
            (lambda (name) (invoke-pending-restart name)))
          (pine.echo:message "no evaluation in the debugger"))))
  (defcmd "debugger-abort" ()
    (invoke-pending-restart "ABORT"))
  (defcmd "debugger-quit" ()
    (%debugger-quit))
  (defcmd "debugger-next-session" ()
    "Page to the next live debugger session without resolving the current one."
    (let ((ordered (reverse *debugger-sessions*)))
      (if (> (length ordered) 1)
          (let* ((pos (or (position *attended-session* ordered) 0))
                 (next (nth (mod (1+ pos) (length ordered)) ordered)))
            (%attend-session next))
          (pine.echo:message "only one debugger session"))))
  (defcmd "debugger" ()
    "Reopen the *debugger* on the attended session (after q), if one is parked."
    (if *attended-session*
        (%attend-session *attended-session*)
        (pine.echo:message "no debugger session")))
  (defcmd "toggle-debug-on-error" ()
    (let ((new (not (pine.var:var :debug-on-error))))
      (setf (pine.var:var :debug-on-error) new)
      (pine.echo:message (format nil "debug-on-error ~:[disabled~;enabled~]" new))))
  (defcmd "jobs" ()
    (show-layout "*jobs*" (%jobs-builder)))
  (defcmd "eval-last-sexp" () (eval-last-sexp))
  (defcmd "eval-defun" ()     (eval-defun))
  (defcmd "eval-buffer" ()    (eval-buffer))
  (defcmd "find-definition" () (find-definition))
  (defcmd "arglist" ()        (symbol-arglist))
  (defcmd "complete-symbol" () (complete-symbol))
  (defcmd "load-file" ()      (load-file))
  (defcmd "set-eval-target" ()
    (completing-read
     "Eval in: "
     (cons "local"
           (mapcar #'pine.actor:agent-info-name
                   (pine.actor:list-agents
                    (pine.client:server-of (pine.client:current-client)))))
     (lambda (name)
       (setf *eval-target* (if (string= name "local") :local name))
       (pine.echo:message (format nil "eval target: ~a" name)))))
  (defcmd "new-buffer" ()
    (prompt "New buffer: "
      (lambda (name)
        (let ((buf (pine.buffer:make-buffer name))) (pine.render:subscribe-to-buffer buf)))))
  (defcmd "open-repl" ()
    (handler-case
        (let* ((client (pine.client:current-client))
               (buf (or (pine.client:repl-buffer client) (pine.repl:start-repl))))
          (pine.buffer:switch-buffer "*repl*")
          (pine.render:subscribe-to-buffer buf)
          (sento.actor:tell (pine.client:renderer client)
                            (list :switch-buffer :buffer buf :name "*repl*")))
      (error (c) (pine.echo:message (format nil "error: ~a" c)))))
  (defcmd "terminal" ()
    (handler-case
        (let* ((client (pine.client:current-client))
               (f (pine.client:frame client))
               (cols (pine.buffer:frame-cols f))
               (rows (max 1 (- (pine.buffer:frame-rows f) 2)))
               (buf (pine.buffer:make-buffer "*terminal*")))
          (pine.term:open-terminal client buf :rows rows :cols cols)
          (pine.mode:set-buffer-mode buf :terminal-mode)
          (pine.buffer:switch-buffer "*terminal*")
          (sento.actor:tell (pine.client:renderer client)
                            (list :switch-buffer :buffer buf :name "*terminal*")))
      (error (c) (pine.echo:message (format nil "error: ~a" c)))))
  (defcmd "overwrite-mode" ()
    (let ((on (pine.mode:toggle-minor-mode (pine.client:current-client) :overwrite-mode)))
      (pine.echo:message (if on "Overwrite mode enabled" "Overwrite mode disabled"))))
  (defcmd "describe-key" ()
    (pine.echo:message "Describe key: ")
    (pine.command:read-next-key
     (pine.client:current-client)
     (lambda (key) (pine.echo:message (%describe-key-text key)))))
  (defcmd "describe-bindings" ()
    (show-layout "*bindings*" (%text-layout (%bindings-text))))
  (defcmd "describe-mode" ()
    (show-layout "*mode*" (%text-layout (%mode-text))))
  (defcmd "describe-variables" ()
    (show-layout "*variables*" (%text-layout (%variables-text))))
  (defcmd "insert-tab" ()
    (let* ((cli (pine.client:current-client))
           (buf (pine.client:current-buffer cli))
           (n (max 0 (pine.var:var :tab-width buf))))
      (when buf
        (pine.buffer:tell buf :insert :text (make-string n :initial-element #\Space)))))
  (defcmd "indent-for-tab-command" ()
    "Reindent the current line to the column its mode dictates."
    (let ((buf (cur-buffer)))
      (when buf (pine.buffer:tell buf :indent-lines))))
  (defcmd "indent-region" ()
    "Reindent every line spanned by the region."
    (let* ((buf (cur-buffer))
           (state (and buf (sento.actor:ask-s buf '(:get-state) :time-out 5))))
      (when state
        (multiple-value-bind (sl sc el ec) (region-bounds state)
          (declare (ignore sc ec))
          (if sl
              (pine.buffer:tell buf :indent-lines :from sl :to el)
              (pine.echo:message "no region"))))))
  (defcmd "format-buffer" ()
    "Reindent the whole buffer off the parse tree, point preserved (in-image)."
    (let* ((buf (cur-buffer))
           (snap (and buf (sento.actor:ask-s buf '(:get-snapshot) :time-out 5))))
      (when snap
        (pine.buffer:tell buf :indent-lines
                          :from 0 :to (1- (pine.buffer:line-count snap))))))
  ;; layout buffers: selection nav + activation on the node tree
  (defcmd "layout-next" () (layout-select 1))
  (defcmd "layout-prev" () (layout-select -1))
  (defcmd "layout-activate" () (layout-activate))
  ;; minibuffer-mode: the only keys the prompt binds; everything else is the
  ;; ordinary buffer editing commands, so the prompt edits like any buffer.
  (defcmd "minibuffer-accept" () (minibuffer-accept))
  (defcmd "minibuffer-abort" () (minibuffer-abort))
  (defcmd "minibuffer-complete" () (minibuffer-complete))
  (defcmd "minibuffer-next-candidate" () (completion-next))
  (defcmd "minibuffer-prev-candidate" () (completion-prev)))

;;;; Bindings

(defun k (spec) (pine.key:parse-key spec))

(defun install-bindings ()
  (let ((g (pine.mode:global-keymap))
        (tm (pine.mode:mode-keymap (pine.mode:find-mode :text-mode)))
        (dm (pine.mode:mode-keymap (pine.mode:find-mode :debugger-mode))))
    ;; debugger-mode: restart rows answer to the surface interaction
    ;; (Return/C-n/C-p from layout-mode); these are the extras
    (pine.keymap:define-key dm (k "a") "debugger-abort")
    (pine.keymap:define-key dm (k "q") "debugger-quit")
    (pine.keymap:define-key dm (k "Tab") "debugger-next-session")
    ;; global: quit, chords, M-x
    (pine.keymap:define-key g (k "C-g") "keyboard-quit")
    (pine.keymap:define-key g (k "Escape") "keyboard-quit")
    (pine.keymap:define-key g (list (k "C-x") (k "C-f")) "find-file")
    (pine.keymap:define-key g (list (k "C-x") (k "C-s")) "save-file")
    (pine.keymap:define-key g (list (k "C-x") (k "b")) "switch-buffer")
    (pine.keymap:define-key g (list (k "C-x") (k "2")) "split-window-below")
    (pine.keymap:define-key g (list (k "C-x") (k "3")) "split-window-right")
    (pine.keymap:define-key g (list (k "C-x") (k "0")) "delete-window")
    (pine.keymap:define-key g (list (k "C-x") (k "1")) "delete-other-windows")
    (pine.keymap:define-key g (list (k "C-x") (k "o")) "other-window")
    (pine.keymap:define-key g (list (k "C-x") (k "n")) "new-buffer")
    (pine.keymap:define-key g (list (k "C-x") (k "r")) "open-repl")
    (pine.keymap:define-key g (list (k "C-x") (k "t")) "terminal")
    (pine.keymap:define-key g (list (k "C-x") (k "C-e")) "eval-last-sexp")
    (pine.keymap:define-key g (k "C-M-x") "eval-defun")
    (pine.keymap:define-key g (k "M-.") "find-definition")
    (pine.keymap:define-key g (k "M-x") "execute-command")
    (pine.keymap:define-key g (k "Insert") "overwrite-mode")
    ;; prefix argument
    (pine.keymap:define-key g (k "C-u") "universal-argument")
    (pine.keymap:define-key g (k "M--") "negative-argument")
    (dotimes (d 10)
      (pine.keymap:define-key g (k (format nil "M-~d" d)) "digit-argument"))
    ;; help
    (pine.keymap:define-key g (list (k "C-h") (k "k")) "describe-key")
    (pine.keymap:define-key g (list (k "C-h") (k "b")) "describe-bindings")
    (pine.keymap:define-key g (list (k "C-h") (k "m")) "describe-mode")
    (pine.keymap:define-key g (list (k "C-h") (k "v")) "describe-variables")
    ;; text-mode editing
    (pine.keymap:define-key tm (k "BackSpace") "backspace")
    (pine.keymap:define-key tm (k "Return") "newline")
    (pine.keymap:define-key tm (k "Tab") "indent-for-tab-command")
    (pine.keymap:define-key tm (k "Up") "previous-line")
    (pine.keymap:define-key tm (k "Down") "next-line")
    (pine.keymap:define-key tm (k "Left") "backward-char")
    (pine.keymap:define-key tm (k "Right") "forward-char")
    (pine.keymap:define-key tm (k "C-f") "forward-char")
    (pine.keymap:define-key tm (k "C-b") "backward-char")
    (pine.keymap:define-key tm (k "C-n") "next-line")
    (pine.keymap:define-key tm (k "C-p") "previous-line")
    (pine.keymap:define-key tm (k "C-a") "beginning-of-line")
    (pine.keymap:define-key tm (k "C-e") "end-of-line")
    (pine.keymap:define-key tm (k "C-d") "delete-char")
    (pine.keymap:define-key tm (k "M-f") "forward-word")
    (pine.keymap:define-key tm (k "M-b") "backward-word")
    (pine.keymap:define-key tm (k "M-d") "kill-word")
    (pine.keymap:define-key tm (k "M-BackSpace") "backward-kill-word")
    (pine.keymap:define-key tm (k "C-s") "isearch-forward")
    (pine.keymap:define-key tm (k "C-r") "isearch-backward")
    (pine.keymap:define-key tm (k "M-<") "beginning-of-buffer")
    (pine.keymap:define-key tm (k "M->") "end-of-buffer")
    (pine.keymap:define-key tm (k "C-space") "set-mark")
    (pine.keymap:define-key tm (k "C-k") "kill-line")
    (pine.keymap:define-key tm (k "C-w") "kill-region")
    (pine.keymap:define-key tm (k "M-w") "copy-region")
    (pine.keymap:define-key tm (k "C-y") "yank")
    (pine.keymap:define-key tm (k "M-y") "yank-pop")
    (pine.keymap:define-key tm (k "C-v") "scroll-down")
    (pine.keymap:define-key tm (k "M-v") "scroll-up")
    (pine.keymap:define-key tm (k "Prior") "scroll-up")
    (pine.keymap:define-key tm (k "Next") "scroll-down")
    (pine.keymap:define-key tm (k "C-z") "undo")
    (pine.keymap:define-key tm (k "C-/") "undo")
    (pine.keymap:define-key tm (k "C-?") "redo")
    ;; structural navigation (tree-sitter)
    (pine.keymap:define-key tm (k "C-M-f") "forward-sexp")
    (pine.keymap:define-key tm (k "C-M-b") "backward-sexp")
    (pine.keymap:define-key tm (k "C-M-a") "beginning-of-defun")
    (pine.keymap:define-key tm (k "C-M-e") "end-of-defun")
    (pine.keymap:define-key tm (k "C-M-space") "mark-sexp")
    ;; indentation: Tab reindents (mode-aware), completion moves to the Emacs
    ;; completion binding, region + whole-buffer reformat get their own keys
    (pine.keymap:define-key tm (k "C-M-\\") "indent-region")
    ;; layout-mode: selection nav + activation on a layout buffer
    (let ((sm (pine.mode:mode-keymap (pine.mode:find-mode :layout-mode))))
      (pine.keymap:define-key sm (k "Down") "layout-next")
      (pine.keymap:define-key sm (k "C-n") "layout-next")
      (pine.keymap:define-key sm (k "Up") "layout-prev")
      (pine.keymap:define-key sm (k "C-p") "layout-prev")
      (pine.keymap:define-key sm (k "Return") "layout-activate"))
    ;; minibuffer-mode: accept / abort / complete / candidate motion. All other
    ;; keys fall through to text-mode, so the prompt has full editing.
    (let ((mm (pine.mode:mode-keymap (pine.mode:find-mode :minibuffer-mode))))
      (pine.keymap:define-key mm (k "Return") "minibuffer-accept")
      (pine.keymap:define-key mm (k "Escape") "minibuffer-abort")
      (pine.keymap:define-key mm (k "C-g") "minibuffer-abort")
      (pine.keymap:define-key mm (k "Tab") "minibuffer-complete")
      (pine.keymap:define-key mm (k "Down") "minibuffer-next-candidate")
      (pine.keymap:define-key mm (k "C-n") "minibuffer-next-candidate")
      (pine.keymap:define-key mm (k "Up") "minibuffer-prev-candidate")
      (pine.keymap:define-key mm (k "C-p") "minibuffer-prev-candidate"))
    ;; lisp-mode: the SLIME chord set (mode-keymap chords resolve fine)
    (let ((lm (pine.mode:mode-keymap (pine.mode:find-mode :lisp-mode))))
      (pine.keymap:define-key lm (list (k "C-c") (k "C-c")) "eval-defun")
      (pine.keymap:define-key lm (list (k "C-c") (k "C-k")) "eval-buffer")
      (pine.keymap:define-key lm (list (k "C-c") (k "C-l")) "load-file")
      (pine.keymap:define-key lm (list (k "C-c") (k "C-d")) "arglist")
      (pine.keymap:define-key lm (k "C-M-i") "complete-symbol")
      (pine.keymap:define-key lm (k "M-Tab") "complete-symbol"))))
