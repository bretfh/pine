(in-package #:pine.editor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(defun start-editor ()
  (let* ((client (pine.editor.frame:current-client))
         (server (pine.editor.frame:server-of client)))
    (pine.text.buffer:start-buffer-registry server)
    (handler-case (pine.ts:ensure-ts (pine.core.server:ts-runtime server))
      (error () nil))
    (pine.ui.render:start-renderer client)
    (ensure-minibuffer client)
    (setf pine.editor.command:*terminal-handler* #'pine.term:terminal-dispatch)
    (setf pine.core.eval:*on-debug* #'%eval-error)
    (let ((buf (pine.editor.frame:make-buffer "scratch")))
      (pine.editor.frame:make-window buf "scratch"
                               :row 0 :col 0 :width 80 :height 29 :focused t)
      (pine.ui.render:subscribe-to-buffer buf)
      (pine.editor.frame:set-buffer-mode buf :text-mode)
      (pine.editor.ask:tell buf :set-local :key :package :value :pine-user))
    (pine.ui.render:relayout)))

;;;; Motion / eval helpers (command implementations)

(defun focused-snap ()
  "The snapshot commands read for point/line context. While a prompt is active
this is the minibuffer's snapshot -- the minibuffer is the current buffer, so a
command like beginning-of-line must see its input, not the window behind it."
  (let ((c (pine.editor.frame:current-client)))
    (if (pine.editor.frame:prompt-active c)
        (pine.editor.frame:minibuffer-snap c)
        (let ((w (pine.editor.frame:focused-window c)))
          (when w (pine.text.buffer:snap w))))))

(defun cur-buffer () (pine.editor.frame:current-buffer (pine.editor.frame:current-client)))

(defun %fresh-snap ()
  (let ((buf (cur-buffer))) (when buf (pine.editor.ask:ask buf :snapshot))))

(defun %buffer-ts-lang ()
  (let ((mode (pine.editor.frame:current-buffer-mode)))
    (and (typep mode 'pine.editor.mode:major-mode) (pine.editor.mode:ts-language mode))))

(defun %ts-runtime ()
  (pine.core.server:ts-runtime (pine.editor.frame:server-of (pine.editor.frame:current-client))))

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
  (let ((pl (pine.text.buffer:point-line snap))
        (pc (pine.text.buffer:point-col snap))
        (lines (pine.text.buffer:lines snap)))
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
  (let ((inferred (%infer-package (pine.text.buffer:state->string state)))
        (name (pine.text.buffer:buffer-local state :package nil)))
    (or inferred (and name (find-package name)) (find-package :cl-user))))

(defun %lc->offset (text line col)
  "Character offset of LINE/COL in TEXT."
  (let ((i 0) (l 0) (n (length text)))
    (loop while (and (< i n) (< l line))
          do (when (char= (char text i) #\Newline) (incf l))
             (incf i))
    (min (+ i col) n)))

;;;; Evaluation runs through pine.core.eval on its own thread, never on the UI
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
  ev                 ; the pine.core.eval:evaluation (kind :local), resumed by pick-restart
  agent eval-id      ; agent name + eval-id (kind :agent), resumed by :resume
  header             ; one-line title
  condition          ; condition text
  restarts           ; list of (name report)
  backtrace)         ; text, or nil

(defvar *debugger-sessions* nil "Live debugger sessions, most-recent first.")
(defvar *attended-session* nil "The session the *debugger* buffer currently shows.")
(defvar *debugger-session-counter* 0)

(defun %eval-notify (text)
  "Show TEXT in the echo area and repaint, safely from the eval thread."
  (pine.editor.echo:message text)
  (let ((r (ignore-errors (pine.editor.frame:renderer (pine.editor.frame:current-client)))))
    (when r (sento.actor:tell r '(:force-render)))))

(defun %eval-done (ev &optional at)
  "Surface a finished eval: the result echoes, and lands inline as an overlay
on the form's line when AT is (BUFFER . LINE)."
  (case (pine.core.eval:evaluation-status ev)
    (:ok (let ((txt (format nil "=> ~{~s~^, ~}" (pine.core.eval:evaluation-values ev))))
           (when at
             (ignore-errors
              (sento.actor:tell (car at)
                                (list :overlay :line (cdr at) :text txt))))
           (%eval-notify txt)))
    (:aborted (%eval-notify "aborted"))))

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
    (apply #'pine.ui.node:column :align :stretch
           (append
            (when (> (length ordered) 1)
              (list (pine.ui.node:label
                     (format nil "session ~d/~d  (Tab: next)   ~{~a~^  |  ~}"
                             (1+ (or (position session ordered) 0)) (length ordered)
                             (mapcar #'dbg-session-header ordered))
                     :class "dbg-switch")
                    (pine.ui.node:label "")))
            (list (pine.ui.node:label (dbg-session-header session) :class "dbg-header")
                  (pine.ui.node:label ""))
            (mapcar (lambda (l) (pine.ui.node:label l :class "dbg-cond"))
                    (pine.text.buffer:split-lines (dbg-session-condition session)))
            (list (pine.ui.node:label "")
                  (pine.ui.node:label "Restarts (Return on a line):" :class "dbg-note"))
            (loop for (name report) in (dbg-session-restarts session) collect
              (pine.ui.node:choice
               :class "restart" :prefix-selected "" :prefix-unselected ""
               :data (let ((nm name)) (lambda () (invoke-pending-restart nm)))
               (pine.ui.node:label (format nil "  [~a]  ~a" (or name "") (or report ""))
                                  :class "restart-lbl")))
            (when (dbg-session-backtrace session)
              (list* (pine.ui.node:label "")
                     (pine.ui.node:label "Backtrace:" :class "dbg-note")
                     (mapcar (lambda (l) (pine.ui.node:label l :class "dbg-bt"))
                             (pine.text.buffer:split-lines (dbg-session-backtrace session)))))))))

(defun %attend-session (session)
  "Open SESSION in the *debugger* buffer, make it the attended one, and switch
to it. The eval target follows the attended fault, so C-x C-e / recompile land
in the image that broke -- fix the defun there, then pick retry."
  (setf *attended-session* session
        pine.editor.target:*eval-target* (ecase (dbg-session-kind session)
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
    (setf *debugger-return-to* (ignore-errors (pine.editor.ask:ask :current :name))
          pine.editor.target:*eval-target-saved* pine.editor.target:*eval-target*))
  (push session *debugger-sessions*)
  (%attend-session session))

(defun %dismiss-debugger ()
  "Hide the *debugger* buffer and return to the pre-debugger buffer. Does not
resolve anything -- any sessions still in the registry stay parked."
  (when *debugger-return-to*
    (%switch-to-buffer *debugger-return-to*))
  (ignore-errors (pine.editor.frame:kill-buffer "*debugger*")))

(defun %resolve-session (session)
  "Drop SESSION from the registry (its thread was just resumed); attend the next
live session, or dismiss the buffer and clear the return-to when none remain."
  (setf *debugger-sessions* (remove session *debugger-sessions*))
  (let ((next (first *debugger-sessions*)))
    (cond (next (%attend-session next))
          (t (setf *attended-session* nil
                   pine.editor.target:*eval-target* pine.editor.target:*eval-target-saved*)   ; back to the pre-fault target
             (%dismiss-debugger)
             (setf *debugger-return-to* nil)))))

(defun %eval-error (ev)
  (%push-session
   (make-dbg-session
    :id (incf *debugger-session-counter*) :kind :local :ev ev
    :header (format nil "Evaluation error: ~a" (pine.core.eval:evaluation-condition-type ev))
    :condition (pine.core.eval:evaluation-condition ev)
    :restarts (pine.core.eval:evaluation-restarts ev)
    :backtrace (pine.core.eval:evaluation-backtrace ev)))
  (%eval-notify (format nil "eval error: ~a  (0-9/Return picks a restart, q quits)"
                        (pine.core.eval:evaluation-condition-type ev))))

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
                (eq (pine.core.eval:evaluation-status (dbg-session-ev session)) :error))
       (pine.core.eval:pick-restart (dbg-session-ev session) name)))
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
         (%resolve-session session)
         (pine.editor.echo:message (format nil "invoked ~a" name))
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
    (let ((lines (pine.text.buffer:split-lines text)))
      (apply #'pine.ui.node:column :align :stretch
             (cons (pine.ui.node:label (or (first lines) "") :class "help-head")
                   (mapcar (lambda (l) (pine.ui.node:label l :class "help-entry"))
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
                             (eql (pine.core.eval:evaluation-id (dbg-session-ev s))
                                  (getf j :id))))))
            *debugger-sessions*)))
    (if s
        (%attend-session s)
        (pine.editor.echo:message (format nil "job ~a: ~a" (getf j :id) (getf j :status))))))

(defun %jobs-builder ()
  "The *jobs* layout: every live evaluation across the daemon and the agents,
one selectable row each; Return attends an errored one's debugger session."
  (lambda (state)
    (declare (ignore state))
    (apply #'pine.ui.node:column :align :stretch
           (list* (pine.ui.node:label "Jobs  (Return attends an errored one)"
                                     :class "help-head")
                  (pine.ui.node:label (format nil "~4@a  ~10a ~9a ~a"
                                             "id" "agent" "status" "form / condition")
                                     :class "dbg-note")
                  (loop for j in (pine.core.jobs:list-jobs) collect
                    (let ((j j))
                      (pine.ui.node:choice
                       :class "job-row" :prefix-selected "" :prefix-unselected ""
                       :data (lambda () (%attend-job j))
                       (pine.ui.node:label
                        (format nil "~4@a  ~10a ~9a ~a"
                                (getf j :id) (getf j :agent) (getf j :status)
                                (or (getf j :form) (getf j :condition) ""))
                        :class "help-entry"))))))))

(defun %eval-form-string (str package &key at)
  ;; errors reach *on-debug* (local) or come home from a process agent via
  ;; agent-debug; the client binding rides along for :local. AT = (BUFFER .
  ;; LINE) puts the result inline on the form's line.
  (pine.editor.target:eval-in-target str package
                  :on-done (lambda (ev) (%eval-done ev at))
                  :bindings (list (cons 'pine.editor.frame:*client*
                                        (pine.editor.frame:current-client)))))

(defun eval-defun ()
  "Evaluate the top-level form point is inside (C-M-x), via the buffer's tree."
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.text.buffer:state->string state))
             (snap (pine.text.buffer:state->snapshot state))
             (lang (%buffer-ts-lang)))
        (if (null lang)
            (eval-last-sexp)
            (multiple-value-bind (sl sc el ec)
                (pine.ts:defun-bounds-pos (%ts-runtime) lang text
                                          (pine.text.buffer:point-line snap)
                                          (pine.text.buffer:point-col snap))
              (if sl
                  (%eval-form-string
                   (subseq text (%lc->offset text sl sc) (%lc->offset text el ec))
                   (%buffer-package state)
                   :at (cons buf el))
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
       (pine.editor.file:find-file (namestring path))
       (when coff
         (let ((nbuf (cur-buffer)))
           (when nbuf
             (let ((ntext (pine.text.buffer:state->string
                           (sento.actor:ask-s nbuf '(:get-state) :time-out 5))))
               (multiple-value-bind (l c) (%offset->lc ntext coff)
                 (sento.actor:tell nbuf (list :move-point :line l :col c)))))))
       (pine.editor.echo:message (format nil "~a" (file-namestring path))))
      (t (pine.editor.echo:message (format nil "no source for ~a" label))))))

(defun find-definition ()
  "Jump to the source of the symbol at point (M-.), via sb-introspect."
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.text.buffer:state->string state))
             (snap (pine.text.buffer:state->snapshot state))
             (off (min (%point->offset snap) (length text)))
             (tok (%token-at text off))
             (pkg (%buffer-package state)))
        (if (null tok)
            (pine.editor.echo:message "no symbol at point")
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
             (text (pine.text.buffer:state->string state))
             (snap (pine.text.buffer:state->snapshot state))
             (off (min (%point->offset snap) (length text)))
             (start (%token-start text off))
             (prefix (subseq text start off))
             (pkg (%buffer-package state)))
        (if (zerop (length prefix))
            (pine.editor.command:call-command "insert-tab")
            (let ((cands (%symbol-candidates prefix pkg)))
              (cond
                ((null cands) (pine.editor.echo:message "no completions"))
                ((null (rest cands)) (%replace-prefix buf prefix (first cands)))
                (t (completing-read "Complete: " cands
                     (lambda (choice) (%replace-prefix buf prefix choice)))))))))))

(defun %replace-prefix (buf prefix choice)
  (dotimes (i (length prefix)) (sento.actor:tell buf '(:backspace)))
  (pine.editor.ask:tell buf :insert :text choice))

(defun symbol-arglist ()
  "Echo the lambda list of the function named at point (M-x arglist)."
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.text.buffer:state->string state))
             (snap (pine.text.buffer:state->snapshot state))
             (off (min (%point->offset snap) (length text)))
             (tok (%token-at text off))
             (pkg (%buffer-package state)))
        (when tok
          (let ((sym (let ((*package* pkg)) (ignore-errors (read-from-string tok)))))
            (if (and (symbolp sym) (fboundp sym))
                (pine.editor.echo:message
                 (format nil "~a ~(~a~)" tok (sb-introspect:function-lambda-list sym)))
                (pine.editor.echo:message (format nil "~a: not a function" tok)))))))))

(defun load-file ()
  "Compile and load the current buffer's file into the eval target's image."
  (let* ((buf (cur-buffer))
         (state (and buf (sento.actor:ask-s buf '(:get-state) :time-out 5)))
         (path (and state (pine.text.buffer:buffer-local state :pathname nil))))
    (if path
        (%eval-form-string (format nil "(load (compile-file ~s))" (namestring path))
                           (find-package :cl-user))
        (pine.editor.echo:message "buffer has no file"))))

(defun eval-last-sexp ()
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.text.buffer:state->string state))
             (snap (pine.text.buffer:state->snapshot state))
             (offset (min (%point->offset snap) (length text))))
        (multiple-value-bind (start end) (%preceding-sexp-bounds text offset)
          (if start
              (%eval-form-string (subseq text start end) (%buffer-package state)
                                 :at (cons buf (%offset->lc text end)))
              (pine.editor.echo:message "no form before point")))))))

(defun eval-buffer ()
  ;; one evaluation on the eval thread reads and runs every form in order, so
  ;; *package* changes (in-package) carry across forms, a looping form can't
  ;; hang the editor, and a reader/eval error reaches the shared debugger
  ;; surface like every other eval path.
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.text.buffer:state->string state))
             (package (%buffer-package state))
             (c (pine.editor.frame:current-client))
             (thunk (lambda ()
                      (let ((pine.editor.frame:*client* c) (*package* package)
                            (pos 0) (count 0))
                        (loop
                          (multiple-value-bind (form new-pos)
                              (read-from-string text nil :eof :start pos)
                            (when (eq form :eof) (return count))
                            (eval form) (incf count) (setf pos new-pos))))))
             (done (lambda (ev)
                     (%eval-notify
                      (case (pine.core.eval:evaluation-status ev)
                        (:ok (format nil "eval-buffer: ~a forms"
                                     (first (pine.core.eval:evaluation-values ev))))
                        (:aborted "eval-buffer aborted")
                        (t "eval-buffer: error"))))))
        ;; one eval path: route the whole-buffer eval through the local agent.
        (if pine.core.actor:*local-agent*
            (pine.core.actor:agent-run nil pine.core.actor:*local-agent* thunk
                                  :package package :on-done done)
            (pine.core.eval:evaluate-thunk thunk :package package :on-done done))))))

(defun scroll-window (delta)
  (let* ((client (pine.editor.frame:current-client))
         (w (pine.editor.frame:focused-window client))
         (buf (pine.editor.frame:current-buffer client)))
    (when (and w buf)
      (let ((snap (pine.text.buffer:snap w)))
        (when (and snap (typep snap 'pine.text.buffer:snapshot))
          (let* ((max-scroll (max 0 (- (pine.text.buffer:line-count snap)
                                       (pine.text.buffer:win-height w))))
                 (new-scroll (max 0 (min max-scroll
                                         (+ (pine.text.buffer:scroll-top w) delta))))
                 (h (pine.text.buffer:win-height w))
                 (pl (pine.text.buffer:point-line snap))
                 (pc (pine.text.buffer:point-col snap)))
            (setf (pine.text.buffer:scroll-top w) new-scroll)
            (cond
              ((< pl new-scroll)
               (sento.actor:tell buf (list :move-point :line new-scroll
                 :col (min pc (length (fset:@ (pine.text.buffer:lines snap) new-scroll))))))
              ((>= pl (+ new-scroll h))
               (let ((target (+ new-scroll h -1)))
                 (sento.actor:tell buf (list :move-point :line target
                   :col (min pc (length (fset:@ (pine.text.buffer:lines snap) target))))))))
            (sento.actor:tell (pine.editor.frame:renderer client) '(:force-render))))))))

;;;; Help / self-documentation. Help buffers are read-only layout buffers
;;;; (%text-layout via show-layout); describe-key echoes.

(defun %describe-key-text (key)
  (let ((entry (pine.editor.command:key-binding (pine.editor.frame:current-client) key))
        (s (pine.editor.key:key->string key)))
    (cond ((consp entry) (format nil "~a is a prefix key" s))
          ((stringp entry) (format nil "~a runs the command ~a" s entry))
          ((pine.editor.command:self-insert-key-p key)
           (format nil "~a runs self-insert-command" s))
          (t (format nil "~a is undefined" s)))))

(defun %bindings-text ()
  (let* ((client (pine.editor.frame:current-client))
         (rows (loop for km in (pine.editor.frame:active-keymaps client)
                     append (pine.editor.keymap:keymap-bindings km t))))
    (with-output-to-string (out)
      (format out "Active bindings~%~%")
      (loop for (keys . cmd) in (sort (remove-duplicates rows :test #'equal
                                                         :key #'car :from-end t)
                                      #'string< :key #'car)
            do (format out "~16a  ~a~%" keys cmd)))))

(pine.state.var:defonce :tab-width :default 8
  :documentation "Tab stop width for the plain-text indent fallback.")

(pine.state.var:defonce :format-on-save :default nil
  :documentation "When non-nil, save-file reindents the whole buffer first.")

(pine.state.var:defonce :debug-on-error :default nil
  :documentation "When non-nil, an error in a command opens the *debugger*
restart menu instead of only echoing the message. Same knob as Emacs's
debug-on-error; edit-actor and eval errors always reach the debugger.")

(defun %variables-text ()
  (with-output-to-string (out)
    (format out "Editor variables~%~%")
    (dolist (name (pine.state.var:all-variable-names))
      (let ((v (pine.state.var:find-variable name))
            (buf (pine.editor.frame:buffer-in-scope)))
        (format out "~a = ~s [~(~a~)]~%    default ~s~a~%"
                name (pine.state.var:var name buf) (pine.state.var:variable-scope name buf)
                (pine.state.var:evar-default v)
                (let ((d (pine.state.var:evar-documentation v)))
                  (if (plusp (length d)) (format nil "~%    ~a" d) "")))))))

(defun %mode-text ()
  (let* ((client (pine.editor.frame:current-client))
         (major (pine.editor.frame:current-buffer-mode))
         (minors (pine.editor.frame:active-minor-modes client)))
    (with-output-to-string (out)
      (format out "Major mode: ~a (~a)~%"
              (pine.editor.mode:mode-name major) (pine.editor.mode:mode-indicator major))
      (loop for m = major then (pine.editor.mode:parent-mode m) while m
            do (format out "  ~a~%" (pine.editor.mode:mode-name m)))
      (let ((lang (and (typep major 'pine.editor.mode:major-mode)
                       (pine.editor.mode:ts-language major))))
        (when lang (format out "  tree-sitter language: ~a~%" lang)))
      (format out "~%Minor modes:~%")
      (if minors
          (dolist (m minors)
            (format out "  ~a (~a) precedence ~a~a~%"
                    (pine.editor.mode:mode-name m) (pine.editor.mode:mode-indicator m)
                    (pine.editor.mode:precedence m)
                    (if (pine.editor.mode:transparent m) " transparent" "")))
          (format out "  none~%")))))

;;;; Interaction on layout buffers. A layout buffer's snapshot carries the
;;;; arranged node tree (:layout-tree) and the selection index
;;;; (:layout-selection), so navigation is data + re-render -- the published
;;;; tree is never mutated -- and activation resolves point (or the selection)
;;;; to a node and runs its thunk: an action's callback, or a selectable whose
;;;; :data is a function.

(defun %layout-buffer ()
  (pine.editor.frame:current-buffer (pine.editor.frame:current-client)))

(defun %layout-snap (&optional (buf (%layout-buffer)))
  (and buf (sento.actor:ask-s buf '(:get-snapshot) :time-out 5)))

(defun layout-tree (&optional (snap (%layout-snap)))
  (and snap (pine.text.buffer:buffer-local snap :layout-tree)))

(defun layout-node-at-point ()
  "The node under point on the current buffer's layout tree, or nil."
  (let* ((snap (%layout-snap))
         (tree (layout-tree snap)))
    (when tree
      (pine.ui.node:node-at tree (pine.text.buffer:point-line snap)
                           (pine.text.buffer:point-col snap)))))

(defun layout-select (delta)
  "Move the layout selection by DELTA (wrapping), reproject, and land point on
the selected row. The reproject and the snapshot read serialize in the buffer's
mailbox, so the tree we read is the reprojected one."
  (let* ((buf (%layout-buffer))
         (snap (%layout-snap buf))
         (tree (layout-tree snap)))
    (when tree
      (let* ((n (length (pine.ui.node:collect-selectables tree)))
             (cur (pine.text.buffer:buffer-local snap :layout-selection 0))
             (new (if (plusp n) (mod (+ cur delta) n) 0)))
        (sento.actor:tell buf (list :reproject :selection new))
        (let* ((snap2 (%layout-snap buf))
               (tree2 (layout-tree snap2))
               (sel (and tree2 (nth new (pine.ui.node:collect-selectables tree2)))))
          (when sel
            (sento.actor:tell buf (list :move-point
                                        :line (pine.ui.node:start-line sel)
                                        :col 0))))))))

(defun %node-activation (node)
  "The thunk NODE activates to: an action's callback, a selectable whose data is
a function, or such a node anywhere below."
  (typecase node
    (null nil)
    (pine.ui.node:action (pine.ui.node:callback node))
    (pine.ui.node:selectable
     (let ((d (pine.ui.node:data node)))
       (if (functionp d) d (some #'%node-activation (pine.ui.node:nodes-of node)))))
    (t (some #'%node-activation (pine.ui.node:nodes-of node)))))

(defun layout-activate ()
  "Run the activation under point, else the selected row's."
  (let* ((snap (%layout-snap))
         (tree (layout-tree snap)))
    (when tree
      (let* ((at (pine.ui.node:node-at tree (pine.text.buffer:point-line snap)
                                      (pine.text.buffer:point-col snap)))
             (sel (nth (pine.text.buffer:buffer-local snap :layout-selection 0)
                       (pine.ui.node:collect-selectables tree)))
             (thunk (or (%node-activation at) (%node-activation sel))))
        (if thunk
            (funcall thunk)
            (pine.editor.echo:message "nothing to activate here"))))))

;;;; Window commands over the live editor tree. The arrangement is layout
;;;; nodes; a split wraps the focused window leaf in a column/row with a new
;;;; window on the same buffer, delete prunes, other-window cycles the leaves.

(defun %editor-leaves (&optional (client (pine.editor.frame:current-client)))
  "The window leaves of CLIENT's live tree, in tree order."
  (let ((tree (pine.editor.frame:arrangement client)) (acc nil))
    (when tree
      (labels ((walk (n)
                 (when (and (typep n 'pine.ui.node:window-node)
                            (eq (pine.ui.node:window-kind n) :window)
                            (pine.ui.node:window-of n))
                   (push n acc))
                 (dolist (c (pine.ui.node:nodes-of n)) (walk c))))
        (walk tree)))
    (nreverse acc)))

(defun %focused-leaf (&optional (client (pine.editor.frame:current-client)))
  (find (pine.editor.frame:focused-window client) (%editor-leaves client)
        :key #'pine.ui.node:window-of))

(defun %focus-leaf (leaf)
  "Focus LEAF's backing window and follow with the current buffer, so typing
lands in it."
  (let ((client (pine.editor.frame:current-client))
        (w (pine.ui.node:window-of leaf)))
    (pine.editor.frame:focus-window w)
    (setf (pine.editor.frame:current-buffer client) (pine.text.buffer:buffer-ref w))
    (pine.state.world:save-world :arrangement)))

(defun %split-window (orient)
  "Split the focused window along ORIENT (:column below, :row beside): a new
window on the same buffer joins the parent as a flat sibling when the parent
already stacks that way, else the leaf wraps in a fresh stack. A divider sits
between; sizes stay even because siblings share one weight."
  (let* ((client (pine.editor.frame:current-client))
         (tree (pine.editor.frame:arrangement client))
         (leaf (%focused-leaf client)))
    (unless leaf
      (pine.editor.echo:message "no window to split")
      (return-from %split-window))
    (let* ((w (pine.ui.node:window-of leaf))
           (buf (pine.text.buffer:buffer-ref w))
           (weight (max 1 (pine.ui.node:expand-of leaf)))
           (nw (pine.editor.frame:make-window buf (pine.text.buffer:window-name w)))
           (nn (pine.ui.node:window nil :of nw :kind :window :expand weight
                                   :font-px (pine.ui.node:font-px leaf)
                                   :opacity (pine.ui.node:window-opacity leaf)))
           (div (pine.ui.node:rule :vertical (eq orient :row)
                                  :face :border-inactive))
           (root (pine.ui.node:split-node tree leaf nn orient :divider div)))
      (setf (pine.text.buffer:snap nw) (pine.text.buffer:snap w)
            (pine.text.buffer:scroll-top nw) (pine.text.buffer:scroll-top w))
      (unless root
        (pine.editor.echo:message "cannot split here")
        (return-from %split-window))
      (setf (pine.editor.frame:arrangement client) root)
      (%focus-leaf leaf)
      (pine.ui.render:relayout))))

(defun %delete-leaf (leaf)
  "Remove LEAF and its divider from the live tree, splicing out a container
left with one child, and dropping its backing window."
  (let* ((client (pine.editor.frame:current-client))
         (root (pine.ui.node:remove-node (pine.editor.frame:arrangement client) leaf)))
    (when root
      (setf (pine.editor.frame:arrangement client) root)
      (pine.editor.frame:remove-window (pine.ui.node:window-of leaf))
      t)))

(defun delete-window-cmd ()
  (let* ((client (pine.editor.frame:current-client))
         (leaves (%editor-leaves client))
         (leaf (%focused-leaf client)))
    (cond
      ((null (rest leaves)) (pine.editor.echo:message "only one window"))
      ((and leaf (%delete-leaf leaf))
       (let ((next (first (%editor-leaves client))))
         (when next (%focus-leaf next)))
       (pine.ui.render:relayout))
      (t (pine.editor.echo:message "cannot delete this window")))))

(defun delete-other-windows-cmd ()
  (let* ((client (pine.editor.frame:current-client))
         (leaf (%focused-leaf client)))
    (when leaf
      (dolist (other (remove leaf (%editor-leaves client) :test #'eq))
        (%delete-leaf other))
      (%focus-leaf leaf)
      (pine.ui.render:relayout))))

(defun other-window-cmd ()
  (let* ((client (pine.editor.frame:current-client))
         (leaves (%editor-leaves client))
         (pos (position (%focused-leaf client) leaves :test #'eq)))
    (when (and leaves (rest leaves))
      (%focus-leaf (nth (mod (1+ (or pos 0)) (length leaves)) leaves)))))

(defun show-layout (name builder &key (mode :base-mode) (selection 0))
  "Open buffer NAME as a layout buffer showing BUILDER (state -> node tree),
switch to it, and enable layout-mode on it. Returns the buffer."
  (let* ((client (pine.editor.frame:current-client))
         (cols (pine.text.buffer:frame-cols (pine.editor.frame:frame client)))
         (buf (pine.editor.frame:make-buffer name)))
    (pine.editor.frame:set-buffer-mode buf mode)
    (pine.editor.ask:tell buf :set-layout :builder builder :width cols
                          :selection selection)
    (pine.ui.render:subscribe-to-buffer buf)
    (pine.editor.frame:switch-buffer name)
    (let ((r (ignore-errors (pine.editor.frame:renderer client))))
      (when r (sento.actor:tell r (list :switch-buffer :buffer buf :name name))))
    (ignore-errors (pine.editor.frame:enable-minor-mode client :layout-mode))
    buf))


;;;; Commands

(defmacro defcmd (name (&rest args) &body body)
  `(pine.editor.command:define-command ,name ,args ,@body))

;;;; Commands. Top-level: define-command registers as it is read, so loading
;;;; this file is what makes them reachable.

(defcmd "keyboard-quit" ()
  (setf (pine.editor.frame:pending-keys (pine.editor.frame:current-client)) nil)
  ;; C-g while attending a local eval interrupts it into its abort restart
  ;; (kills a runaway loop) and resolves the session.
  (let ((s *attended-session*))
    (when (and s (eq (dbg-session-kind s) :local) (dbg-session-ev s))
      (pine.core.eval:abort-evaluation (dbg-session-ev s))
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
              :start-line (pine.text.buffer:point-line snap) :start-col (pine.text.buffer:point-col snap)
              :end-line (pine.text.buffer:point-line snap) :end-col (1+ (pine.text.buffer:point-col snap)))))))
(defcmd "newline" ()
  (let ((buf (cur-buffer))) (when buf (pine.editor.ask:tell buf :newline))))
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
  (let ((c (pine.editor.frame:current-client)))
    (setf (pine.editor.frame:prefix-arg c)
          (list (* 4 (pine.editor.command:prefix-numeric-value
                      (pine.editor.frame:prefix-arg c)))))))
(defcmd "digit-argument" () (:prefix)
  (let* ((c (pine.editor.frame:current-client))
         (key (pine.editor.frame:this-command-key c))
         (d (and key (digit-char-p (char (pine.editor.key:key-sym key) 0))))
         (cur (pine.editor.frame:prefix-arg c)))
    (when d
      (setf (pine.editor.frame:prefix-arg c)
            (cond ((eq cur '-) (- d))
                  ((and (integerp cur) (minusp cur)) (- (+ (* 10 (- cur)) d)))
                  ((integerp cur) (+ (* 10 cur) d))
                  (t d))))))
(defcmd "negative-argument" () (:prefix)
  (let* ((c (pine.editor.frame:current-client))
         (cur (pine.editor.frame:prefix-arg c)))
    (setf (pine.editor.frame:prefix-arg c)
          (cond ((eq cur '-) nil)
                ((integerp cur) (- cur))
                (t '-)))))

(defcmd "beginning-of-line" ()
  (let ((buf (cur-buffer)) (snap (focused-snap)))
    (when (and buf snap)
      (sento.actor:tell buf (list :move-point :line (pine.text.buffer:point-line snap) :col 0)))))
(defcmd "end-of-line" ()
  (let ((buf (cur-buffer)) (snap (focused-snap)))
    (when (and buf snap)
      (let ((len (length (fset:@ (pine.text.buffer:lines snap) (pine.text.buffer:point-line snap)))))
        (sento.actor:tell buf (list :move-point :line (pine.text.buffer:point-line snap) :col len))))))
(defcmd "beginning-of-buffer" ()
  (let ((buf (cur-buffer))) (when buf (sento.actor:tell buf (list :move-point :line 0 :col 0)))))
(defcmd "end-of-buffer" ()
  (let ((buf (cur-buffer)) (snap (focused-snap)))
    (when (and buf snap)
      (let* ((ll (1- (pine.text.buffer:line-count snap)))
             (lc (length (fset:@ (pine.text.buffer:lines snap) ll))))
        (sento.actor:tell buf (list :move-point :line ll :col lc))))))

(defcmd "scroll-down" ()
  (let ((w (pine.editor.frame:focused-window (pine.editor.frame:current-client))))
    (when w (scroll-window (- (pine.text.buffer:win-height w) 2)))))
(defcmd "scroll-up" ()
  (let ((w (pine.editor.frame:focused-window (pine.editor.frame:current-client))))
    (when w (scroll-window (- 2 (pine.text.buffer:win-height w))))))

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
      (handler-case (pine.editor.file:find-file path)
        (error (c) (pine.editor.echo:message (format nil "error: ~a" c)))))
    :history :files))
(defcmd "find-recent" ()
  (let ((items (pine.state.store:store-items :recent-files)))
    (if items
        (completing-read "Recent: " items
          (lambda (path)
            (handler-case (pine.editor.file:find-file path)
              (error (c) (pine.editor.echo:message (format nil "error: ~a" c))))))
        (pine.editor.echo:message "no recent files"))))
(defcmd "save-file" ()
  (handler-case
      (let ((buf (cur-buffer)))
        ;; opt-in apheleia-style reformat before write; the reindent is a tell
        ;; and save's :get-state queues behind it in the actor mailbox, so the
        ;; write sees the formatted text
        (when (and buf (pine.state.var:var :format-on-save buf))
          (let ((snap (sento.actor:ask-s buf '(:get-snapshot) :time-out 5)))
            (pine.editor.ask:tell buf :indent-lines
                              :from 0 :to (1- (pine.text.buffer:line-count snap)))))
        (pine.editor.file:save-current-buffer))
    (error (c) (pine.editor.echo:message (format nil "error: ~a" c)))))
(defcmd "split-window-below" () (%split-window :column))
(defcmd "split-window-right" () (%split-window :row))
(defcmd "delete-window" () (delete-window-cmd))
(defcmd "delete-other-windows" () (delete-other-windows-cmd))
(defcmd "other-window" () (other-window-cmd))
(defcmd "switch-buffer" ()
  (completing-read "Switch to: " (pine.editor.frame:list-buffers)
    (lambda (name)
      (let* ((client (pine.editor.frame:current-client))
             (buf (pine.editor.frame:switch-buffer name)))
        (when buf
          (sento.actor:tell (pine.editor.frame:renderer client)
                            (list :switch-buffer :buffer buf :name name))
          (pine.ui.render:subscribe-to-buffer buf))))))
(defcmd "list-buffers" ()
  (pine.editor.echo:message (format nil "buffers: ~{~a~^, ~}" (pine.editor.frame:list-buffers))))
(defcmd "execute-command" ()
  (completing-read "M-x " (pine.editor.command:all-command-names)
    (lambda (name) (pine.editor.command:call-command name))
    :history :commands))
(defcmd "eval-expression" ()
  (prompt "Eval: "
    (lambda (text)
      (let ((pkg (let ((buf (cur-buffer)))
                   (if buf
                       (%buffer-package (sento.actor:ask-s buf '(:get-state) :time-out 5))
                       (find-package :cl-user)))))
        (%eval-form-string text pkg)))
    :history :eval))
(defcmd "choose-restart" ()
  (let ((names (and *attended-session*
                    (remove nil (mapcar #'first
                                        (dbg-session-restarts *attended-session*))))))
    (if names
        (completing-read "Restart: " names
          (lambda (name) (invoke-pending-restart name)))
        (pine.editor.echo:message "no evaluation in the debugger"))))
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
        (pine.editor.echo:message "only one debugger session"))))
(defcmd "debugger" ()
  "Reopen the *debugger* on the attended session (after q), if one is parked."
  (if *attended-session*
      (%attend-session *attended-session*)
      (pine.editor.echo:message "no debugger session")))
(defcmd "toggle-debug-on-error" ()
  (let ((new (not (pine.state.var:var :debug-on-error))))
    (setf (pine.state.var:var :debug-on-error) new)
    (pine.editor.echo:message (format nil "debug-on-error ~:[disabled~;enabled~]" new))))
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
         (mapcar #'pine.core.actor:agent-info-name
                 (pine.core.actor:list-agents
                  (pine.editor.frame:server-of (pine.editor.frame:current-client)))))
   (lambda (name)
     (setf pine.editor.target:*eval-target* (if (string= name "local") :local name))
     (pine.editor.echo:message (format nil "eval target: ~a" name)))))
(defcmd "new-buffer" ()
  (prompt "New buffer: "
    (lambda (name)
      (let ((buf (pine.editor.frame:make-buffer name))) (pine.ui.render:subscribe-to-buffer buf)))))
(defcmd "open-repl" ()
  (handler-case
      (let* ((client (pine.editor.frame:current-client))
             (buf (or (pine.editor.frame:repl-buffer client) (pine.editor.repl:start-repl))))
        (pine.editor.frame:switch-buffer "*repl*")
        (pine.ui.render:subscribe-to-buffer buf)
        (sento.actor:tell (pine.editor.frame:renderer client)
                          (list :switch-buffer :buffer buf :name "*repl*")))
    (error (c) (pine.editor.echo:message (format nil "error: ~a" c)))))
(defcmd "terminal" ()
  (handler-case
      (let* ((client (pine.editor.frame:current-client))
             (f (pine.editor.frame:frame client))
             (cols (pine.text.buffer:frame-cols f))
             (rows (max 1 (- (pine.text.buffer:frame-rows f) 2)))
             (buf (pine.editor.frame:make-buffer "*terminal*")))
        (pine.term:open-terminal client buf :rows rows :cols cols)
        (pine.editor.frame:set-buffer-mode buf :terminal-mode)
        (pine.editor.frame:switch-buffer "*terminal*")
        (sento.actor:tell (pine.editor.frame:renderer client)
                          (list :switch-buffer :buffer buf :name "*terminal*")))
    (error (c) (pine.editor.echo:message (format nil "error: ~a" c)))))
(defcmd "overwrite-mode" ()
  (let ((on (pine.editor.frame:toggle-minor-mode (pine.editor.frame:current-client) :overwrite-mode)))
    (pine.editor.echo:message (if on "Overwrite mode enabled" "Overwrite mode disabled"))))
(defcmd "describe-key" ()
  (pine.editor.echo:message "Describe key: ")
  (pine.editor.command:read-next-key
   (pine.editor.frame:current-client)
   (lambda (key) (pine.editor.echo:message (%describe-key-text key)))))
(defcmd "describe-bindings" ()
  (show-layout "*bindings*" (%text-layout (%bindings-text))))
(defcmd "describe-mode" ()
  (show-layout "*mode*" (%text-layout (%mode-text))))
(defcmd "describe-variables" ()
  (show-layout "*variables*" (%text-layout (%variables-text))))
(defcmd "insert-tab" ()
  (let* ((c (pine.editor.frame:current-client))
         (buf (pine.editor.frame:current-buffer c))
         (n (max 0 (pine.state.var:var :tab-width buf))))
    (when buf
      (pine.editor.ask:tell buf :insert :text (make-string n :initial-element #\Space)))))
(defcmd "indent-for-tab-command" ()
  "Reindent the current line to the column its mode dictates."
  (let ((buf (cur-buffer)))
    (when buf (pine.editor.ask:tell buf :indent-lines))))
(defcmd "indent-region" ()
  "Reindent every line spanned by the region."
  (let* ((buf (cur-buffer))
         (state (and buf (sento.actor:ask-s buf '(:get-state) :time-out 5))))
    (when state
      (multiple-value-bind (sl sc el ec) (pine.text.buffer:region-bounds state)
        (declare (ignore sc ec))
        (if sl
            (pine.editor.ask:tell buf :indent-lines :from sl :to el)
            (pine.editor.echo:message "no region"))))))
(defcmd "format-buffer" ()
  "Reindent the whole buffer off the parse tree, point preserved (in-image)."
  (let* ((buf (cur-buffer))
         (snap (and buf (sento.actor:ask-s buf '(:get-snapshot) :time-out 5))))
    (when snap
      (pine.editor.ask:tell buf :indent-lines
                        :from 0 :to (1- (pine.text.buffer:line-count snap))))))
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
(defcmd "minibuffer-prev-candidate" () (completion-prev))
(defcmd "minibuffer-history-prev" () (minibuffer-history-prev))
(defcmd "minibuffer-history-next" () (minibuffer-history-next))


;;;; Bindings. Top-level, grouped by the keymap they go into, beside the
;;;; commands they name. Nothing installs them; loading this file is what
;;;; binds them, and a config's define-key afterwards is never overwritten.

(pine.editor.keymap:define-keys :global
  "C-g"      "keyboard-quit"
  "Escape"   "keyboard-quit"
  "C-x C-f"  "find-file"
  "C-x C-r"  "find-recent"
  "C-x C-s"  "save-file"
  "C-x b"    "switch-buffer"
  "C-x 2"    "split-window-below"
  "C-x 3"    "split-window-right"
  "C-x 0"    "delete-window"
  "C-x 1"    "delete-other-windows"
  "C-x o"    "other-window"
  "C-x n"    "new-buffer"
  "C-x r"    "open-repl"
  "C-x t"    "terminal"
  "C-x C-e"  "eval-last-sexp"
  "C-M-x"    "eval-defun"
  "M-."      "find-definition"
  "M-x"      "execute-command"
  "Insert"   "overwrite-mode"
  ;; prefix argument
  "C-u"      "universal-argument"
  "M--"      "negative-argument"
  ;; help
  "C-h k"    "describe-key"
  "C-h b"    "describe-bindings"
  "C-h m"    "describe-mode"
  "C-h v"    "describe-variables")

(let ((map (pine.editor.keymap:keymap :global)))
  (dotimes (d 10)
    (pine.editor.keymap:define-key map (pine.editor.key:parse-chord (format nil "M-~d" d))
                            "digit-argument")))

(pine.editor.keymap:define-keys :text-mode
  "BackSpace"   "backspace"
  "Return"      "newline"
  "Tab"         "indent-for-tab-command"
  "Up"          "previous-line"
  "Down"        "next-line"
  "Left"        "backward-char"
  "Right"       "forward-char"
  "C-f"         "forward-char"
  "C-b"         "backward-char"
  "C-n"         "next-line"
  "C-p"         "previous-line"
  "C-a"         "beginning-of-line"
  "C-e"         "end-of-line"
  "C-d"         "delete-char"
  "M-f"         "forward-word"
  "M-b"         "backward-word"
  "M-d"         "kill-word"
  "M-BackSpace" "backward-kill-word"
  "C-s"         "isearch-forward"
  "C-r"         "isearch-backward"
  "M-<"         "beginning-of-buffer"
  "M->"         "end-of-buffer"
  "C-space"     "set-mark"
  "C-k"         "kill-line"
  "C-w"         "kill-region"
  "M-w"         "copy-region"
  "C-y"         "yank"
  "M-y"         "yank-pop"
  "C-v"         "scroll-down"
  "M-v"         "scroll-up"
  "Prior"       "scroll-up"
  "Next"        "scroll-down"
  "C-z"         "undo"
  "C-/"         "undo"
  "C-?"         "redo"
  ;; structural navigation (tree-sitter)
  "C-M-f"       "forward-sexp"
  "C-M-b"       "backward-sexp"
  "C-M-a"       "beginning-of-defun"
  "C-M-e"       "end-of-defun"
  "C-M-space"   "mark-sexp"
  ;; Tab reindents (mode-aware); completion has the Emacs binding, and
  ;; region and whole-buffer reformat get their own keys
  "C-M-\\"      "indent-region")

;;;; lisp-mode: the SLIME chord set.
(pine.editor.keymap:define-keys :lisp-mode
  "C-c C-c"  "eval-defun"
  "C-c C-k"  "eval-buffer"
  "C-c C-l"  "load-file"
  "C-c C-d"  "arglist"
  "C-M-i"    "complete-symbol"
  "M-Tab"    "complete-symbol")

;;;; debugger-mode: the restart rows answer to layout-mode's Return/C-n/C-p;
;;;; these are the extras.
(pine.editor.keymap:define-keys :debugger-mode
  "a"    "debugger-abort"
  "q"    "debugger-quit"
  "Tab"  "debugger-next-session")

;;;; layout-mode: selection nav and activation on a layout buffer.
(pine.editor.keymap:define-keys :layout-mode
  "Down"    "layout-next"
  "C-n"     "layout-next"
  "Up"      "layout-prev"
  "C-p"     "layout-prev"
  "Return"  "layout-activate")

;;;; minibuffer-mode: accept, abort, complete, candidate motion. Every other
;;;; key falls through to text-mode, so the prompt has full editing.
(pine.editor.keymap:define-keys :minibuffer-mode
  "Return"  "minibuffer-accept"
  "Escape"  "minibuffer-abort"
  "C-g"     "minibuffer-abort"
  "Tab"     "minibuffer-complete"
  "Down"    "minibuffer-next-candidate"
  "C-n"     "minibuffer-next-candidate"
  "Up"      "minibuffer-prev-candidate"
  "C-p"     "minibuffer-prev-candidate"
  "M-p"     "minibuffer-history-prev"
  "M-n"     "minibuffer-history-next")
