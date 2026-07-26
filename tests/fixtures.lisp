(in-package :pine.test)

;;;; An actor system costs more to build than every check in this suite put
;;;; together, so the live substrate is built once and each test is handed it
;;;; through SUBSTRATE, which clears the shared session on both sides of the
;;;; body rather than letting one test's leftovers be another's premise.

(defvar *server* nil)
(defvar *client* nil)

(defparameter +store-path+ "/tmp/pine-test-store.db"
  "The suite's store. Never OPEN-STORE's default, which is under XDG data home
and holds the user's own recents, places and kill ring.")

(defun open-fresh-store (&optional (path +store-path+))
  "Delete the database at PATH and open it."
  (pine.state.store:close-store)
  (dolist (f (list path
                   (concatenate 'string path "-wal")
                   (concatenate 'string path "-shm")))
    (uiop:delete-file-if-exists f))
  (pine.state.store:open-store path))

(defun build-substrate ()
  "Start the daemon's actors, a client with a renderer, scratch and the
minibuffer. Returns the server."
  (open-fresh-store)
  (let ((srv (pine.core.server:start-server)))
    (setf pine.core.server:*server* srv
          *server* srv
          (pine.core.server:ts-runtime srv) (pine.ts.runtime:make-ts-runtime))
    (pine.core.event:make-event-bus srv)
    (pine.core.actor:start-agent-registry srv)
    (pine.core.actor:start-local-agent srv)
    (pine.text.buffer:start-buffer-registry srv)
    (setf (pine.state.var:var :world-save) nil)
    (let ((client (pine.editor.frame::start-client srv)))
      (setf pine.editor.frame::*client* client
            *client* client)
      (pine.ui.render:start-renderer client)
      (setf (pine.editor.frame::paint-sink client)
            (lambda (&rest args) (declare (ignore args)) nil))
      (let ((buf (pine.editor.frame::make-buffer "scratch")))
        (pine.editor.frame::make-window buf "scratch"
                                        :row 0 :col 0 :width 80 :height 29
                                        :focused t)
        (setf (pine.editor.frame::current-buffer client) buf)
        (pine.editor.frame::set-buffer-mode buf :text-mode)
        ;; the window needs snapshots for the commands that read point through
        ;; the focused window, which is what a live session's renderer does
        (pine.ui.render:subscribe-to-buffer buf))
      (pine.editor.minibuffer:ensure-minibuffer client)
      (sleep 0.2))
    srv))

(defun reset-session ()
  "Make scratch current and clear the prompt, the pending keys, the prefix
argument, the debugger sessions and the world gate."
  (let ((c *client*))
    (when c
      (pine.editor.minibuffer:minibuffer-abort)
      (setf (pine.editor.frame::pending-keys c) nil
            (pine.editor.frame::prefix-arg c) nil
            (pine.editor.frame::pending-key-reader c) nil
            (pine.editor.frame::prompt-callback c) nil)
      (setf pine.editor.debugger:*debugger-sessions* nil
            pine.editor.debugger:*attended-session* nil
            pine.editor.target:*eval-target* :local
            pine.core.eval:*on-debug* nil
            pine.editor.isearch:*isearch* nil)
      (pine.editor.echo:message "")
      (setf (pine.state.var:var :world-save) nil
            (pine.state.var:var :debug-on-error) nil)
      (let ((scratch (pine.editor.frame::buffer "scratch")))
        (when scratch
          (setf (pine.editor.frame::current-buffer c) scratch)
          (pine.editor.frame::set-buffer-mode scratch :text-mode))))))

(def-fixture substrate ()
  "The live daemon substrate with *client* bound, reset around the body."
  (unless *client* (build-substrate))
  (let ((pine.editor.frame::*client* *client*))
    (reset-session)
    (unwind-protect (&body)
      (reset-session))))

(def-fixture memory-store ()
  "A private in-memory store; the suite's store is restored after the body."
  (pine.state.store:open-store ":memory:")
  (unwind-protect (&body)
    (open-fresh-store)))

(defun press (spec)
  "Dispatch the chord SPEC names and let the actors settle."
  (pine.editor.command::dispatch *client* (pine.editor.key::parse-key spec))
  (sleep 0.06))

(defun press* (&rest specs)
  (mapc #'press specs))

(defun type-text (string)
  (loop :for ch :across string :do (press (string ch))))

(defun btext (buffer)
  (sento.actor:ask-s (pine.editor.frame::buffer buffer) '(:get-text) :time-out 5))

(defun bsnap (buffer)
  (sento.actor:ask-s (pine.editor.frame::buffer buffer) '(:get-snapshot) :time-out 5))

(defun minibuffer-input ()
  (btext (pine.editor.frame::minibuffer-buffer *client*)))

(defun minibuffer-column ()
  (pine.text.buffer:point-col
   (bsnap (pine.editor.frame::minibuffer-buffer *client*))))

(defun in-user (form)
  "Read and evaluate FORM, a string, in PINE.USER."
  (let ((*package* (find-package :pine.user)))
    (eval (read-from-string form))))

(defun user-value (name)
  (symbol-value (find-symbol name :pine.user)))
