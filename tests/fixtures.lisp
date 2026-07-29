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

(defvar *store* nil "The path store the suite writes through.")

(defun open-fresh-store (&optional (path +store-path+))
  "Delete the database at PATH and open it, both stores."
  (pine.state.world:close)
  (when *store* (ignore-errors (pine.store:close *store*)) (setf *store* nil))
  (dolist (f (list path (concatenate 'string path "-paths")
                   (concatenate 'string path "-wal")
                   (concatenate 'string path "-shm")
                   (concatenate 'string path "-paths-wal")
                   (concatenate 'string path "-paths-shm")))
    (uiop:delete-file-if-exists f))
  (pine.state.world:open path)
  ;; the path store: held paths write through, so /history is real and undo is
  ;; what the file remembers
  (setf *store* (pine.store:open (concatenate 'string path "-paths"))))

(defun build-substrate ()
  "Start the daemon's actors, a client with a renderer, scratch and the
minibuffer. Returns the server."
  (open-fresh-store)
  (let ((srv (pine.core.server:start-server)))
    (setf pine.core.server:*server* srv
          *server* srv
          (pine.core.server:ts-runtime srv) (pine.ts.runtime:make-ts-runtime))
    (pine.core.actor:start-agent-registry srv)
    (pine.core.actor:start-local-agent srv)
    (pine.log:mount)
    (pine.doc:mount)
    (pine.provider.file:mount)
    (pine.provider.sh:mount)
    (pine.provider.env:mount)
    (pine.provider.clock:mount)
    (pine.win:mount)
    (pine.mode:mount)
    (pine.cmd:mount)
    (pine.editor.keymap:mount)
    (pine.term:mount-mode)
    (pine.editor.overwrite:mount-mode)
    (pine.editor.repl:mount-mode)
    (pine.editor.view:install)
    (pine.editor.echo:mount)
    (pine.editor.help:mount)
    (pine.editor.minibuffer:mount)
    (pine.desktop:mount-surfaces)
    (pine.buf:mount :system (pine.core.server:actor-system srv)
                    :runtime (pine.core.server:ts-runtime srv))
    (setf pine.state.world:*enabled* nil)
    (let ((client (pine.editor.frame::start-client srv)))
      ;; *client* is NOT set globally. The daemon binds it per thread -- the
      ;; session loop, the renderer actor, the minibuffer controller each bind
      ;; their own -- and a buffer actor never does. Setting the global here
      ;; would let a buffer actor reach a current client in tests and fault in
      ;; the daemon, which is how a wedged repl reached a running session.
      (setf *client* client)
      (pine.ui.render:start-renderer client)
      (setf (pine.editor.frame::paint-sink client)
            (lambda (&rest args) (declare (ignore args)) nil))
      (let ((pine.editor.frame::*client* client))
        (let ((buf (pine.editor.frame::make-buffer "scratch")))
          (pine.editor.frame::make-window buf "scratch"
                                          :row 0 :col 0 :width 80 :height 29
                                          :focused t)
          (setf (pine.editor.frame::current-buffer client) buf)
          (pine.editor.frame::set-buffer-mode buf :text)
          ;; the window needs snapshots for the commands that read point through
          ;; the focused window, which is what a live session's renderer does
          )
        (pine.editor.minibuffer:ensure-minibuffer client))
      (sleep 0.2))
    srv))

(defun reset-session ()
  "Make scratch current and clear the prompt, the pending keys, the prefix
argument, the debugger sessions and the world gate."
  (let ((c *client*))
    (when c
      (pine.editor.minibuffer:minibuffer-abort)
      (setf (pine.cmd:said :pending) nil
            (pine.cmd:prefix) nil
            (pine.cmd:said :reader) nil)
      (setf pine.editor.debugger:*debugger-sessions* nil
            pine.editor.debugger:*attended-session* nil
            pine.editor.target:*eval-target* :local
            pine.err:*on-debug* nil
            pine.editor.isearch:*isearch* nil)
      (pine.editor.echo:message "")
      (setf pine.state.world:*enabled* nil)
      (pine.ns:write (pine.path:parse "/debug-on-error") nil)
      (let ((scratch (pine.editor.frame::buffer "scratch")))
        (when scratch
          (pine.win:reset (pine.buf:at "scratch"))
          (pine.ns:write (pine.buf:at "scratch" :text) "")
          (setf (pine.editor.frame::current-buffer c) scratch)
          (pine.editor.frame::set-buffer-mode scratch :text))))))

(def-fixture substrate ()
  "The live daemon substrate, reset around the body.

*client* is bound for this thread only, the way the session loop binds it. The
actors have their own threads and bind their own, so a test runs under the same
conditions the daemon does."
  (unless *client* (build-substrate))
  (let ((pine.editor.frame::*client* *client*))
    (reset-session)
    (unwind-protect (&body)
      (reset-session))))

(defun mailbox-thread (actor)
  "ACTOR's own mailbox thread, for a test that has to kill one."
  (let ((box (sento.actor-cell:msgbox actor)))
    (when (typep box 'sento.messageb:message-box/bt)
      (slot-value box 'sento.messageb::queue-thread))))

(def-fixture memory-store ()
  "A private in-memory store; the suite's store is restored after the body."
  (pine.state.world:open ":memory:")
  (unwind-protect (&body)
    (open-fresh-store)))

(defparameter +agent-port+ 17055
  "The port the suite's daemon listens on when a test needs remoting: process
agents, and the attach handshake.")

(defun ensure-remoting ()
  "Turn on remoting for the suite's server, once. Returns the server."
  (let ((server *server*))
    (unless (pine.core.server:remoting-port server)
      (sento.remoting:enable-remoting (pine.core.server:actor-system server)
                                      :host pine.core.server:*host*
                                      :port +agent-port+)
      (setf (pine.core.server:remoting-port server) +agent-port+)
      (pine.core.actor:start-agent-debug server))
    server))

(defmacro within-seconds (seconds &body body)
  "Run BODY, failing rather than hanging if it takes longer than SECONDS."
  `(handler-case (sb-ext:with-timeout ,seconds ,@body)
     (sb-ext:timeout ()
       (fail "did not finish within ~d second~:p; a receive is wedged" ,seconds))))

(defun press (spec)
  "Dispatch the chord SPEC names and let the actors settle."
  (pine.editor.command::dispatch *client* (pine.editor.key::parse-key spec))
  (sleep 0.06))

(defun press* (&rest specs)
  (mapc #'press specs))

(defun type-text (string)
  (loop :for ch :across string :do (press (string ch))))

(defun btext (buffer)
  (pine.text.buffer:text-of (pine.editor.frame::buffer buffer)))

(defun bsnap (buffer)
  (pine.text.buffer:snapshot-of (pine.editor.frame::buffer buffer)))

(defun minibuffer-input ()
  (btext (pine.editor.frame::minibuffer-buffer *client*)))

(defun minibuffer-column ()
  (pine.text.buffer:point-col
   (bsnap (pine.editor.frame::minibuffer-buffer *client*))))

(defun in-user (form)
  "Read and evaluate FORM, a string, in PINE.USER."
  (let ((*package* (find-package :pine.user))
        (*readtable* (named-readtables:find-readtable (quote pine.path:syntax))))
    (eval (read-from-string form))))

(defun user-value (name)
  (symbol-value (find-symbol name :pine.user)))
