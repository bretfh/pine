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

(defun store-file (&optional (path +store-path+))
  "The suite's store file, deleted so a run starts from nothing."
  (dolist (f (list path (concatenate 'string path "-paths")
                   (concatenate 'string path "-wal")
                   (concatenate 'string path "-shm")
                   (concatenate 'string path "-paths-wal")
                   (concatenate 'string path "-paths-shm")))
    (uiop:delete-file-if-exists f))
  (concatenate 'string path "-paths"))

(defun open-fresh-store (&optional (path +store-path+))
  "Delete the database at PATH and open it again, for a test that needs the
store back the way it found it."
  (when *store* (ignore-errors (pine.store:close *store*)) (setf *store* nil))
  (setf *store* (pine.store:open (store-file path))))

(defun build-substrate ()
  "Start the daemon's actors, raise everything pine serves, and make a client
with a renderer, scratch and the minibuffer. Returns the server."
  (let ((srv (pine.core.server:start-server)))
    (setf pine.core.server:*server* srv
          *server* srv
          (pine.core.server:ts-runtime srv) (pine.ts.runtime:make-ts-runtime))
    (pine.core.actor:start-agent-registry srv)
    (pine.core.actor:start-local-agent srv)
    ;; the same call the daemon makes: there is one list of what pine serves
    (let ((up (pine.ns:up-all
               (fset:map (:system (pine.core.server:actor-system srv))
                         (:runtime (pine.core.server:ts-runtime srv))
                         (:store-path (store-file))))))
      (setf (pine.core.server:proc srv) (fset:lookup up :proc)
            (pine.core.server:store srv) (fset:lookup up :store)
            *store* (fset:lookup up :store)))
    (let ((client (pine.editor.frame::start-client srv)))
      ;; *client* is NOT set globally. The daemon binds it per thread -- the
      ;; session loop, the renderer actor, the minibuffer controller each bind
      ;; their own -- and a buffer actor never does. Setting the global here
      ;; would let a buffer actor reach a current client in tests and fault in
      ;; the daemon, which is how a wedged repl reached a running session.
      (setf *client* client)
      (pine.editor.render:start-renderer client)
      (setf (pine.editor.frame::paint-sink client)
            (lambda (&rest args) (declare (ignore args)) nil))
      (let ((pine.editor.frame::*client* client))
        (let ((buf (pine.editor.frame::make-buffer "scratch")))
          (setf (pine.editor.frame::current-buffer) buf)
          (pine.editor.frame::set-buffer-mode buf :text))
        (pine.echo:ensure))
      (sleep 0.2))
    srv))

(defun reset-faults ()
  "Empty /err and stop the editor looking at it.

A test that wants the debugger installs it, so one that does not is not
surprised by a buffer opening on another test's leftover fault."
  (pine.ns:watch (pine.path:parse "/err") nil :as :debugger)
  (mapc #'pine.err:forget (pine.err:faults))
  (dolist (leaf '(:attended :seen :return-to))
    (pine.ns:write (pine.editor.debugger:at leaf) nil)))

(defun reset-session ()
  "Make scratch current and clear the prompt, the pending keys, the prefix
argument, what is at /err and the world gate."
  (reset-faults)
  (let ((c *client*))
    (when c
      (pine.echo:abort)
      (setf (pine.key:said :pending) nil
            (pine.key:prefix) nil
            (pine.key:said :reader) nil)
      (setf (pine.eval:target) :local)
      (pine.echo:message "")
        (pine.ns:write (pine.path:parse "/debug-on-error") nil)
      (let ((scratch (pine.buf:live "scratch")))
        (when scratch
          (pine.win:reset (pine.buf:at "scratch"))
          (pine.ns:write (pine.buf:at "scratch" :text) "")
          (setf (pine.editor.frame::current-buffer) scratch)
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
  (let ((mine (pine.store:open ":memory:")))
    (unwind-protect (&body)
      (ignore-errors (pine.store:close mine))
      (open-fresh-store))))

(defparameter +agent-port-from+ 17055
  "Where the suite starts looking for a port to listen on when a test needs
remoting: process agents, and the attach handshake.")

(defvar *agent-port* nil "The port remoting took, once it has taken one.")

(defun agent-port ()
  "A port the suite can listen on. An image left over from an interrupted run
holds one, and a suite that insists on a single number fails for a reason that
has nothing to do with pine, so the first free one is taken."
  (or *agent-port*
      (setf *agent-port*
            (loop :for port :from +agent-port-from+ :below (+ +agent-port-from+ 50)
                  :when (pine:port-free-p port) :return port
                  :finally (return +agent-port-from+)))))

(defun ensure-remoting ()
  "Turn on remoting for the suite's server, once. Returns the server."
  (let ((server *server*))
    (unless (pine.core.server:remoting-port server)
      (let ((port (agent-port)))
        (sento.remoting:enable-remoting (pine.core.server:actor-system server)
                                        :host pine.core.server:*host*
                                        :port port)
        (setf (pine.core.server:remoting-port server) port))
      (pine.core.actor:start-agent-debug server))
    server))

(defmacro within-seconds (seconds &body body)
  "Run BODY, failing rather than hanging if it takes longer than SECONDS."
  `(handler-case (sb-ext:with-timeout ,seconds ,@body)
     (sb-ext:timeout ()
       (fail "did not finish within ~d second~:p; a receive is wedged" ,seconds))))

(defun press (spec)
  "Dispatch the chord SPEC names and let the actors settle."
  (pine.key:dispatch (pine.key:parse-key spec))
  (sleep 0.06))

(defun press* (&rest specs)
  (mapc #'press specs))

(defun type-text (string)
  (loop :for ch :across string :do (press (string ch))))

(defun btext (buffer)
  (pine.buf:text-of (pine.buf:live buffer)))

(defun bsnap (buffer)
  (pine.buf:snapshot-of (pine.buf:live buffer)))

(defun minibuffer-input () (pine.echo:input))

(defun minibuffer-column ()
  (nth-value 1 (pine.buf:point pine.echo:+buffer+)))

(defun in-user (form)
  "Read and evaluate FORM, a string, in PINE.USER."
  (let ((*package* (find-package :pine.user))
        (*readtable* (named-readtables:find-readtable (quote pine.path:syntax))))
    (eval (read-from-string form))))

(defun user-value (name)
  (symbol-value (find-symbol name :pine.user)))
