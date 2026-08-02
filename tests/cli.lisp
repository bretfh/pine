(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.cli :in :pine)

;;;; A daemon started the way `pine daemon' starts one, and every verb `pine'
;;;; says to it over the control socket.

(defun cli-system ()
  (let ((sys (sento.actor-system:make-actor-system
              (pine.core.server:actor-config :workers 1 :scheduler nil))))
    (sento.remoting:enable-remoting sys :host pine.core.server:*host* :port 0)
    sys))

(defun say (port &rest message)
  (pine.core.actor:ask
   (sento.remoting:make-remote-ref
    (cli-system)
    (pine.core.server:daemon-uri "control" :port port))
   message :timeout 10))

(def-fixture daemon ()
  (let* ((port 17071)
         (config (merge-pathnames "pine-cli-probe/" (uiop:temporary-directory)))
         (pine.core.server:*port* port)
         (was pine.ns:*space*)
         (server nil))
    (ensure-directories-exist config)
    (flet ((cli (&rest message) (apply #'say port message)))
      (declare (ignorable #'cli))
      (setf pine.ns:*space* (pine.ns:fresh))
      (unwind-protect
           (progn
             (setf server (pine::start-daemon :remoting-port port :config config))
             (&body))
        (ignore-errors (pine.ns:lower-all))
        (when server (pine.core.server:stop-server server))
        (setf pine.ns:*space* was)
        (loop :repeat 40 :until (pine:port-free-p port) :do (sleep 0.25))))))

(test the-daemon-starts-and-answers-status
  "pine daemon stands up the actor system, raises everything pine serves,
opens the attach listener and the control endpoint. pine status is the first
thing anyone asks it."
  (with-fixture daemon ()
    (let ((said (cli :status)))
      (is (stringp said))
      (is (search "pine up" said) "status said ~s" said))))

(test the-three-verbs-cross-the-control-socket
  "read, write and diff are what pine is. They go over a socket as text, so
this is the CLI's own path rather than a call into the image."
  (with-fixture daemon ()
    (is (equal "ok" (cli :write "/tab-width" "9" nil)))
    (is (equal "9" (cli :read "/tab-width")))
    (is (equal "ok" (cli :write "/wm-terminal" "\"alacritty\"" nil)))
    (is (equal "\"alacritty\"" (cli :read "/wm-terminal")))))

(test a-value-crosses-as-itself
  "A map, a seq, a set and a path go over as readable lisp and come back as
what they are, so the shell reaches a path added today."
  (with-fixture daemon ()
    (cli :write "/probe" "{:a 1 :b [1 2] :c #{:x}}" nil)
    (let ((said (cli :read "/probe")))
      (is (search ":A 1" said) "a map crossed as ~s" said))
    (cli :write "/probe-path" "/buf/scratch" nil)
    (is (equal "/buf/scratch" (cli :read "/probe-path")))))

(test a-write-says-where-it-lands-before-it-lands
  "A pattern write from the shell lists what it touches. :matches is what the
CLI asks to do that, and it is the whole of the refusal rule."
  (with-fixture daemon ()
    (cli :write "/buf/a/text" "\"one\"" nil)
    (cli :write "/buf/b/text" "\"two\"" nil)
    (let ((matches (cli :matches "/buf/*/text")))
      (is (listp matches))
      (is (<= 2 (length matches)) "matched ~s" matches))))

(test a-verb-crosses-as-a-verb
  "[:insert \"x\"] typed at the shell is a verb on the buffer, not a value
written over it."
  (with-fixture daemon ()
    (cli :write "/buf/scratch/text" "\"hello\"" nil)
    (cli :write "/buf/scratch/text" "[:insert \" there\"]" nil)
    (is (search "there" (cli :read "/buf/scratch/text")))))

(test eval-runs-a-form-in-the-config-package
  "pine eval is how a running daemon is reached from the shell."
  (with-fixture daemon ()
    (is (equal "3" (cli :eval "(+ 1 2)")))))

(test watch-tells-the-caller-what-moved
  "pine watch stands up an actor of its own and the daemon tells it each
change. This is the third verb, and the only one that keeps a channel."
  (with-fixture daemon ()
    (let* ((sys (cli-system))
           (heard nil)
           (name (format nil "probe-watch-~d" (random 100000)))
           (uri (pine.core.server:local-uri
                 name (sento.remoting:remoting-port sys))))
      (sento.actor-context:actor-of
       sys :name name :dispatcher :pinned
       :receive (lambda (message)
                  (when (eq :moved (first message))
                    (push (list (second message) (third message)) heard))
                  nil))
      (is (equal "watching" (cli :watch "/probe-watched" uri)))
      (cli :write "/probe-watched" "42" nil)
      (wait-for (lambda () heard) :seconds 5)
      (is (not (null heard)) "nothing was told to the watcher")
      (when heard
        (is (equal "/probe-watched" (first (first heard))))
        (is (equal "42" (second (first heard))))))))

(test the-daemon-declares-its-frontends-under-proc
  "start-frontends is what `pine daemon' does after it is up: the editor and
the desktop are /proc declarations, so what runs is readable and killing one
is a write."
  (with-fixture daemon ()
    (pine::declare-frontends)
    (is (pine.ns:read /proc/editor) "the editor was not declared")
    (is (pine.ns:read /proc/desktop) "the desktop was not declared")
    (let ((declared (pine.ns:read /proc/editor)))
      (is (fset:lookup declared :run) "a frontend declares what to run")
      (is (fset:lookup declared :needs)
          "a frontend needs a display before it starts"))))

(test a-session-tells-the-daemon-which-display-to-use
  "The daemon never picks a display: `pine session' hands it one, and /display
is what the frontends then need."
  (with-fixture daemon ()
    (is (equal "wayland-probe" (cli :session "wayland-probe")))
    (is (equal "wayland-probe" (pine.ns:read /display)))))

(test an-unknown-verb-says-so
  (with-fixture daemon ()
    (let ((said (cli :no-such-verb)))
      (is (equal '(:unknown :no-such-verb) said)))))

(test a-verb-that-fails-answers-rather-than-killing-the-daemon
  "The control actor is one pinned thread. A verb that signals must come back
as an answer, or the next thing anyone asks never gets one."
  (with-fixture daemon ()
    (let ((said (cli :eval "(error \"probe\")")))
      (is (stringp said))
      (is (search "error" said) "a failing eval said ~s" said))
    (is (search "pine up" (cli :status))
        "the daemon stopped answering after a verb signalled")))
