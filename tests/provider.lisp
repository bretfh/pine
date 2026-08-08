(in-package :pine.test)

(def-suite* :pine.provider :in :pine)

(defmacro with-providers (&body body)
  `(unwind-protect (progn (pine:start) ,@body) (pine:stop)))

(defun held-at (path)
  (pine.fs.node:contents (pine.world.world:at pine.world.world:*world* path)))

(test the-providers-are-nodes-under-the-root
  (with-providers
    (dolist (name '("sh" "env" "sys" "clock" "file"))
      (is-true (pine.world.world:at pine.world.world:*world* name)
               "~a is not mounted" name))))

(test reading-a-command-runs-it-and-answers-what-it-said
  "Reading asks the machine a question and answers what it said. Writing runs
the person's own program, and that is what /sh remembers."
  (with-providers
    (is (equal "from the provider" (held-at "sh/echo from the provider")))
    (is (null (held-at "sh")) "a question asked is not a program run")
    (setf (pine.fs.node:contents
           (pine.world.world:at pine.world.world:*world* "sh/true"))
          t)
    (is (member "true" (held-at "sh") :test #'equal)
        "what was launched is what /sh lists")))

(test an-environment-variable-reads-and-writes
  (with-providers
    (let ((n (pine.world.world:at pine.world.world:*world* "env/PINE_PROBE")))
      (is (null (pine.fs.node:contents n)))
      (setf (pine.fs.node:contents n) "a value")
      (is (equal "a value" (uiop:getenv "PINE_PROBE")))
      (is (equal "a value" (pine.fs.node:contents n)))
      (setf (pine.fs.node:contents n) nil)
      (is (null (uiop:getenv "PINE_PROBE"))))
    (is (member "PATH" (held-at "env") :test #'equal))))

(test the-machine-reads-through-sys
  (with-providers
    (is (integerp (held-at "sys/ram")))
    (is (<= 0 (held-at "sys/ram") 100))
    (is (integerp (held-at "sys/uptime")))
    (is (stringp (held-at "sys/host")))
    (is (= 3 (length (held-at "sys/load"))))
    (is (member "cpu" (pine.fs.tree:listing
                       (pine.world.world:at pine.world.world:*world* "sys"))
                :test #'equal))))

(test the-clock-is-a-node-and-a-process-keeps-it-current
  (with-providers
    (is (integerp (held-at "clock")))
    (is (stringp (held-at "clock/hour")))
    (is (= 2 (length (held-at "clock/hour"))))
    (is (integerp (held-at "clock/year")))
    (is (find "clock" (pine.proc.supervisor:processes pine:*supervisor*)
              :key #'pine.proc.process:name :test #'equal)
        "the time is kept current by a process, not a thread that sleeps")))

(test the-filesystem-mounts-at-file
  (with-providers
    (let ((where (merge-pathnames "pine-probe-provider/" (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (ensure-directories-exist where)
             (with-open-file (out (merge-pathnames "note.txt" where)
                                  :direction :output :if-exists :supersede)
               (write-string "through /file" out))
             (let ((path (format nil "file~anote.txt" (namestring where))))
               (is (equal "through /file" (held-at path)))))
        (uiop:delete-directory-tree where :validate t :if-does-not-exist :ignore)))))

(test the-shell-reaches-the-providers
  (with-providers
    (let* ((out (make-string-output-stream))
           (s (pine.repl.session:open-session
               :input (make-string-input-stream "") :output out
               :node (pine.world.world:root pine.world.world:*world*)
               :package (find-package :pine))))
      (unwind-protect
           (progn
             (is (member "sys" (first (pine.repl.session:answered
                                       (pine.repl.session:evaluate s 'ls)))
                         :test #'equal))
             (is (integerp (first (pine.repl.session:answered
                                   (pine.repl.session:evaluate
                                    s '(cat "/sys/uptime")))))))
        (pine.repl.session:close s)))))

(test what-answers-from-the-world-is-not-walked-into-by-the-store
  (with-providers
    (let ((live (pine.repl.command:run "live")))
      (dolist (name '("/sh" "/env" "/sys" "/clock" "/file"))
        (is (member name live :test #'equal) "~a should be live" name)))
    (is (null (pine.fs.node:livep (pine.edit.buffer:current)))
        "a buffer is pine's own, so the store keeps it")))

(test the-machine-providers-read-without-signalling
  "They shell out to tools that may not be on a given machine. Absent is nil,
never a backtrace, because a bar reading /audio/volume must not take the daemon
down when pipewire is not running."
  (with-providers
    (let ((root (pine.world.world:root pine.world.world:*world*)))
      (pine.provider.audio:install root)
      (pine.provider.screen:install root)
      (pine.provider.power:install root)
      (pine.provider.net:install root)
      (pine.provider.media:install root))
    (dolist (path '("audio/volume" "audio/muted" "screen/brightness"
                    "power/battery" "power/charging" "net/connection"
                    "net/online" "media/status" "media/title"))
      (finishes (held-at path)))))

(test a-provider-lists-what-it-has-and-each-is-a-node
  (with-providers
    (pine.provider.audio:install (pine.world.world:root pine.world.world:*world*))
    (let ((audio (pine.world.world:at pine.world.world:*world* "audio")))
      (is (member "volume" (pine.fs.tree:listing audio) :test #'equal))
      (is (member "sinks" (pine.fs.tree:listing audio) :test #'equal))
      (is-true (pine.fs.node:livep audio)))))

(test power-verbs-are-nodes-so-a-config-writes-one-rather-than-calling-out
  (with-providers
    (pine.provider.power:install (pine.world.world:root pine.world.world:*world*))
    (let ((power (pine.world.world:at pine.world.world:*world* "power")))
      (dolist (verb '("lock" "suspend" "reboot" "poweroff" "logout"))
        (is (member verb (pine.fs.tree:listing power) :test #'equal)
            "~a should be a node under /power" verb)))))
