(in-package #:pine)

(defun %cursor ()
  "Where a command given no place looks: the node a session was moved to by CD, or
the root. A default for an argument nobody supplied, and nothing more -- it is not
a place a name can be measured from, so it reaches neither a config nor the wire."
  (or (and session:*session* (session:in session:*session*)) (tree:root)))

(defun at (where &rest names)
  "The node WHERE names, and NAMES on from there."
  (apply #'tree:at where names))

(defun read (where &key (else nil elsep))
  "What stands at WHERE, and which of three things that is.

:HELD is a value, and NIL is one of them. :ABSENT is nothing standing there at
all. :BRANCH is a name with things under it and nothing of its own, which is what
/dev is. All three answered NIL and nothing said which, so every caller guessed,
and every one of them guessed the same way: OR, which reads a written NIL as an
absence.

ELSE is what to say instead of nothing, said once where it is read rather than as
an OR at every call site. It does not change the second answer: what to say and
what was found are two questions.

A live node is not asked what is under it. What a device has beneath it belongs to
the world, and walking it to label a read would be shelling out to answer a
question about the answer."
  (let ((n (tree:at where)))
    (if (null n)
        (values (if elsep else nil) :absent)
        (let ((value (node:contents n)))
          (values (if (and (null value) elsep) else value)
                  (cond (value :held)
                        ((and (not (node:livep n)) (node:nodes n)) :branch)
                        (t :held)))))))

(defun standsp (where)
  "Whether anything stands at WHERE."
  (and (tree:at where) t))

(defun write (where value)
  "Put VALUE at WHERE, making the place if nothing has been put there yet: a read
finds what is there and a write makes what is not."
  (setf (node:contents (tree:ensure where)) value))

(defun ls (where)
  "The names directly under WHERE, and none where nothing stands.

The fourth verb. It was a command and not a word, so a session could say it and a
config could not."
  (let ((n (tree:at where)))
    (if n (tree:listing n) (list))))

(defun watch (where tells &rest options)
  "Say TELLS whenever what stands at WHERE moves. It is given the node and what it
now holds.

WHERE names a place the way the other three verbs do. Watching one nothing stands
at is a mistake rather than a silence: the watcher would be told about a node the
world is going to replace."
  (let ((n (tree:at where)))
    (unless n (error 'tree:absent :where where))
    (apply #'watch:watch n tells options)))

(defun toggle (where)
  "Flip what stands at WHERE.

A write, like the three below it: the four of them are what NODE:VERB has always
done, said in words rather than by writing a seq that begins with a keyword. That
spelling worked from the shell and not from lisp, which is why the mute button in
a config could mute and never unmute."
  (node:verb (tree:ensure where) :toggle nil))

(defun include (where value)
  "Put VALUE into the set at WHERE."
  (node:verb (tree:ensure where) :conj (list value)))

(defun exclude (where value)
  "Take VALUE out of the set at WHERE."
  (node:verb (tree:ensure where) :disj (list value)))

(defun blend (where map)
  "Merge MAP into the map at WHERE."
  (node:verb (tree:ensure where) :merge (list map)))

(defun describe (where)
  (let ((n (tree:at where)))
    (when n
      (list :name (node:full-name n)
            :class (class-name (class-of n))
            :describes (node:describes n)
            :under (tree:listing n)
            :saved (node:savedp n)
            :live (node:livep n)))))

(command:defcommand "pwd" () (:describes "where this session is")
                    (node:full-name (%cursor)))

(command:defcommand "ls" (&optional where) (:describes "what is under a node")
                    (let ((n (if where (tree:at where) (%cursor))))
                      (if n (tree:listing n) (list))))

(command:defcommand "cd" (&optional where) (:describes "go to a node")
                    (let ((n (if where (tree:at where) (tree:root))))
                      (when (and n session:*session*) (setf (session:in session:*session*) n))
                      (and n (node:full-name n))))

(command:defcommand "cat" (where) (:describes "what a node holds")
                    (let ((n (tree:at where)))
                      (and n (node:contents n))))

(command:defcommand "put" (where value) (:describes "write a node")
                    (setf (node:contents (tree:ensure where))
                          value))

(command:defcommand "mkdir" (where) (:describes "make a branch")
                    (node:full-name (tree:ensure where)))

(command:defcommand "rm" (where) (:describes "take a node off")
                    (and (tree:erase where) t))

(command:defcommand "tree" (&optional where)
                    (:describes "every node under one that pine keeps")
  (let ((n (if where (tree:at where) (%cursor))))
    (unless n (error 'tree:absent :where where))
    (tree:paths n)))

(command:defcommand "live" ()
                    (:describes "what answers from the world, not the store")
                    (let (out)
                      (tree:walk (tree:root)
                                 (lambda (n) (when (node:livep n) (push (node:full-name n) out))))
                      (nreverse out)))

(command:defcommand "mount" (what name)
                    (:describes "put a directory, or another pine, in the tree")
                    (let ((it (or (peer:named what) (pathname (princ-to-string what)))))
                      (node:full-name (mount:mount it (tree:root) (princ-to-string name)))))

(command:defcommand "reach" (name port &optional host)
                    (:describes "get to another pine")
                    (job:name (peer:reach (princ-to-string name)
                                          :host (and host (princ-to-string host))
                                          :port (if (integerp port)
                                                    port
                                                  (parse-integer (princ-to-string port))))))

(command:defcommand "use" (name) (:describes "load a system and start it")
                    (let ((s (use name))) (and s (job:name s))))

(command:defcommand "drop" (name) (:describes "stop a system and take it off")
                    (let ((s (drop name))) (and s (job:name s))))

(command:defcommand "systems" () (:describes "what pine has loaded, and what it offers")
                    (list :running (mapcar #'job:name (system:systems))
                          :offered (system:offered)))

(command:defcommand "jobs" () (:describes "what is running")
                    (loop :for j :in (job:jobs)
                          :collect (list (job:name j) (job:state j) (job:tries j))))

(command:defcommand "spawn" (name) (:describes "another lisp of pine's own")
                    (job:name (spawn name)))

(command:defcommand "kill" (name) (:describes "stop a job and forget it")
                    (let ((j (job:named (princ-to-string name))))
                      (when j (job:stop j) (job:forget (job:name j)) t)))

(command:defcommand "help" (&optional name) (:describes "what a command is for")
                    (if name
                        (let ((c (command:named (princ-to-string name))))
                          (and c (command:describes c)))
                      (loop :for c :in (command:sorted)
                            :collect (list (command:name c) (command:describes c)))))

(command:defcommand "faults" () (:describes "what has broken here")
                    (loop :for f :in (fault:faults)
                          :collect (list (fault:label f)
                                         (if (fault:standingp f) :standing :done)
                                         (princ-to-string (fault:condition-of f)))))

(command:defcommand "take" (restart)
                    (:describes "hand a standing fault one of its restarts")
                    (let ((f (first (fault:standing))))
                      (and f (fault:take f (princ-to-string restart)))))

(command:defcommand "snapshot" () (:describes "write the tree to its store")
                    (and store:*store* (store:snapshot store:*store*)))

(command:defcommand "metrics" ()
                    (:describes "how long what pine does is taking")
                    (meter:readings))

(command:defcommand "metrics-reset" ()
                    (:describes "start a fresh window of samples")
                    (meter:reset)
                    :reset)

(command:defcommand "describe" (where) (:describes "what stands at a place")
                    (describe where))

