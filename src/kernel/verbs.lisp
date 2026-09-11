(in-package #:pine)

(defun %cursor ()
  (or (and listener:*listener* (listener:in listener:*listener*)) (fs:root)))

(defun at (where &rest names)
  (apply #'fs:at where names))

(defun %leaf (where)
  (or (fs:at where) (fs:make where :value)))

(defun read (where &key (else nil elsep) await)
  (let ((n (fs:at where)))
    (if (null n)
        (values (if elsep else nil) :absent)
        (let* ((fs:*await-inline* (or await fs:*await-inline*))
               (fs:*give-up-seconds* (if (numberp await) await fs:*give-up-seconds*))
               (value (fs:contents n)))
          (values (if (and (null value) elsep) else value)
                  (fs:kind n)
                  (fs:pendingp n))))))

(defun standsp (where)
  (and (fs:at where) t))

(defun write (where value)
  (setf (fs:contents (%leaf where)) (fs:as-value value)))

(defun ls (where)
  (let ((n (fs:at where)))
    (if n (mapcar #'fs:name (fs:children n)) (list))))

(defun watch (where tells &rest options)
  (let ((n (fs:at where)))
    (unless n (error 'fs:absent :where where))
    (apply #'watch:watch n tells options)))

(defun toggle (where)
  (fs:verb (%leaf where) :toggle nil))

(defun include (where value)
  (fs:verb (%leaf where) :conj (list value)))

(defun exclude (where value)
  (fs:verb (%leaf where) :disj (list value)))

(defun blend (where map)
  (fs:verb (%leaf where) :merge (list map)))

(defun %behind (n)
  (cond ((fs:volatile-p n) :the-world)
        ((typep n 'fs:derived) :worked-out)
        ((and (fs:persistent-p n) fs:*backing-store*) :the-store)
        (t :this-image)))

(defun describe (where)
  (let ((n (fs:at where)))
    (when n
      (list :name (fs:full-name n)
            :class (string-downcase (princ-to-string (class-name (class-of n))))
            :describes (fs:describes n)
            :takes (fs:taking n)
            :under (mapcar #'fs:name (fs:children n))
            :behind (%behind n)
            :live (fs:volatile-p n)
            :owner (fs:owner n)))))

(command:defcommand "pwd" () (:describes "where this listener is")
                    (fs:full-name (%cursor)))

(command:defcommand "ls" (&optional where) (:describes "what is under a node")
                    (let ((n (if where (fs:at where) (%cursor))))
                      (if n (mapcar #'fs:name (fs:children n)) (list))))

(command:defcommand "cd" (&optional where) (:describes "go to a node")
                    (let ((n (if where (fs:at where) (fs:root))))
                      (when (and n listener:*listener*) (setf (listener:in listener:*listener*) n))
                      (and n (fs:full-name n))))

(command:defcommand "cat" (where) (:describes "what a node holds")
                    (let ((n (fs:at where)))
                      (and n (fs:contents n))))

(command:defcommand "put" (where value) (:describes "write a value")
                    (setf (fs:contents (%leaf where)) value))

(command:defcommand "mkdir" (where) (:describes "make a mount")
                    (fs:full-name (fs:mount (make-instance 'fs:mount) where)))

(command:defcommand "rm" (where) (:describes "take a node off")
                    (and (fs:erase where) t))

(command:defcommand "tree" (&optional where)
                    (:describes "every node under one that pine keeps")
  (let ((n (if where (fs:at where) (%cursor))))
    (unless n (error 'fs:absent :where where))
    (fs:paths n)))

(command:defcommand "live" ()
                    (:describes "what answers from the world, not the store")
                    (let (out)
                      (fs:walk (fs:root)
                                 (lambda (n) (when (fs:volatile-p n) (push (fs:full-name n) out))))
                      (nreverse out)))

(command:defcommand "mount" (what name)
                    (:describes "put a directory, or another pine, in the tree")
                    (let ((it (or (peer:named what) (pathname (princ-to-string what)))))
                      (fs:full-name (fs:mount it (format nil "/~a" name)))))

(command:defcommand "reach" (name port &optional host)
                    (:describes "get to another pine")
                    (job:name (peer:reach (princ-to-string name)
                                          :host (and host (princ-to-string host))
                                          :port (if (integerp port)
                                                    port
                                                  (parse-integer (princ-to-string port))))))

(command:defcommand "use" (name) (:describes "load a module and start it")
                    (let ((s (use name))) (and s (job:name s))))

(command:defcommand "drop" (name) (:describes "stop a module and take it off")
                    (let ((s (drop name))) (and s (job:name s))))

(command:defcommand "systems" () (:describes "what pine has loaded, and what it can")
                    (list :running (mapcar #'job:name (module:modules))
                          :available (module:kinds)))

(command:defcommand "jobs" () (:describes "what is running")
                    (loop :for j :in (job:jobs)
                          :collect (list (job:name j) (job:state j) (job:tries j))))

(command:defcommand "spawn" (name) (:describes "another lisp of pine's own")
                    (job:name (spawn name)))

(command:defcommand "start" (name &rest said)
                    (:describes "start a job again, or a program or image by name")
                    (let ((name (princ-to-string name))
                          (j (job:named (princ-to-string name))))
                      (if j
                          (progn (job:again j) (job:state j))
                          (job:started (list* :name name said)))))

(command:defcommand "stop" (name) (:describes "stop a job")
                    (let ((j (job:named (princ-to-string name))))
                      (when j (job:stop j) (job:state j))))

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
                                         (if (fault:suspendedp f) :suspended :done)
                                         (princ-to-string (fault:condition-of f)))))

(command:defcommand "take" (restart)
                    (:describes "hand a suspended fault one of its restarts")
                    (let ((f (first (fault:suspended))))
                      (and f (fault:take f (princ-to-string restart)))))

(command:defcommand "metrics" ()
                    (:describes "how long what pine does is taking")
                    (meter:readings))

(command:defcommand "metrics-reset" ()
                    (:describes "start a fresh window of samples")
                    (meter:reset)
                    :reset)

(command:defcommand "describe" (where) (:describes "what stands at a place")
                    (describe where))

