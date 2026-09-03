(in-package #:pine/run/job)


(defvar *tries* 8
  "How many times a job is started again before it is held.

A job that dies as fast as it starts is not one more try away from working. The
backoff already spaces the tries out; this is what says to stop, so a crash loop
is something you can read at /proc rather than something the image does for the
rest of its life. Surviving *SETTLED* seconds forgets the tries, so this counts
a run of failures and not a long life with bad days in it.")

(defvar *backoff-cap* 60)

(defvar *settled* 30
  "Seconds a job has to survive before its tries are forgotten. Without this a job
that failed six times is held at the longest backoff for the life of the image,
however well it runs afterwards.")

(defvar *every* 1)

(defun backoff (j)
  (min *backoff-cap* (expt 2 (min 16 (tries j)))))

(defun settle (j)
  "Forget a job's tries once it has run long enough to have earned it."
  (let ((at (since j)))
    (when (and at (plusp (tries j))
               (>= (- (get-universal-time) at) *settled*))
      (setf (tries j) 0)))
  j)

(defun heldp (j)
  "Whether this job has been given up on. Not stopped: nobody asked it to go, and
nothing will start it again until somebody says so."
  (eq :held (state j)))

(defun %hold (j)
  "Stop trying, and say why where the job stands.

Written down rather than logged, because the question a person asks is about
this job and /proc/<name> is where they ask it."
  (setf (state j) :held
        (fault j) (make-condition
                   'simple-error
                   :format-control "gave up after ~d tr~:@p, none lasting ~d second~:p"
                   :format-arguments (list (tries j) *settled*)))
  j)

(defun supervised () (remove-if-not #'supervisedp (jobs)))

(defun supervise (j)
  "Keep J running: what dies is started again, and what will not run is held."
  (setf (supervisedp j) t)
  j)

(defun forget (name)
  "Stop what runs under NAME and take it out of what this image knows about.

Out of the jobs whether or not it was supervised. A job puts itself there as it is
made, and one that nothing supervises was one nothing could ever take out again --
which is a parser actor for every document ever opened, held for the life of the
image."
  (let ((j (named name)))
    (when j
      (fault:or-nothing "forgetting a job it could not stop still forgets it"
        (stop j))
      (fs:erase-entry (%proc) (princ-to-string name)))
    j))

(defun due (j now)
  "Whether enough has passed since the last try to make another. Without this a
program that dies as fast as it starts is started once a second for as long as pine
runs, and the backoff is a number nobody reads."
  (let ((last (since j)))
    (or (null last) (>= now (+ last (backoff j))))))

(defun sweep ()
  "One pass: start again what died and is owed a try, forget the tries of what has
run long enough to have earned it, and give up on what will not run at all."
  (let ((now (get-universal-time)))
    (dolist (j (supervised) t)
      (cond ((alivep j) (settle j))
            ((heldp j))
            ((and (eq :restart (on-fault j))
                  (member (state j) '(:running :failed))
                  (due j now))
             (if (>= (tries j) *tries*)
                 (%hold j)
                 (progn
                   (setf (state j) :failed (since j) now)
                   (handler-case (start j)
                     (error (e) (setf (fault j) e (state j) :failed))))))))))

(defun attend (&key (every *every*))
  "Look over what is supervised, on the wheel. Without this the backoff, the tries
and the restart are all written down and none of them ever happens."
  (repeat every #'sweep :as "proc" :what "starting again what died"))

(defgeneric told (kind name said)
  (:documentation "A job of KIND, made from what somebody said: a value describes
a program by its argv and an image by the systems it loads, and a kind that a
value can describe answers here."))

(defun kinds ()
  (sort (loop :for m :in (sb-mop:generic-function-methods #'told)
              :for spec := (first (sb-mop:method-specializers m))
              :when (typep spec 'sb-mop:eql-specializer)
                :collect (princ-to-string (sb-mop:eql-specializer-object spec)))
        #'string<))

(defun started (said)
  "Start what SAID asks for, and answer where it stands.

A kind that can be asked for is one a value can describe: a program is its argv,
an image the systems it loads. A thread and an actor are a function, which no
value carries, so asking for one says so. The name is given and not minted:
whoever asked has to find it again, and two asking at once must not race."
  (let* ((name (and (getf said :name) (princ-to-string (getf said :name))))
         (want (getf said :kind))
         (want (and want (intern (string-upcase (princ-to-string want)) :keyword))))
    (unless name (error "a job is started under a name; none was given."))
    (when (named name) (error "~a is already running." name))
    (unless (member (princ-to-string want) (kinds) :test #'equal)
      (error "~(~a~) is not a kind that can be asked for. There is ~{~a~^, ~}: a ~
              thread and an actor are a function, and a value cannot carry one."
             want (kinds)))
    (let ((j (told want name said)))
      (supervise j)
      (start j)
      (fs:full-name j))))

(defmethod told ((kind (eql :program)) name said)
  (make-instance 'program :name name
                          :on-fault (asked-for said :on-fault :restart)
                          :env (getf said :env)
                          :argv (mapcar #'princ-to-string (getf said :argv))))

(defun %attach (root)
  (setf (fs:describes (fs:ensure root "proc")) "what this pine is running"))

(pine/fs:builder #'%attach)
