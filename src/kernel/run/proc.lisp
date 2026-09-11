(in-package #:pine/run/job)

(defvar *tries* 8)

(defvar *backoff-cap* 60)

(defvar *settled-seconds* 30)

(defvar *every* 1)

(defun backoff (j)
  (min *backoff-cap* (expt 2 (min 16 (tries j)))))

(defun settle (j)
  (let ((at (since j)))
    (when (and at (plusp (tries j))
               (>= (- (get-universal-time) at) *settled-seconds*))
      (setf (tries j) 0)))
  j)

(defun giving-up-p (j)
  (eq :given-up (state j)))

(defun %hold (j)
  (setf (state j) :given-up
        (fault j) (make-condition
                   'simple-error
                   :format-control "gave up after ~d tr~:@p, none lasting ~d second~:p"
                   :format-arguments (list (tries j) *settled-seconds*)))
  j)

(defun supervised () (remove-if-not #'supervisedp (jobs)))

(defun supervise (j)
  (setf (supervisedp j) t)
  j)

(defun forget (name)
  (let ((j (named name)))
    (when j
      (fault:or-nothing "forgetting a job it could not stop still forgets it"
        (stop j))
      (fs:unlink (%proc) (princ-to-string name)))
    j))

(defun due (j now)
  (let ((last (since j)))
    (or (null last) (>= now (+ last (backoff j))))))

(defun sweep ()
  (let ((now (get-universal-time)))
    (dolist (j (supervised) t)
      (cond ((alivep j) (settle j))
            ((giving-up-p j))
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
  (repeat every #'sweep :as "proc" :what "starting again what died"))

(defgeneric make-job (kind name said))

(defun kinds ()
  (sort (loop :for m :in (sb-mop:generic-function-methods #'make-job)
              :for spec := (first (sb-mop:method-specializers m))
              :when (typep spec 'sb-mop:eql-specializer)
                :collect (princ-to-string (sb-mop:eql-specializer-object spec)))
        #'string<))

(defun started (said)
  (let* ((name (and (getf said :name) (princ-to-string (getf said :name))))
         (want (getf said :kind))
         (want (and want (intern (string-upcase (princ-to-string want)) :keyword))))
    (unless name (error "a job is started under a name; none was given."))
    (when (named name) (error "~a is already running." name))
    (unless (member (princ-to-string want) (kinds) :test #'equal)
      (error "~(~a~) is not a kind that can be asked for. There is ~{~a~^, ~}: a ~
              thread and an actor are a function, and a value cannot carry one."
             want (kinds)))
    (let ((j (make-job want name said)))
      (supervise j)
      (start j)
      (fs:full-name j))))

(defmethod make-job ((kind (eql :program)) name said)
  (make-instance 'program :name name
                          :on-fault (getf said :on-fault :restart)
                          :env (getf said :env)
                          :argv (mapcar #'princ-to-string (getf said :argv))))

(fs:mount (lambda () (make-instance 'fs:mount :describes "what this pine is running"))
          "/proc")
