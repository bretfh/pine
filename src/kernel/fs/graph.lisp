(in-package #:pine/fs)

(defvar *waited* 5
  "How long to wait on somebody else's working-out before looking again.")

(defvar *waiting-on* 30
  "Seconds to wait for whoever holds a working-out before giving up on them.")

(defvar *brief* 0.05
  "Seconds a working-out may take and still be one a reader waits for.")

(defstruct (worked (:constructor worked (value from at)) (:copier nil)
                   (:predicate workedp))
  "A worked-out value, what it was worked out from, and which version it is. FROM
is every entry it read with the version each was at, which is the whole of how
anybody decides afterwards whether this value still stands."
  value from at)

(defun %moved-on (x)
  (d:swap (slot-value x 'version) #'1+)
  (d:swap *writes* #'1+)
  x)

(defun %noted (x at value)
  (when *reading* (push (cons x at) (cdr *reading*)))
  value)

(defun %seen (x epoch)
  (loop :for had := (stamp x)
        :do (when (>= had epoch) (return nil))
            (when (d:cas (slot-value x 'stamp) had epoch) (return t))))

(defun mark (x)
  "Which version of itself X is holding now."
  (let ((h (and (typep x 'derived) (cached x))))
    (if (workedp h) (worked-at h) (version x))))

(defun currentp (x at)
  "Whether X is still what it was when somebody read it at version AT."
  (and (eql at (mark x)) (soundp x)))

(defun %whole (from)
  (loop :for (each . at) :in from :always (currentp each at)))

(defun soundp (x)
  "Whether what X holds still stands. Remembered against the count of writes, so
the usual case, where nothing is being written, is one comparison."
  (cond ((not (typep x 'derived)) t)
        ((livep x) t)
        ((not (workedp (cached x))) nil)
        ((eql (checked x) *writes*) t)
        (t (let ((h (cached x))
                 (w *writes*))
             (when (%whole (worked-from h))
               (when (eql w *writes*) (setf (checked x) w))
               t)))))

(defun freshp (n)
  (let ((h (cached n)))
    (and (workedp h) (%whole (worked-from h)))))

(defun stalep (n) (not (freshp n)))

(defun %holds (n)
  "What N holds if that still stands: the value, what it was worked out from and
which version it is, read as one object."
  (let ((h (cached n)))
    (when (and (workedp h) (%whole (worked-from h))) h)))

(defun %waiter (n)
  (or (waiting n)
      (progn (d:cas (slot-value n 'waiting) nil
                    (cons (bordeaux-threads:make-lock "pine/working")
                          (bordeaux-threads:make-condition-variable)))
             (waiting n))))

(defun %wait (n)
  (let ((it (%waiter n)))
    (bordeaux-threads:with-lock-held ((car it))
      (if (claim n)
          (bordeaux-threads:condition-wait (cdr it) (car it) :timeout *waited*)
          (bordeaux-threads:condition-notify (cdr it))))))

(defun %done (n)
  (let ((it (%waiter n)))
    (bordeaux-threads:with-lock-held ((car it))
      (setf (claim n) nil (claimed n) nil)
      (bordeaux-threads:condition-notify (cdr it)))))

(defun %overdue (n)
  (let ((since (claimed n)))
    (and (claim n) since (> (- (get-universal-time) since) *waiting-on*))))

(defun %timed (n began)
  (setf (waits-of n)
        (> (- (get-internal-real-time) began)
           (* *brief* internal-time-units-per-second)))
  n)

(defun %edges (n from)
  "Record what it read, and stop reading what it no longer reads."
  (let ((had (saw n))
        (now (remove-duplicates (mapcar #'car from))))
    (dolist (on had) (unless (member on now) (undepend n on)))
    (setf (saw n) now)
    (dolist (on now) (unless (eq on n) (depend n on)))))

(defun %broke (n condition)
  (when *broke* (funcall *broke* condition (full-name n)))
  nil)

(defun %mine (n)
  "Work it out, holding the claim. Answers the value, whether it stands, and which
version it is. What it read is checked again afterwards: if any of it moved, what
was added up is part one state and part another, and it is given up. One that
threw puts nothing down and says :BROKE, so the next read asks again."
  (let ((reading (cons :reading nil))
        (from nil)
        (began (get-internal-real-time)))
    (unwind-protect
         (block mine
           (handler-bind ((error (lambda (c)
                                   (%broke n c)
                                   (return-from mine (values nil :broke nil)))))
             (let ((v (if (in-of n)
                          (funcall *elsewhere* (in-of n) (reads n))
                          (let ((*reading* reading)) (works n)))))
               (%timed n began)
               (setf from (cdr reading))
               (cond ((%whole from)
                      (let* ((had (cached n))
                             (same (and (workedp had)
                                        (d:same v (worked-value had))))
                             (at (if same (worked-at had) (1+ (mark n)))))
                        (setf (cached n) (worked v from at)
                              (stood n) (cached n)
                              (checked n) -1)
                        (values v t at)))
                     (t (values v nil nil))))))
      (%edges n (or from (cdr reading))))))

(defun %held (n)
  (let ((h (stood n)))
    (when (workedp h) (worked-value h))))

(defun %gave-up (n)
  (%broke n (make-condition
             'simple-error
             :format-control "~a could not be worked out within ~d second~:p"
             :format-arguments (list (full-name n) *waiting-on*)))
  (values (%held n) (mark n)))

(defun %work-out (n)
  "What it works out to, working it out if what it holds no longer stands. One
thread works one entry out; a second wanting the same one waits for the first, and
nobody waits for ever: past *WAITING-ON* what it last worked out to is handed back
and the fault says why it is old."
  (let ((me (bordeaux-threads:current-thread))
        (due (+ (get-universal-time) *waiting-on*)))
    (loop
      (let ((h (%holds n)))
        (when h (return (values (worked-value h) (worked-at h)))))
      (when (eq (claim n) me)
        (error "~a is worked out from itself." (full-name n)))
      (when (> (get-universal-time) due) (return (%gave-up n)))
      (when (%overdue n) (return (values (%held n) (mark n))))
      (if (and (null (claim n))
               (d:cas (slot-value n 'claim) nil me))
          (multiple-value-bind (v stands at)
              (unwind-protect (progn (setf (claimed n) (get-universal-time))
                                     (%mine n))
                (%done n))
            (cond ((eq stands :broke) (return (values (%held n) (mark n))))
                  (stands (return (values v at)))))
          (%wait n)))))

(defun %landed (n)
  "What read N is worked out again, without giving up what N just worked out to."
  (d:swap *writes* #'1+)
  (let ((epoch (d:swap *epoch* #'1+)))
    (let ((*walking* epoch))
      (d:do-each (each (readers n)) (moved each))))
  (%announce n)
  n)

(defun %hand-off (n)
  (let ((how *working*))
    (cond
      ((null how) (%work-out n))
      (t (when (d:cas (slot-value n 'scheduled) nil t)
           (funcall how n
                    (lambda ()
                      (unwind-protect
                           (progn (%work-out n) (%landed n))
                        (setf (scheduled n) nil)))))
         (let ((h (%holds n)))
           (if h
               (values (worked-value h) (worked-at h))
               (values (%held n) (mark n))))))))

(defun waitsp (n)
  (and (typep n 'derived) (waits-of n) t))

(defmethod contents ((n derived))
  (if (livep n)
      (works n)
      (let ((h (%holds n)))
        (cond (h (%noted n (worked-at h) (worked-value h)))
              ((and (waitsp n) (not *awaiting*))
               (multiple-value-bind (v at) (%hand-off n) (%noted n at v)))
              (t (multiple-value-bind (v at) (%work-out n) (%noted n at v)))))))

(defmethod holding ((n derived))
  (cond ((livep n) :held)
        ((%holds n) :held)
        (t :working)))

(defmethod (setf contents) (v (n derived))
  (takes n v)
  v)

(defun reading (x)
  "Say the working-out running on this thread read X. One that is worked out and
remembered is worked out, so what is written down is a version a value stands at."
  (if (and (typep x 'derived) (not (livep x)))
      (progn (contents x) x)
      (%noted x (mark x) x)))

(defmethod moved ((x standing))
  "Whatever read X is worked out again. One walk reaches a ring once: the stamp is
shared and only ever goes up."
  (%moved-on x)
  (let ((epoch (or *walking* (d:swap *epoch* #'1+))))
    (when (%seen x epoch)
      (let ((*walking* epoch))
        (d:do-each (each (readers x)) (moved each)))))
  x)

(defmethod moved :before ((n derived))
  (setf (version n) (mark n))
  (setf (cached n) +unread+))

(defun announced (x)
  "Say X moved: what read it is worked out again, and whoever is waiting for a
batch of writes to land is told."
  (moved x)
  (%announce x)
  x)

(defmethod contents :around ((x standing))
  "What was read and the version it was read at, written down together. The version
is read on both sides of the value, because a write moves the two one after the
other."
  (if (and (typep x 'derived) (not (livep x)))
      (call-next-method)
      (loop
        (let* ((before (version x))
               (v (call-next-method))
               (after (version x)))
          (when (eql before after) (return (%noted x before v)))))))

(defmethod entries :around ((d dir))
  "Listing a dir is reading it, against MARK on both sides."
  (loop
    (let* ((before (mark d))
           (v (call-next-method))
           (after (mark d)))
      (when (eql before after) (return (%noted d before v))))))

(defmethod entry :around ((d dir) name)
  "A name that stands for nothing is still something that was read."
  (or (call-next-method) (%noted d (mark d) nil)))

(defmethod (setf contents) :after (value (x standing))
  (declare (ignore value))
  (announced x))
