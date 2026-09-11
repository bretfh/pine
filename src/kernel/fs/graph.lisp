(in-package #:pine/fs)

(defvar *retry-seconds* 5)

(defvar *give-up-seconds* 30)

(defvar *brief-seconds* 0.05)

(defstruct (memo (:constructor memo (value from at)) (:copier nil)
                   (:predicate memop))
  value from at)

(defun %moved-on (x)
  (sb-ext:atomic-update (slot-value x 'mtime) (lambda (old) (1+ old)))
  (sb-ext:atomic-update *revision* (lambda (old) (1+ old)))
  x)

(defun %noted (x at value)
  (when *reading* (push (cons x at) (cdr *reading*)))
  value)

(defun %seen (x epoch)
  (loop :for had := (visited-at x)
        :do (when (>= had epoch) (return nil))
            (when (d:cas-p (slot-value x 'visited-at) had epoch) (return t))))

(defun current-mtime (x)
  (let ((h (and (typep x 'derived) (cached x))))
    (if (memop h) (memo-at h) (mtime x))))

(defun currentp (x at)
  (and (eql at (current-mtime x)) (validp x)))

(defun %whole (from)
  (loop :for (each . at) :in from :always (currentp each at)))

(defun validp (x)
  (cond ((not (typep x 'derived)) t)
        ((volatile-p x) t)
        ((not (memop (cached x))) nil)
        ((eql (verified-at x) *revision*) t)
        (t (let ((h (cached x))
                 (w *revision*))
             (when (%whole (memo-from h))
               (when (eql w *revision*) (setf (verified-at x) w))
               t)))))

(defun %freshp (n)
  (let ((h (cached n)))
    (and (memop h) (%whole (memo-from h)))))

(defun dirtyp (n) (not (%freshp n)))

(defun %holds (n)
  (let ((h (cached n)))
    (when (and (memop h) (%whole (memo-from h))) h)))

(defun %waiter (n)
  (or (waiting n)
      (progn (d:cas-p (slot-value n 'waiting) nil
                    (cons (bordeaux-threads:make-lock "pine/working")
                          (bordeaux-threads:make-condition-variable)))
             (waiting n))))

(defun %wait (n)
  (let ((it (%waiter n)))
    (bordeaux-threads:with-lock-held ((car it))
      (if (computing-thread n)
          (bordeaux-threads:condition-wait (cdr it) (car it) :timeout *retry-seconds*)
          (bordeaux-threads:condition-notify (cdr it))))))

(defun %done (n)
  (let ((it (%waiter n)))
    (bordeaux-threads:with-lock-held ((car it))
      (setf (computing-thread n) nil (claimed-at n) nil)
      (bordeaux-threads:condition-notify (cdr it)))))

(defun %overdue (n)
  (let ((since (claimed-at n)))
    (and (computing-thread n) since (> (- (get-universal-time) since) *give-up-seconds*))))

(defun %timed (n began)
  (setf (waits-of n)
        (> (- (get-internal-real-time) began)
           (* *brief-seconds* internal-time-units-per-second)))
  n)

(defun %edges (n from)
  (let ((had (depends-on n))
        (now (remove-duplicates (mapcar #'car from))))
    (dolist (on had) (unless (member on now) (undepend n on)))
    (setf (depends-on n) now)
    (dolist (on now) (unless (eq on n) (depend n on)))))

(defun %broke (n condition)
  (when *broke* (funcall *broke* condition (full-name n)))
  nil)

(defun %mine (n)
  (let ((depend-on (cons :reading nil))
        (from nil)
        (began (get-internal-real-time)))
    (unwind-protect
         (block mine
           (handler-bind ((error (lambda (c)
                                   (%broke n c)
                                   (return-from mine (values nil :broke nil)))))
             (let ((v (if (in-of n)
                          (funcall *elsewhere* (in-of n) (recompute n))
                          (let ((*reading* depend-on)) (works n)))))
               (%timed n began)
               (setf from (cdr depend-on))
               (cond ((%whole from)
                      (let* ((had (last-good n))
                             (same (and (memop had)
                                        (d:same v (memo-value had))))
                             (at (if same (memo-at had) (1+ (current-mtime n)))))
                        (setf (cached n) (memo v from at)
                              (last-good n) (cached n)
                              (verified-at n) -1)
                        (values v t at)))
                     (t (values v nil nil))))))
      (%edges n (or from (cdr depend-on))))))

(defun %held (n)
  (let ((h (last-good n)))
    (when (memop h) (memo-value h))))

(defun %gave-up (n)
  (%broke n (make-condition
             'simple-error
             :format-control "~a could not be worked out within ~d second~:p"
             :format-arguments (list (full-name n) *give-up-seconds*)))
  (values (%held n) (current-mtime n)))

(defun %work-out (n)
  (let ((me (bordeaux-threads:current-thread))
        (due (+ (get-universal-time) *give-up-seconds*)))
    (loop
      (let ((h (%holds n)))
        (when h (return (values (memo-value h) (memo-at h)))))
      (when (eq (computing-thread n) me)
        (error "~a is worked out from itself." (full-name n)))
      (when (> (get-universal-time) due) (return (%gave-up n)))
      (when (%overdue n) (return (values (%held n) (current-mtime n))))
      (if (and (null (computing-thread n))
               (d:cas-p (slot-value n 'computing-thread) nil me))
          (multiple-value-bind (v stands at)
              (unwind-protect (progn (setf (claimed-at n) (get-universal-time))
                                     (%mine n))
                (%done n))
            (cond ((eq stands :broke) (return (values (%held n) (current-mtime n))))
                  (stands (return (values v at)))))
          (%wait n)))))

(defun %landed (n)
  (sb-ext:atomic-update *revision* (lambda (old) (1+ old)))
  (let ((epoch (sb-ext:atomic-update *visit* (lambda (old) (1+ old)))))
    (let ((*visiting* epoch))
      (d:do-each (each (dependents n)) (touch each))))
  (%announce n)
  n)

(defun %hand-off (n)
  (let ((how *slow-pool*))
    (cond
      ((null how) (%work-out n))
      (t (when (d:cas-p (slot-value n 'scheduled) nil t)
           (funcall how n
                    (lambda ()
                      (unwind-protect
                           (progn (%work-out n) (%landed n))
                        (setf (scheduled n) nil)))))
         (let ((h (%holds n)))
           (if h
               (values (memo-value h) (memo-at h))
               (values (%held n) (current-mtime n))))))))

(defun waitsp (n)
  (and (typep n 'derived) (waits-of n) t))

(defmethod contents ((n derived))
  (if (volatile-p n)
      (works n)
      (let ((h (%holds n)))
        (cond (h (%noted n (memo-at h) (memo-value h)))
              ((and (waitsp n) (not *await-inline*))
               (multiple-value-bind (v at) (%hand-off n) (%noted n at v)))
              (t (multiple-value-bind (v at) (%work-out n) (%noted n at v)))))))

(defun pendingp (n)
  (and (typep n 'derived) (not (volatile-p n)) (not (%holds n))))

(defmethod (setf contents) (v (n derived))
  (takes n v)
  v)

(defun depend-on (x)
  (if (and (typep x 'derived) (not (volatile-p x)))
      (progn (contents x) x)
      (%noted x (current-mtime x) x)))

(defmethod touch ((x node))
  (%moved-on x)
  (let ((epoch (or *visiting* (sb-ext:atomic-update *visit* (lambda (old) (1+ old))))))
    (when (%seen x epoch)
      (let ((*visiting* epoch))
        (d:do-each (each (dependents x)) (touch each)))))
  x)

(defmethod touch :before ((n derived))
  (setf (mtime n) (current-mtime n))
  (setf (cached n) +unread+))

(defun commit (x)
  (touch x)
  (%announce x)
  x)

(defmethod contents :around ((x node))
  (if (and (typep x 'derived) (not (volatile-p x)))
      (call-next-method)
      (loop
        (let* ((before (mtime x))
               (v (call-next-method))
               (after (mtime x)))
          (when (eql before after) (return (%noted x before v)))))))

(defmethod children :around ((d mount))
  (loop
    (let* ((before (current-mtime d))
           (v (call-next-method))
           (after (current-mtime d)))
      (when (eql before after) (return (%noted d before v))))))

(defmethod child :around ((d mount) name)
  (or (call-next-method) (%noted d (current-mtime d) nil)))

(defmethod (setf contents) :after (value (x node))
  (declare (ignore value))
  (commit x))
