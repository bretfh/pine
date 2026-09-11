(defpackage #:pine/mode
  (:use #:cl)
  (:local-nicknames (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:fs #:pine/fs) (#:command #:pine/run/command)
                    (#:fault #:pine/run/fault) (#:module #:pine/run/module))
  (:export
   #:mode #:text #:prose #:code #:lisp
   #:pine #:scheme #:org #:press #:typing
   #:indent #:complete #:saving #:regions #:setting #:says
   #:covering #:name-of #:from-of #:to-of #:inside-of
   #:handles #:mode-for #:bind #:unbind #:binding #:bindings
   #:dispatch #:modes #:mode-node))
(in-package #:pine/mode)

(defclass mode () ())

(defclass fundamental (mode) ())
(defclass text (mode) ())
(defclass prose (text) ())
(defclass org (prose) ())
(defclass code (text) ())
(defclass lisp (code) ())
(defclass pine (lisp) ())
(defclass scheme (code) ())

(defgeneric press (mode buffer key)
  (:method ((m mode) d k) (declare (ignore d k)) nil))

(defgeneric typing (mode buffer string)
  (:method ((m mode) d s) (declare (ignore d s)) nil))

(defgeneric indent (mode buffer line)
  (:method ((m mode) d line) (declare (ignore d line)) nil))

(defgeneric complete (mode buffer prefix)
  (:method ((m mode) d prefix) (declare (ignore d prefix)) nil))

(defgeneric saving (mode buffer)
  (:method ((m mode) d) (declare (ignore d)) nil))

(defclass covering ()
  ((name   :initarg :name   :reader name-of)
   (from   :initarg :from   :reader from-of)
   (to     :initarg :to     :reader to-of)
   (inside :initarg :inside :reader inside-of :initform nil)))

(defun covering (name from to &optional inside)
  (make-instance 'covering :name (princ-to-string name) :from from :to to
                           :inside inside))

(defgeneric regions (mode buffer)
  (:method ((m mode) d) (declare (ignore d)) nil))

(defgeneric setting (of key)
  (:method ((m mode) key) (declare (ignore key)) :default))

(defun says (of key else)
  (let ((said (setting of key)))
    (if (eq said :default) else said)))

(defgeneric (setf setting) (value of key))

(defgeneric handles (mode)
  (:method ((m mode)) nil))

(defmethod fs:name ((m mode))
  (string-downcase (symbol-name (class-name (class-of m)))))

(defmethod setting ((m text) key)
  (case key (:tab-width 8) (t (call-next-method))))

(defmethod setting ((m code) key)
  (case key (:indent 2) (:comment ";") (t (call-next-method))))

(defmethod setting ((m lisp) key)
  (case key (:grammar :commonlisp) (t (call-next-method))))

(defmethod setting ((m pine) key)
  (case key (:grammar :pine) (t (call-next-method))))

(defmethod setting ((m scheme) key)
  (case key (:grammar :scheme) (t (call-next-method))))

(defmethod setting ((m org) key)
  (case key (:comment "#") (t (call-next-method))))

(defmethod handles ((m lisp)) '("*.lisp" "*.asd" "*.cl"))
(defmethod handles ((m scheme)) '("*.scm" "*.ss"))
(defmethod handles ((m org)) '("*.org"))

(defun glob (pattern text)
  (labels ((walk (p n)
             (cond ((and (null p) (null n)) t)
                   ((null p) nil)
                   ((char= (first p) #\*)
                    (or (walk (rest p) n) (and n (walk p (rest n)))))
                   ((null n) nil)
                   ((char-equal (first p) (first n)) (walk (rest p) (rest n)))
                   (t nil))))
    (walk (coerce pattern 'list) (coerce text 'list))))

(defun modes ()
  (labels ((under (class)
             (c2mop:ensure-finalized class)
             (cons class (mapcan #'under (c2mop:class-direct-subclasses class))))
           (depth (c) (length (c2mop:class-precedence-list c))))
    (sort (remove-duplicates (remove (find-class 'mode) (under (find-class 'mode))))
          (lambda (a b)
            (let ((da (depth a)) (db (depth b)))
              (if (= da db)
                  (string< (symbol-name (class-name a)) (symbol-name (class-name b)))
                  (> da db)))))))

(defun %class (name)
  (find (princ-to-string name) (modes)
        :key (lambda (c) (string-downcase (symbol-name (class-name c))))
        :test #'string-equal))

(defun mode (name)
  (let ((class (%class name)))
    (when class (fault:or-nothing "a mode class may take initargs nobody gave"
                  (make-instance (class-name class))))))

(defun claimsp (m path)
  (let ((leaf (file-namestring (pathname path)))
        (full (namestring (pathname path))))
    (some (lambda (p) (or (glob p leaf) (glob p full))) (handles m))))

(defun mode-for (path)
  (loop :for class :in (modes)
        :for it := (c2mop:class-prototype class)
        :when (and (handles it) (claimsp it path))
          :do (return (fault:or-nothing "a mode class may take initargs nobody gave"
                        (make-instance (class-name class))))))

(defun %named-as (class)
  (string-downcase (if (symbolp class) (symbol-name class) (princ-to-string class))))

(defclass keys (fs:value)
  ((owners :initform (d:no-map) :accessor owners)))

(defmethod fs:persistent-p ((k keys)) nil)

(defmethod fs:let-go ((k keys) owner)
  (let ((mine (loop :for (chord . who) :in (d:pairs (owners k))
                    :when (equal who owner) :collect chord)))
    (when mine
      (setf (owners k) (reduce #'d:without mine :initial-value (owners k)))
      (setf (fs:contents k) (reduce #'d:without mine :initial-value (fs:contents k))))
    mine))

(defclass modes (fs:mount) ())

(defmethod fs:children ((d modes))
  (dolist (name (%names)) (fs:child d name))
  (call-next-method))

(defmethod fs:child ((d modes) name)
  (or (call-next-method)
      (and (%class name) (%make-mode-dir d (%named-as name)))))

(defun mode-node () (make-instance 'modes :name "mode"
                                          :describes "every mode there is, and its chords"))

(defun %root () (fs:at "/mode"))

(defun %walked (name)
  (let ((cmd (fs:at "/cmd")) (out (d:no-map)))
    (when cmd (fs:depend-on cmd))
    (dolist (c (command:commands) out)
      (let ((on (command:on c)))
        (when (and on (string-equal name (string (first on))))
          (dolist (chord (rest on))
            (setf out (d:with out chord (command:name c)))))))))

(defclass mode-dir (fs:mount) ())

(defmethod fs:names ((d mode-dir))
  '((:keymap . "every chord in force here: what was bound, over what the commands carry")
    (:said   . "what the mode says about itself")))

(defmethod fs:read ((d mode-dir) (name (eql :keymap)))
  (d:merged (%walked (fs:name d)) (fs:contents (fs:child d "keys"))))

(defmethod fs:read ((d mode-dir) (name (eql :said)))
  (%said (fs:name d)))

(defmethod fs:volatile-p ((d mode-dir) &optional name)
  (if name (not (equal name "keymap")) (call-next-method)))

(defun %make-mode-dir (root name)
  (let ((d (fs:mount (make-instance 'mode-dir :name name) root)))
    (fs:mount (make-instance 'keys :name "keys" :held (d:no-map)) d)
    d))

(defun %mode-dir (root name)
  (or (fs:child root name) (%make-mode-dir root name)))

(defun %keymap (class)
  (let ((d (%mode-dir (%root) (%named-as class))))
    (fs:contents (fs:child d "keymap"))))

(defun keys (class) (%keymap class))

(defun %chain (m)
  (loop :for class :in (c2mop:class-precedence-list (class-of m))
        :when (subtypep class 'mode) :collect (class-name class)))

(defun binding (m chord)
  (loop :for class :in (%chain m)
        :for found := (d:lookup (%keymap class) chord)
        :when found :do (return (values (command:named found) found))))

(defun bindings (m)
  (loop :for class :in (%chain m)
        :append (d:pairs (%keymap class))))

(defun %keys (class)
  (fs:child (%mode-dir (%root) (%named-as class)) "keys"))

(defun bind (class chord command)
  (let ((k (%keys class)))
    (when fs:*owner*
      (setf (owners k) (d:with (owners k) chord fs:*owner*)))
    (setf (fs:contents k) (d:with (fs:contents k) chord command))
    chord))

(defun unbind (class chord)
  (let ((k (%keys class)))
    (setf (owners k) (d:without (owners k) chord))
    (setf (fs:contents k) (d:without (fs:contents k) chord))
    chord))

(defun dispatch (m subject k &optional (pending (ui:pending)))
  (if (press m subject k)
      (values :taken nil)
      (let* ((typed (append pending (list k)))
             (chord (ui:spelled typed)))
        (multiple-value-bind (found named) (binding m chord)
          (cond (found (values (fault:attempt (lambda () (command:run found))
                                              (command:name found))
                               nil
                               (command:name found)))
                (named (values :unbound nil named))
                ((prefixp m chord) (values :pending typed))
                ((and (null pending) (ui:typed k))
                 (values (cons :insert (ui:typed k)) nil))
                (t (values :unbound nil)))))))

(defun prefixp (m chord)
  (block found
    (dolist (class (%chain m) nil)
      (dolist (had (d:keys (%keymap class)))
        (when (and (> (length had) (length chord))
                   (string= chord had :end2 (length chord))
                   (char= #\Space (char had (length chord))))
          (return-from found t))))))

(defun %names ()
  (mapcar (lambda (c) (string-downcase (symbol-name (class-name c)))) (modes)))

(defun %said (name)
  (let ((m (mode name)))
    (when m (list :type (fs:name m) :handles (handles m)))))

(fs:mount #'mode-node "/mode")
