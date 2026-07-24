(in-package #:pine.var)

;;;; Editor variables. Three scopes resolved in order: buffer-local, then
;;;; global, then the declared default. Buffer-local values live in the
;;;; buffer's fset meta under :vars (set via the actor-atomic :set-var), so
;;;; they ride snapshots for free. The API is CL-native: DEFONCE declares,
;;;; VAR reads with resolution, and (SETF VAR) writes -- bare for the global,
;;;; with a buffer for buffer-local. No other setters.

(defstruct (evar (:constructor %make-evar) (:copier nil))
  (name          nil :read-only t)
  (default       nil)
  (documentation "")
  (global        nil)
  (global-set    nil))

(defvar *variables* (make-hash-table :test 'eq))

(defun %declare (name default documentation)
  (let ((existing (gethash name *variables*)))
    (if existing
        (setf (evar-default existing) default
              (evar-documentation existing) (or documentation ""))
        (setf (gethash name *variables*)
              (%make-evar :name name :default default
                          :documentation (or documentation ""))))
    (gethash name *variables*)))

(defmacro defonce (name &key default documentation)
  "Declare editor variable NAME. Redeclaring updates the default and the
documentation but KEEPS a value already set, so reloading init.lisp (and the
editor's own install-variables) never clobbers a setting."
  `(%declare ,name ,default ,documentation))

(defun find-variable (name)
  (or (gethash name *variables*) (error "No editor variable ~s" name)))

(defun all-variable-names ()
  (sort (loop for k being the hash-keys of *variables* collect k) #'string<))

(defun %buffer-vars (buffer)
  (and buffer
       (let ((state (pine.buffer:ask buffer :state)))
         (and state (fset:@ (pine.buffer:meta state) :vars)))))

(defun %buffer-var (buffer name)
  "(values VALUE PRESENT-P) for NAME's buffer-local binding."
  (let ((vars (%buffer-vars buffer)))
    (if (and vars (fset:domain-contains? vars name))
        (values (fset:@ vars name) t)
        (values nil nil))))

(defun current-buffer* ()
  (let ((cli pine.client:*client*)) (and cli (pine.client:current-buffer cli))))

(defun var (name &optional (buffer (current-buffer*)))
  "Editor variable NAME: buffer-local in BUFFER (default the current buffer),
else global, else the declared default. setf-able: (setf (var NAME) V) sets the
global, (setf (var NAME BUFFER) V) sets it buffer-locally."
  (let ((v (find-variable name)))
    (when buffer
      (multiple-value-bind (local present) (%buffer-var buffer name)
        (when present (return-from var local))))
    (if (evar-global-set v) (evar-global v) (evar-default v))))

(defun (setf var) (value name &optional buffer)
  (find-variable name)
  (if buffer
      (pine.buffer:tell buffer :set-var :key name :value value)
      (let ((v (find-variable name)))
        (setf (evar-global v) value (evar-global-set v) t)))
  value)

(defun variable-scope (name &optional (buffer (current-buffer*)))
  "Which scope NAME currently resolves through: :buffer, :global, or :default.
Introspection for describe-variables."
  (find-variable name)
  (cond ((and buffer (nth-value 1 (%buffer-var buffer name))) :buffer)
        ((evar-global-set (find-variable name)) :global)
        (t :default)))
