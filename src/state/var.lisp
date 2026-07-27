(defpackage #:pine.state.var
  (:use :cl)
  (:local-nicknames (#:world #:pine.state.world))
  (:export
   ;; the API: declare once, one setf-able accessor
   #:defonce #:var
   ;; introspection (describe-variables)
   #:find-variable #:all-variable-names #:variable-scope
   #:evar-default #:evar-documentation))

(in-package #:pine.state.var)

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
  (global-set    nil)
  (persist       nil))

(defvar *variables* (make-hash-table :test 'eq))

(defun %declare (name default documentation persist)
  (let* ((existing (gethash name *variables*))
         (v (or existing
                (setf (gethash name *variables*)
                      (%make-evar :name name)))))
    (setf (evar-default v) default
          (evar-documentation v) (or documentation "")
          (evar-persist v) persist)
    (when (and persist (not (evar-global-set v)))
      (let ((stored (world:value (list :var name) '%absent)))
        (unless (eq stored '%absent)
          (setf (evar-global v) stored (evar-global-set v) t))))
    v))

(defmacro defonce (name &key default documentation persist)
  "Declare editor variable NAME. Redeclaring updates the default and the
documentation but KEEPS a value already set, so reloading init.lisp (and the
editor's own install-variables) never clobbers a setting. With PERSIST the
global value survives restarts: the declaration seeds it from the store and
every global setf writes through."
  `(%declare ,name ,default ,documentation ,persist))

(defun find-variable (name)
  (or (gethash name *variables*) (error "No editor variable ~s" name)))

(defun all-variable-names ()
  (sort (loop for k being the hash-keys of *variables* collect k) #'string<))

(defun %buffer-vars (buffer)
  (and buffer
       (let ((state (pine.core.actor:ask buffer '(:get-state) :timeout 5)))
         (and state (fset:@ (pine.text.buffer:meta state) :vars)))))

(defun %buffer-var (buffer name)
  "(values VALUE PRESENT-P) for NAME's buffer-local binding."
  (let ((vars (%buffer-vars buffer)))
    (if (and vars (fset:domain-contains? vars name))
        (values (fset:@ vars name) t)
        (values nil nil))))

(defun var (name &optional buffer)
  "Editor variable NAME: buffer-local in BUFFER when one is given, else global,
else the declared default. setf-able: (setf (var NAME) V) sets the global,
(setf (var NAME BUFFER) V) sets it buffer-locally. The buffer is explicit:
this layer is below any client, so it never guesses which one is current."
  (let ((v (find-variable name)))
    (when buffer
      (multiple-value-bind (local present) (%buffer-var buffer name)
        (when present (return-from var local))))
    (if (evar-global-set v) (evar-global v) (evar-default v))))

(defun (setf var) (value name &optional buffer)
  (find-variable name)
  (if buffer
      (sento.actor:tell buffer (list :set-var :key name :value value))
      (let ((v (find-variable name)))
        (setf (evar-global v) value (evar-global-set v) t)
        (when (evar-persist v)
          (setf (world:value (list :var name)) value))))
  value)

(defun variable-scope (name &optional buffer)
  "Which scope NAME currently resolves through: :buffer, :global, or :default.
Introspection for describe-variables."
  (find-variable name)
  (cond ((and buffer (nth-value 1 (%buffer-var buffer name))) :buffer)
        ((evar-global-set (find-variable name)) :global)
        (t :default)))
