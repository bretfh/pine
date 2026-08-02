(defpackage #:pine.key
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:shadow #:last)
  (:export #:key #:key-p #:make-key
           #:key-sym #:key-ctrl #:key-meta #:key-shift #:key-super
           #:key= #:parse-key #:parse-chord #:key->string
           #:at #:bind #:define-keys #:lookup #:bindings
           #:prefix-p #:roots #:server
           #:dispatch #:self-insert
           #:said #:prefix #:times #:last #:key-of
           #:call-command #:command-error #:key-binding #:read-next-key
           #:self-insert-key-p #:*terminal-handler*))

(in-package #:pine.key)
(named-readtables:in-readtable pine.path:syntax)

(defvar *terminal-handler* nil)

(defvar *cache* (pine.data:table)
  "Interned chords, so KEY= is EQ.")

(defvar *bound*
  (sento.atomic:make-atomic-reference :value (fset:empty-map))
  "The path of every binding this image declared, to what it was bound.

A registry, replayed into a space that has not seen it. Keyed by the path, so
binding a chord again replaces it rather than leaving the old one to be
replayed over the new.")

(defstruct (key (:constructor %make-key) (:copier nil))
  (sym   "" :type string  :read-only t)
  (ctrl  nil :type boolean :read-only t)
  (meta  nil :type boolean :read-only t)
  (shift nil :type boolean :read-only t)
  (super nil :type boolean :read-only t))

(defun make-key (sym &key ctrl meta shift super)
  "The one key object for this chord. Interned, so KEY= is EQ."
  (let ((id (list sym ctrl meta shift super)))
    (or (pine.data:at *cache* id)
        (pine.data:claim *cache* id
                         (%make-key :sym sym :ctrl ctrl :meta meta
                                    :shift shift :super super)))))

(declaim (inline key=))
(defun key= (a b) (eq a b))

(defun parse-key (spec)
  (let ((ctrl nil) (meta nil) (shift nil) (super nil)
        (i 0) (n (length spec)))
    (loop :while (and (< (1+ i) n) (char= (char spec (1+ i)) #\-))
          :do (case (char spec i)
                (#\C (setf ctrl t))
                (#\M (setf meta t))
                (#\S (setf shift t))
                (#\s (setf super t))
                (t (return)))
              (incf i 2))
    (make-key (subseq spec i) :ctrl ctrl :meta meta :shift shift :super super)))

(defun parse-chord (spec)
  (let ((keys (mapcar #'parse-key
                      (remove "" (uiop:split-string spec :separator '(#\Space))
                              :test #'string=))))
    (if (rest keys) keys (first keys))))

(defun key->string (k)
  (with-output-to-string (s)
    (when (key-ctrl k)  (write-string "C-" s))
    (when (key-meta k)  (write-string "M-" s))
    (when (key-shift k) (write-string "S-" s))
    (when (key-super k) (write-string "s-" s))
    (write-string (key-sym k) s)))

;;;; /key

(defun %chord (segment)
  (let ((key (ignore-errors (parse-key segment))))
    (if key (key->string key) segment)))

(defun %current-mode ()
  (let ((name (pine.buf:current-name)))
    (and name (ns:read (pine.buf:at name :mode)))))

(defun %current-minors ()
  (let ((name (pine.buf:current-name)))
    (and name (pine.mode:minors name))))

(defun %map (map)
  (case map
    ((:global :wm) (p:path /key map))
    (t (p:path /key :mode map))))

(defun %chords (chord)
  (loop :for x :in (alexandria:flatten (list chord))
        :append (if (stringp x)
                    (remove "" (uiop:split-string x :separator '(#\space))
                            :test #'string=)
                    (list (key->string x)))
          :into parts
        :finally (return (mapcar #'%chord parts))))

(defun at (map &rest chord)
  (apply #'p:path (%map map) (%chords chord)))

(defun provider ()
  (ns:provider
   (/key/?map/?@chord
    {:at (pine.data:fn []
           (apply #'p:path /key map (mapcar #'%chord chord)))
     :doc "a command path, a write-map or a handler"})
   (/key {:doc "the keymaps: :global, :wm, and one per mode"})))

(defun dispatch-provider ()
  (ns:provider
   (/dispatch/?leaf {:doc "one thing the dispatch in flight is holding"})
   (/dispatch
    {:doc "the dispatch in flight: :pending the chord so far, :prefix the
argument, :key what fired, :last the command before it, :reader who is taking
the next key"})))

(defun bind (map chord command)
  (let ((value (cond ((null command) nil)
                     ((or (p:pathp command) (fset:map? command)
                          (functionp command))
                      command)
                     (t (pine.cmd:at command))))
        (path (at map chord)))
    (sento.atomic:atomic-swap *bound*
                              (lambda (all) (fset:with all (p:text path) value)))
    (ns:write path value)))

(defmacro define-keys (map &body pairs)
  `(progn
     ,@(loop :for (chord command) :on pairs :by #'cddr
             :collect `(bind ,map ,chord ,command))
     ,map))

(defun prefix-p (value)
  "Whether VALUE is a keymap rather than something to run.

A binding may be a write-map -- {/wm [:close]} -- or carry a :run, and both are
maps, so the question is not whether it is one but whether it says where a chord
goes next. A map keyed by paths is writes, and one carrying :run is what to do;
neither is somewhere to go."
  (and (fset:map? value)
       (not (p:transactionp value))
       (not (fset:lookup value :run))))

(defun lookup (map chord)
  (ns:read (at map chord)))

(defun roots (&optional (mode (%current-mode)) (minors (%current-minors)))
  "Where a key is looked up: the current buffer's minor modes, most specific
first, then its mode and everything that mode falls back to, then global.

It is read off the tree, so nothing has to be told which maps are in effect and
a script that never heard of a window dispatches the same keys."
  (append (mapcar #'%map minors)
          (mapcar #'%map (pine.mode:chain mode))
          (list (%map :global))))

(defun bindings (map &optional (prefix nil))
  (let ((acc nil))
    (labels ((walk (path so-far)
               (let ((value (ns:read path)))
                 (cond ((prefix-p value)
                        (fset:do-map (key inner value)
                          (declare (ignore inner))
                          (let ((name (p:name key)))
                            (walk (p:child path name)
                                  (if so-far
                                      (concatenate 'string so-far " " name)
                                      name)))))
                       (value (push (cons so-far value) acc))))))
      (walk (if prefix (at map prefix) (%map map)) nil))
    (nreverse acc)))

;;;; What the dispatch carries: the chord typed so far, the prefix argument,
;;;; the key that fired, and the command before it. At /dispatch, so it is the
;;;; space's -- two frontends attached to one daemon each have their own C-u,
;;;; and what a command is about to be given is readable rather than inside a
;;;; variable of this file's.

(defun said (key &optional default)
  (ns:read (p:path /dispatch key) default))

(defun (setf said) (value key)
  (ns:write (p:path /dispatch key) value)
  value)

(defun prefix () (said :prefix))
(defun (setf prefix) (value) (setf (said :prefix) value))

(defun last () (said :last))
(defun (setf last) (value) (setf (said :last) value))

(defun key-of () (said :key))
(defun (setf key-of) (value) (setf (said :key) value))

(defun times (&optional (default 1))
  (let ((arg (prefix)))
    (cond ((null arg) default)
          ((integerp arg) arg)
          ((consp arg) (car arg))
          ((eq arg '-) -1)
          (t default))))

(defun command-error (condition)
  "What a command that signalled leaves behind: a fault at /err when the tree
asks for one, and a line in the echo area otherwise.

This runs on the session's input thread, which takes the next keystroke, so it
records and returns. Whoever is watching /err decides."
  (if (ns:read /debug-on-error)
      (ignore-errors (pine.err:report-failure condition "a command"))
      (ns:write /echo/hint (format nil "error: ~a" condition))))

(defmacro %guarding (&body body)
  `(block %guarded
     (handler-bind ((error (lambda (c)
                             (command-error c)
                             (return-from %guarded nil))))
       ,@body)))

(defun call-command (name)
  (let ((command (if (p:pathp name) name (pine.cmd:at name)))
        (before (prefix)))
    (when (ns:read command)
      (%guarding (pine.cmd:run command))
      (setf (last) (p:leaf command))
      (when (eq before (prefix))
        (setf (prefix) nil)))))

(defun self-insert-key-p (key)
  (and (= 1 (length (key-sym key)))
       (not (key-ctrl key))
       (not (key-meta key))
       (not (key-super key))))

(defun self-insert (buf key)
  (when (and buf key (self-insert-key-p key))
    (dotimes (i (max 1 (times)))
      (ns:write (pine.buf:at buf :text) (fset:seq :insert (key-sym key))))))

(defun %step (roots chord key)
  (let* ((so-far (append chord (list (key->string key))))
         (found (loop :for root :in roots
                      :for value = (ns:read (apply #'p:path root so-far))
                      :when value :collect value)))
    (let ((binding (find-if-not #'prefix-p found)))
      (if binding
          (values binding nil)
          (values nil (and found t))))))

(defun key-binding (key &optional (roots (roots)))
  "What KEY runs where it is being looked up, or T when it starts a chord."
  (multiple-value-bind (found prefix) (%step roots nil key)
    (or found prefix)))

(defun read-next-key (fn)
  (setf (said :reader) fn))

(defun %run (found)
  (if (p:pathp found)
      (call-command found)
      (let ((before (prefix)))
        (%guarding (pine.cmd:run found))
        (when (eq before (prefix))
          (setf (prefix) nil)))))

(defun %plain (key)
  "A key nothing bound: it types itself into the buffer that is current."
  (self-insert (pine.buf:current-name) key))

(defun dispatch (key &key (roots (roots)) (on-plain #'%plain))
  (setf (key-of) key)
  (let ((reader (said :reader)))
    (when reader
      (setf (said :reader) nil)
      (return-from dispatch (funcall reader key))))
  (%guarding
    (handler-bind ((error (lambda (c) (declare (ignore c))
                            (setf (said :pending) nil))))
      (let ((pending (said :pending)))
        (multiple-value-bind (found prefix) (%step roots pending key)
          (cond
            (found
             (setf (said :pending) nil)
             (%run found))
            (prefix
             (setf (said :pending) (append pending (list (key->string key)))))
            (pending
             (setf (said :pending) nil
                   (prefix) nil)
             (let ((top (%step roots nil key)))
               (if (and (p:pathp top) (equal "keyboard-quit" (p:leaf top)))
                   (%run top)
                   (ns:write /echo/hint
                             (format nil "~{~a~^ ~} is undefined"
                                     (append pending (list (key->string key))))))))
            (t
             (if (and (self-insert-key-p key) on-plain)
                 (funcall on-plain key)
                 (setf (prefix) nil)))))))))

(defclass server (ns:server) ()
  (:default-initargs :name :key :serves (list /key /dispatch) :after (list :cmd))
  (:documentation "The keymaps, the bindings pine ships, and the dispatch. After
:CMD because it puts a clause of its own at /cmd, and the newest provider at a
path is the one that answers."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  (ns:write /key (provider))
  (ns:write /dispatch (dispatch-provider))
  (ns:write /cmd (ns:provider
                  (/cmd/last {:read (pine.data:fn [] (last))
                              :doc "the command that ran last"})))
  (fset:do-map (text value (sento.atomic:atomic-get *bound*))
    (ns:write (p:parse text) value))
  nil)

(ns:register (make-instance 'server))
