(defpackage #:pine.editor.keymap
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:at #:bind #:define-key #:define-keys #:lookup #:bindings
           #:prefix-p #:roots #:mount))

(in-package #:pine.editor.keymap)
(named-readtables:in-readtable pine.path:syntax)

;;;; A key sequence is a path, because a prefix map is a directory:
;;;;
;;;;   (write /key/global/C-x/C-f /cmd/find-file)
;;;;   (write /key/global/C-c/s   {/win/focused/buf /buf/scratch})
;;;;   (read  /key/global/C-x/*)          ; the prefix map
;;;;
;;;; A binding's value is a command path, a write-map or a handler, which is
;;;; exactly what pine.cmd:run takes. A map at a path is a prefix, because a
;;;; map is what a directory is.

(defun %chord (segment)
  "SEGMENT as the one spelling of that chord, so C-M-x and M-C-x are one path
and there is no aliasing to remember."
  (let ((key (ignore-errors (pine.editor.key:parse-key segment))))
    (if key (pine.editor.key:key->string key) segment)))

(defun %map (map)
  "Where MAP's bindings live. A mode's are under /key/mode, so /key/global and
/key/wm are the two a mode cannot shadow by being named the same."
  (case map
    ((:global :wm) (p:path /key map))
    (t (p:path /key :mode map))))

(defun %chords (chord)
  "CHORD as the segments it names: keys, key strings, a space-joined sequence,
or a list of any of those -- whatever KBD answers."
  (loop :for x :in (alexandria:flatten (list chord))
        :append (if (stringp x)
                    (remove "" (uiop:split-string x :separator '(#\space))
                            :test #'string=)
                    (list (pine.editor.key:key->string x)))
          :into parts
        :finally (return (mapcar #'%chord parts))))

(defun at (map &rest chord)
  "The path binding CHORD in MAP."
  (apply #'p:path (%map map) (%chords chord)))

(defun provider ()
  "Serve /key. A chord normalizes on the way in, so what is written and what is
read are the same path."
  (ns:provider
   (/key/?map/?@chord
    {:at (pine.data:fn []
           (apply #'p:path /key map (mapcar #'%chord chord)))
     :doc "a command path, a write-map or a handler"})
   (/key {:doc "the keymaps: :global, :wm, and one per mode"})))

;;;; Binding and looking up

(defvar *bound* nil
  "What has been bound, newest first, so MOUNT can put it into a namespace
that has not seen it: a fresh pine, or the one a test holds.")

(defun bind (map chord command)
  "Bind CHORD in MAP to COMMAND, which is a command path, a name, a write-map
or a handler. NIL unbinds."
  (let ((value (cond ((null command) nil)
                     ((or (p:pathp command) (fset:map? command)
                          (functionp command))
                      command)
                     (t (pine.cmd:at command)))))
    (push (list map chord value) *bound*)
    (ns:write (at map chord) value)))

(defun mount ()
  (ns:write /key (provider))
  (dolist (binding (reverse *bound*))
    (destructuring-bind (map chord value) binding
      (ns:write (at map chord) value)))
  nil)

(defun define-key (map chord command)
  (bind map chord command))

(defmacro define-keys (map &body pairs)
  "Bind CHORD COMMAND pairs in MAP."
  `(progn
     ,@(loop :for (chord command) :on pairs :by #'cddr
             :collect `(bind ,map ,chord ,command))
     ,map))

(defun prefix-p (value)
  "True when what is bound is a prefix: a directory, which is a map."
  (fset:map? value))

(defun lookup (map chord)
  "What CHORD is bound to in MAP: a value, a map when it is a prefix, or NIL."
  (ns:read (at map chord)))

(defun roots (mode minors)
  "Where a key is looked up, in the order it is looked up: each minor mode's
map, then the major mode's and every mode it falls back to, then global."
  (append (mapcar #'%map minors)
          (mapcar #'%map (pine.mode:chain mode))
          (list (%map :global))))

(defun bindings (map &optional (prefix nil))
  "Every (CHORD-STRING . BINDING) under MAP, chords space-joined."
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
