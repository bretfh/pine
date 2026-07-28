(defpackage #:pine.editor.echo
  (:use :cl)
  (:local-nicknames (#:ns #:pine.ns))
  (:export #:message #:current-message
           #:prompt #:prompt-active-p #:prompt-text #:result
           #:said #:forget #:popup-rows #:input-snap))

(in-package #:pine.editor.echo)
(named-readtables:in-readtable pine.path:syntax)

;;;; The echo line and the prompt are paths:
;;;;
;;;;   /echo         the prompt map, or nothing when none is up
;;;;   /echo/hint    the line pine is saying
;;;;   /echo/result  what was typed
;;;;
;;;; so a command that prompts holds no callback and is re-entrant, and what
;;;; happens when the prompt is accepted is the prompt's own :then.

(defun message (text)
  (ns:write /echo/hint (or text ""))
  nil)

(defun current-message ()
  (or (ns:read /echo/hint) ""))

(defun prompt ()
  "The prompt that is up, as its map, or NIL.

/echo is a directory as well as a value -- the hint and the result are under
it -- so what makes a prompt is that it says what to prompt with."
  (let ((value (ns:held /echo)))
    (when (and (fset:map? value) (fset:lookup value :prompt)) value)))

(defun prompt-active-p ()
  (and (prompt) t))

(defun prompt-text ()
  (let ((p (prompt)))
    (and p (fset:lookup p :prompt))))

(defun result ()
  (ns:read /echo/result))

;;;; What the prompt and its completion UI say to each other: the filtered
;;;; candidates, which one is selected, the rendered popup, the input snapshot,
;;;; where the history cycle is. One side of a conversation rather than a place
;;;; anyone addresses, so it lives here, beside the line it is drawn on.

(defvar *said* (sento.atomic:make-atomic-reference :value (fset:empty-map)))

(defun said (key &optional default)
  (let ((value (fset:lookup (sento.atomic:atomic-get *said*) key)))
    (if (null value) default value)))

(defun (setf said) (value key)
  (sento.atomic:atomic-swap *said*
                            (lambda (m) (if (null value)
                                            (fset:less m key)
                                            (fset:with m key value))))
  value)

(defun forget ()
  (sento.atomic:atomic-swap *said* (lambda (m) (declare (ignore m))
                                     (fset:empty-map))))

(defun popup-rows () (said :popup-rows))
(defun input-snap () (said :snap))
