(defpackage #:pine/term/mode
  (:use #:cl)
  (:local-nicknames (#:mode #:pine/mode))
  (:export #:shell #:*send*))
(in-package #:pine/term/mode)

(defvar *send* nil
  "What a key means here: give it to whatever is on the other end of the pty. The
terminal puts itself here, so the mode names nothing that runs.")

(defclass shell (mode:text) ()
  (:documentation "Text a program is writing. A key is not an edit: it goes to the
program, and what comes back is the text.

This is what a mode is for. Nothing about the document is special -- PRESS and
INSERT are methods, and the keymap this class inherits is still in force for what
they do not take."))

(defmethod mode:setting ((m shell) key)
  (case key
    (:aside t)
    (:tab-width 8)
    (t (call-next-method))))

(defmethod mode:press ((m shell) document key)
  (when *send* (funcall *send* document key)))

(defmethod mode:insert ((m shell) document string)
  (when *send* (funcall *send* document string)))
