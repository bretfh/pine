(defpackage #:pine.self
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns))
  (:export #:claim))

(in-package #:pine.self)
(named-readtables:in-readtable pine.path:syntax)

;;;; Who this pine is. Another one addresses it by name, and tells two apart by
;;;; id -- not by hostname, which is neither unique nor stable.

(defun %hostname ()
  (or (uiop:hostname) "localhost"))

(defun %fresh-id ()
  "A 128 bit identity, made once and then held."
  (let ((state (make-random-state t)))
    (string-downcase (format nil "~32,'0x" (random (expt 2 128) state)))))

(defun claim (&key name)
  "Make this image addressable, and answer what it now says about itself.

The id is made the first time and kept from then on, so it survives restarts
where a hostname would not. NAME is what other pines address it by, and it
defaults to the hostname, so nothing is configured on a single machine."
  (unless (ns:read /self/id)
    (ns:write /self/id (%fresh-id)))
  (ns:write /self/name (or name (ns:read /self/name) (%hostname)))
  (ns:write /self/host (%hostname))
  (ns:write /self/since (get-universal-time))
  (ns:read /self))
