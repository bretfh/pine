(defpackage #:pine/edit/keys
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:meter #:pine/run/meter)
                    (#:command #:pine/run/command) (#:key #:pine/ui/key)
                    (#:mode #:pine/text/mode) (#:doc #:pine/text/document)
                    (#:fault #:pine/run/fault))
  (:export #:dispatch #:bindings #:*on-insert*))
(in-package #:pine/edit/keys)

(defvar *on-insert* nil)

(defun bindings (m)
  "Every chord in force for a mode: its own, and its parents', nearest first."
  (loop :for class :in (c2mop:class-precedence-list (class-of m))
        :append (d:pairs (mode:keys (class-name class)))))

(defun %dispatch (document k)
  (let* ((m (doc:mode-of document))
         (so-far (append (key:pending) (list k)))
         (text (key:text so-far))
         (found (mode:binding m text)))
    (cond ((mode:press m document k)
           (setf (key:pending) nil)
           (setf (key:last) "the mode took it")
           :taken)
          (found
           (setf (key:pending) nil)
           (setf (key:last) (command:name found))
           (fault:attempt (lambda () (command:run found)) (command:name found)))
          ((mode:prefixp m text)
           (setf (key:pending) so-far)
           :pending)
          ((and (null (key:pending)) (key:typed k))
           (setf (key:last) "insert")
           (let ((said (key:typed k)))
             (unless (mode:insert m document said)
               (doc:insert document said)))
           (when *on-insert* (funcall *on-insert* k))
           :inserted)
          (t
           (setf (key:pending) nil)
           :unbound))))

(defun dispatch (k)
  "What a key means: whoever asked to read the next one, else this document's mode
and the chords its class inherits."
  (meter:timing (:key)
    (or (key:reading k)
        (let ((document (doc:current)))
          (and document (%dispatch document k))))))
