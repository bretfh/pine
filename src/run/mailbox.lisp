(defpackage #:pine.run.mailbox
  (:use #:cl)
  (:shadow #:count)
  (:local-nicknames (#:c #:pine.run.cell))
  (:export #:mailbox #:send #:take #:peek #:count #:drain #:emptyp #:closed
           #:close-mailbox #:closedp))

(in-package #:pine.run.mailbox)

(defclass mailbox ()
  ((queue     :initform (c:cell nil) :reader queue)
   (lock      :initform (bordeaux-threads:make-lock "mailbox") :reader lock)
   (arrived   :initform (bordeaux-threads:make-condition-variable) :reader arrived)
   (closedp   :initform nil :accessor closedp)))

(defmethod print-object ((m mailbox) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~d" (count m))))

(defun mailbox () (make-instance 'mailbox))

(defgeneric count (mailbox)
  (:method ((m mailbox)) (length (c:held (queue m)))))

(defgeneric emptyp (mailbox)
  (:method ((m mailbox)) (null (c:held (queue m)))))

(defgeneric send (mailbox message)
  (:method ((m mailbox) message)
    (c:swap (queue m) (lambda (q) (append q (list message))))
    (bordeaux-threads:with-lock-held ((lock m))
      (bordeaux-threads:condition-notify (arrived m)))
    message))

(defgeneric peek (mailbox)
  (:method ((m mailbox)) (first (c:held (queue m)))))

(defun %pull (m)
  (loop :for q := (c:held (queue m))
        :while q
        :when (c:cas (queue m) q (rest q))
          :do (return (first q))))

(defgeneric take (mailbox &key timeout)
  (:method ((m mailbox) &key timeout)
    (loop :for taken := (%pull m)
          :when taken :do (return taken)
          :when (closedp m) :do (return nil)
          :do (bordeaux-threads:with-lock-held ((lock m))
                (bordeaux-threads:condition-wait (arrived m) (lock m)
                                                 :timeout (or timeout 0.05)))
          :when (and timeout (emptyp m)) :do (return nil))))

(defgeneric drain (mailbox)
  (:method ((m mailbox))
    (loop :for q := (c:held (queue m))
          :when (c:cas (queue m) q nil) :do (return q))))

(defgeneric close-mailbox (mailbox)
  (:method ((m mailbox))
    (setf (closedp m) t)
    (bordeaux-threads:with-lock-held ((lock m))
      (bordeaux-threads:condition-notify (arrived m)))
    m))
