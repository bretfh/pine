(defpackage #:pine.provider.net
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:out #:pine.provider.out))
  (:export #:net-node #:install #:connection #:online #:wifi #:saved #:rescan
           #:*asking*
           #:connect #:disconnect #:forget))

(in-package #:pine.provider.net)

(defparameter +readings+ '("connection" "online"))
(defvar *asking* nil)

(defclass net-node (node:node) ())
(defclass reading-node (node:node) ())
(defclass wifi-node (node:node) ())
(defclass network-node (node:node) ())

(defun connection ()
  (let ((line (out:firstp (out:sh "nmcli -t -f NAME connection show --active"))))
    (when (and line (plusp (length line))) line)))

(defun online ()
  (and (search "connected" (out:sh "nmcli -t -f STATE general")) t))

(defun saved ()
  (remove "" (out:lines (out:sh "nmcli -t -f NAME connection show"))
          :test #'equal))

(defun wifi ()
  (loop :for line :in (out:lines
                       (out:sh "nmcli -t -f IN-USE,SSID,SIGNAL,SECURITY device wifi list"))
        :for parts := (uiop:split-string line :separator '(#\:))
        :for ssid := (second parts)
        :when (and ssid (plusp (length ssid)))
          :collect (list ssid
                         :in-use (equal "*" (first parts))
                         :signal (or (out:number-in (third parts)) 0)
                         :secure (and (fourth parts)
                                      (plusp (length (fourth parts)))
                                      (not (equal "--" (fourth parts)))))))

(defun rescan () (out:sh "nmcli device wifi rescan") t)

(defun connect (ssid &optional password)
  (if password
      (out:sh "nmcli device wifi connect ~s password ~s" ssid password)
      (out:sh "nmcli device wifi connect ~s" ssid))
  t)

(defun disconnect (ssid) (out:sh "nmcli connection down ~s" ssid) t)

(defun forget (ssid) (out:sh "nmcli connection delete ~s" ssid) t)

(defun %kid (n name class &rest initargs)
  (node:child n name
              (lambda ()
                (apply #'make-instance class :name (princ-to-string name)
                       :parent n initargs))))

(defmethod node:announces ((n net-node)) (list "nmcli monitor"))

(defmethod node:nodes ((n net-node))
  (append (loop :for name :in +readings+ :collect (%kid n name 'reading-node))
          (list (%kid n "wifi" 'wifi-node))))

(defmethod node:resolve ((n net-node) name)
  (cond ((member name +readings+ :test #'equal) (%kid n name 'reading-node))
        ((equal name "wifi") (%kid n "wifi" 'wifi-node))))

(defmethod node:contents ((n net-node)) (connection))

(defmethod node:contents ((n reading-node))
  (if (equal "connection" (node:name n)) (connection) (online)))

(defmethod node:nodes ((n wifi-node))
  (loop :for (ssid . nil) :in (wifi) :collect (%kid n ssid 'network-node)))

(defmethod node:resolve ((n wifi-node) name)
  (%kid n name 'network-node))

(defmethod node:contents ((n wifi-node)) (mapcar #'first (wifi)))

(defmethod (setf node:contents) (value (n wifi-node))
  (declare (ignore value))
  (rescan))

(defmethod node:contents ((n network-node))
  (let ((it (rest (assoc (node:name n) (wifi) :test #'equal))))
    (when it
      (append it (list :saved (and (member (node:name n) (saved) :test #'equal) t))))))

(defmethod (setf node:contents) (value (n network-node))
  "Connect, disconnect or forget. A secured network nobody has saved needs a
password, and asking for one is the frontend's business, not this file's."
  (let ((ssid (node:name n)))
    (cond ((null value) (disconnect ssid))
          ((eq value :forget) (forget ssid))
          ((stringp value) (connect ssid value))
          ((and *asking*
                (getf (node:contents n) :secure)
                (not (getf (node:contents n) :saved)))
           (funcall *asking* ssid (lambda (password) (connect ssid password))))
          (t (connect ssid))))
  value)

(defmethod node:leafp ((n reading-node)) t)
(defmethod node:leafp ((n network-node)) t)
(defmethod node:livep ((n net-node)) t)
(defmethod node:livep ((n reading-node)) t)
(defmethod node:livep ((n wifi-node)) t)
(defmethod node:livep ((n network-node)) t)

(defun install (root &optional (name "net"))
  (node:attach (make-instance 'net-node :name name
                                        :describes "what is connected, and the wifi there is")
               root))
