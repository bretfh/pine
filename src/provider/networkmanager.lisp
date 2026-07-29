(defpackage #:pine.provider.networkmanager
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:out #:pine.provider.out))
  (:export #:networkmanager))

(in-package #:pine.provider.networkmanager)
(named-readtables:in-readtable pine.path:syntax)

;;;; The network, through nmcli.
;;;;
;;;;   (write /net (networkmanager))
;;;;
;;;; nmcli monitor says when anything moved, so a bar showing the connection is
;;;; right the moment it changes and nothing polls.
;;;;
;;;; A secure network with no saved credential answers [:connect] by writing a
;;;; prompt to /echo, because a password is something a person types, not
;;;; something a config holds.

(defun %terse (fields command &rest arguments)
  "nmcli's terse output as a list of field lists."
  (mapcar (lambda (line) (out:split line #\:))
          (out:lines (apply #'out:sh
                            (concatenate 'string
                                         "nmcli -t -f " fields " " command)
                            arguments))))

(defun %connection ()
  (loop :for row :in (%terse "NAME,DEVICE" "connection show --active")
        :when (and (second row) (plusp (length (second row))))
          :return (first row)))

(defun %onlinep ()
  (let ((state (out:sh "nmcli -t networking connectivity")))
    (and (string= state "full") t)))

(defun %wifi ()
  "Every network in range, ssid to what nmcli says about it."
  (loop :for row :in (%terse "SSID,SIGNAL,SECURITY,IN-USE" "device wifi list")
        :for ssid = (first row)
        :when (and ssid (plusp (length ssid)))
          :collect (cons ssid row)))

(defun %savedp (ssid)
  (loop :for row :in (%terse "NAME" "connection show")
        :thereis (equal ssid (first row))))

(defun %ask-for-a-password (ssid)
  "The prompt that connects once it is answered. Held nowhere: what a person
types goes straight into the command that needs it."
  (fset:map
   (/echo (fset:map (:prompt (format nil "Password for ~a: " ssid))
                    (:secret t)
                    (:then (fset:map
                            (/net/wifi (fset:seq :connected ssid))))))))

(defun %connect (ssid)
  (cond ((or (%savedp ssid) (not (%securep ssid)))
         (out:sh "nmcli device wifi connect '~a'" ssid)
         t)
        (t (%ask-for-a-password ssid))))

(defun %securep (ssid)
  (let ((row (cdr (assoc ssid (%wifi) :test #'string=))))
    (and row (third row) (plusp (length (third row))))))

(defun networkmanager ()
  "The network: what is connected, whether it reaches anything, and the wifi."
  (ns:provider
   {:on /sh/${"nmcli monitor"}
    :doc "networkmanager, through nmcli"}

   (/net/connection {:read (pine.data:fn [] (%connection))
                     :doc "the active connection's name"})
   (/net/online {:read (pine.data:fn [] (%onlinep))
                 :doc "whether it reaches anything"})

   (/net/wifi/?ssid
    {:read (pine.data:fn []
             (let ((row (cdr (assoc ssid (%wifi) :test #'string=))))
               (when row
                 (fset:map (:signal (out:number (or (second row) "0")))
                           (:secure (%securep ssid))
                           (:saved (%savedp ssid))
                           (:in-use (equal "*" (fourth row)))))))
     :verbs {:connect (pine.data:fn [] (%connect ssid))
             :disconnect (pine.data:fn []
                           (out:sh "nmcli connection down '~a'" ssid) t)
             :forget (pine.data:fn []
                       (out:sh "nmcli connection delete '~a'" ssid) t)}
     :doc "{:signal .. :secure .. :saved .. :in-use ..}; [:connect] [:disconnect] [:forget]"})

   (/net/wifi
    {:ls (pine.data:fn [] (mapcar #'car (%wifi)))
     :verbs {:rescan (pine.data:fn [] (out:sh "nmcli device wifi rescan") t)
             ;; the prompt's :then answers here, with what was typed
             :connected (pine.data:fn [ssid]
                          (out:sh "nmcli device wifi connect '~a' password '~a'"
                                  ssid (ns:read /echo/result))
                          t)}
     :doc "every network in range; [:rescan]"})

   (/net {:doc "connectivity, wifi, interfaces"})))
