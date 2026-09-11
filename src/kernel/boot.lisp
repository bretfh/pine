(defpackage #:pine
  (:use #:cl)
  (:shadow #:describe #:read #:write)
  (:shadowing-import-from #:pine/data #:map #:set)
  (:import-from #:pine/data #:seq)
  (:import-from #:pine/fs/log #:note)
  (:import-from #:pine/fs
   #:contents #:derived #:describes #:mount #:name #:value #:erase #:root)
  (:import-from #:pine/run/command #:defcommand #:run)
  (:import-from #:pine/run/fault #:attempt)
  (:import-from #:pine/run/job #:start #:stop)
  (:import-from #:pine/run/peer #:reach #:serve)
  (:import-from #:pine/run/module #:drop #:module #:use)
  (:import-from #:pine/run/watch #:unwatch)
  (:local-nicknames (#:d #:pine/data)
                    (#:fs #:pine/fs)
                    (#:path #:pine/fs/path)
                    (#:store #:pine/fs/store)
                    (#:libs #:pine/run/libs) (#:log #:pine/fs/log)
                    (#:meter #:pine/run/meter) (#:fault #:pine/run/fault)
                    (#:actors #:pine/run/actors) (#:job #:pine/run/job)
                    (#:watch #:pine/run/watch)
                    (#:command #:pine/run/command)
                    (#:image #:pine/run/image)
                    (#:peer #:pine/run/peer) (#:module #:pine/run/module)
                    (#:listener #:pine/run/listener))
  (:export
   #:boot #:leave #:main #:daemon #:quit #:console #:opening #:load-config #:spawn
   #:at #:read #:write #:watch #:ls #:standsp #:toggle #:include #:exclude #:blend
   #:use #:drop #:reach #:serve
   #:seq #:map #:set #:note #:mount
   #:contents #:derived #:describes #:name #:value
   #:erase #:root
   #:defcommand #:run #:attempt #:start #:stop #:module #:unwatch))
(in-package #:pine)

(defgeneric opening (what)
            (:method (what) (declare (ignore what)) nil))

(defun spawn (name &key (systems '(:pine)))
  (let ((j (make-instance 'image:child :name (princ-to-string name)
                          :systems systems)))
    (job:supervise j)
    (job:start j)
    j))

(defun boot (&key (name "pine") store remoting)
  (libs:attend)
  (unless (actors:runningp) (actors:boot :remoting remoting))
  (setf pine/serve/socket:*name* name)
  (fs:mount #p"/" "/file")
  (job:attend)
  (when store
    (store:open-store store)
    (store:keeping))
  (fs:root))

(defun leave ()
  (fault:or-nothing "there may be no socket to close"
    (pine/serve/socket:close-socket))
  (fs:forget-listeners)
  (dolist (s (listener:listeners)) (listener:close s))
  (dolist (j (module:modules)) (fault:attempt (lambda () (job:stop j)) (job:name j)))
  (watch:forget-all)
  (dolist (j (job:jobs)) (fault:attempt (lambda () (job:stop j)) (job:name j)))
  (when store:*store* (store:close-store store:*store*))
  (actors:leave)
  t)

(defun config-file ()
  (merge-pathnames "pine/init.lisp" (uiop:xdg-config-home)))

(defun store-file ()
  (merge-pathnames "pine/tree.db" (uiop:xdg-data-home)))

(defun %the-used (c)
  (let* ((mine (find-package '#:pine/user))
         (had (find-if (lambda (s) (eq (symbol-package s) mine))
                       (sb-ext:name-conflict-symbols c)))
         (theirs (find-if-not (lambda (s) (eq (symbol-package s) mine))
                              (sb-ext:name-conflict-symbols c))))
    (when (and had theirs
               (not (fboundp had)) (not (boundp had)) (not (find-class had nil)))
      (invoke-restart (find-restart 'sb-ext:resolve-conflict c) theirs))))

(defun load-config (&optional (file (config-file)))
  (when (and file (probe-file file))
    (let ((*package* (find-package '#:pine/user))
          (*readtable* (named-readtables:find-readtable 'pine/fs/reader:syntax))
          (before (length (fault:faults))))
      (log:note "reading ~a" file)
      (fault:attempt
       (lambda ()
         (handler-bind ((sb-kernel:redefinition-with-defmethod #'muffle-warning)
                        (sb-ext:name-conflict #'%the-used))
           (let ((fs:*declaring* t)) (fs:writing (load file)))))
       (format nil "reading ~a" file))
      (let ((broke (- (length (fault:faults)) before)))
        (when (plusp broke)
          (log:note "~a did not read: ~a" file
                    (fault:condition-of (first (fault:faults)))))
        (zerop broke)))))

(defun quit (&optional (grace 5))
  (job:start (make-instance 'job:thread :name "quit-watchdog" :on-fault :leave
                            :body (lambda ()
                                     (sleep grace)
                                     (sb-ext:exit :abort t :code 0))))
  (job:start (make-instance 'job:thread :name "quit" :on-fault :leave
                            :body (lambda ()
                                     (sleep 0.2)
                                     (fault:or-nothing
                                      "leaving anyway"
                                      (leave))
                                     (sb-ext:exit :abort t :code 0))))
  t)

(defun console ()
  (listener:open-listener :name "console" :in (fs:root)
                        :package (find-package '#:pine/user)
                        :readtable (named-readtables:find-readtable
                                    'pine/fs/reader:syntax)))

(defun main (&key (store (store-file)))
  (boot :store store)
  (let ((s (console)))
    (unwind-protect (listener:interact s)
      (leave))))

(defun daemon (&key (store (store-file)) (remoting actors:*port*)
                    (config (config-file)))
  (boot :remoting remoting)
  (command:defcommand "quit" () (:describes "stop this pine")
                      (quit))
  (command:defcommand "reload" () (:describes "read the config again")
                      (load-config config)
                      :reloaded)
  (load-config config)
  (when store
    (store:open-store store)
    (store:keeping)
    (log:note "keeping ~a" store))
  (peer:serve)
  (fault:attempt (lambda () (pine/serve/socket:open-socket))
                 "answering on a socket")
  (fault:attempt (lambda () (opening :display)) "opening the display")
  (log:note "~a: remoting ~a, ~d command~:p, ~d running"
            pine/serve/socket:*name*
            (actors:remoting)
            (length (command:commands))
            (length (job:jobs)))
  (fs:root))

