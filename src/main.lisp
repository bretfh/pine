(in-package :pine)

(defvar *version* "0.2.0")

(defvar *target*
  #+darwin :macos
  #+linux :linux
  #+windows :windows
  #-(or darwin linux windows) :linux)

(defun desktop? ()
  (member *target* '(:linux :macos :windows)))

(defun mobile? ()
  (member *target* '(:android :ios)))

(defun main (&key (workers 4) (remoting-port 0))
  (format t "pine ~a on ~a ~a [~a]~%"
          *version*
          (lisp-implementation-type)
          (lisp-implementation-version)
          *target*)
  (let ((srv (pine.server:start-server :workers workers
                                       :remoting-port remoting-port)))
    (setf (pine.server:ts-runtime srv) (pine.ts:make-ts-runtime))
    (pine.event:make-event-bus srv)
    (pine.actor:start-agent-registry srv)
    (pine.actor:start-local-agent srv)
    (let ((cli (pine.client:start-client srv)))
      (setf pine.client:*client* cli)
      (pine.buffer:install-default-faces)
      (pine.editor:start-editor)
      (pine.hooks:run-init-hooks)
      #+lqml (pine.qml:init-ui cli)
      (format t "pine ready [port ~a]~%"
              (or (pine.server:remoting-port srv) "none"))
      (values srv cli))))

(defun stop ()
  (pine.hooks:run-shutdown-hooks))

(defun on-key (key modifiers text)
  (ignore-errors
    (with-open-file (s "/tmp/pine-keys.log"
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
      (format s "on-key key=~a mods=~a text=~s client=~a current-buffer=~a~%"
              key modifiers text
              pine.client:*client*
              (when pine.client:*client*
                (pine.client:current-buffer pine.client:*client*)))))
  (handler-case (pine.editor:on-key key modifiers text)
    (error (c)
      (ignore-errors
        (with-open-file (s "/tmp/pine-keys.log"
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
          (format s "  !! on-key error: ~a~%" c))))))

(defun on-minibuffer-accept (text)
  (pine.editor:on-minibuffer-accept text))
