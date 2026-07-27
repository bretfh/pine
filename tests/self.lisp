(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.self :in :pine)

;;;; identity

(test claiming-fills-in-who-this-is
  (pine.ns:with-space ()
    (pine.self:claim)
    (is (stringp (pine.ns:read /self/id)))
    (is (= 32 (length (pine.ns:read /self/id))))
    (is (stringp (pine.ns:read /self/name)))
    (is (stringp (pine.ns:read /self/host)))
    (is (integerp (pine.ns:read /self/since)))))

(test the-name-defaults-to-the-hostname-and-can-be-said
  (pine.ns:with-space ()
    (pine.self:claim)
    (is (string= (pine.ns:read /self/host) (pine.ns:read /self/name)))
    (pine.self:claim :name "elsewhere")
    (is (string= "elsewhere" (pine.ns:read /self/name)))))

(test the-id-is-made-once-and-kept
  (pine.ns:with-space ()
    (pine.self:claim)
    (let ((id (pine.ns:read /self/id)))
      (pine.self:claim)
      (is (string= id (pine.ns:read /self/id))))))

(test two-images-are-two-identities
  (let ((a (pine.ns:with-space () (pine.self:claim) (pine.ns:read /self/id)))
        (b (pine.ns:with-space () (pine.self:claim) (pine.ns:read /self/id))))
    (is (not (string= a b)))))

(defun %scratch-file (name)
  (namestring (merge-pathnames name (uiop:temporary-directory))))

(defun %forget-file (file)
  (dolist (suffix '("" "-wal" "-shm"))
    (uiop:delete-file-if-exists (concatenate 'string file suffix))))

(test the-id-survives-a-restart-because-it-is-held
  (let ((file (%scratch-file "pine-self-probe.db"))
        (id nil))
    (unwind-protect
         (progn
           (%forget-file file)
           (pine.ns:with-space ()
             (pine.keep:open file)
             (pine.self:claim)
             (setf id (pine.ns:read /self/id))
             (pine.keep:close))
           ;; a fresh image, knowing nothing but the file
           (pine.ns:with-space ()
             (pine.keep:open file)
             (is (string= id (pine.ns:read /self/id)))
             (pine.self:claim)
             (is (string= id (pine.ns:read /self/id))
                 "claiming again does not make a second identity")
             (pine.keep:close)))
      (%forget-file file))))

;;;; the actor system's own timer

(test the-scheduler-is-on-and-fires
  "Every interval in pine belongs on sento's wheel timer. It was switched off,
and hand-rolled loops filled the hole."
  (let ((server (pine.core.server:start-server :workers 1)))
    (unwind-protect
         (let ((timer (sento.actor-system:scheduler
                       (pine.core.server:actor-system server)))
               (fired 0))
           (is-true timer "the actor system has a scheduler")
           (sento.wheel-timer:schedule-once timer 0.1 (lambda () (incf fired)))
           (sento.wheel-timer:schedule-recurring timer 0.1 0.1
                                                 (lambda () (incf fired)) :probe)
           (sleep 0.5)
           (sento.wheel-timer:cancel timer :probe)
           (is (> fired 1) "one shot and a recurring one both ran, saw ~d" fired))
      (pine.core.server:stop-server server))))
