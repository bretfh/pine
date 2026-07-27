(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.host :in :pine)

(defparameter +host-port+ 17061
  "The port the host suite's pine listens on.")

(defmacro with-host (&body body)
  "A pine serving its own namespace, reached as /host/probe over the socket
another pine would use."
  `(let ((server (pine.core.server:start-server :workers 2
                                                :remoting-port +host-port+)))
     (unwind-protect
          (pine.ns:with-space ()
            (let ((system (pine.core.server:actor-system server)))
              (pine.host:serve system)
              (pine.host:mount :probe :system system
                                      :host pine.core.server:*host*
                                      :port +host-port+)
              ,@body))
       (pine.core.server:stop-server server))))

(test a-path-on-another-pine-reads
  (with-host
    (pine.ns:write /audio/volume 40)
    (is (= 40 (pine.ns:read /host/probe/audio/volume)))))

(test a-path-on-another-pine-is-written
  (with-host
    (pine.ns:write /host/probe/theme :ef-dream)
    (is (eq :ef-dream (pine.ns:read /theme))
        "the write crossed and landed")))

(test everything-the-data-model-holds-survives-the-crossing
  (with-host
    (dolist (value (list 42 "text" :keyword {:a 1 :b [1 2]} #{:x :y} /buf/scratch))
      (pine.ns:write /host/probe/probe value)
      (is (fset:equal? value (pine.ns:read /host/probe/probe))
          "~a did not survive" value))))

(test a-map-crosses-as-a-directory
  (with-host
    (pine.ns:write /host/probe/mode/lisp {:parent :prog :indicator "Lisp"})
    (is (eq :prog (pine.ns:read /mode/lisp/parent)))
    (is (eq :prog (pine.ns:read /host/probe/mode/lisp/parent)))))

(test a-listing-crosses
  (with-host
    (pine.ns:write /proc {:editor {:state :running} :desktop {:state :running}})
    (let ((names (pine.data:keys (pine.ns:read /host/probe/proc/*))))
      (is (= 2 (length names))))))

(test what-is-not-there-is-not-there-on-the-other-side-either
  (with-host
    (is (null (pine.ns:read /host/probe/nothing/at/all)))))

(test a-pine-that-does-not-answer-says-so
  (pine.ns:with-space ()
    (let ((server (pine.core.server:start-server :workers 1 :remoting-port 17062)))
      (unwind-protect
           (let ((pine.host:*timeout* 1))
             (pine.host:mount :absent
                              :system (pine.core.server:actor-system server)
                              :host pine.core.server:*host*
                              :port 17099)
             (signals error (pine.ns:read /host/absent/anything)))
        (pine.core.server:stop-server server)))))
