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
              (pine.host:answer-for system)
              (pine.host:mount :probe :system system
                                      :host pine.core.server:*host*
                                      :port +host-port+)
              ,@body))
       (pine.core.server:stop-server server))))

(test another-pine-is-a-provider-a-config-writes
  "Distribution is a prefix, and a config says so with a write: (write
/host/box (pine \"box:17000\")). Nothing about /host/box/... is different from
the paths it mirrors."
  (let ((server (pine.core.server:start-server :workers 2
                                               :remoting-port +host-port+)))
    (unwind-protect
         (pine.ns:with-space ()
           (let ((system (pine.core.server:actor-system server)))
             (pine.host:answer-for system)
             (pine.ns:write /theme :ef-dream)
             (pine.ns:write /host/probe
                            (pine.host:pine
                             (format nil "~a:~d" pine.core.server:*host*
                                     +host-port+)
                             :system system))
             (is (eq :ef-dream (pine.ns:read /host/probe/theme))
                 "a path on the other pine did not read through the prefix")
             (pine.ns:write /host/probe/tab-width 7)
             (is (= 7 (pine.ns:read /tab-width))
                 "a write through the prefix did not land on the other pine")))
      (pine.core.server:stop-server server))))

(test a-place-with-a-port-in-it-is-taken-apart
  (is (equal '("box" 17000)
             (multiple-value-list (pine.host::%where "box:17000"))))
  (is (equal (list "box" pine.core.server:*port*)
             (multiple-value-list (pine.host::%where "box")))
      "a bare host is that host on the port a pine listens on"))

(defmacro eventually (form &rest reason)
  "Wait for FORM, which the other pine has to say before it is true.

A mounted pine is mirrored rather than asked, so what it holds arrives; it is
not fetched. That is what makes reading one a slot read like any other, and it
is the price: a value written there is here in a moment, not in this call."
  `(progn (wait-for (lambda () ,form) :seconds 5)
          (is ,form ,@reason)))

(test a-path-on-another-pine-reads
  (with-host
    (pine.ns:write /audio/volume 40)
    (eventually (eql 40 (pine.ns:read /host/probe/audio/volume)))))

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
    (eventually
     (= 2 (length (pine.data:keys (pine.ns:read /host/probe/proc/*)))))))

(test what-is-not-there-is-not-there-on-the-other-side-either
  (with-host
    (is (null (pine.ns:read /host/probe/nothing/at/all)))))

(test a-pine-that-does-not-answer-says-so
  (pine.ns:with-space ()
    (let ((server (pine.core.server:start-server :workers 1 :remoting-port 17062)))
      (unwind-protect
           (let ((pine.host:*timeout* 1))
             ;; mounting is where a pine that is not there says so: the mount
             ;; is a subscription, so it is the first thing that has to reach it
             (signals error
               (pine.host:mount :absent
                                :system (pine.core.server:actor-system server)
                                :host pine.core.server:*host*
                                :port 17099)))
        (pine.core.server:stop-server server)))))

(test the-daemon-serves-its-own-namespace
  "A pine that can reach others and cannot be reached is half a pine.

The daemon answers for its namespace as soon as /host goes up, so mounting
itself under /host is the same code path as mounting another machine: two
pines, one of which happens to be this one."
  (with-fixture substrate ()
    (ensure-remoting)
    (pine.ns:write /theme :ef-dream)
    (unwind-protect
         (progn
           (pine.ns:write /host/self
                          (pine.host:pine
                           (format nil "~a:~d" pine.core.server:*host*
                                   (pine.core.server:remoting-port *server*))
                           :system (pine.core.server:actor-system *server*)))
           (eventually (eq :ef-dream (pine.ns:read /host/self/theme))
                       "the daemon did not answer for its own namespace")
           (pine.ns:write /host/self/tab-width 7)
           (is (= 7 (pine.ns:read /tab-width))
               "a write through the prefix did not land"))
      (pine.host:unmount :self))))

;;;; Invariant 5: within and without are one code path.
;;;;
;;;; "A second pine on this machine and a second pine on another are the same
;;;; code path. If one of those works and the other does not, the abstraction
;;;; has failed." Reading a local path is a slot read, wait-free and safe from
;;;; anywhere. These ask whether reading a mounted one is the same thing.

(test a-path-on-another-pine-is-watched
  "Watch is one of the three verbs, and distribution is a prefix, so a path on
another pine is followed the way any path is."
  (with-host
    (let ((heard nil))
      (pine.ns:watch /host/probe/audio/volume
                     (lambda (value) (push value heard) (fset:empty-map))
                     :as :probe-remote-watch)
      (pine.ns:write /audio/volume 40)
      (wait-for (lambda () heard) :seconds 5)
      (is (member 40 heard) "a change on the other pine did not reach the watch"))))

(test a-pattern-read-across-a-prefix-answers-prefixed-paths
  "Everything in the API works under a prefix, so the map a pattern answers is
keyed by the paths the asker asked about, not the ones the far side holds."
  (with-host
    (pine.ns:write /audio {:volume 40 :muted nil})
    (let ((answer (pine.ns:read /host/probe/audio/*)))
      (is (fset:map? answer))
      (is (every (lambda (key) (pine.path:prefixp /host/probe key))
                 (pine.data:keys answer))
          "the keys came back as the other pine's paths: ~a"
          (mapcar #'pine.path:text (pine.data:keys answer))))))

(test a-mounted-path-reads-from-inside-a-watch
  "A watch reaction runs on whatever thread committed. A local read is safe
there; a mounted one has to be too, or half of pine cannot use the far half of
its own interface."
  (with-host
    (pine.ns:write /audio/volume 40)
    (wait-for (lambda () (pine.ns:read /host/probe/audio/volume)) :seconds 5)
    (let ((seen :never-ran))
      (pine.ns:watch /trigger
                     (lambda (value)
                       (declare (ignore value))
                       (setf seen (ignore-errors (pine.ns:read /host/probe/audio/volume)))
                       (fset:empty-map))
                     :as :probe-read-in-watch)
      (pine.ns:write /trigger t)
      (is (eql 40 seen) "reading a mounted path from inside a watch answered ~s" seen))))

(test a-mounted-path-reads-from-inside-a-receive
  "An actor's receive is the other place a local read is safe and a blocking
ask is refused outright."
  (with-host
    (pine.ns:write /audio/volume 40)
    (wait-for (lambda () (pine.ns:read /host/probe/audio/volume)) :seconds 5)
    (let ((answer (sento.actor:ask-s
                   (sento.actor-context:actor-of
                    system :name "probe-remote-reader"
                    :dispatcher :pinned
                    :receive (lambda (msg)
                               (declare (ignore msg))
                               (sento.actor:reply
                                (list (ignore-errors
                                       (pine.ns:read /host/probe/audio/volume))))))
                   '(:go) :time-out 5)))
      (is (eql 40 (first answer))
          "reading a mounted path from inside a receive answered ~s" answer))))

(test write-options-cross-a-prefix
  "A write's options are part of the write. :max under a prefix makes a ring on
the other pine, the way it does bare."
  (with-host
    (dolist (word '("one" "two" "three"))
      (pine.ns:write /host/probe/kill word :max 2 :keep nil))
    (is (string= "three" (pine.ns:read /kill)))
    (is (= 2 (fset:size (pine.ns:read /kill/*)))
        "the ring bound did not cross: ~d entries"
        (fset:size (pine.ns:read /kill/*)))))

(test a-transaction-spans-two-pines
  "The tree is one persistent map, so a multi-path write is one new root. A map
whose keys have different prefixes is the same sentence across two pines."
  (with-host
    (pine.ns:write {/tab-width 4 /host/probe/format-on-save t})
    (is (= 4 (pine.ns:read /tab-width)))
    (is (eq t (pine.ns:read /host/probe/format-on-save))
        "the half of the transaction that crossed did not land")))

(test what-a-mounted-path-takes-crosses
  "/doc is how you ask what a path is and what it takes, and a mounted path is
a path."
  (with-host
    (pine.ns:up :doc)
    (pine.ns:write /audio (pine.provider.pipewire:pipewire))
    (is (pine.ns:read /doc/host/probe/audio/volume)
        "a mounted path said nothing about itself")))
