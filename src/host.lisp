(defpackage #:pine.host
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:serve #:mount #:server #:pine #:unmount #:*timeout*))

(in-package #:pine.host)
(named-readtables:in-readtable pine.path:syntax)

;;;; Another pine, under a prefix.
;;;;
;;;; A mounted pine is mirrored, not queried. The far side says what each commit
;;;; moved and those values land here under the prefix, so a read of
;;;; /host/box/audio/volume is the same wait-free slot read that a read of
;;;; /audio/volume is, and watching one is an ordinary local watch. Writes go
;;;; the other way as messages carrying their options, so :keep, :max and :when
;;;; mean under a prefix what they mean bare. Only text crosses, in the
;;;; serialization the store uses.

(defstruct (asker (:constructor %asker (ref prefix uri)) (:copier nil))
  ref prefix uri)

(defstruct (mounted (:constructor %mounted (ref inbox uri)) (:copier nil))
  ref inbox uri (prefix nil))

(defparameter *timeout* 5
  "Seconds to wait on another pine before saying it did not answer.")

(defvar *landing* nil
  "True while what the other pine said is being put into the tree, so it is not
sent straight back to it as a write of ours.")

(defun %out (value) (pine.data:serialize value))

(defun %in (text) (and text (pine.data:deserialize text 'p:data)))

(defun %where (text)
  "HOST:PORT as (values host port). Bare text is a host on the default port."
  (let ((colon (position #\: text)))
    (if colon
        (values (subseq text 0 colon)
                (or (parse-integer (subseq text (1+ colon)) :junk-allowed t)
                    pine.core.server:*port*))
        (values text pine.core.server:*port*))))

(defun %ask (ref message)
  (let ((answer (pine.core.actor:ask ref message :timeout *timeout*)))
    (when (and (consp answer) (eq :error (first answer)))
      (error "the other pine refused: ~a" (second answer)))
    answer))

(defun %mounts ()
  "Prefix text to what is mounted there. A mount is a subscription on another
pine and an actor here, neither of which is a value, so it is not in the tree.
The space keeps it: two pines in one image mount their own."
  (or (ns:kept :host)
      (setf (ns:kept :host)
            (sento.atomic:make-atomic-reference :value (fset:empty-map)))))

;;;; This side: answering for our own namespace.

(defun %mine-p (path)
  "Whether PATH is this pine's own to say anything about. /host is not: what
stands there is another pine's tree, mirrored, and that pine already knows it."
  (not (p:prefixp /host path)))

(defun %held-change-p (path new)
  "Whether the tree took this change.

A provider's value is not in the tree, and a subscription starts from what is
held, so a snapshot of a live path crossing on a write would put a value on the
far side that it could not have got any other way and that nothing here will
correct."
  (let ((held (ns:held path)))
    (if (null new) (null held) (eq held new))))

(defun %shipped (moved prefix &key settled)
  "MOVED as what an asker is told: each path under its own PREFIX, and each
value as text. Code does not cross, so a value that will not serialize is
shipped as nothing.

SETTLED says these values came out of the tree already, which is what walking it
gives, so there is nothing to ask about them. Without it each one is checked,
and that check is a walk of the tree: it belongs on the handful of paths a commit
moved, not on every path there is."
  (loop :for change :in moved
        :for path := (first change)
        :for new := (third change)
        :when (and (%mine-p path)
                   (or settled (%held-change-p path new))
                   (pine.store:storablep new))
          :collect (list (p:text (p:path prefix (p:spliced path))) (%out new))))

(defun %telling (askers)
  (lambda (moved)
    (dolist (asker (sento.atomic:atomic-get askers))
      (let ((rows (%shipped moved (asker-prefix asker))))
        (when rows
          (ignore-errors
           (sento.actor:tell (asker-ref asker) (list :moved rows))))))))

(defun %answer (message)
  "What this pine says to MESSAGE, as text. A condition here is the asker's to
hear about: it asked about a path, and that a path refused is an answer."
  (handler-case
      (destructuring-bind (verb &optional path value options) message
        (ecase verb
          (:read (%out (ns:read (p:parse path))))
          (:write (ns:put (p:parse path) (%in value) (%in options))
                  (%out t))
          (:doc (%out (ns:doc (p:parse path))))
          (:ls (%out (mapcar #'p:text
                             (pine.data:keys
                              (ns:read (p:path (p:parse path) (p:any)))))))))
    (error (c) (list :error (princ-to-string c)))))

(defun %everything ()
  "What this pine holds, as a MOVED list, so a fresh subscription starts from
the whole tree. Held, not read: asking would run every provider on this machine
because somebody mounted us, and a live path arrives when it moves."
  (labels ((subtreep (value)
             ;; a map keyed by paths is a transaction: what /key/wm/S-Q holds is
             ;; the write that key makes, one value at one path, not two places
             ;; named /wm and /sh under it
             (and (fset:map? value) (not (p:transactionp value))))
           (walk (path value acc)
             (if (subtreep value)
                 (let ((out acc))
                   (fset:do-map (key inner value)
                     (setf out (walk (p:path path (p:name key)) inner out)))
                   out)
                 (cons (list path nil value) acc))))
    (walk (p:root) (or (ns:held (p:root)) (fset:empty-map)) nil)))

(defun serve (system)
  "Make this image's namespace reachable by another pine: reads, writes and doc,
and a subscription naming an actor of its own and the prefix it mounted us at."
  (let ((askers (sento.atomic:make-atomic-reference :value nil)))
    (setf (ns:on-commit :host) (%telling askers))
    (sento.actor-context:actor-of system
      :name "ns"
      :dispatcher :pinned
      :receive
      (lambda (message)
        (case (first message)
          (:subscribe
           (destructuring-bind (uri prefix) (rest message)
             (let ((asker (%asker (sento.remoting:make-remote-ref system uri)
                                  (p:parse prefix) uri)))
               ;; keyed by the asker's own actor, so mounting again replaces
               ;; rather than doubling what it is told
               (sento.atomic:atomic-swap
                askers
                (lambda (all) (cons asker (remove uri all :key #'asker-uri
                                                          :test #'string=))))
               (sento.actor:reply
                (%out (%shipped (%everything) (asker-prefix asker)
                                :settled t))))))
          (:unsubscribe
           (let ((uri (second message)))
             (sento.atomic:atomic-swap
              askers
              (lambda (all) (remove uri all :key #'asker-uri :test #'string=)))
             (sento.actor:reply (%out t))))
          (t (sento.actor:reply (%answer message))))))))

;;;; The other side: what a mounted pine is here.

(defun %land (rows)
  "Put what the other pine said into the tree. :FORCE, because the far side
already decided the write was allowed."
  (when rows
    (let ((*landing* t))
      (dolist (row rows)
        (destructuring-bind (text value) row
          (ns:put (p:parse text) (%in value) (list :force t)))))))

(defun %inbox (system name space)
  "The actor a mounted pine tells what moved. It lands into SPACE, the one that
did the mounting: WITH-SPACE sets rather than binds, and this runs on a thread
that would never see a binding."
  (sento.actor-context:actor-of system
    :name (format nil "host-~a" name)
    :dispatcher :pinned
    :receive (lambda (message)
               (when (eq :moved (first message))
                 (let ((ns:*space* space))
                   (pine.err:attempt (lambda () (%land (second message)))
                                     (format nil "what ~a said moved" name)))))))

(defun %detach (prefix-text)
  "Take down whatever is mounted at PREFIX-TEXT: stop hearing from it, and stop
it holding an actor here for news that is no longer wanted."
  (let ((m (fset:lookup (sento.atomic:atomic-get (%mounts)) prefix-text)))
    (when m
      (ignore-errors (%ask (mounted-ref m) (list :unsubscribe (mounted-uri m))))
      (ignore-errors (sento.actor:tell (mounted-inbox m) :stop))
      (sento.atomic:atomic-swap
       (%mounts) (lambda (all) (fset:less all prefix-text))))
    m))

(defun %up (m prefix)
  "Subscribe and land what the other pine already holds, under PREFIX: the path
the provider was mounted at, which is the only thing that knows it."
  (%detach (p:text prefix))
  (setf (mounted-prefix m) prefix)
  (sento.atomic:atomic-swap
   (%mounts) (lambda (all) (fset:with all (p:text prefix) m)))
  (%land (%in (%ask (mounted-ref m)
                    (list :subscribe (mounted-uri m) (p:text prefix))))))

(defun %attach (system ref name)
  "What a mounted pine is here. Nothing is subscribed yet: that happens when the
provider is mounted, and where it is mounted is what says the prefix."
  (let ((flat (string-downcase (string name))))
    (%mounted ref (%inbox system flat ns:*space*)
              (pine.core.server:local-uri
               (format nil "host-~a" flat)
               (sento.remoting:remoting-port system)))))

(defun provider (m)
  "What /host/NAME is: values that are here, and writes that go there.

There is no :READ clause. The mirror already landed the value, so reading a
mounted path is the ordinary read of a held path. :OUT says the subtree is
spoken for, and a path the other pine has never committed reads as nothing,
which is the honest answer. :IN lands a write as soon as the other pine takes
it, rather than only when the subscription comes round; what arrives on the
subscription writes over it, so where the two differ the other pine is right."
  (ns:provider
   {:mounted (lambda (at) (%up m at))}
   (/host/?name/?@rest
    {:out (pine.data:fn [held] held)
     :in (pine.data:fn [v]
           (unless *landing*
             (%ask (mounted-ref m)
                   (list :write (p:text (apply #'p:path rest)) (%out v)
                         (%out (ns:options)))))
           v)
     :doc (pine.data:fn []
            (%in (%ask (mounted-ref m)
                       (list :doc (p:text (apply #'p:path rest))))))})
   (/host/?name {:doc "another pine, as a subtree"})))

(defun pine (where &key name
                        (system (and pine.core.server:*server*
                                     (pine.core.server:actor-system
                                      pine.core.server:*server*))))
  "The pine at WHERE, as a provider: (write /host/box (pine \"box:17000\")).
Distribution is a prefix, so another pine is a provider like a mixer is."
  (multiple-value-bind (host port) (%where where)
    (unless system
      (error "there is no actor system here to reach ~a from." where))
    (provider (%attach system
                       (sento.remoting:make-remote-ref
                        system (pine.core.server:daemon-uri "ns" :host host
                                                                 :port port))
                       (or name host)))))

(defun mount (name &key system host port)
  "Reach the pine at HOST:PORT as NAME, so /host/NAME/... is its namespace."
  (let ((ref (sento.remoting:make-remote-ref
              system (pine.core.server:daemon-uri "ns" :host host :port port))))
    (ns:write (p:path /host (string-downcase (string name)))
              (provider (%attach system ref name)))
    ref))

(defun unmount (name)
  "Take the pine mounted as NAME down: the provider comes off the tree, the
subscription ends, and the actor that was hearing about it goes."
  (let ((at (p:path /host (string-downcase (string name)))))
    (%detach (p:text at))
    (ns:unmount at)))

(defclass server (ns:server) ()
  (:default-initargs :name :host :serves (list /host))
  (:documentation "Other pines, and being one. Distribution is a prefix: this
says what the prefix means, and it answers for this image's own namespace so
another pine can put it under theirs."))

(defmethod ns:raise ((s server) &key system &allow-other-keys)
  (ns:write /host
            (ns:provider
             ;; writing nil here is how a mount comes down, and a mount is a
             ;; subscription on another pine and an actor here. Neither goes
             ;; away because the provider did, so this is where they are told.
             (/host/?name
              {:in (pine.data:fn [v]
                     (unless v (%detach (p:text (p:path /host name))))
                     v)
               :doc "another pine, as a subtree"})
             (/host {:doc "other pines"})))
  (when system (serve system))
  ;; what the space keeps is the mounts, not the actor: an actor is this
  ;; image's and the mounts are this pine's
  (sento.atomic:make-atomic-reference :value (fset:empty-map)))

(ns:register (make-instance 'server))
