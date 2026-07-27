(in-package :pine.test)

(def-suite* :pine.state :in :pine)

;;;; store

(test lisp-values-round-trip-through-the-store
  (with-fixture memory-store ()
    (setf (pine.state.world:value :n) 42
          (pine.state.world:value :f) 0.5
          (pine.state.world:value :s) "hi"
          (pine.state.world:value :list) '(1 (2 3) "four" :five)
          (pine.state.world:value (list :place "/x")) '(3 7))
    (is (= 42 (pine.state.world:value :n)))
    (is (= 0.5 (pine.state.world:value :f)))
    (is (string= "hi" (pine.state.world:value :s)))
    (is (equal '(1 (2 3) "four" :five) (pine.state.world:value :list)))
    (is (equal '(3 7) (pine.state.world:value (list :place "/x"))))
    (is (eq :none (pine.state.world:value :missing :none)))))

(test an-unstorable-value-fails-at-the-write
  (with-fixture memory-store ()
    (signals error
      (setf (pine.state.world:value :fn) (lambda () nil)))))

(test forgetting-a-key-drops-it
  (with-fixture memory-store ()
    (setf (pine.state.world:value :gone) 1)
    (pine.state.world:forget :gone)
    (is (null (pine.state.world:value :gone)))))

(test a-bounded-list-keeps-order-uniqueness-and-its-cap
  (with-fixture memory-store ()
    (dolist (v '("a" "b" "c")) (pine.state.world:push :log v))
    (is (equal '("c" "b" "a") (pine.state.world:items :log)))
    (pine.state.world:push :log "a")
    (is (equal '("a" "c" "b") (pine.state.world:items :log)))
    (is (equal '("a" "c") (pine.state.world:items :log :limit 2)))
    (dotimes (i 5) (pine.state.world:push :trim (format nil "t~d" i) :max 3))
    (is (equal '("t4" "t3" "t2") (pine.state.world:items :trim)))
    (pine.state.world:clear :log)
    (is (null (pine.state.world:items :log)))))

(test a-list-may-hold-duplicates-when-asked
  (with-fixture memory-store ()
    (dotimes (i 3) (pine.state.world:push :dup "x" :unique nil))
    (is (equal '("x" "x" "x") (pine.state.world:items :dup)))))

(test a-fresh-connection-sees-what-was-written
  (let ((path "/tmp/pine-test-reopen.db"))
    (open-fresh-store path)
    (setf (pine.state.world:value :kept) "yes")
    (pine.state.world:open path)
    (is (string= "yes" (pine.state.world:value :kept)))
    (open-fresh-store)))

(test a-closed-store-answers-defaults-and-swallows-no-writes
  (pine.state.world:close)
  (is (eq :closed (pine.state.world:value :anything :closed)))
  (finishes (setf (pine.state.world:value :anything) "zz"))
  (is (null (pine.state.world:items :anything)))
  (open-fresh-store))

;;;; refs

(test a-ref-reads-and-writes-through-one-accessor
  (is (= 41 (setf (pine.state.ref:ref :probe-ref) 41)))
  (is (= 41 (pine.state.ref:ref :probe-ref)))
  (is (eq :dflt (pine.state.ref:ref :probe-absent :dflt))))

(test an-equal-write-is-deduped
  (let ((ref (pine.state.ref:make-ref :name :probe-dedup :value 1))
        (hits 0))
    (pine.state.ref:ref-subscribe ref (lambda () (incf hits)))
    (is-true (pine.state.ref:set-ref ref 2))
    (is (= 1 hits))
    (is-false (pine.state.ref:set-ref ref 2))
    (is (= 1 hits))))

(test update-ref-applies-a-function-to-the-old-value
  (let ((ref (pine.state.ref:make-ref :name :probe-update :value 5)))
    (pine.state.ref:update-ref ref #'1+)
    (is (= 6 (pine.state.ref:deref ref)))))

(test unsubscribing-stops-the-callback
  (let* ((ref (pine.state.ref:make-ref :name :probe-unsub :value 0))
         (hits 0)
         (id (pine.state.ref:ref-subscribe ref (lambda () (incf hits)))))
    (pine.state.ref:set-ref ref 1)
    (pine.state.ref:ref-unsubscribe ref id)
    (pine.state.ref:set-ref ref 2)
    (is (= 1 hits))))

(test a-broken-subscriber-does-not-stop-the-others
  (let ((ref (pine.state.ref:make-ref :name :probe-broken :value 0))
        (ran nil))
    (pine.state.ref:ref-subscribe ref (lambda () (error "probe")))
    (pine.state.ref:ref-subscribe ref (lambda () (setf ran t)))
    (let ((*error-output* (make-broadcast-stream)))
      (pine.state.ref:set-ref ref 1))
    (is-true ran)))

(test a-view-re-renders-only-for-the-refs-it-read
  (let ((hits 0) (view nil))
    (setf (pine.state.ref:ref :probe-view) 1)
    (setf (pine.state.ref:ref :probe-unread) 1)
    (setf view (pine.state.ref:make-view
                (lambda () (pine.state.ref:ref :probe-view))
                (lambda () (incf hits) (pine.state.ref:render-view view))))
    (is (= 1 (pine.state.ref:render-view view)))
    (setf (pine.state.ref:ref :probe-unread) 2)
    (is (= 0 hits))
    (setf (pine.state.ref:ref :probe-view) 2)
    (is (= 1 hits))
    (setf (pine.state.ref:ref :probe-view) 2)
    (is (= 1 hits))
    (pine.state.ref:dispose-view view)
    (setf (pine.state.ref:ref :probe-view) 3)
    (is (= 1 hits))))

(test a-persistent-ref-reseeds-from-the-store
  (with-fixture memory-store ()
    (let ((ref (pine.state.ref:make-ref :name :probe-persist :value 0 :persist t)))
      (pine.state.ref:set-ref ref 9))
    (is (= 9 (pine.state.world:value '(:ref :probe-persist))))
    (remhash :probe-persist pine.state.ref::*refs*)
    (is (= 9 (pine.state.ref:deref
              (pine.state.ref:make-ref :name :probe-persist :value 0 :persist t))))))

;;;; variables

(test an-undeclared-variable-is-an-error-not-a-nil
  (signals error (pine.state.var:var :never-declared)))

(test a-variable-falls-back-to-its-default
  (pine.state.var:defonce :probe-default :default 7)
  (is (= 7 (pine.state.var:var :probe-default)))
  (is (eq :default (pine.state.var:variable-scope :probe-default))))

(test setting-the-global-shadows-the-default
  (pine.state.var:defonce :probe-global :default 1)
  (setf (pine.state.var:var :probe-global) 2)
  (is (= 2 (pine.state.var:var :probe-global)))
  (is (eq :global (pine.state.var:variable-scope :probe-global))))

(test redeclaring-updates-the-default-and-keeps-the-value
  (pine.state.var:defonce :probe-redeclare :default 1)
  (setf (pine.state.var:var :probe-redeclare) 5)
  (pine.state.var:defonce :probe-redeclare :default 99 :documentation "second")
  (is (= 5 (pine.state.var:var :probe-redeclare)))
  (is (= 99 (pine.state.var:evar-default
             (pine.state.var:find-variable :probe-redeclare))))
  (is (string= "second" (pine.state.var:evar-documentation
                         (pine.state.var:find-variable :probe-redeclare)))))

(test a-buffer-local-value-outranks-the-global
  (with-fixture substrate ()
    (pine.state.var:defonce :probe-local :default 1)
    (setf (pine.state.var:var :probe-local) 2)
    (let ((buf (pine.editor.frame::make-buffer "var-probe")))
      (setf (pine.state.var:var :probe-local buf) 3)
      (sleep 0.1)
      (is (= 3 (pine.state.var:var :probe-local buf)))
      (is (= 2 (pine.state.var:var :probe-local)))
      (is (eq :buffer (pine.state.var:variable-scope :probe-local buf)))
      (is (eq :global (pine.state.var:variable-scope :probe-local))))))

(test a-persistent-variable-reseeds-from-the-store
  (with-fixture memory-store ()
    (pine.state.var:defonce :probe-var-persist :default 1 :persist t)
    (setf (pine.state.var:var :probe-var-persist) 5)
    (is (= 5 (pine.state.world:value '(:var :probe-var-persist))))
    (remhash :probe-var-persist pine.state.var::*variables*)
    (pine.state.var:defonce :probe-var-persist :default 1 :persist t)
    (is (= 5 (pine.state.var:var :probe-var-persist)))))

;;;; the world
;;;;
;;;; A subsystem answers SNAPSHOT and REVIVE for its own name; the method is
;;;; the registration. These probes hold their halves in specials, so a test
;;;; says what each does without defining methods while it runs.

(defvar *probe-save* nil "Plist name -> thunk answering that name's snapshot.")
(defvar *probe-revive* nil "Plist name -> function of the data.")

(defmacro define-probe (name)
  `(progn
     (defmethod pine.state.world:snapshot ((n (eql ,name)))
       (let ((fn (getf *probe-save* ,name))) (and fn (funcall fn))))
     (defmethod pine.state.world:revive ((n (eql ,name)) data)
       (let ((fn (getf *probe-revive* ,name))) (when fn (funcall fn data))))))

(define-probe :probe-a)
(define-probe :probe-b)
(define-probe :probe-bad)
(define-probe :probe-after)
(define-probe :probe-nil)
(define-probe :probe-gate)

(defmacro with-probes ((&rest specs) &body body)
  "Bind each (NAME :save FN :restore FN) for the extent of BODY."
  `(let ((*probe-save* (list ,@(loop :for (name . plist) :in specs
                                     :append (list name (getf plist :save)))))
         (*probe-revive* (list ,@(loop :for (name . plist) :in specs
                                       :append (list name (getf plist :restore))))))
     ,@body))

(test the-world-saves-and-restores-through-the-store
  (with-fixture memory-store ()
    (setf pine.state.world:*enabled* t)
    (let ((a nil) (b nil))
      (unwind-protect
           (with-probes ((:probe-a :save (lambda () :init)
                                   :restore (lambda (d) (setf a d)))
                         (:probe-b :save (lambda () (list 1 2))
                                   :restore (lambda (d) (setf b d))))
             (pine.state.world:save :probe-a)
             (pine.state.world:save :probe-b)
             (is (eq :init (pine.state.world:value :probe-a)))
             (pine.state.world:restore :probe-a)
             (pine.state.world:restore :probe-b)
             (is (eq :init a))
             (is (equal '(1 2) b)))
        (setf pine.state.world:*enabled* nil)))))

(test a-throwing-restore-skips-without-breaking-the-rest
  (with-fixture memory-store ()
    (setf pine.state.world:*enabled* t)
    (let ((reached nil))
      (unwind-protect
           (let ((*error-output* (make-broadcast-stream)))
             (with-probes ((:probe-bad :save (lambda () :x)
                                       :restore (lambda (d) (declare (ignore d))
                                                  (error "boom")))
                           (:probe-after :save (lambda () :y)
                                         :restore (lambda (d) (setf reached d))))
               (pine.state.world:save)
               (pine.state.world:restore)
               (is (eq :y reached))))
        (setf pine.state.world:*enabled* nil)))))

(test a-nil-save-keeps-the-previous-entry
  (with-fixture memory-store ()
    (setf pine.state.world:*enabled* t)
    (unwind-protect
         (progn
           (with-probes ((:probe-nil :save (lambda () :first)))
             (pine.state.world:save :probe-nil))
           (with-probes ((:probe-nil :save (lambda () nil)))
             (pine.state.world:save :probe-nil))
           (is (eq :first (pine.state.world:value :probe-nil))))
      (setf pine.state.world:*enabled* nil))))

(test the-world-gate-stops-both-directions
  (with-fixture memory-store ()
    (let ((restored :untouched))
      (unwind-protect
           (with-probes ((:probe-gate :save (lambda () :fresh)
                                      :restore (lambda (d) (setf restored d))))
             (setf pine.state.world:*enabled* t)
             (setf (pine.state.world:value :probe-gate) :stale)
             (setf pine.state.world:*enabled* nil)
             (pine.state.world:save :probe-gate)
             (is (eq :stale (pine.state.world:value :probe-gate)))
             (pine.state.world:restore :probe-gate)
             (is (eq :untouched restored)))
        (setf pine.state.world:*enabled* nil)))))

;;;; Which buffers the world keeps. A tool buffer is a projection of live state
;;;; -- the debugger's frames, the supervisor's table -- so its text means
;;;; nothing on the next start, and restoring it puts a stale view on screen
;;;; over state that no longer exists.

(test the-world-keeps-written-buffers-and-drops-tool-buffers
  (with-fixture substrate ()
    (let ((pine.state.world:*enabled* t))
      (pine.editor.frame::make-buffer "notes")
      (sento.actor:tell (pine.editor.frame::buffer "notes")
                        (list :replace-content :content "kept"))
      (pine.editor.frame::make-buffer "*debugger*")
      (sento.actor:tell (pine.editor.frame::buffer "*debugger*")
                        (list :replace-content :content "dropped"))
      (sleep 0.3)
      (pine.state.world:save :buffers)
      (let ((names (mapcar (lambda (e) (getf e :name))
                           (pine.state.world:value :buffers))))
        (is (member "notes" names :test #'equal)
            "a buffer someone wrote in should be kept, saw ~s" names)
        (is (not (member "*debugger*" names :test #'equal))
            "a tool buffer should not be kept, saw ~s" names)))))
