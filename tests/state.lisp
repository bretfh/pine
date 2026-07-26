(in-package :pine.test)

(def-suite* :pine.state :in :pine)

;;;; store

(test lisp-values-round-trip-through-the-store
  (with-fixture memory-store ()
    (setf (pine.state.store:store :n) 42
          (pine.state.store:store :f) 0.5
          (pine.state.store:store :s) "hi"
          (pine.state.store:store :list) '(1 (2 3) "four" :five)
          (pine.state.store:store (list :place "/x")) '(3 7))
    (is (= 42 (pine.state.store:store :n)))
    (is (= 0.5 (pine.state.store:store :f)))
    (is (string= "hi" (pine.state.store:store :s)))
    (is (equal '(1 (2 3) "four" :five) (pine.state.store:store :list)))
    (is (equal '(3 7) (pine.state.store:store (list :place "/x"))))
    (is (eq :none (pine.state.store:store :missing :none)))))

(test an-unstorable-value-fails-at-the-write
  (with-fixture memory-store ()
    (signals error
      (setf (pine.state.store:store :fn) (lambda () nil)))))

(test forgetting-a-key-drops-it
  (with-fixture memory-store ()
    (setf (pine.state.store:store :gone) 1)
    (pine.state.store:store-forget :gone)
    (is (null (pine.state.store:store :gone)))))

(test a-bounded-list-keeps-order-uniqueness-and-its-cap
  (with-fixture memory-store ()
    (dolist (v '("a" "b" "c")) (pine.state.store:store-push :log v))
    (is (equal '("c" "b" "a") (pine.state.store:store-items :log)))
    (pine.state.store:store-push :log "a")
    (is (equal '("a" "c" "b") (pine.state.store:store-items :log)))
    (is (equal '("a" "c") (pine.state.store:store-items :log :limit 2)))
    (dotimes (i 5) (pine.state.store:store-push :trim (format nil "t~d" i) :max 3))
    (is (equal '("t4" "t3" "t2") (pine.state.store:store-items :trim)))
    (pine.state.store:store-clear :log)
    (is (null (pine.state.store:store-items :log)))))

(test a-list-may-hold-duplicates-when-asked
  (with-fixture memory-store ()
    (dotimes (i 3) (pine.state.store:store-push :dup "x" :unique nil))
    (is (equal '("x" "x" "x") (pine.state.store:store-items :dup)))))

(test a-fresh-connection-sees-what-was-written
  (let ((path "/tmp/pine-test-reopen.db"))
    (open-fresh-store path)
    (setf (pine.state.store:store :kept) "yes")
    (pine.state.store:open-store path)
    (is (string= "yes" (pine.state.store:store :kept)))
    (open-fresh-store)))

(test a-closed-store-answers-defaults-and-swallows-no-writes
  (pine.state.store:close-store)
  (is (eq :closed (pine.state.store:store :anything :closed)))
  (finishes (setf (pine.state.store:store :anything) "zz"))
  (is (null (pine.state.store:store-items :anything)))
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
    (is (= 9 (pine.state.store:store '(:ref :probe-persist))))
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
    (is (= 5 (pine.state.store:store '(:var :probe-var-persist))))
    (remhash :probe-var-persist pine.state.var::*variables*)
    (pine.state.var:defonce :probe-var-persist :default 1 :persist t)
    (is (= 5 (pine.state.var:var :probe-var-persist)))))

;;;; the world

(defun forget-world-contributors (&rest names)
  (setf pine.state.world::*contributors*
        (remove-if (lambda (entry) (member (first entry) names))
                   pine.state.world::*contributors*)))

(test the-world-saves-and-restores-through-the-store
  (with-fixture memory-store ()
    (setf (pine.state.var:var :world-save) t)
    (let ((a nil) (b nil))
      (unwind-protect
           (progn
             (pine.state.world:register :probe-a
               :save (lambda () :init) :restore (lambda (d) (setf a d)))
             (pine.state.world:register :probe-b
               :save (lambda () '(1 2)) :restore (lambda (d) (setf b d)))
             (pine.state.world:save-world)
             (is (eq :init (pine.state.store:store '(:world :probe-a))))
             (pine.state.world:restore-world)
             (is (eq :init a))
             (is (equal '(1 2) b)))
        (forget-world-contributors :probe-a :probe-b)
        (setf (pine.state.var:var :world-save) nil)))))

(test a-throwing-restore-skips-without-breaking-the-rest
  (with-fixture memory-store ()
    (setf (pine.state.var:var :world-save) t)
    (let ((reached nil))
      (unwind-protect
           (let ((*error-output* (make-broadcast-stream)))
             (pine.state.world:register :probe-bad
               :save (lambda () :x)
               :restore (lambda (d) (declare (ignore d)) (error "boom")))
             (pine.state.world:register :probe-after
               :save (lambda () :y) :restore (lambda (d) (setf reached d)))
             (pine.state.world:save-world)
             (pine.state.world:restore-world)
             (is (eq :y reached)))
        (forget-world-contributors :probe-bad :probe-after)
        (setf (pine.state.var:var :world-save) nil)))))

(test a-nil-save-keeps-the-previous-entry
  (with-fixture memory-store ()
    (setf (pine.state.var:var :world-save) t)
    (unwind-protect
         (progn
           (pine.state.world:register :probe-nil :save (lambda () :first))
           (pine.state.world:save-world :probe-nil)
           (pine.state.world:register :probe-nil :save (lambda () nil))
           (pine.state.world:save-world :probe-nil)
           (is (eq :first (pine.state.store:store '(:world :probe-nil)))))
      (forget-world-contributors :probe-nil)
      (setf (pine.state.var:var :world-save) nil))))

(test the-world-gate-stops-both-directions
  (with-fixture memory-store ()
    (let ((restored :untouched))
      (unwind-protect
           (progn
             (setf (pine.state.var:var :world-save) t)
             (pine.state.world:register :probe-gate
               :save (lambda () :fresh)
               :restore (lambda (d) (setf restored d)))
             (setf (pine.state.store:store '(:world :probe-gate)) :stale)
             (setf (pine.state.var:var :world-save) nil)
             (pine.state.world:save-world :probe-gate)
             (is (eq :stale (pine.state.store:store '(:world :probe-gate))))
             (pine.state.world:restore-world :probe-gate)
             (is (eq :untouched restored)))
        (forget-world-contributors :probe-gate)
        (setf (pine.state.var:var :world-save) nil)))))
