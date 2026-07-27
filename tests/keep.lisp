(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.keep :in :pine)

(defmacro with-keep (&body body)
  "A namespace with a file of its own, closed afterwards."
  `(pine.ns:with-space ()
     (unwind-protect (progn (pine.keep:open ":memory:") ,@body)
       (pine.keep:close))))

;;;; what is stored

(test held-is-stored-and-comes-back
  (let ((space (pine.ns:fresh)))
    (pine.ns:with-space (space)
      (pine.keep:open ":memory:")
      (pine.ns:write /tab-width 8)
      (pine.ns:write /theme :ef-dream)
      ;; the same file, read into a namespace that knows nothing
      (pine.keep:restore)
      (is (= 8 (pine.ns:read /tab-width)))
      (is (eq :ef-dream (pine.ns:read /theme)))
      (pine.keep:close))))

(test a-stored-value-wins-over-the-one-a-config-seeds
  (with-keep
    (pine.ns:write /tab-width 2)
    (pine.ns:write /tab-width 8)
    (pine.ns:write /tab-width 2)
    (is (= 1 (pine.keep:restore)))
    (is (= 2 (pine.ns:read /tab-width)) "the newest write is what the file holds")))

(test derived-is-not-stored-because-it-is-computed-again
  (with-keep
    (pine.ns:write /sys/user "bfh")
    (pine.ns:write /greeting (format nil "hello ~a" (pine.ns:read /sys/user)))
    (is (eq :held (pine.ns:kind /sys/user)))
    (is (eq :derived (pine.ns:kind /greeting)))
    (pine.ns:write /greeting nil)
    (is (= 1 (pine.keep:restore)) "only the held path was in the file")))

(test live-is-not-stored-because-the-world-is-the-storage
  (with-keep
    (let ((state (make-hash-table)))
      (setf (gethash :volume state) 40)
      (pine.ns:write /audio
                     (pine.ns:provider
                      (/audio/volume
                       {:read (pine.data:fn [] (gethash :volume state))
                        :write (pine.data:fn [v] (setf (gethash :volume state) v))})))
      (is (eq :live (pine.ns:kind /audio/volume)))
      (pine.ns:write /audio/volume 70)
      (is (zerop (pine.keep:restore))))))

(test keep-nil-opts-a-path-out
  (with-keep
    (pine.ns:write /scratch "noise" :keep nil)
    (pine.ns:write /real "kept")
    (is (= 1 (pine.keep:restore)))
    (is (string= "kept" (pine.ns:read /real)))))

;;;; what can be stored

(test code-is-not-data-and-is-not-stored
  (is (pine.keep:storablep {:a 1 :b [1 "two" :three]}))
  (is (pine.keep:storablep /a/b))
  (is (pine.keep:storablep #{:x :y}))
  (is (not (pine.keep:storablep (lambda () nil))))
  (is (not (pine.keep:storablep {:run (lambda () nil)}))))

(test a-command-is-not-stored-because-its-config-declares-it-again
  (with-keep
    (pine.ns:write /cmd/scratch (pine.data:fn [] {}))
    (is (zerop (pine.keep:restore)))))

(test a-path-round-trips-through-the-file
  (with-keep
    (pine.ns:write /win/focused/buf /buf/scratch)
    (pine.keep:restore)
    (is (fset:equal? /buf/scratch (pine.ns:read /win/focused/buf)))))

;;;; history

(test history-lists-what-changed-newest-first
  (with-keep
    (pine.ns:write /a 1)
    (pine.ns:write /a 2)
    (pine.ns:write /b 3)
    (let ((log (pine.ns:read /history)))
      (is (= 3 (fset:size log)))
      (is (fset:equal? /b (fset:lookup (fset:lookup log 0) :path)))
      (is (= 3 (fset:lookup (fset:lookup log 0) :new)))
      (is (= 1 (fset:lookup (fset:lookup log 1) :old))))))

(test was-answers-what-a-path-held-then
  (with-keep
    (pine.ns:write /a 1)
    (let ((n (fset:lookup (fset:lookup (pine.ns:read /history) 0) :n)))
      (pine.ns:write /a 2)
      (pine.ns:write /a 3)
      (is (= 3 (pine.ns:read /a)))
      (is (= 1 (pine.ns:read /was/${n}/a))))))

(test was-answers-what-is-there-now-for-a-path-it-never-saw-change
  (with-keep
    (pine.ns:write /a 1)
    (let ((n (fset:lookup (fset:lookup (pine.ns:read /history) 0) :n)))
      (is (null (pine.ns:read /was/${n}/never/written))))))

(test revert-puts-the-world-back
  (with-keep
    (pine.ns:write /theme :one)
    (pine.ns:write /tab-width 8)
    (let ((n (fset:lookup (fset:lookup (pine.ns:read /history) 0) :n)))
      (pine.ns:write /theme :two)
      (pine.ns:write /tab-width 2)
      (pine.ns:write /extra "added")
      (is (eq :two (pine.ns:read /theme)))
      (pine.ns:write /history [:revert n])
      (is (eq :one (pine.ns:read /theme)))
      (is (= 8 (pine.ns:read /tab-width)))
      (is (null (pine.ns:read /extra))
          "a path that did not exist then does not exist now"))))

;;;; rings

(test a-ring-survives-the-file
  (with-keep
    (dolist (word '("one" "two" "three"))
      (pine.ns:write /kill word :max 2))
    (pine.keep:restore)
    (is (fset:equal? ["three" "two"] (pine.ns:read /kill)))))

;;;; nothing open

(test with-no-file-open-nothing-persists-and-nothing-breaks
  (pine.ns:with-space ()
    (pine.ns:write /a 1)
    (is (= 1 (pine.ns:read /a)))
    (is (zerop (pine.keep:restore)))))
