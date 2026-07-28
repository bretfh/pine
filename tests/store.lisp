(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.store :in :pine)

(defmacro with-store (&body body)
  "A space with a file of its own, bound to STORE and closed afterwards."
  `(pine.ns:with-space ()
     (let ((store (pine.store:open ":memory:")))
       (unwind-protect (progn ,@body)
         (pine.store:close store)))))

;;;; what is stored

(test held-is-stored-and-comes-back
  (with-store
    (pine.ns:write /tab-width 8)
    (pine.ns:write /theme :ef-dream)
    ;; the same file, read into a namespace that knows nothing
    (pine.store:restore store)
    (is (= 8 (pine.ns:read /tab-width)))
    (is (eq :ef-dream (pine.ns:read /theme)))))

(test a-stored-value-wins-over-the-one-a-config-seeds
  (with-store
    (pine.ns:write /tab-width 2)
    (pine.ns:write /tab-width 8)
    (pine.ns:write /tab-width 2)
    (is (= 1 (pine.store:restore store)))
    (is (= 2 (pine.ns:read /tab-width)) "the newest write is what the file holds")))

(test derived-is-not-stored-because-it-is-computed-again
  (with-store
    (pine.ns:write /sys/user "bfh")
    (pine.ns:write /greeting (format nil "hello ~a" (pine.ns:read /sys/user)))
    (is (eq :held (pine.ns:kind /sys/user)))
    (is (eq :derived (pine.ns:kind /greeting)))
    (pine.ns:write /greeting nil)
    (is (= 1 (pine.store:restore store)) "only the held path was in the file")))

(test live-is-not-stored-because-the-world-is-the-storage
  (with-store
    (let ((state (make-hash-table)))
      (setf (gethash :volume state) 40)
      (pine.ns:write /audio
                     (pine.ns:provider
                      (/audio/volume
                       {:read (pine.data:fn [] (gethash :volume state))
                        :write (pine.data:fn [v] (setf (gethash :volume state) v))})))
      (is (eq :live (pine.ns:kind /audio/volume)))
      (pine.ns:write /audio/volume 70)
      (is (zerop (pine.store:restore store))))))

(test keep-nil-opts-a-path-out
  (with-store
    (pine.ns:write /scratch "noise" :keep nil)
    (pine.ns:write /real "kept")
    (is (= 1 (pine.store:restore store)))
    (is (string= "kept" (pine.ns:read /real)))))

;;;; what can be stored

(test code-is-not-data-and-is-not-stored
  (is (pine.store:storablep {:a 1 :b [1 "two" :three]}))
  (is (pine.store:storablep /a/b))
  (is (pine.store:storablep #{:x :y}))
  (is (not (pine.store:storablep (lambda () nil))))
  (is (not (pine.store:storablep {:run (lambda () nil)}))))

(test a-command-is-not-stored-because-its-config-declares-it-again
  (with-store
    (pine.ns:write /cmd/scratch (pine.data:fn [] {}))
    (is (zerop (pine.store:restore store)))))

(test a-path-round-trips-through-the-file
  (with-store
    (pine.ns:write /win/focused/buf /buf/scratch)
    (pine.store:restore store)
    (is (fset:equal? /buf/scratch (pine.ns:read /win/focused/buf)))))

;;;; history

(test history-lists-what-changed-newest-first
  (with-store
    (pine.ns:write /a 1)
    (pine.ns:write /a 2)
    (pine.ns:write /b 3)
    (let ((log (pine.ns:read /history)))
      (is (= 3 (fset:size log)))
      (is (fset:equal? /b (fset:lookup (fset:lookup log 0) :path)))
      (is (= 3 (fset:lookup (fset:lookup log 0) :new)))
      (is (= 1 (fset:lookup (fset:lookup log 1) :old))))))

(test was-answers-what-a-path-held-then
  (with-store
    (pine.ns:write /a 1)
    (let ((n (fset:lookup (fset:lookup (pine.ns:read /history) 0) :n)))
      (pine.ns:write /a 2)
      (pine.ns:write /a 3)
      (is (= 3 (pine.ns:read /a)))
      (is (= 1 (pine.ns:read /was/${n}/a))))))

(test was-answers-what-is-there-now-for-a-path-it-never-saw-change
  (with-store
    (pine.ns:write /a 1)
    (let ((n (fset:lookup (fset:lookup (pine.ns:read /history) 0) :n)))
      (is (null (pine.ns:read /was/${n}/never/written))))))

(test revert-puts-the-world-back
  (with-store
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
  (with-store
    (dolist (word '("one" "two" "three"))
      (pine.ns:write /kill word :max 2))
    (pine.store:restore store)
    (is (string= "three" (pine.ns:read /kill)))
    (is (= 2 (fset:size (pine.ns:read /kill/*))))))

;;;; nothing open

(test with-no-file-open-nothing-persists-and-nothing-breaks
  (pine.ns:with-space ()
    (pine.ns:write /a 1)
    (is (= 1 (pine.ns:read /a)))
    (is (null (pine.ns:on-commit)) "nothing is being told about commits")))

;;;; the file is behind the write, not in it

(test the-log-reads-back-every-write-that-preceded-it
  (with-store
    (dotimes (i 50) (pine.ns:write /n i))
    (let ((history (pine.ns:read /history)))
      (is (= 50 (fset:size history))
          "recording does not wait, and the log still has all of it")
      (is (= 49 (fset:lookup (fset:lookup history 0) :new))
          "newest first"))))

(test writers-on-many-threads-all-reach-the-file
  (with-store
    (let ((threads (loop :for i :below 8
                         :collect (let ((n i))
                                    (bordeaux-threads:make-thread
                                     (lambda ()
                                       (dotimes (j 20)
                                         (pine.ns:write (pine.path:path "n" n) j))))))))
      (mapc #'bordeaux-threads:join-thread threads))
    (is (= 19 (pine.ns:read /n/${0})))
    (is (= 19 (pine.ns:read /n/${7})))
    (is (= 8 (pine.store:restore store))
        "every writer's newest value is in the file")))

;;;; one file per pine

(test two-spaces-each-keep-their-own-file
  "A pine is a space, and an image may hold several. Nothing the store owns
lives in an image global, so two do not write through each other's file."
  (let ((a (pine.ns:fresh))
        (b (pine.ns:fresh))
        (store-a nil)
        (store-b nil))
    (unwind-protect
         (progn
           (pine.ns:with-space (a) (setf store-a (pine.store:open ":memory:"))
             (pine.ns:write /theme :one))
           (pine.ns:with-space (b) (setf store-b (pine.store:open ":memory:"))
             (pine.ns:write /theme :two))
           (pine.ns:with-space (a)
             (is (= 1 (pine.store:restore store-a)))
             (is (eq :one (pine.ns:read /theme))))
           (pine.ns:with-space (b)
             (is (= 1 (pine.store:restore store-b)))
             (is (eq :two (pine.ns:read /theme)))))
      (when store-a (pine.ns:with-space (a) (pine.store:close store-a)))
      (when store-b (pine.ns:with-space (b) (pine.store:close store-b))))))
