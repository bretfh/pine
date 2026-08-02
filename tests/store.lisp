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

(test keep-is-stored-and-comes-back
  (with-store
    (pine.ns:write /tab-width 8 :keep t)
    (pine.ns:write /wm-terminal "alacritty" :keep t)
    (pine.store:restore store)
    (is (= 8 (pine.ns:read /tab-width)))
    (is (string= "alacritty" (pine.ns:read /wm-terminal)))))

(test a-stored-value-wins-over-the-one-a-config-seeds
  (with-store
    (pine.ns:write /tab-width 2 :keep t)
    (pine.ns:write /tab-width 8)
    (pine.ns:write /tab-width 2)
    (is (= 1 (pine.store:restore store)))
    (is (= 2 (pine.ns:read /tab-width)) "the newest write is what the file holds")))

(test nothing-is-stored-unless-someone-said-keep
  (with-store
    (pine.ns:write /sys/user "bfh")
    (pine.ns:write /greeting (format nil "hello ~a" (pine.ns:read /sys/user)))
    (is (zerop (pine.store:restore store)))))

(test a-provider-answers-a-write-and-nothing-is-kept
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

(test keep-is-per-path-and-the-rest-is-not-in-the-file
  (with-store
    (pine.ns:write /scratch "noise")
    (pine.ns:write /real "kept" :keep t)
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
    (pine.ns:write /cmd/scratch (pine.data:fn [] {}) :keep t)
    (is (zerop (pine.store:restore store)))))

(test a-path-round-trips-through-the-file
  (with-store
    (pine.ns:write /win/focused/buf /buf/scratch :keep t)
    (pine.store:restore store)
    (is (fset:equal? /buf/scratch (pine.ns:read /win/focused/buf)))))

;;;; rings

(test a-ring-survives-the-file
  (with-store
    (dolist (word '("one" "two" "three"))
      (pine.ns:write /kill word :max 2 :keep t))
    (pine.store:restore store)
    (is (string= "three" (pine.ns:read /kill)))
    (is (= 2 (fset:size (pine.ns:read /kill/*))))))

;;;; nothing open

(test with-no-file-open-nothing-persists-and-nothing-breaks
  (pine.ns:with-space ()
    (pine.ns:write /a 1)
    (is (= 1 (pine.ns:read /a)))
    (is (null (pine.ns:on-commit :store)) "nothing is being told about commits")))

;;;; the file is behind the write, not in it

(test writers-on-many-threads-all-reach-the-file
  (with-store
    (let ((threads (loop :for i :below 8
                         :collect (let ((n i))
                                    (bordeaux-threads:make-thread
                                     (lambda ()
                                       (dotimes (j 20)
                                         (pine.ns:write (pine.path:path "n" n) j
                                                        :keep t))))))))
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
             (pine.ns:write /theme :one :keep t))
           (pine.ns:with-space (b) (setf store-b (pine.store:open ":memory:"))
             (pine.ns:write /theme :two :keep t))
           (pine.ns:with-space (a)
             (is (= 1 (pine.store:restore store-a)))
             (is (eq :one (pine.ns:read /theme))))
           (pine.ns:with-space (b)
             (is (= 1 (pine.store:restore store-b)))
             (is (eq :two (pine.ns:read /theme)))))
      (when store-a (pine.ns:with-space (a) (pine.store:close store-a)))
      (when store-b (pine.ns:with-space (b) (pine.store:close store-b))))))
