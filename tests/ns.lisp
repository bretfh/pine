(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.ns :in :pine)

(defmacro with-ns (&body body)
  `(pine.ns:with-space () ,@body))

;;;; the tree

(test a-value-reads-back
  (with-ns
    (pine.ns:write /audio/volume 40)
    (is (= 40 (pine.ns:read /audio/volume)))))

(test nothing-there-is-nil-not-an-error
  (with-ns
    (is (null (pine.ns:read /nothing/at/all)))
    (is (eq :none (pine.ns:read /nothing/at/all :none)))))

(test a-directory-and-a-map-are-the-same-thing
  (with-ns
    (pine.ns:write /audio {:volume 40 :muted nil})
    (is (= 40 (pine.ns:read /audio/volume)))
    (is (fset:equal? {:volume 40} (pine.ns:read /audio))
        "a nil leaf is nothing, so it is not in the directory")
    (pine.ns:write /audio/sink "hdmi")
    (is (fset:equal? {:volume 40 :sink "hdmi"} (pine.ns:read /audio)))))

(test a-map-written-to-a-directory-writes-its-leaves
  (with-ns
    (pine.ns:write /mode/lisp {:parent :prog :indicator "Lisp"})
    (is (eq :prog (pine.ns:read /mode/lisp/parent)))
    (is (string= "Lisp" (pine.ns:read /mode/lisp/indicator)))))

(test nil-is-nothing
  (with-ns
    (pine.ns:write /a/b 1)
    (pine.ns:write /a/b nil)
    (is (null (pine.ns:read /a/b)))
    (is (fset:empty? (pine.ns:read /a {})))))

(test deep-values-share-what-did-not-move
  "The de-duplication rule is a pointer compare, so an untouched subtree has
to come back as the same object."
  (with-ns
    (pine.ns:write /x/keep {:a 1 :b 2})
    (let ((before (pine.ns:read /x/keep)))
      (pine.ns:write /x/other 1)
      (is (eq before (pine.ns:read /x/keep))))))

;;;; values against verbs

(test a-value-that-did-not-change-is-not-a-change
  (with-ns
    (pine.ns:write /audio/volume 40)
    (is (fset:empty? (pine.ns:write /audio/volume 40)))
    (is (not (fset:empty? (pine.ns:write /audio/volume 41))))))

(test a-write-answers-what-it-changed
  (with-ns
    (pine.ns:write /audio/volume 40)
    (let ((changed (pine.ns:write /audio/volume 41)))
      (is (fset:equal? [40 41] (fset:lookup changed /audio/volume))))))

(test toggle-is-a-verb-and-happens-every-time
  (with-ns
    (pine.ns:write /audio/muted nil)
    (pine.ns:toggle /audio/muted)
    (is (eq t (pine.ns:read /audio/muted)))
    (pine.ns:toggle /audio/muted)
    (is (null (pine.ns:read /audio/muted)))))

(test the-built-in-verbs
  (with-ns
    (pine.ns:write /buf/scratch/minor #{:paren})
    (pine.ns:write /buf/scratch/minor [:conj :flycheck])
    (is (fset:equal? #{:paren :flycheck} (pine.ns:read /buf/scratch/minor)))
    (pine.ns:write /buf/scratch/minor [:disj :paren])
    (is (fset:equal? #{:flycheck} (pine.ns:read /buf/scratch/minor)))
    (pine.ns:write /style/bar {:padding 8})
    (pine.ns:write /style/bar [:merge {:opacity 0.9}])
    (is (= 8 (pine.ns:read /style/bar/padding)))
    (is (= 0.9 (pine.ns:read /style/bar/opacity)))
    (pine.ns:write /literal [:set [1 2 3]])
    (is (fset:equal? [1 2 3] (pine.ns:read /literal)))))

(test an-unknown-verb-names-the-path-and-the-verb
  (with-ns
    (signals pine.ns:no-verb (pine.ns:write /a/b [:frobnicate 1]))))

;;;; transactions

(test a-map-is-one-change
  (with-ns
    (let ((changed (pine.ns:write {/audio/volume 40
                                   /audio/muted nil
                                   /surface/audio/shown t})))
      (is (= 40 (pine.ns:read /audio/volume)))
      (is (eq t (pine.ns:read /surface/audio/shown)))
      (is (= 2 (fset:size changed)) "a nil that was already nothing did not move"))))

;;;; fan out

(test a-pattern-writes-every-match
  (with-ns
    (pine.ns:write /win {:0 {:weight 3} :1 {:weight 1}})
    (pine.ns:write /win/*/weight 1)
    (is (= 1 (pine.ns:read /win/0/weight)))
    (is (= 1 (pine.ns:read /win/1/weight)))))

(test a-fan-out-binds-its-segments-on-the-right
  (with-ns
    (pine.ns:write /wm/workspaces {:1 {:idx "?"} :2 {:idx "?"}})
    (pine.ns:write /wm/workspaces/?n/idx n)
    (is (string= "1" (pine.ns:read /wm/workspaces/1/idx)))
    (is (string= "2" (pine.ns:read /wm/workspaces/2/idx)))))

(test a-fan-out-honours-a-constraint
  (with-ns
    (pine.ns:write /proc {:editor {:state :running} :backup {:state :failed}})
    (pine.ns:write /proc/*{:state :failed}/state :restarting)
    (is (eq :running (pine.ns:read /proc/editor/state)))
    (is (eq :restarting (pine.ns:read /proc/backup/state)))))

(test a-pattern-only-matches-what-exists
  (with-ns
    (pine.ns:write /win/*/weight 1)
    (is (fset:empty? (pine.ns:read /win {})))))

;;;; pattern reads

(test a-pattern-read-answers-a-map-keyed-by-path
  (with-ns
    (pine.ns:write /proc {:editor {:state :running} :backup {:state :failed}})
    (let ((states (pine.ns:read /proc/*/state)))
      (is (= 2 (fset:size states)))
      (is (eq :running (fset:lookup states /proc/editor/state)))
      (is (eq :failed (fset:lookup states /proc/backup/state))))))

(test a-deep-pattern-reaches-any-depth
  (with-ns
    (pine.ns:write /host/box/err/1 "boom")
    (pine.ns:write /err/2 "bang")
    (let ((faults (pine.ns:read /**/err/*)))
      (is (= 2 (fset:size faults))))))

;;;; guards

(test a-guarded-write-applies-only-against-what-it-expected
  (with-ns
    (pine.ns:write /buf/notes/text "one")
    (pine.ns:write /buf/notes/text "two" :when "one")
    (is (string= "two" (pine.ns:read /buf/notes/text)))
    (pine.ns:write /buf/notes/text "three" :when "one")
    (is (string= "two" (pine.ns:read /buf/notes/text))
        "the guard did not hold, so nothing was written")))

(test the-whole-tree-is-refused-unless-you-mean-it
  (with-ns
    (pine.ns:write /a 1)
    (signals pine.ns:refused (pine.ns:write /** nil))
    (is (= 1 (pine.ns:read /a)))))

;;;; rings

(test a-ring-pushes-and-keeps-the-newest
  (with-ns
    (dolist (word '("one" "two" "three"))
      (pine.ns:write /kill word :max 2))
    (is (string= "three" (pine.ns:read /kill)) "a ring reads as its newest")
    (is (= 2 (fset:size (pine.ns:read /kill/*))) "and each entry has a place")
    (is (string= "three" (pine.ns:read /kill/0)))
    (is (string= "two" (pine.ns:read /kill/1)))))

(test a-path-that-is-a-ring-stays-one
  "The bound belongs to the path, not to the write that first said it, so
whoever pushes next does not have to know."
  (with-ns
    (pine.ns:write /kill "one" :max 3)
    (pine.ns:write /kill "two")
    (pine.ns:write /kill "three")
    (pine.ns:write /kill "four")
    (is (string= "four" (pine.ns:read /kill)))
    (is (= 3 (fset:size (pine.ns:read /kill/*)))
        "a later write pushed onto the ring instead of replacing it")))

(test a-bound-set-without-a-write-makes-the-path-a-ring
  (with-ns
    (setf (pine.ns:setting /log :max) 2)
    (pine.ns:write /log "first")
    (pine.ns:write /log "second")
    (pine.ns:write /log "third")
    (is (string= "third" (pine.ns:read /log)))
    (is (= 2 (fset:size (pine.ns:read /log/*))))))

;;;; diff

(test a-diff-answers-only-the-leaves-that-differ
  (with-ns
    (pine.ns:write /a {:volume 40 :sink "hdmi" :muted nil})
    (pine.ns:write /b {:volume 70 :sink "hdmi"})
    (let ((moved (pine.ns:diff /a /b)))
      (is (= 1 (fset:size moved)) "only volume moved")
      (is (fset:equal? (fset:seq 40 70) (fset:lookup moved /b/volume))
          "a diff says what it was and what it is"))))

(test a-diff-descends-and-reports-what-is-gone-and-what-is-new
  (with-ns
    (pine.ns:write /a {:win {:0 "scratch" :1 "notes"}})
    (pine.ns:write /b {:win {:0 "scratch" :2 "repl"}})
    (let ((moved (pine.ns:diff /a /b)))
      (is (= 2 (fset:size moved)))
      (is (fset:equal? (fset:seq "notes" nil) (fset:lookup moved /b/win/1))
          "a path that is gone reads as nil on the side it is gone from")
      (is (fset:equal? (fset:seq nil "repl") (fset:lookup moved /b/win/2))))))

(test two-subtrees-that-hold-the-same-thing-differ-in-nothing
  (with-ns
    (pine.ns:write /a {:x 1 :y {:z "same"}})
    (pine.ns:write /b {:x 1 :y {:z "same"}})
    (is (zerop (fset:size (pine.ns:diff /a /b))))))

;;;; re-evaluation

(test an-expression-is-computed-again-when-what-it-read-moves
  (with-ns
    (pine.ns:write /sys/user "bfh")
    (pine.ns:write /sys/host "arraniz")
    (pine.ns:write /greeting (format nil "~a@~a"
                                     (pine.ns:read /sys/user)
                                     (pine.ns:read /sys/host)))
    (is (string= "bfh@arraniz" (pine.ns:read /greeting)))
    (pine.ns:write /sys/host "elsewhere")
    (is (string= "bfh@elsewhere" (pine.ns:read /greeting)))))

(test re-evaluation-carries-down-a-chain
  (with-ns
    (pine.ns:write /a 1)
    (pine.ns:write /b (* 10 (pine.ns:read /a)))
    (pine.ns:write /c (+ 1 (pine.ns:read /b)))
    (is (= 11 (pine.ns:read /c)))
    (pine.ns:write /a 2)
    (is (= 20 (pine.ns:read /b)))
    (is (= 21 (pine.ns:read /c)))))

(test an-expression-that-read-nothing-is-computed-once
  (with-ns
    (let ((runs 0))
      (pine.ns:write /once (progn (incf runs) 1))
      (pine.ns:write /other 5)
      (is (= 1 runs)))))

(test a-cycle-is-loud
  (with-ns
    (pine.ns:write /a 1)
    (pine.ns:write /b (1+ (pine.ns:read /a)))
    (signals pine.ns:cycle
      (pine.ns:write /a (1+ (pine.ns:read /b))))))

;;;; watches

(test a-watch-runs-on-a-change-and-its-answer-is-applied
  (with-ns
    (pine.ns:watch /battery/level
                   (pine.data:fn [v] (if (> v 90) {/notify "full"} {}))
                   :as :osd)
    (pine.ns:write /battery/level 50)
    (is (null (pine.ns:read /notify)))
    (pine.ns:write /battery/level 95)
    (is (string= "full" (pine.ns:read /notify)))))

(test watching-a-directory-watches-the-subtree
  (with-ns
    (let ((seen nil))
      (pine.ns:watch /audio (pine.data:fn [v] (push v seen) {}) :as :any)
      (pine.ns:write /audio/volume 40)
      (pine.ns:write /audio/muted t)
      (is (equal '(t 40) seen)))))

(test naming-a-watch-replaces-it
  (with-ns
    (let ((runs 0))
      (pine.ns:watch /x (pine.data:fn [v] (declare (ignore v)) (incf runs) {})
                     :as :counter)
      (pine.ns:watch /x (pine.data:fn [v] (declare (ignore v)) (incf runs) {})
                     :as :counter)
      (pine.ns:write /x 1)
      (is (= 1 runs) "the second registration replaced the first"))))

(test a-watch-can-be-removed
  (with-ns
    (let ((runs 0))
      (pine.ns:watch /x (pine.data:fn [v] (declare (ignore v)) (incf runs) {})
                     :as :counter)
      (pine.ns:write /x 1)
      (pine.ns:watch /x nil :as :counter)
      (pine.ns:write /x 2)
      (is (= 1 runs)))))

;;;; preview

(test preview-says-what-a-write-would-do-without-doing-it
  (with-ns
    (pine.ns:write /buf {:a {:modified t} :b {:modified nil}})
    (let ((would (pine.ns:preview
                   (pine.ns:write /buf/*{:modified t}/modified nil))))
      (is (= 1 (fset:size would)))
      (is (fset:equal? [t nil] (fset:lookup would /buf/a/modified))
          "the same map a write answers: path to [old new]")
      (is (eq t (pine.ns:read /buf/a/modified))
          "and it really did not do it"))))

;;;; providers

(defun stub-audio (state)
  "A provider over a hash table, standing in for wpctl."
  (pine.ns:provider
   (/audio/volume {:read (pine.data:fn [] (gethash :volume state 0))
                   :write (pine.data:fn [v] (setf (gethash :volume state) v))})
   (/audio/muted {:read (pine.data:fn [] (gethash :muted state))
                  :verbs {:toggle (pine.data:fn []
                                    (setf (gethash :muted state)
                                          (not (gethash :muted state))))}})
   (/audio/sinks {:ls (pine.data:fn [] '("hdmi" "usb"))})
   (/audio/sinks/?name/desc
    {:read (pine.data:fn [] (format nil "the ~a sink" name))})))

(test a-provider-answers-for-its-subtree
  (with-ns
    (let ((state (make-hash-table)))
      (setf (gethash :volume state) 33)
      (pine.ns:write /audio (stub-audio state))
      (is (= 33 (pine.ns:read /audio/volume)))
      (pine.ns:write /audio/volume 70)
      (is (= 70 (gethash :volume state)) "the write reached the system")
      (is (= 70 (pine.ns:read /audio/volume))))))

(test a-provider-clause-binds-its-segments
  (with-ns
    (pine.ns:write /audio (stub-audio (make-hash-table)))
    (is (string= "the hdmi sink" (pine.ns:read /audio/sinks/hdmi/desc)))))

(test a-provider-ls-is-what-a-pattern-read-walks
  (with-ns
    (pine.ns:write /audio (stub-audio (make-hash-table)))
    (let ((descs (pine.ns:read /audio/sinks/*/desc)))
      (is (= 2 (fset:size descs)))
      (is (string= "the usb sink" (fset:lookup descs /audio/sinks/usb/desc))))))

(test a-provider-takes-its-own-verbs
  (with-ns
    (let ((state (make-hash-table)))
      (pine.ns:write /audio (stub-audio state))
      (pine.ns:toggle /audio/muted)
      (is (eq t (gethash :muted state)))
      (pine.ns:toggle /audio/muted)
      (is (null (gethash :muted state))))))

(test a-read-only-path-refuses-a-write-and-names-itself
  (with-ns
    (pine.ns:write /audio (stub-audio (make-hash-table)))
    (signals pine.ns:refused (pine.ns:write /audio/sinks/hdmi/desc "no"))))

(test a-provider-over-another-falls-through-for-what-it-does-not-answer
  (with-ns
    (pine.ns:write /audio (stub-audio (make-hash-table)))
    (pine.ns:write /audio
                   (pine.ns:provider
                    (/audio/volume {:read (pine.data:fn [] 11)})))
    (is (= 11 (pine.ns:read /audio/volume)) "the top one answers")
    (is (string= "the hdmi sink" (pine.ns:read /audio/sinks/hdmi/desc))
        "and the one underneath still answers the rest")))

(test a-surface-repaints-because-what-it-read-moved
  "The whole reactive story, in the shape a config actually writes."
  (with-ns
    (let ((state (make-hash-table)))
      (setf (gethash :volume state) 40)
      (pine.ns:write /audio (stub-audio state))
      (pine.ns:write /surface/bar (format nil "volume ~d"
                                          (pine.ns:read /audio/volume)))
      (is (string= "volume 40" (pine.ns:read /surface/bar)))
      (setf (gethash :volume state) 70)
      (pine.ns:write /audio/volume 70)
      (is (string= "volume 70" (pine.ns:read /surface/bar))))))

;;;; several threads at once

(test concurrent-writes-do-not-lose-each-other
  "The read, the verb and the write are one compare-and-swap. Read the value
first and write it after, and most of these disappear."
  (with-ns
    (pine.ns:write /marks #{})
    (let ((workers (loop :for i :below 8
                         :collect (let ((n i))
                                    (bordeaux-threads:make-thread
                                     (lambda ()
                                       (dotimes (k 50)
                                         (pine.ns:write /marks
                                                        [:conj (+ (* n 50) k)])))
                                     :name "ns-probe")))))
      (mapc #'bordeaux-threads:join-thread workers)
      (is (= 400 (fset:size (pine.ns:read /marks)))
          "8 threads x 50 marks; ~d landed" (fset:size (pine.ns:read /marks))))))

(test a-guard-is-tested-against-the-value-the-write-lands-on
  "Two threads racing a compare-and-set: exactly one may win."
  (with-ns
    (pine.ns:write /slot :start)
    (let ((won 0)
          (lock (bordeaux-threads:make-lock)))
      (let ((workers (loop :for i :below 8
                           :collect (bordeaux-threads:make-thread
                                     (lambda ()
                                       (let ((moved (pine.ns:write /slot :taken
                                                                   :when :start)))
                                         (unless (fset:empty? moved)
                                           (bordeaux-threads:with-lock-held (lock)
                                             (incf won)))))
                                     :name "ns-guard"))))
        (mapc #'bordeaux-threads:join-thread workers))
      (is (= 1 won) "~d threads thought they took it" won)
      (is (eq :taken (pine.ns:read /slot))))))

(test a-reader-never-sees-a-half-written-tree
  (with-ns
    (pine.ns:write /tree {:a 1 :b 2 :c 3})
    (let ((bad 0)
          (stop nil))
      (let ((reader (bordeaux-threads:make-thread
                     (lambda ()
                       (loop :until stop
                             :do (let ((m (pine.ns:read /tree)))
                                   (unless (or (null m) (= 3 (fset:size m)))
                                     (incf bad)))))
                     :name "ns-reader")))
        (dotimes (i 200) (pine.ns:write /tree {:a i :b i :c i}))
        (setf stop t)
        (bordeaux-threads:join-thread reader))
      (is (zerop bad) "saw a torn tree ~d times" bad))))

;;;; isolation

(test a-space-is-its-own
  (let ((a (pine.ns:fresh))
        (b (pine.ns:fresh)))
    (pine.ns:with-space (a) (pine.ns:write /x 1))
    (pine.ns:with-space (b) (is (null (pine.ns:read /x))))
    (pine.ns:with-space (a) (is (= 1 (pine.ns:read /x))))))
