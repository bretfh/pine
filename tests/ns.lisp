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

(test a-handler-that-takes-a-buffer-is-given-the-one-it-runs-on
  "A command that wants to know where it is says so in its arguments, and that
lambda list is the whole declaration: there is no interactive spec."
  (with-ns
    (pine.ns:write /buf/notes/text "here")
    (pine.ns:write /buf/current /buf/notes)
    (let ((saw :none))
      (pine.cmd:run (pine.data:fn [buf] (setf saw buf) {}))
      (is (equal "notes" saw) "the handler was not told which buffer"))
    (let ((ran nil))
      (pine.cmd:run (pine.data:fn [] (setf ran t) {}))
      (is (eq t ran) "a handler that takes none was called with none"))))

(test a-handler-that-signals-is-run-once
  "Which buffer a handler is on is read off its lambda list rather than found
out by calling it and catching, because a handler that signals must not be
retried with a different argument."
  (with-ns
    (let ((runs 0))
      (signals program-error
        (pine.cmd:run (lambda () (incf runs) (error 'program-error))))
      (is (= 1 runs) "the handler ran more than once"))))

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
    (pine.ns:write /proc/*[state = :failed]/state :restarting)
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

(test a-map-merges-its-keys-and-set-replaces-the-subtree
  "Writing a map is writing the leaves it names and nothing else, so a config
that says one key does not take the rest of the directory with it."
  (with-ns
    (pine.ns:write /surface/bar {:as :bar :shown t})
    (pine.ns:write /surface/bar {:tree :a-node})
    (is (eq :bar (pine.ns:read /surface/bar/as))
        "a second map wrote away what the first one said")
    (is (eq :a-node (pine.ns:read /surface/bar/tree)))
    (pine.ns:write /surface/bar [:set {:tree :only-this}])
    (is (null (pine.ns:read /surface/bar/as))
        "[:set MAP] replaces the subtree outright")
    (is (eq :only-this (pine.ns:read /surface/bar/tree)))))

(test a-key-written-as-nothing-goes-away
  (with-ns
    (pine.ns:write /audio {:volume 40 :sink "hdmi"})
    (pine.ns:write /audio {:sink nil})
    (is (= 40 (pine.ns:read /audio/volume)))
    (is (null (pine.ns:read /audio/sink)))))

(test a-transaction-takes-a-guard-and-lands-only-when-it-holds
  "A caller that computed its change from what it read refuses to overwrite
what landed in between: the test is inside the swap, not before it."
  (with-ns
    (pine.ns:write /buf/notes/text "one")
    (pine.ns:write {/buf/notes/text "two" /buf/notes/tick 1}
                   :when {/buf/notes/text "one"})
    (is (string= "two" (pine.ns:read /buf/notes/text)))
    (is (= 1 (pine.ns:read /buf/notes/tick)))
    (pine.ns:write {/buf/notes/text "three" /buf/notes/tick 2}
                   :when {/buf/notes/text "one"})
    (is (string= "two" (pine.ns:read /buf/notes/text))
        "the guard did not hold, so none of the transaction landed")
    (is (= 1 (pine.ns:read /buf/notes/tick)))))

(test a-fan-out-write-lands-on-a-leaf-that-is-not-there-yet
  "A run of literal segments at the end of a pattern is where the write goes,
not something it has to find: every window's weight, whether or not any window
has one yet."
  (with-ns
    (pine.ns:write /win/0/buf /buf/scratch)
    (pine.ns:write /win/1/buf /buf/notes)
    (is (null (pine.ns:read /win/0/weight)))
    (pine.ns:write /win/*/weight 1)
    (is (= 1 (pine.ns:read /win/0/weight)))
    (is (= 1 (pine.ns:read /win/1/weight)))
    (pine.ns:write /buf/a/modified t)
    (pine.ns:write /buf/b/modified nil)
    (pine.ns:write /buf/*[modified]/seen t)
    (is (eq t (pine.ns:read /buf/a/seen)))
    (is (null (pine.ns:read /buf/b/seen))
        "the constraint still decides which ones, only not whether the leaf existed")))

(test a-pattern-read-still-answers-only-what-is-there
  "Where a write lands and what a read finds are different questions: a read of
a pattern is a listing, and a leaf nobody wrote is not in it."
  (with-ns
    (pine.ns:write /win/0/buf /buf/scratch)
    (pine.ns:write /win/1/buf /buf/notes)
    (is (fset:empty? (pine.ns:read /win/*/weight {})))
    (is (= 2 (fset:size (pine.ns:read /win/*/buf {}))))))

(test matches-says-where-a-write-would-land
  (with-ns
    (pine.ns:write /proc/editor/state :running)
    (pine.ns:write /proc/desktop/state :running)
    (is (= 2 (length (pine.ns:matches /proc/*/state))))
    (is (= 9 (length (pine.ns:matches /key/wm/s-{1..9})))
        "a group names its places whether or not anything is there yet")
    (is (equal '("/nothing/here")
               (mapcar #'pine.path:text (pine.ns:matches /nothing/here)))
        "a path that is not a pattern lands in the one place it names")))

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

(test emptying-a-ring-the-file-keeps-is-refused
  "A ring the file keeps is a history, and clearing one is a thing to mean."
  (with-ns
    (pine.ns:write /kill "one" :max 3 :keep t)
    (pine.ns:write /kill "two")
    (signals pine.ns:refused (pine.ns:write /kill nil))
    (is (string= "two" (pine.ns:read /kill)))
    (pine.ns:write /kill nil :force t)
    (is (null (pine.ns:read /kill)) ":force t means it")))

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

(test on-adds-a-path-the-expression-did-not-read
  "A write depends on what it read, and :ON is how it says it also depends on
something it did not: a subscription's output, a poll's answer."
  (with-ns
    (pine.ns:write /sh/subscribe "first")
    (let ((runs 0))
      (pine.ns:write /wm/workspaces (progn (incf runs) (list :built runs))
                     :on /sh/subscribe)
      (is (= 1 runs))
      (pine.ns:write /sh/subscribe "second")
      (is (= 2 runs) "the write was not computed again when :on moved")
      (is (equal '(:built 2) (pine.ns:read /wm/workspaces))))))

(test every-says-what-it-asked-and-the-clock-is-somebody-else-s
  "A swap is pure and a timer is not, so the namespace records the interval and
tells whoever has a scheduler. AGAIN is what that scheduler calls."
  (with-ns
    (let ((asked nil) (runs 0))
      (setf (pine.ns:on-interval) (lambda (path seconds) (push (cons path seconds) asked)))
      (pine.ns:write /sys/load (progn (incf runs) runs) :every 3)
      (is (= 1 (length asked)) "nobody was told an interval was asked for")
      (is (= 3 (cdr (first asked))))
      (is (fset:equal? /sys/load (car (first asked))))
      (is (= 3 (pine.ns:setting /sys/load :every))
          "the interval is a property of the path, so it survives the write")
      (pine.ns:again /sys/load)
      (is (= 2 runs) "again did not compute the expression again")
      (is (= 2 (pine.ns:read /sys/load))))))

(test a-trigger-outlives-the-first-time-it-fires
  "A poll that reads nothing is still a poll on the second tick. Its triggers
belong to the path, so computing it again does not forget them."
  (with-ns
    (let ((runs 0))
      (setf (pine.ns:on-interval) (lambda (path seconds)
                                    (declare (ignore path seconds))
                                    nil))
      (pine.ns:write /sys/cpu (progn (incf runs) runs) :every 1)
      (dotimes (i 5) (pine.ns:again /sys/cpu))
      (is (= 6 runs) "the interval stopped asking after the first tick")
      (is (= 6 (pine.ns:read /sys/cpu))))))

(test an-on-path-goes-on-triggering-after-the-first-time
  (with-ns
    (pine.ns:write /sh/stream "one")
    (let ((runs 0))
      (pine.ns:write /wm/windows (progn (incf runs) runs) :on /sh/stream)
      (pine.ns:write /sh/stream "two")
      (pine.ns:write /sh/stream "three")
      (is (= 3 runs) ":on stopped triggering after the first change")
      (is (= 3 (pine.ns:read /wm/windows))))))

(test again-on-a-path-nobody-computed-is-nothing
  (with-ns
    (pine.ns:write /a 1)
    (is (null (pine.ns:again /a)))
    (is (= 1 (pine.ns:read /a)))))

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
                   (pine.ns:write /buf/*[modified = t]/modified nil))))
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

;;;; what serves a subtree

(defun %names ()
  (mapcar (lambda (d) (fset:lookup d :name)) (pine.ns:served)))

(test a-subsystem-declared-outside-the-tree-joins-the-list
  "Adding a subsystem is one form. There is no mount list for it to be left out
of, and no class for it to be a subclass of."
  (pine.ns:serve :probe
    {:at [/probe]
     :doc "a subtree this file invented, to check that pine brings up one it
has never heard of"
     :up (lambda () (pine.ns:write /probe :up))})
  (unwind-protect
       (pine.ns:with-space ()
         (is (member :probe (%names)))
         (pine.ns:up :probe)
         (is (eq :up (pine.ns:read /probe)))
         (pine.ns:down :probe)
         (is (null (pine.ns:read /probe))))
    (pine.ns:unserve :probe)))

(test what-it-is-given-is-read-where-it-is-wanted
  ":UP takes no arguments. What UP was handed is read through GIVEN, so a
declaration that wants an actor system asks for one and the twenty-two that
want nothing say nothing."
  (pine.ns:serve :probe
    {:at [/probe]
     :up (lambda () (pine.ns:write /probe (pine.ns:given :sample)))})
  (unwind-protect
       (pine.ns:with-space ()
         (pine.ns:up :probe {:sample 7})
         (is (eql 7 (pine.ns:read /probe)))
         ;; nobody passed one, so it reads nil rather than signalling
         (pine.ns:up :probe)
         (is (null (pine.ns:read /probe))))
    (pine.ns:unserve :probe)))

(test what-a-space-kept-comes-back-to-down-and-is-then-forgotten
  "The five that used to define LOWER all did the same thing by hand: take what
the space kept, dispose of it, and call the next method to unwrite the paths."
  (let ((given nil))
    (pine.ns:serve :probe
      {:at [/probe]
       :up (lambda () (pine.ns:write /probe :up) :a-handle)
       :down (lambda (handle) (setf given handle))})
    (unwind-protect
         (pine.ns:with-space ()
           (pine.ns:up :probe)
           (is (eq :a-handle (pine.ns:kept :probe)))
           (pine.ns:down :probe)
           (is (eq :a-handle given) "what it kept was not handed back")
           (is (null (pine.ns:kept :probe)) "the space is still holding it")
           (is (null (pine.ns:read /probe))))
      (pine.ns:unserve :probe))))

(test going-down-takes-the-provider-with-it
  "A subtree comes off the way writing nothing at a mount does: the provider
first, then the value. It went down by writing over the provider, which a
read-only one like /sh refuses -- so DOWN-ALL signalled at the first of them
and everything behind it in the order stayed up and mounted."
  (pine.ns:serve :probe
    {:at [/probe]
     :up (lambda ()
           (pine.ns:write /probe
                          (pine.ns:provider
                           (/probe {:read (pine.data:fn [] :answered)
                                    :doc "read-only, like /sh"}))))})
  (unwind-protect
       (pine.ns:with-space ()
         (pine.ns:up :probe)
         (is (eq :answered (pine.ns:read /probe)))
         (is (find /probe (pine.ns:mounts) :test #'fset:equal?))
         (finishes (pine.ns:down :probe))
         (is (null (pine.ns:read /probe)))
         (is (null (find /probe (pine.ns:mounts) :test #'fset:equal?))
             "the provider is still mounted"))
    (pine.ns:unserve :probe)))

(test a-subsystem-comes-up-after-what-it-says-it-is-after
  "Declared the other way round, so the order comes from what they say rather
than from when they were declared."
  (pine.ns:serve :probe-after
    {:at [/probe-after]
     :after [:probe]
     :up (lambda () (pine.ns:write /probe-after (pine.ns:read /probe)))})
  (pine.ns:serve :probe {:at [/probe] :up (lambda () (pine.ns:write /probe :up))})
  (unwind-protect
       (let* ((names (%names))
              (a (position :probe names))
              (b (position :probe-after names)))
         (is (< a b) "the order put :probe-after before :probe"))
    (pine.ns:unserve :probe)
    (pine.ns:unserve :probe-after)))

(test everything-served-is-named-once
  (let ((names (%names)))
    (is (= (length names) (length (remove-duplicates names)))
        "two declarations answer to the same name: ~a"
        (loop :for n :in names
              :when (< 1 (count n names)) :collect n))))

(test the-list-holds-what-a-running-pine-serves
  "Not an inventory: this is the check that the things the daemon used to mount
by hand are in the one list, so nothing has to be kept in step with it."
  (let ((names (%names)))
    (dolist (want '(:log :doc :err :file :sh :env :clock :sys :proc :win :host
                    :mode :cmd :key :terminal :view :echo :surface :theme :buf
                    :store :settings))
      (is (member want names) "~a serves nothing anyone brings up" want))))

(test two-spaces-in-one-image-style-themselves-separately
  "The one-line statement of who owns what.

The theme in force, the faces on it and the rules a config wrote are values, so
a space that writes one is the only space that changed. They used to be three
variables this image shared, which meant (read /theme) in one space answered
what another space had written and a second pine could not look different from
the first."
  (let ((a (pine.ns:fresh))
        (b (pine.ns:fresh)))
    (pine.ns:with-space (a)
      (pine.ns:up :theme)
      (pine.ns:write /face/probe-face {:fg "#ff0000"})
      (pine.ui.css:install (list (list :probe-two-spaces {:min-width 11}))))
    (pine.ns:with-space (b)
      (pine.ns:up :theme)
      (is (null (pine.ui.face:find-face :probe-face))
          "a face written in one space is in another space's table")
      (is (null (assoc ".probe-two-spaces" (pine.ui.css:styles) :test #'string=))
          "a rule written in one space is in another space's stylesheet"))
    (pine.ns:with-space (a)
      (is (string= "#ff0000" (pine.ui.face:fg (pine.ui.face:find-face :probe-face)))
          "the space that wrote the face lost it")
      (is (assoc ".probe-two-spaces" (pine.ui.css:styles) :test #'string=)
          "the space that wrote the rule lost it"))))

;;;; The roots, at /history and /was. They are the space's, so a pine with no
;;;; store has them.

(defmacro with-roots (&body body)
  `(pine.ns:with-space ()
     (pine.ns:up :roots)
     ,@body))

(test history-is-the-roots-newest-first
  (with-roots
    (pine.ns:write /a 1)
    (pine.ns:write /a 2)
    (pine.ns:write /b 3)
    (let ((roots (pine.ns:read /history)))
      (is (= 3 (fset:size roots)))
      (is (= 0 (fset:lookup (fset:lookup roots 0) :n))))))

(test history-is-there-with-no-store-open
  "The roots are what a write leaves behind, so recovery is a read and no file
has to be open for it. /history came off the store because a pine that never
opened one still has everything it needs to answer."
  (with-roots
    (is (null (pine.ns:on-commit :store)) "no file is behind this")
    (pine.ns:write /a 1)
    (pine.ns:write /a 2)
    (is (= 1 (pine.ns:read /was/${0}/a)))
    (is (plusp (fset:size (pine.ns:read /history))))))

(test was-answers-what-a-path-held-then
  (with-roots
    (pine.ns:write /a 1)
    (pine.ns:write /a 2)
    (pine.ns:write /a 3)
    (is (= 3 (pine.ns:read /a)))
    (is (= 2 (pine.ns:read /was/${0}/a)) "one supersession back")
    (is (= 1 (pine.ns:read /was/${1}/a)))))

(test was-answers-nothing-for-a-path-that-was-not-there
  (with-roots
    (pine.ns:write /a 1)
    (is (null (pine.ns:read /was/${0}/never/written)))))

(test a-relative-time-names-the-newest-root-that-old
  "A count of writes is not a clock: an idle pine has the root it had an hour
ago and a busy one has hundreds since, so a relative time is answered by when
the roots were superseded rather than by how many there are."
  (with-roots
    (pine.ns:write /theme :one)
    (pine.ns:write /theme :two)
    (pine.ns:write /theme :three)
    (is (= 0 (pine.ns:as-of "0")))
    (is (= 2 (pine.ns:as-of "2")) "a plain number is still supersessions")
    (is (= 0 (pine.ns:as-of "-0s"))
        "every root is zero seconds old, so the newest one answers")
    (is (eq :two (pine.ns:read /was/${(pine.ns:as-of "-0s")}/theme)))
    (is (= (1- (length (pine.ns:roots))) (pine.ns:as-of "-30s"))
        "nothing is thirty seconds old, so the oldest root still kept answers")
    (is (= (pine.ns:as-of "-30s") (pine.ns:as-of "-1h"))
        "and further back than that is the same root, not an error")))

(test revert-puts-the-world-back
  (with-roots
    (pine.ns:write /theme :one)
    (pine.ns:write /tab-width 8)
    (pine.ns:write /theme :two)
    (pine.ns:write /extra "added")
    (is (eq :two (pine.ns:read /theme)))
    (pine.ns:write /history (fset:seq :revert 1))
    (is (eq :one (pine.ns:read /theme)))
    (is (= 8 (pine.ns:read /tab-width)))
    (is (null (pine.ns:read /extra))
        "a path that did not exist then does not exist now")))

(test every-write-supersedes-and-the-root-is-kept
  (with-roots
    (dotimes (i 50) (pine.ns:write /n i))
    (is (= 50 (fset:size (pine.ns:read /history))))
    (is (= 48 (pine.ns:read /was/${0}/n)) "one supersession back")))
