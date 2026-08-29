(defpackage :pine/test/kernel
  (:use :cl :fiveam)
  (:local-nicknames (:d :pine/data) (:bt :bordeaux-threads)
                    (:name :pine/kernel/name) (:place :pine/kernel/place)
                    (:graph :pine/kernel/graph) (:tell :pine/kernel/tell)
                    (:tree :pine/kernel/tree) (:watch :pine/kernel/watch)
                    (:hands :pine/run/hands) (:log :pine/kernel/log)
                    (:k :pine/kernel/call)))
(in-package :pine/test/kernel)

(def-suite :pine/kernel)
(in-suite :pine/kernel)

(defmacro with-tree (&body body)
  "A fresh namespace for one test, and nobody listening from the last one."
  `(let ((was tree:*root*))
     (unwind-protect
          (progn (setf tree:*root* (tree:make-root))
                 (tell:forget-all)
                 (watch:forget-all)
                 ,@body)
       (setf tree:*root* was)
       (tell:forget-all)
       (watch:forget-all))))

(defmacro with-hands ((&key (workers 8)) &body body)
  "One actor system for one test, and the kernel given its hands.

The kernel keeps no threads. What spreads work over the cores is the image's
shared dispatcher, which is the same one messages go through, and this is how a
test lends it one."
  `(let ((sys (sento.actor-system:make-actor-system
               (list :dispatchers
                     (list :shared (list :workers ,workers :strategy :random))))))
     (unwind-protect (progn (hands:take-up sys) ,@body)
       (hands:let-go)
       (ignore-errors (sento.actor-context:shutdown sys :wait t)))))

(defun spun (n thunk)
  "N threads doing the same thing, all of them finished before this answers."
  (let ((threads (loop :repeat n :collect (bt:make-thread thunk))))
    (mapc #'bt:join-thread threads)
    t))

(test a-name-is-from-the-root-however-it-is-written
  (is (equal "/dev/audio" (name:whole (name:parse "/dev/audio"))))
  (is (equal "/dev/audio" (name:whole (name:parse "dev/audio")))
      "a leading separator is spelling, not meaning")
  (is (equal "/dev/audio" (name:whole (name:parse "/dev//audio/")))
      "nothing stands between two separators, so there is nothing to name")
  (is (equal "/" (name:whole (name:parse ""))))
  (is (equal '("dev" "audio") (name:spelled (name:parse "/dev/audio")))))

(test read-tells-the-four-ways-of-holding-nothing-apart
  (with-tree
    (k:write "/holds/a-value" 50)
    (k:write "/holds/nothing" nil)
    (multiple-value-bind (v how) (k:read "/holds/a-value")
      (is (eql 50 v)) (is (eq :held how)))
    (multiple-value-bind (v how) (k:read "/holds/nothing")
      (is (null v))
      (is (eq :held how) "somebody wrote nothing there, and that is a value"))
    (multiple-value-bind (v how) (k:read "/holds")
      (is (null v))
      (is (eq :branch how) "a branch holds nothing by definition"))
    (multiple-value-bind (v how) (k:read "/nowhere")
      (is (null v))
      (is (eq :absent how) "nothing stands there at all"))))

(test a-write-makes-what-is-not-there-and-erase-takes-it-away
  (with-tree
    (is (not (k:standsp "/a/b/c")))
    (k:write "/a/b/c" :made)
    (is (k:standsp "/a/b/c"))
    (is (k:standsp "/a/b") "what is above a place is a place")
    (is (equal '("b") (k:ls "/a")))
    (k:erase "/a/b")
    (is (not (k:standsp "/a/b")))
    (is (not (k:standsp "/a/b/c")) "what was under it went with it")
    (is (k:standsp "/a"))))

(test a-derived-place-is-worked-out-again-when-what-it-read-moves
  (with-tree
    (let ((ran 0))
      (k:write "/n" 1)
      (k:make "/twice" :derived (lambda () (incf ran) (* 2 (k:read "/n"))))
      (is (eql 2 (k:read "/twice")))
      (is (eql 2 (k:read "/twice")))
      (is (eql 1 ran) "worked out once and remembered")
      (k:write "/n" 5)
      (is (eql 10 (k:read "/twice")))
      (is (eql 2 ran) "and worked out again, once, when what it read moved"))))

(test nothing-declares-what-it-reads
  (with-tree
    (k:write "/which" :a)
    (k:write "/a" 1)
    (k:write "/b" 2)
    (k:make "/either" :derived
            (lambda () (if (eq :a (k:read "/which")) (k:read "/a") (k:read "/b"))))
    (is (eql 1 (k:read "/either")))
    (k:write "/b" 99)
    (is (eql 1 (k:read "/either")) "it does not read /b, so /b moving is nothing")
    (k:write "/which" :b)
    (is (eql 99 (k:read "/either")))
    (k:write "/a" 1000)
    (is (eql 99 (k:read "/either"))
        "and now it does not read /a, so what it stopped reading was given up")))

(test an-erased-place-stops-being-worked-out
  (with-tree
    (k:write "/cpu" 1)
    (k:make "/watcher" :derived (lambda () (k:read "/cpu")))
    (k:read "/watcher")
    (let ((cpu (tree:reach "/cpu")))
      (is (eql 1 (d:size (place:readers cpu))))
      (k:erase "/watcher")
      (is (eql 0 (d:size (place:readers cpu)))
          "a place taken away gives up what it read, or it is worked out for ever"))))

(test a-thousand-erased-places-leave-nothing-behind
  (with-tree
    (k:write "/cpu" 1)
    (dotimes (i 1000)
      (k:make (format nil "/each/~d" i) :derived (lambda () (k:read "/cpu"))))
    (dotimes (i 1000) (k:read (format nil "/each/~d" i)))
    (is (eql 1000 (d:size (place:readers (tree:reach "/cpu")))))
    (k:erase "/each")
    (is (eql 0 (d:size (place:readers (tree:reach "/cpu"))))
        "the reader set comes back to where it started")))

(test a-diamond-answers-from-one-state
  (with-tree
    (k:write "/top" 1)
    (k:make "/left" :derived (lambda () (* 10 (k:read "/top"))))
    (k:make "/right" :derived (lambda () (* 100 (k:read "/top"))))
    (k:make "/bottom" :derived (lambda () (+ (k:read "/left") (k:read "/right"))))
    (is (eql 110 (k:read "/bottom")))
    (k:write "/top" 2)
    (is (eql 220 (k:read "/bottom"))
        "both sides are worked out from the same /top, never one from each")))

(test a-place-worked-out-from-itself-says-so
  (with-tree
    (k:make "/mine" :derived (lambda () (k:read "/mine")))
    (signals error (k:read "/mine"))))

(test what-is-worked-out-is-not-written
  (with-tree
    (k:write "/n" 1)
    (k:make "/twice" :derived (lambda () (* 2 (k:read "/n"))))
    (signals error (k:write "/twice" 99))
    (is (eql 2 (k:read "/twice"))
        "and it still works itself out, rather than having quietly become 99")))

(test swap-is-one-act
  (with-tree
    (k:write "/muted" nil)
    (is (eq t (k:swap "/muted" #'not)))
    (is (eq nil (k:swap "/muted" #'not)) "which is what a button that flips needs")
    (k:write "/n" 0)
    (spun 8 (lambda () (dotimes (i 250) (k:swap "/n" #'1+))))
    (is (eql 2000 (k:read "/n"))
        "eight threads and two thousand raises, and not one of them lost")))

(test one-thread-works-one-place-out
  (with-tree
    (let ((ran 0))
      (k:write "/n" 1)
      (k:make "/slow" :derived
              (lambda () (incf ran) (sleep 0.05) (k:read "/n")))
      (spun 8 (lambda () (k:read "/slow")))
      (is (eql 1 ran)
          "the other seven waited rather than running somebody's code again"))))

(test what-was-worked-out-from-what-has-moved-is-never-anybodys-answer
  (with-tree
    (k:write "/n" 0)
    (k:make "/twice" :derived (lambda () (* 2 (k:read "/n"))))
    (let ((wrong 0) (stop nil))
      (let ((writers (loop :repeat 4
                           :collect (bt:make-thread
                                     (lambda ()
                                       (loop :until stop
                                             :do (k:write "/n" (random 1000)))))))
            (readers (loop :repeat 4
                           :collect (bt:make-thread
                                     (lambda ()
                                       (loop :repeat 2000
                                             :do (let* ((twice (k:read "/twice"))
                                                        (n (k:read "/n")))
                                                   (declare (ignorable n))
                                                   (unless (evenp twice)
                                                     (incf wrong)))))))))
        (mapc #'bt:join-thread readers)
        (setf stop t)
        (mapc #'bt:join-thread writers))
      (is (eql 0 wrong)
          "~d answers were worked out from a state that never stood" wrong))))

(test a-write-that-reaches-forty-places-is-one-piece-of-news
  (with-tree
    (let ((tellings 0) (places 0))
      (k:write "/n" 1)
      (dotimes (i 40)
        (k:make (format nil "/each/~d" i) :derived (lambda () (k:read "/n"))))
      (dotimes (i 40) (k:read (format nil "/each/~d" i)))
      (setf (tell:on-move :counting)
            (lambda (moved) (incf tellings) (incf places (length moved))))
      (k:write "/n" 2)
      (is (eql 1 tellings) "told once")
      (is (eql 41 places)
          "and told the whole of it: what was written, and the forty it reached"))))

(test places-that-move-together-are-told-together
  (with-tree
    (let ((tellings 0))
      (k:write "/a" 1)
      (k:write "/b" 1)
      (setf (tell:on-move :counting) (lambda (moved) (declare (ignore moved))
                                       (incf tellings)))
      (k:together (k:write "/a" 2) (k:write "/b" 2))
      (is (eql 1 tellings)
          "so nothing is ever worked out from the pair that stood between them"))))

(test a-watch-is-told-when-what-it-watches-moves
  (with-tree
    (watch:attend)
    (let ((said nil))
      (k:write "/title" "one")
      (k:watch "/title" (lambda (p now) (declare (ignore p)) (push now said)))
      (k:write "/title" "two")
      (k:write "/title" "two")
      (k:write "/title" "three")
      (is (equal '("three" "two") said)
          "and not told again where it did not move"))))

(test a-watch-on-something-worked-out-is-told-when-its-inputs-move
  (with-tree
    (watch:attend)
    (let ((said nil))
      (k:write "/percent" 90)
      (k:make "/low" :derived (lambda () (< (k:read "/percent") 15)))
      (k:watch "/low" (lambda (p now) (declare (ignore p)) (push now said)))
      (k:write "/percent" 10)
      (k:write "/percent" 5)
      (k:write "/percent" 90)
      (is (equal '(nil t) said)
          "nobody watching /low had to know it was worked out from /percent"))))

(test the-world-is-asked-and-never-remembered
  (with-tree
    (let ((asked 0) (n 7))
      (k:make "/dev/thing" :world (lambda () (incf asked) n))
      (is (eql 7 (k:read "/dev/thing")))
      (is (eql 7 (k:read "/dev/thing")))
      (is (eql 2 asked) "asked every time, because the world moves on its own")
      (setf n 9)
      (is (eql 9 (k:read "/dev/thing"))))))

(test a-branch-the-world-fills-lists-what-is-there-now
  (with-tree
    (let ((there '("a" "b")))
      (k:make "/sinks" :listing (lambda () there)
              :each (lambda (said) (place:make-place :world said
                                                     :asks (constantly said))))
      (is (equal '("a" "b") (k:ls "/sinks")))
      (is (equal "a" (k:read "/sinks/a")))
      (setf there '("a" "b" "c"))
      (is (equal '("a" "b" "c") (k:ls "/sinks")))
      (is (eq (tree:reach "/sinks/a") (tree:reach "/sinks/a"))
          "and hands out the same place every time, so a watch on one keeps"))))

(defmacro with-log ((where) &body body)
  "A log of its own for one test, taken away afterwards."
  `(let ((,where (format nil "/tmp/pine-kernel-log-~d.log" (random 100000000))))
     (unwind-protect (progn (log:keeping ,where) ,@body)
       (log:forget-keeping)
       (ignore-errors (delete-file ,where)))))

(test what-outlives-the-image-is-written-down-and-comes-back
  (with-tree
    (with-log (where)
      (k:write "/dev/name" "laptop")
      (k:write "/n" 41)
      (k:swap "/n" #'1+)
      (log:settled)
      (setf tree:*root* (tree:make-root))
      (is (not (k:standsp "/n")) "a fresh image stands empty")
      (log:replay where)
      (is (eql 42 (k:read "/n")))
      (is (equal "laptop" (k:read "/dev/name"))
          "and the tree is the fold over what was said"))))

(test what-is-worked-out-is-not-written-down
  (with-tree
    (with-log (where)
      (k:write "/n" 2)
      (k:make "/twice" :derived (lambda () (* 2 (k:read "/n"))))
      (k:read "/twice")
      (k:write "/n" 3)
      (k:read "/twice")
      (log:settled)
      (let ((names (loop :for entry :in (log:entries where)
                         :append (mapcar #'car (second entry)))))
        (is (not (member "/twice" names :test #'equal))
            "it is worked out again from what it read, and that is written")
        (is (member "/n" names :test #'equal))))))

(test a-group-of-writes-is-one-line-in-the-log
  (with-tree
    (with-log (where)
      (k:together (k:write "/a" 1) (k:write "/b" 2) (k:write "/c" 3))
      (log:settled)
      (let ((entries (log:entries where)))
        (is (eql 1 (length entries))
            "one telling is one entry, so the log is a list of states that stood")
        (is (eql 3 (length (second (first entries)))))))))

(test the-log-says-what-stood-at-a-time
  (with-tree
    (with-log (where)
      (k:write "/n" 1)
      (log:settled)
      (let ((then (get-universal-time)))
        (sleep 1.1)
        (k:write "/n" 2)
        (log:settled)
        (is (eql 2 (k:read "/n")))
        (is (eql 1 (d:lookup (log:at-time then where) "/n"))
            "and what stood then is still what stood then")))))

(test compacting-says-the-same-thing-in-fewer-lines
  (with-tree
    (with-log (where)
      (dotimes (i 50) (k:write "/n" i))
      (k:write "/other" :kept)
      (log:settled)
      (is (< 1 (length (log:entries where))))
      (log:compact where)
      (is (eql 1 (length (log:entries where))))
      (setf tree:*root* (tree:make-root))
      (log:replay where)
      (is (eql 49 (k:read "/n")) "and the fold is unchanged")
      (is (eq :kept (k:read "/other"))))))

(test the-cores-work-them-all-out-and-get-them-all-right
  (with-tree
    (with-hands (:workers 8)
      (progn
           (k:write "/seed" 1)
           (let ((places (loop :for i :below 200
                               :collect (let ((i i))
                                          (k:make (format nil "/each/~d" i) :derived
                                                  (lambda () (+ i (k:read "/seed"))))))))
             (graph:all-worked places)
             (is (every #'graph:freshp places))
             (is (equal (loop :for i :below 200 :collect (+ i 1))
                        (loop :for i :below 200
                              :collect (k:read (format nil "/each/~d" i)))))
             (k:write "/seed" 100)
             (graph:all-worked places)
             (is (equal (loop :for i :below 200 :collect (+ i 100))
                        (loop :for i :below 200
                              :collect (k:read (format nil "/each/~d" i))))
                 "two hundred places on eight hands, and every one of them its own"))))))

(test a-watcher-that-will-not-hurry-does-not-hold-up-a-write
  (with-tree
    (with-hands (:workers 4)
      (progn
           (watch:attend)
           (k:write "/n" 0)
           (k:watch "/n" (lambda (p now) (declare (ignore p now)) (sleep 0.2)))
           (let ((at (get-internal-real-time)))
             (k:write "/n" 1)
             (let ((took (/ (- (get-internal-real-time) at)
                            (float internal-time-units-per-second))))
               (is (< took 0.1)
                   "the write took ~,3f s, so it waited for the watcher" took)))))
    (sleep 0.3)))

(test what-is-added-up-was-true-all-at-once
  "The one that took the longest to make true.

A place that reads many others reads them one after another. If something moved
partway through, what it added up is part one state and part another -- and its
own mark has not moved either, because the walk that would move it has not got
there yet. So it looks whole and is not. What makes it exact is that every value
carries the list of what it read and which version each of those was, and that
list is looked at again before the answer is handed back."
  (with-tree
    (let ((width 8) (layers 3) (stop nil) (torn 0) (reads 0))
      (k:write "/n" 0)
      (let ((below (loop :for i :below width
                         :collect (let ((name (format nil "/l0/~d" i)))
                                    (k:make name :derived (lambda () (k:read "/n")))
                                    name))))
        (loop :for layer :from 1 :below layers
              :do (setf below
                        (loop :for i :below width
                              :collect (let ((a (nth (mod (* 2 i) (length below))
                                                     below))
                                             (b (nth (mod (1+ (* 2 i)) (length below))
                                                     below))
                                             (name (format nil "/l~d/~d" layer i)))
                                         (k:make name :derived
                                                 (lambda () (+ (k:read a) (k:read b))))
                                         name))))
        (let ((top below))
          (k:make "/all" :derived
                  (lambda () (loop :for each :in top :sum (k:read each))))))
      (k:write "/n" 1)
      (let ((factor (k:read "/all")))
        (let ((writers (loop :repeat 2
                             :collect (bt:make-thread
                                       (lambda ()
                                         (loop :until stop
                                               :do (k:write "/n" (1+ (random 1000)))))))))
          (loop :repeat 20000
                :do (incf reads)
                    (unless (zerop (mod (k:read "/all") factor)) (incf torn)))
          (setf stop t)
          (mapc #'bt:join-thread writers))
        (is (eql 0 torn)
            "~d of ~d answers were added up from a state that never stood"
            torn reads)))))

(test a-reading-that-still-stands-is-a-reading-that-was-right
  "The version moves before the value as well as after, so there is a version in
the middle of every write that names the old value and then the new one. Nothing
rests on that one: a write always moves the version again after the value, so a
reading taken in the middle never survives being checked.

Which is the thing worth asserting -- not that a version never names two values,
but that a reading which still stands at the end was a true one."
  (with-tree
    (let ((stop nil) (kept nil) (wrong 0))
      (k:write "/n" 0)
      (let ((w (bt:make-thread (lambda ()
                                 (loop :until stop
                                       :do (k:write "/n" (1+ (random 1000))))))))
        (let ((p (tree:reach "/n")))
          (loop :repeat 60000
                :do (let ((reading (cons :reading nil)))
                      (let ((v (let ((place:*reading* reading))
                                 (place:held p))))
                        (push (cons (cdr (first (cdr reading))) v) kept)))))
        (setf stop t)
        (bt:join-thread w))
      (let* ((p (tree:reach "/n"))
             (now (place:version p))
             (value (place:holds p)))
        (dolist (each kept)
          (when (and (eql (car each) now) (not (eql (cdr each) value)))
            (incf wrong)))
        (is (eql 0 wrong)
            "~d readings still stood at version ~d and were wrong about it"
            wrong now)))))
