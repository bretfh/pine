(in-package :pine/test)

(def-suite* :pine/data :in :pine)

(test a-map-shares-what-an-edit-did-not-touch
  (let* ((had (d:map :a 1 :b 2))
         (now (d:with had :b 3)))
    (is (= 2 (d:lookup had :b)))
    (is (= 3 (d:lookup now :b)))
    (is (= 1 (d:lookup now :a)))))

(test a-seq-is-indexed-and-immutable
  (let* ((had (d:as :seq '("one" "two")))
         (now (d:with had 1 "three")))
    (is (equal "two" (d:lookup had 1)))
    (is (equal "three" (d:lookup now 1)))
    (is (= 2 (d:size now)))))

(test a-place-swaps-under-two-threads
  "SWAP is a compare-and-swap loop over a place, so four threads racing on one
land a thousand increments and not fewer."
  (booted)
  (let ((cell (cons 0 nil))
        (threads nil))
    (dotimes (i 4)
      (push (bordeaux-threads:make-thread
             (lambda () (dotimes (n 250) (d:swap (car cell) #'1+))))
            threads))
    (mapc #'bordeaux-threads:join-thread threads)
    (is (= 1000 (car cell)))))

(test a-swap-evaluates-what-it-was-handed-once
  "The place's subforms, the function and the arguments are each evaluated once,
left to right. A place whose subforms ran twice would step an index twice, and a
retry that built the function again would build it once per contending thread."
  (let* ((order nil)
         (v (vector 10 20 30))
         (i 0)
         (fn-built 0))
    (flet ((where () (push :place order) v)
           (index () (push :index order) (prog1 i (incf i)))
           (adder () (push :function order) (incf fn-built) #'+)
           (by () (push :argument order) 5))
      (is (= 15 (d:swap (svref (where) (index)) (adder) (by))))
      (is (equal '(:place :index :function :argument) (reverse order)))
      (is (= 15 (svref v 0)) "and it wrote the place it read")
      (is (= 1 i) "the index moved once")
      (is (= 1 fn-built)))))

(test a-cas-evaluates-what-it-was-handed-once
  (let ((order nil)
        (v (vector :was)))
    (flet ((where () (push :place order) v)
           (had () (push :old order) :was)
           (fresh () (push :new order) :now))
      (is (d:cas (svref (where) 0) (had) (fresh)))
      (is (equal '(:place :old :new) (reverse order)))
      (is (eq :now (svref v 0)))
      (is (not (d:cas (svref v 0) :was :never))))))

(test a-table-claims-once
  (let ((table (d:table)))
    (is (equal "mine" (d:claim table :k "mine")))
    (is (equal "mine" (d:claim table :k "yours")))
    (d:drop! table :k)
    (is (null (d:lookup (d:all table) :k)))))

(test do-map-binds-what-it-was-given
  (let ((seen nil))
    (d:do-map (k v (d:map :a 1 :b 2))
      (push (cons k v) seen))
    (is (= 2 (length seen)))
    (is (equal 1 (cdr (assoc :a seen))))))

(test do-each-walks-a-list-too
  "KEYS and VALS answer lists. A walk that quietly does nothing over one is a loop
that looks like it ran."
  (let ((n 0))
    (d:do-each (v (d:keys (d:map :a 1 :b 2)) n) (declare (ignore v)) (incf n))
    (is (eql 2 n)))
  (let ((n 0))
    (d:do-each (v (d:vals (d:map :a 1 :b 2)) n) (declare (ignore v)) (incf n))
    (is (eql 2 n)))
  (let ((not-a-collection 42))
    (declare (special not-a-collection))
    (signals error (d:do-each (v not-a-collection) v))))

(test do-each-walks-a-map-a-seq-and-a-set
  (flet ((count-of (c) (let ((n 0)) (d:do-each (v c n) (declare (ignore v))
                                     (incf n)))))
    (is (= 2 (count-of (d:map :a 1 :b 2))))
    (is (= 3 (count-of (d:as :seq '(1 2 3)))))
    (is (= 2 (count-of (d:as :set '(1 2)))))))

(test a-take-empties-a-place-and-answers-what-was-there
  "The other half of SWAP: what comes back is exactly what was there, and what is
left is nothing. Four threads pushing while one takes lose nothing between them."
  (booted)
  (let ((cell (cons nil nil)))
    (d:swap (car cell) (lambda (had) (cons 1 had)))
    (d:swap (car cell) (lambda (had) (cons 2 had)))
    (is (equal '(2 1) (d:emptied (car cell))))
    (is (null (car cell)))
    (is (null (d:emptied (car cell)))))
  (let ((cell (cons nil nil))
        (taken nil)
        (threads nil))
    (dotimes (i 4)
      (push (bordeaux-threads:make-thread
             (lambda () (dotimes (n 250)
                          (d:swap (car cell) (lambda (had) (cons n had))))))
            threads))
    (loop :repeat 200
          :do (setf taken (append (d:emptied (car cell)) taken)))
    (mapc #'bordeaux-threads:join-thread threads)
    (setf taken (append (d:emptied (car cell)) taken))
    (is (= 1000 (length taken)) "nothing was pushed that did not come back")))

(test a-lookup-says-whether-anything-was-there
  "A collection may hold NIL, and holding it is not holding nothing. Without the
second answer a slider that has been dragged to nothing reads as one nobody has
touched."
  (is (equal '(nil t) (multiple-value-list (d:lookup (d:map :k nil) :k :none))))
  (is (equal '(:none nil) (multiple-value-list (d:lookup (d:map :k nil) :z :none))))
  (is (equal '(nil t) (multiple-value-list (d:lookup (list 1 nil 3) 1 :none))))
  (is (equal '(:none nil) (multiple-value-list (d:lookup (list 1 2) 9 :none))))
  (is (equal '(nil t) (multiple-value-list (d:lookup (list :a 1 :b nil) :b :none))))
  (is (equal '(:none nil)
             (multiple-value-list (d:lookup (list :a :b :c 1) :b :none)))
      "a key in a value's place is not a key"))

(test a-table-claims-a-nil-once
  "Whether something is there is asked of the table, not of what it holds: a key
claimed with NIL is claimed, and the next to ask must be told so."
  (let ((tb (d:table)))
    (is (null (d:claim tb "k" nil)))
    (is (= 1 (d:size (d:all tb))))
    (is (null (d:claim tb "k" :loser)) "the winner's nothing, not the loser's value")))

(test with-takes-its-shape-from-what-is-being-built
  "Not from the value. Given a value it builds a map whether the value is NIL or
not; a map built a piece at a time must not turn into a seq at the first nothing."
  (is (d:mapp (d:with nil "k" nil)))
  (is (null (d:lookup (d:with nil "k" nil) "k" :none)))
  (is (d:seqp (d:with nil "k")))
  (signals error (d:with (list 1 2) 0 :x)))

(test contains-means-one-thing
  "What a map holds is its values, the way a seq holds its elements. Whether it
has a key is what LOOKUP answers second."
  (is (d:contains (d:map :k :v) :v))
  (is (not (d:contains (d:map :k :v) :k)))
  (is (d:contains (d:seq :a :b) :b))
  (is (d:contains (list :a :b) :b))
  (is (d:contains (d:map :k (d:seq 1 2)) (d:seq 1 2)) "by value, not by identity"))

(test merged-says-so-rather-than-dropping-one
  (is (equal '((:a . 2) (:b . 3))
             (d:as :list (d:merged (d:map :a 1) (d:map :a 2 :b 3)))))
  (is (equal '((:a . 1)) (d:as :list (d:merged (d:map :a 1) nil))))
  (signals error (d:merged (d:seq 1) (d:seq 2))))

(test the-reading-vocabulary-is-total-over-one-domain
  "LOOKUP and SIZE took a list and KEYS and VALS did not, so half of it answered
about a list and half signalled that no method applied."
  (dolist (thunk (list (lambda () (d:lookup (list 10 20) 1))
                       (lambda () (d:size (list 10 20)))
                       (lambda () (d:keys (list 10 20)))
                       (lambda () (d:vals (list 10 20)))
                       (lambda () (d:contains (list 10 20) 20))
                       (lambda () (d:keys (make-hash-table)))
                       (lambda () (d:vals (make-hash-table)))))
    (finishes (funcall thunk))))

(defvar *given* nil
  "What a walk is handed, where the point is what it does at run time. Held in a
special so the shape is not one the compiler can work out and warn about: a walk
over the wrong thing is a question answered when it runs.")

(test the-three-walks-agree-about-nothing-and-about-nonsense
  (let ((*given* nil))
    (finishes (d:do-map (k v *given*) (declare (ignorable k v))))
    (finishes (d:do-pairs (k v *given*) (declare (ignorable k v)))))
  (let ((*given* (d:seq 1)))
    (signals error (d:do-map (k v *given*) (declare (ignorable k v)))))
  (let ((*given* 42))
    (signals error (d:do-pairs (k v *given*) (declare (ignorable k v)))))
  (let ((*given* (make-hash-table))
        (seen nil))
    (setf (gethash :a *given*) 1)
    (d:do-each (v *given*) (push v seen))
    (is (equal '(1) seen) "a hash table is something to walk here too")))

(test same-asks-about-the-value
  "EQUAL on two maps asks whether they are the same object, which for anything
built here is a question about the last edit."
  (is (d:same (d:map :a 1) (d:map :a 1)))
  (is (not (equal (d:map :a 1) (d:map :a 1))))
  (is (not (d:same (d:map :a 1) (d:map :a 2)))))

(test a-table-is-updated-in-one-act
  "A LOOKUP and a KEEP! with a gap between them are two, and whoever writes in the
gap is lost."
  (booted)
  (let ((tb (d:table))
        (threads nil))
    (dotimes (i 8)
      (push (bordeaux-threads:make-thread
             (lambda () (dotimes (n 50)
                          (d:update! tb "n" (lambda (had) (1+ (or had 0)))))))
            threads))
    (mapc #'bordeaux-threads:join-thread threads)
    (is (= 400 (d:lookup (d:all tb) "n")))))
