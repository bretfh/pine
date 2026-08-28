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
