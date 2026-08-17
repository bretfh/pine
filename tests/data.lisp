(in-package :pine/test)

(def-suite* :pine/data :in :pine)

(test a-map-shares-what-an-edit-did-not-touch
  (let* ((had (d:map :a 1 :b 2))
         (now (d:with had :b 3)))
    (is (= 2 (d:at had :b)))
    (is (= 3 (d:at now :b)))
    (is (= 1 (d:at now :a)))))

(test a-seq-is-indexed-and-immutable
  (let* ((had (d:as :seq '("one" "two")))
         (now (d:with-at had 1 "three")))
    (is (equal "two" (d:at had 1)))
    (is (equal "three" (d:at now 1)))
    (is (= 2 (d:size now)))))

(test a-box-swaps-under-two-threads
  (booted)
  (let ((b (d:box 0))
        (threads nil))
    (dotimes (i 4)
      (push (bordeaux-threads:make-thread
             (lambda () (dotimes (n 250) (d:swap! b #'1+))))
            threads))
    (mapc #'bordeaux-threads:join-thread threads)
    (is (= 1000 (d:held b)))))

(test a-table-claims-once
  (let ((table (d:table)))
    (is (equal "mine" (d:claim table :k "mine")))
    (is (equal "mine" (d:claim table :k "yours")))
    (d:drop! table :k)
    (is (null (d:at (d:all table) :k)))))

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
  (let ((not-a-collection (d:held (d:box 42))))
    (signals error (d:do-each (v not-a-collection) (declare (ignore v))))))

(test do-each-walks-a-map-a-seq-and-a-set
  (flet ((count-of (c) (let ((n 0)) (d:do-each (v c n) (declare (ignore v))
                                     (incf n)))))
    (is (= 2 (count-of (d:map :a 1 :b 2))))
    (is (= 3 (count-of (d:as :seq '(1 2 3)))))
    (is (= 2 (count-of (d:as :set '(1 2)))))))
