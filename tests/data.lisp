(in-package :pine.test)
(named-readtables:in-readtable pine.data:syntax)

(def-suite* :pine.data :in :pine)

(defun rd (string)
  "Read STRING under pine's syntax. IN-READTABLE binds the readtable for this
file's own forms, which are read when it compiles; a test that reads at run
time has to say so."
  (let ((*readtable* (named-readtables:find-readtable 'pine.data:syntax)))
    (read-from-string string)))

;;;; the literals

(test a-seq-literal-holds-its-elements-in-order
  (let ((s [1 2 3]))
    (is (fset:seq? s))
    (is (= 3 (fset:size s)))
    (is (= 1 (fset:lookup s 0)))
    (is (= 3 (fset:lookup s 2)))))

(test a-map-literal-pairs-its-forms
  (let ((m {:a 1 :b 2}))
    (is (fset:map? m))
    (is (= 2 (fset:size m)))
    (is (= 1 (fset:lookup m :a)))
    (is (= 2 (fset:lookup m :b)))))

(test a-set-literal-holds-its-members
  (let ((s #{:paren :flycheck}))
    (is (fset:set? s))
    (is (fset:contains? s :paren))
    (is (not (fset:contains? s :none)))))

(test the-empty-literals-are-empty
  (is (fset:empty? {}))
  (is (fset:empty? []))
  (is (fset:empty? #{})))

(test literals-evaluate-their-elements
  (let ((k :dyn) (v 3))
    (is (= 3 (fset:lookup {k v} :dyn)))
    (is (= 10 (fset:lookup {:x (* 2 5)} :x)))
    (is (= 7 (fset:lookup [(+ 3 4)] 0)))))

(test literals-nest
  (let ((m {:xs [1 2] :m {:a 1} :s #{:one}}))
    (is (fset:seq? (fset:lookup m :xs)))
    (is (= 1 (fset:lookup (fset:lookup m :m) :a)))
    (is (fset:contains? (fset:lookup m :s) :one))))

(test an-odd-map-literal-is-a-read-error
  (signals error (rd "{:a 1 :b}")))

(test a-stray-close-is-a-read-error
  (signals error (rd "}"))
  (signals error (rd "]")))

(test the-literals-read-at-run-time-too
  (is (fset:equal? {:a 1} (eval (rd "{:a 1}"))))
  (is (fset:equal? [1 2] (eval (rd "[1 2]"))))
  (is (fset:equal? #{1 2} (eval (rd "#{1 2}")))))

;;;; what the engine rests on

(test equal-values-are-equal-however-they-were-built
  (is (fset:equal? {:a 1 :b [1 2]} {:b [1 2] :a 1}))
  (is (fset:equal? #{1 2} #{2 1}))
  (is (not (fset:equal? {:a 1} {:a 2}))))

(test an-unchanged-subtree-is-the-same-object
  "The de-duplication rule is a pointer compare on everything that did not
move, which is what makes a write cheap and a tree diff shallow."
  (let* ((inner [1 2 3])
         (m {:x inner :y 1})
         (n (fset:with m :y 2)))
    (is (not (eq m n)))
    (is (eq inner (fset:lookup n :x)))
    (is (= 2 (fset:lookup n :y)))
    (is (= 1 (fset:lookup m :y)) "the old value is untouched")))

;;;; fn

(test fn-takes-a-seq-arglist
  (is (= 3 (funcall (pine.data:fn [a b] (+ a b)) 1 2)))
  (is (eq :none (funcall (pine.data:fn [] :none))))
  (is (eq :none (funcall (pine.data:fn () :none))))
  (is (= 5 (funcall (pine.data:fn [x &optional (y 4)] (+ x y)) 1))))

;;;; keys, vals, fold

(test keys-and-vals-correspond
  (let ((m {:a 1 :b 2 :c 3}))
    (is (equal '(1 2 3)
               (mapcar (lambda (k) (fset:lookup m k)) (pine.data:keys m))))
    (is (equal (pine.data:vals m)
               (mapcar (lambda (k) (fset:lookup m k)) (pine.data:keys m))))))

(test keys-of-a-set-are-its-members
  (is (equal '(1 2 3) (sort (pine.data:keys #{3 1 2}) #'<))))

(test fold-visits-a-map-by-key-and-value
  (is (= 6 (pine.data:fold {:a 1 :b 2 :c 3} 0
                           (pine.data:fn [acc k v] (declare (ignore k)) (+ acc v))))))

(test fold-visits-a-seq-and-a-set-by-element
  (is (= 6 (pine.data:fold [1 2 3] 0 (pine.data:fn [acc x] (+ acc x)))))
  (is (= 6 (pine.data:fold #{1 2 3} 0 (pine.data:fn [acc x] (+ acc x))))))

;;;; serialization

(test data-round-trips-through-a-string
  (dolist (value (list {:a 1 :b "two" :c :three}
                       [1 2.5 "x" :y]
                       #{:a :b}
                       {:xs [1 [2 3]] :m {:deep #{1}}}
                       {}
                       []
                       42
                       "plain"
                       :kw))
    (let ((back (pine.data:deserialize (pine.data:serialize value))))
      (is (fset:equal? value back)
          "~a did not round trip, got ~a" (pine.data:serialize value) back))))

(test deserialize-reads-and-does-not-evaluate
  "The store holds whatever anyone wrote, so reading it back must not run it."
  (let ((m (pine.data:deserialize "{:a (error \"boom\") :b 1}")))
    (is (equal '(error "boom") (fset:lookup m :a)))
    (is (= 1 (fset:lookup m :b))))
  (signals error (pine.data:deserialize "#.(error \"boom\")")))
