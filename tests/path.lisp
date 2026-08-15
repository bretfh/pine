(in-package :pine/test)

(def-suite* :pine/path :in :pine)

(test a-path-is-read-as-sugar-and-means-a-list-of-segments
  (let ((p (pine/path/path:parse "/buf/scratch/text")))
    (is (equal "/buf/scratch/text" (pine/path/path:text p)))
    (is (equal "text" (pine/path/path:leaf p)))
    (is (equal "/buf/scratch" (pine/path/path:text (pine/path/path:parent p))))
    (is-false (pine/path/path:patternp p))))

(test a-pattern-binds-what-it-matched
  (let ((p (pine/path/path:parse "/buf/?name/text")))
    (is-true (pine/path/path:patternp p))
    (is (equal '(name) (pine/path/path:binders p)))
    (is (equal '((name . "scratch"))
               (pine/path/path:match p (pine/path/path:parse "/buf/scratch/text"))))
    (is (null (pine/path/path:match p (pine/path/path:parse "/win/scratch/text"))))))

(test a-star-covers-one-name-and-two-cover-any-run
  (is (pine/path/path:match (pine/path/path:parse "/buf/*/text")
                            (pine/path/path:parse "/buf/scratch/text")))
  (is (null (pine/path/path:match (pine/path/path:parse "/buf/*/text")
                                  (pine/path/path:parse "/buf/a/b/text"))))
  (is (pine/path/path:match (pine/path/path:parse "/buf/**/text")
                            (pine/path/path:parse "/buf/a/b/text"))))

(test the-sugar-and-the-generics-mean-the-same-node
  (let ((w (pine/world/world:make-world)))
    (pine/world/world:place w '("buf" "scratch" "text") "hello")
    (is (eq (pine/world/world:at w "buf/scratch/text")
            (pine/path/place:at (pine/path/path:parse "/buf/scratch/text") w)))
    (is (equal "hello" (pine/path/place:contents
                        (pine/path/path:parse "/buf/scratch/text") w)))
    (setf (pine/path/place:contents (pine/path/path:parse "/buf/scratch/text") w)
          "written through the sugar")
    (is (equal "written through the sugar"
               (pine/fs/node:contents (pine/world/world:at w "buf/scratch/text"))))))

(test a-pattern-answers-every-node-it-covers
  (let ((w (pine/world/world:make-world)))
    (pine/world/world:place w '("buf" "a" "text") 1)
    (pine/world/world:place w '("buf" "b" "text") 2)
    (pine/world/world:place w '("win" "c" "text") 3)
    (is (equal '("/buf/a/text" "/buf/b/text")
               (mapcar #'pine/fs/node:full-name
                       (pine/path/place:matching
                        (pine/path/path:parse "/buf/?name/text") w))))))

(test the-reader-is-sugar-and-nothing-underneath-it-reads-it-in
  (let ((found (remove-if-not
                (lambda (file) (search "named-readtables:in-readtable" (uiop:read-file-string file)))
                (%files))))
    (is (null (remove-if (lambda (f)
                           (or (search "/path/" (namestring f))
                               (search "/ts/" (namestring f))))
                         found))
        "only path/ and the language declarations may declare the readtable")))

(test a-path-is-built-from-pieces-and-splices-what-it-is-given
  (let ((p (pine/path/path:path "buf" "scratch" "text")))
    (is (equal "/buf/scratch/text" (pine/path/path:text p)))
    (is (equal "/buf/scratch/text/of"
               (pine/path/path:text (pine/path/path:join p "of"))))
    (is (equal "/buf/scratch"
               (pine/path/path:text
                (pine/path/path:path (pine/path/path:parse "/buf") "scratch")))
        "a path in the pieces is spliced, not printed into a name")))

(test the-root-is-a-path-with-nothing-in-it
  (let ((root (pine/path/path:parse "/")))
    (is-true (pine/path/path:rootp root))
    (is (equal "/" (pine/path/path:text root)))
    (is (null (pine/path/path:leaf root)))
    (is-true (pine/path/path:rootp (pine/path/path:parent root))
             "the parent of the root is the root")))

(test a-prefix-says-what-is-under-it
  (let ((buf (pine/path/path:parse "/buf"))
        (text (pine/path/path:parse "/buf/scratch/text")))
    (is-true (pine/path/path:prefixp buf text))
    (is-false (pine/path/path:prefixp text buf))
    (is (equal "/scratch/text" (pine/path/path:text (pine/path/path:under buf text))))
    (is-false (pine/path/path:prefixp (pine/path/path:parse "/win") text))))

(test a-deep-wildcard-covers-any-depth-and-a-star-covers-one-name
  (let ((one (pine/path/path:parse "/buf/*/text"))
        (deep (pine/path/path:parse "/buf/**")))
    (is-true (pine/path/path:patternp one))
    (is-true (pine/path/path:match one (pine/path/path:parse "/buf/scratch/text")))
    (is-false (pine/path/path:match one (pine/path/path:parse "/buf/a/b/text"))
              "one star is one name")
    (is-true (pine/path/path:match deep (pine/path/path:parse "/buf/a/b/text")))
    (is-true (pine/path/path:match deep (pine/path/path:parse "/buf/a")))))

(test a-binder-says-what-it-bound
  (let ((pattern (pine/path/path:parse "/buf/?name/text")))
    (is (equal '("NAME") (mapcar #'symbol-name (pine/path/path:binders pattern))))
    (let ((bound (pine/path/path:match pattern
                                       (pine/path/path:parse "/buf/scratch/text"))))
      (is-true bound)
      (is (equal "scratch"
                 (cdr (assoc (first (pine/path/path:binders pattern)) bound)))
          "what it matched is on the name it bound"))
    (is-false (pine/path/path:match pattern (pine/path/path:parse "/win/scratch/text")))))

(test a-path-reads-back-as-what-it-printed
  (dolist (text '("/" "/buf" "/buf/scratch/text" "/buf/?name/text" "/buf/*/text"))
    (is (equal text (pine/path/path:text (pine/path/path:parse text)))
        "~a did not survive the round trip" text)))
