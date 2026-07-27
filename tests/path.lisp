(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.path :in :pine)

(defun rdp (string)
  "Read STRING under pine's path syntax, at run time."
  (let ((*readtable* (named-readtables:find-readtable 'pine.path:syntax)))
    (read-from-string string)))

;;;; literals

(test a-path-literal-is-its-segments
  (let ((p /audio/volume))
    (is (pine.path:pathp p))
    (is (equal '("audio" "volume") (pine.path:segments p)))
    (is (string= "/audio/volume" (pine.path:text p)))))

(test the-root-has-no-segments
  (is (pine.path:rootp (pine.path:root)))
  (is (string= "/" (pine.path:text (pine.path:root))))
  (is (pine.path:rootp (eval (rdp "/")))))

(test a-path-prints-as-it-reads
  (dolist (text '("/audio/volume" "/buf/scratch/point" "/win/0/weight" "/"))
    (is (string= text (pine.path:text (eval (rdp text)))))))

(test a-literal-path-is-a-constant
  "No interpolation means the object exists at read time, so it can be a
hash key and a fasl constant."
  (is (pine.path:pathp (rdp "/a/b")))
  (is (not (pine.path:patternp /a/b))))

;;;; taking one apart

(test parent-leaf-child-and-under
  (let ((p /buf/scratch/point))
    (is (string= "/buf/scratch" (pine.path:text (pine.path:parent p))))
    (is (string= "point" (pine.path:leaf p)))
    (is (string= "/buf/scratch/point/x"
                 (pine.path:text (pine.path:child p "x"))))
    (is (equal '("scratch" "point") (pine.path:under /buf p)))
    (is (null (pine.path:under /win p)))
    (is (pine.path:prefixp /buf p))
    (is (not (pine.path:prefixp /win p)))))

(test the-parent-of-the-root-is-the-root
  (is (pine.path:rootp (pine.path:parent (pine.path:root)))))

(test path-splices-paths-and-lists-and-names-anything-else
  (is (string= "/a/b/c" (pine.path:text (pine.path:path /a/b "c"))))
  (is (string= "/a/b/c" (pine.path:text (pine.path:path '("a" "b") "c"))))
  (is (string= "/win/3/buf" (pine.path:text (pine.path:path /win 3 :buf)))
      "an integer names its decimal, a keyword its lowercase")
  (is (string= "/a/b" (pine.path:text (pine.path:parse "/a/b")))))

;;;; interpolation

(test one-segment-interpolates
  (let ((n 3) (which "volume"))
    (is (string= "/win/3/buf" (pine.path:text /win/${n}/buf)))
    (is (string= "/audio/volume" (pine.path:text /audio/${which})))))

(test a-splice-takes-several-segments
  (let ((p /a/b) (s "x/y"))
    (is (string= "/root/a/b/leaf" (pine.path:text /root/$@{p}/leaf)))
    (is (string= "/root/x/y" (pine.path:text /root/$@{s})))
    (is (string= "/root/x/y" (pine.path:text /root/$@{'("x" "y")})))))

(test an-interpolated-value-is-one-segment-even-when-it-splits
  (let ((s "a/b"))
    (is (string= "/x/a/b" (pine.path:text /x/${s}))
        "name does not split; only $@ splices")))

(test an-interpolation-needs-a-brace
  (signals error (rdp "/a/$b")))

;;;; patterns

(test a-wildcard-matches-one-segment
  (is (pine.path:patternp /proc/*))
  (is (pine.path:match /proc/* /proc/editor))
  (is (not (pine.path:match /proc/* /proc/editor/state)))
  (is (not (pine.path:match /proc/* /buf/editor))))

(test a-deep-wildcard-matches-any-depth
  (is (pine.path:match /proc/** /proc))
  (is (pine.path:match /proc/** /proc/editor))
  (is (pine.path:match /proc/** /proc/editor/state))
  (is (pine.path:match /**/err /host/box/err))
  (is (pine.path:match /**/err /err))
  (is (not (pine.path:match /**/err /host/box/log))))

(test alternation-matches-one-of-the-names
  (is (pine.path:match /audio/#{volume muted} /audio/volume))
  (is (pine.path:match /audio/#{volume muted} /audio/muted))
  (is (not (pine.path:match /audio/#{volume muted} /audio/sink))))

(test a-binder-matches-and-binds
  (multiple-value-bind (ok bindings) (pine.path:match /net/wifi/?ssid/signal
                                                      /net/wifi/home/signal)
    (is-true ok)
    (is (string= "home" (fset:lookup bindings 'ssid)))))

(test a-rest-binder-takes-what-is-left
  (multiple-value-bind (ok bindings) (pine.path:match /file/?@rest
                                                      /file/etc/hosts)
    (is-true ok)
    (is (equal '("etc" "hosts") (fset:lookup bindings 'rest))))
  (multiple-value-bind (ok bindings) (pine.path:match /file/?@rest /file)
    (is-true ok)
    (is (null (fset:lookup bindings 'rest)))))

(test a-pattern-never-matches-a-pattern-segment-by-accident
  (is (not (pine.path:match /proc/editor /proc/desktop))))

;;;; constraints

(defun stub-value (table)
  "A namespace lookup for tests: TABLE is an alist of path text to value."
  (lambda (p) (cdr (assoc (pine.path:text p) table :test #'string=))))

(test a-constraint-tests-the-value-at-a-child
  (let ((value (stub-value '(("/proc/editor/state" . :running)
                             ("/proc/backup/state" . :failed)))))
    (is (pine.path:match /proc/*{:state :failed} /proc/backup :value value))
    (is (not (pine.path:match /proc/*{:state :failed} /proc/editor :value value)))))

(test a-constraint-set-is-membership
  (let ((value (stub-value '(("/proc/a/state" . :stopped)
                             ("/proc/b/state" . :running)))))
    (is (pine.path:match /proc/*{:state #{:failed :stopped}} /proc/a :value value))
    (is (not (pine.path:match /proc/*{:state #{:failed :stopped}} /proc/b
                              :value value)))))

(test a-constraint-form-is-lisp-over-percent
  (let ((value (stub-value '(("/net/wifi/near/signal" . 80)
                             ("/net/wifi/far/signal" . 20)))))
    (is (pine.path:match /net/wifi/*{:signal (> % 60)} /net/wifi/near
                         :value value))
    (is (not (pine.path:match /net/wifi/*{:signal (> % 60)} /net/wifi/far
                              :value value)))))

(test a-constraint-form-can-be-anything-lisp-can-say
  (let ((value (stub-value '(("/buf/a/file" . "x.lisp")
                             ("/buf/b/file" . "x.py")
                             ("/buf/c/file" . nil)))))
    (flet ((lispy (n) (pine.path:match
                       /buf/*{:file (and % (string= "lisp" (pathname-type %)))}
                       n :value value)))
      (is (lispy /buf/a))
      (is (not (lispy /buf/b)))
      (is (not (lispy /buf/c))))))

(test a-constraint-binds-too
  (let ((value (stub-value '(("/buf/notes/mode" . :lisp)))))
    (multiple-value-bind (ok bindings)
        (pine.path:match /buf/*{:mode ?m} /buf/notes :value value)
      (is-true ok)
      (is (eq :lisp (fset:lookup bindings 'm))))))

(test several-constraints-must-all-hold
  (let ((value (stub-value '(("/buf/a/modified" . t) ("/buf/a/file" . "x")
                             ("/buf/b/modified" . t) ("/buf/b/file" . nil)))))
    (is (pine.path:match /buf/*{:modified t :file (not (null %))} /buf/a
                         :value value))
    (is (not (pine.path:match /buf/*{:modified t :file (not (null %))} /buf/b
                              :value value)))))

(test a-literal-segment-can-carry-a-constraint
  (let ((value (stub-value '(("/buf/scratch/modified" . t)))))
    (is (pine.path:match /buf/scratch{:modified t} /buf/scratch :value value))
    (is (not (pine.path:match /buf/scratch{:modified nil} /buf/scratch
                              :value value)))))

;;;; paths as data

(test a-path-is-an-fset-key
  (let ((m (fset:with (fset:with (fset:empty-map) /a/b 1) /a/c 2)))
    (is (= 1 (fset:lookup m /a/b)))
    (is (= 2 (fset:lookup m /a/c)))
    (is (= 2 (fset:size m)))
    (is (null (fset:lookup m /a/d)))))

(test equal-paths-are-equal-however-they-were-built
  (let ((n 3))
    (is (fset:equal? /win/3/buf /win/${n}/buf))
    (is (fset:equal? /a/b (pine.path:parse "/a/b")))
    (is (not (fset:equal? /a/b /a/c)))))
