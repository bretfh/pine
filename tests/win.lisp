(in-package :pine.test)

(def-suite* :pine.win :in :pine)
(named-readtables:in-readtable pine.path:syntax)

;;;; A window is a view onto a buffer, and the arrangement is the path: a split
;;;; makes a window a stack of two, so nesting is nesting and there is nothing
;;;; to serialize.

(defmacro with-win (&body body)
  `(pine.ns:with-space ()
     (pine.ns:raise :win)
     (pine.win:seed /buf/scratch)
     ,@body))

(test seeding-makes-one-window-on-a-buffer
  (with-win
    (is (equal (list "/win/0") (mapcar #'pine.path:text (pine.win:windows))))
    (is (fset:equal? /buf/scratch (pine.ns:read /win/0/buf)))
    (is (fset:equal? /win/0 (pine.win:focused)))))

(test a-split-makes-the-window-a-stack-of-two
  (with-win
    (pine.ns:write /win/focused [:split :below])
    (is (eq :column (pine.ns:read /win/0/runs)))
    (is (equal (list "/win/0/0" "/win/0/1")
               (mapcar #'pine.path:text (pine.win:windows))))
    (is (fset:equal? /buf/scratch (pine.ns:read /win/0/1/buf)))
    (is (fset:equal? /win/0/1 (pine.win:focused)))))

(test a-split-inside-a-split-is-a-directory-inside-a-directory
  (with-win
    (pine.ns:write /win/focused [:split :below])
    (pine.ns:write /win/focused [:split :beside])
    (is (eq :column (pine.ns:read /win/0/runs)))
    (is (eq :row (pine.ns:read /win/0/1/runs)))
    (is (equal (list "/win/0/0" "/win/0/1/0" "/win/0/1/1")
               (mapcar #'pine.path:text (pine.win:windows))))))

(test closing-a-window-collapses-the-stack-it-leaves
  (with-win
    (pine.ns:write /win/focused [:split :below])
    (pine.ns:write /win/focused [:close])
    (is (equal (list "/win/0") (mapcar #'pine.path:text (pine.win:windows))))
    (is (null (pine.ns:read /win/0/runs)))
    (is (fset:equal? /buf/scratch (pine.ns:read /win/0/buf)))))

(test only-leaves-the-focused-window-alone
  (with-win
    (pine.ns:write /buf/notes/text "hello")
    (pine.ns:write /win/focused [:split :below])
    (pine.ns:write /win/0/1/buf /buf/notes)
    (pine.ns:write /win/focused [:only])
    (is (equal (list "/win/0") (mapcar #'pine.path:text (pine.win:windows))))
    (is (fset:equal? /buf/notes (pine.ns:read /win/0/buf)))))

(test balancing-is-a-write-over-the-pattern
  (with-win
    (pine.ns:write /win/focused [:split :below])
    (pine.ns:write /win/0/0/weight 3)
    (pine.ns:write /win/*/*/weight 1)
    (is (= 1 (pine.win:weight-of /win/0/0)))
    (is (= 1 (pine.win:weight-of /win/0/1)))))

(test the-last-window-does-not-close
  (with-win
    (pine.ns:write /win/focused [:close])
    (is (equal (list "/win/0") (mapcar #'pine.path:text (pine.win:windows))))))
