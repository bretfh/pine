(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.wm :in :pine)

;;;; Every window management decision is made on the daemon: the frontend holds
;;;; the protocol objects but never picks a rect. That half needs no
;;;; compositor, so it is tested against /wm with a 100x50 output written.

(defun chrome ()
  "The border width the arrangement leaves around every window."
  (pine.ui.face:metric :border 2))

(def-fixture wm-session ()
  "A space of its own with /wm raised and one 100x50 output."
  (pine.ns:with-space ()
    (pine.ns:raise :theme)
    (pine.ns:raise :wm)
    (pine.ns:write /wm/output {:x 0 :y 0 :width 100 :height 50})
    (&body)))

(defun add-windows (&rest ids)
  (dolist (id ids) (pine.wm::add-window id id "probe")))

(defun rect-for (id rects)
  (rest (assoc id rects :test #'equal)))

(defun focused-id () (pine.wm::id-of (pine.wm:focused)))

(test with-nothing-arranged-there-are-no-windows
  (pine.ns:with-space ()
    (pine.ns:raise :wm)
    (is-false (pine.wm:attached-p))
    (is (null (pine.wm:windows)))))

(test the-first-window-becomes-the-whole-arrangement
  (with-fixture wm-session ()
    (add-windows "w1")
    (is (= 1 (length (pine.wm:windows))))
    (is (equal "w1" (focused-id)))))

(test a-second-window-lands-beside-the-focused-one
  (with-fixture wm-session ()
    (pine.ns:write /wm/split :row)
    (add-windows "w1" "w2")
    (is (= 2 (length (pine.wm:windows))))
    (let ((rects (pine.wm::arrange)))
      (is (= 2 (length rects)))
      (destructuring-bind (x1 y1 w1 h1) (rect-for "w1" rects)
        (destructuring-bind (x2 y2 w2 h2) (rect-for "w2" rects)
          (declare (ignore h1 h2))
          (is (= y1 y2) "a :row split puts them side by side")
          (is (< x1 x2))
          (is (<= (+ x1 w1) x2))
          (is (<= (+ x2 w2) 100)))))))

(test a-column-split-stacks-the-next-window
  (with-fixture wm-session ()
    (add-windows "w1")
    (pine.wm:split :column)
    (add-windows "w2")
    (let ((rects (pine.wm::arrange)))
      (destructuring-bind (x1 y1 w1 h1) (rect-for "w1" rects)
        (destructuring-bind (x2 y2 w2 h2) (rect-for "w2" rects)
          (declare (ignore w1 w2 h2))
          (is (= x1 x2))
          (is (< y1 y2))
          (is (<= (+ y1 h1) y2)))))))

(test the-arrangement-stays-inside-the-output
  (with-fixture wm-session ()
    (add-windows "a" "b" "c")
    (dolist (rect (pine.wm::arrange))
      (destructuring-bind (id x y w h) rect
        (is (<= 0 x) "~a starts left of the output" id)
        (is (<= 0 y) "~a starts above the output" id)
        (is (<= (+ x w) 100) "~a runs past the right edge" id)
        (is (<= (+ y h) 50) "~a runs past the bottom edge" id)))))

(test an-output-offset-moves-the-whole-arrangement
  (with-fixture wm-session ()
    (pine.ns:write /wm/output {:x 30 :y 10 :width 100 :height 50})
    (add-windows "w1")
    (let ((border (chrome)))
      (destructuring-bind (x y w h) (rect-for "w1" (pine.wm::arrange))
        (is (= (+ 30 border) x) "the chrome is left outside the window")
        (is (= (+ 10 border) y))
        (is (= (- 100 (* 2 border)) w))
        (is (= (- 50 (* 2 border)) h))))))

(test closing-a-window-drops-it-and-moves-the-focus
  (with-fixture wm-session ()
    (add-windows "w1" "w2")
    (pine.wm::forget-window "w2")
    (is (= 1 (length (pine.wm:windows))))
    (is (equal "w1" (focused-id)))))

(test closing-the-last-window-empties-the-arrangement
  (with-fixture wm-session ()
    (add-windows "only")
    (pine.wm::forget-window "only")
    (is (null (pine.wm:windows)))
    (is (null (pine.wm::arrange)))))

(test focus-steps-round-the-windows-and-wraps
  (with-fixture wm-session ()
    (add-windows "a" "b" "c")
    (let* ((all (pine.wm:windows))
           (n (length all)))
      (is (= 3 n))
      (pine.wm:focus-step 1)
      (is (member (focused-id) '("a" "b" "c") :test #'equal))
      (pine.wm:focus-step -1)
      (is (equal "c" (pine.wm::id-of (pine.win:focused (pine.path:parse "/wm"))))
          "stepping back from the first wraps to the last"))))

(test the-arrangement-survives-the-frontend
  "The windows belong to the compositor and /wm says how they were arranged, so
nothing is lost when a frontend detaches."
  (with-fixture wm-session ()
    (add-windows "w1" "w2")
    (pine.core.attach:detached (make-instance 'pine.wm::wm-app) nil)
    (is (= 2 (length (pine.wm:windows))))))

(test the-binding-table-carries-chord-and-command-strings
  (let ((table (pine.wm:binding-table)))
    (is (equal "wm-terminal" (cdr (assoc "s-Return" table :test #'string=))))
    (is (equal "wm-close-window" (cdr (assoc "s-q" table :test #'string=))))
    (is (every (lambda (entry) (and (stringp (car entry)) (stringp (cdr entry))))
               table))))

(test every-bound-chord-names-something-that-can-run
  "A chord may be bound to a command path, a write-map or a :run, so what is
checked is that RUN knows what to do with it -- and that a path names a command
that is really there, which is the mistake a typo makes."
  (dolist (entry (pine.key:bindings :wm))
    (let ((binding (cdr entry)))
      (is (pine.cmd:runnablep binding)
          "~a is bound to ~s, which is not something to run" (car entry) binding)
      (when (pine.path:pathp binding)
        (is (not (null (pine.ns:read binding)))
            "~a names ~a, and there is no such command" (car entry) binding)))))

(test the-window-manager-keys-are-exactly-what-is-under-key-wm
  "There is no second list of chords, and nothing falls through: a window
manager has no buffer and no mode."
  (is (null (pine.key:lookup :wm "C-x")))
  (is (not (null (pine.key:lookup :wm "s-Return")))))

(test an-unbound-chord-is-reported-rather-than-run
  (with-fixture wm-session ()
    (let ((report (with-output-to-string (*error-output*)
                    (pine.wm:run-binding "s-F12"))))
      (is (search "no command bound" report)))))
