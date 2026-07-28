(in-package :pine.test)

(def-suite* :pine.wm :in :pine)

;;;; Every window management decision is made on the daemon: the frontend
;;;; holds the protocol objects but never picks a rect. That half needs no
;;;; compositor, so it is tested against a session whose frontend has no
;;;; display to push to.

(defun chrome ()
  "The border width the arrangement leaves around every window."
  (pine.ui.face:metric :border 2))

(def-fixture wm-session ()
  "A window manager session with a silent frontend and one 100x50 output."
  (let ((saved pine.wm::*session*))
    (setf pine.wm::*session*
          (pine.wm::%make-session
           :app (pine.core.attach::make-attached-client :id 1 :kind :wm :display nil)
           :client nil))
    (setf (pine.wm::session-output pine.wm::*session*) '(0 0 100 50))
    (unwind-protect (&body)
      (setf pine.wm::*session* saved))))

(defun add-windows (&rest ids)
  (dolist (id ids) (pine.wm::add-window id id "probe")))

(defun rect-for (id rects)
  (rest (assoc id rects :test #'equal)))

(test with-no-session-attached-nothing-is-attached
  (let ((pine.wm::*session* nil))
    (is-false (pine.wm:attached-p))
    (is (null (pine.wm:leaves)))))

(test the-first-window-becomes-the-whole-tree
  (with-fixture wm-session ()
    (add-windows "w1")
    (is (= 1 (length (pine.wm:leaves))))
    (is (equal "w1" (pine.wm::leaf-id (pine.wm:focused-leaf))))))

(test a-second-window-lands-beside-the-focused-one
  (with-fixture wm-session ()
    (add-windows "w1" "w2")
    (is (= 2 (length (pine.wm:leaves))))
    (let ((rects (pine.wm::arrange)))
      (is (= 2 (length rects)))
      (destructuring-bind (x1 y1 w1 h1) (rect-for "w1" rects)
        (destructuring-bind (x2 y2 w2 h2) (rect-for "w2" rects)
          (declare (ignore h1 h2))
          (is (= y1 y2) "the default split is :row, so they sit side by side")
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
    (setf (pine.wm::session-output pine.wm::*session*) '(30 10 100 50))
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
    (is (= 1 (length (pine.wm:leaves))))
    (is (equal "w1" (pine.wm::leaf-id (pine.wm:focused-leaf))))))

(test closing-the-last-window-empties-the-tree
  (with-fixture wm-session ()
    (add-windows "only")
    (pine.wm::forget-window "only")
    (is (null (pine.wm:leaves)))
    (is (null (pine.wm::arrange)))))

(test focus-steps-round-the-leaves-and-wraps
  (with-fixture wm-session ()
    (add-windows "a" "b" "c")
    (pine.wm:focus-step 1)
    (is (equal "a" (pine.wm::leaf-id (pine.wm:focused-leaf))))
    (pine.wm:focus-step -1)
    (is (equal "c" (pine.wm::leaf-id (pine.wm:focused-leaf))))))

(test the-binding-table-carries-chord-and-command-strings
  (let ((table (pine.wm:binding-table)))
    (is (equal "wm-terminal" (cdr (assoc "s-Return" table :test #'string=))))
    (is (equal "wm-close-window" (cdr (assoc "s-q" table :test #'string=))))
    (is (every (lambda (entry) (and (stringp (car entry)) (stringp (cdr entry))))
               table))))

(test every-bound-chord-names-a-command-that-exists
  (dolist (entry (pine.wm:binding-table))
    (is (not (null (pine.ns:read (pine.cmd:at (cdr entry)))))
        "~a is bound to ~a, which is not a command" (car entry) (cdr entry))))

(test the-window-manager-keys-are-exactly-what-is-under-key-wm
  "There is no second list of chords, and nothing falls through: a window
manager has no buffer and no mode."
  (is (eq :wm (pine.wm:wm-keymap)))
  (is (null (pine.editor.keymap:lookup :wm "C-x")))
  (is (not (null (pine.editor.keymap:lookup :wm "s-Return")))))

(test an-unbound-chord-is-reported-rather-than-run
  (with-fixture wm-session ()
    (let ((report (with-output-to-string (*error-output*)
                    (pine.wm:run-binding "s-F12"))))
      (is (search "no command bound" report)))))
