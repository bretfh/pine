(in-package :pine/test)

(def-suite* :pine/wm :in :pine)

(defun %managed ()
  "A pine that is the window manager, told what a compositor has. Everything about
it is a value, so nothing here needs a compositor."
  (editing)
  (setf (node:contents (tree:ensure nil "wm-manage")) t)
  (unless (system:named "wm") (pine:use :wm))
  (let ((c (pine/wm:current)))
    (setf (node:contents (tree:at nil "wm/layout")) "tall")
    (setf (node:contents (tree:at nil "wm/said"))
          (list :windows '((1 "term" "foot") (2 "browser" "chrome")
                           (3 "notes" "pine"))
                :outputs '((0 0 0 1280 720))
                :focused 2))
    c))

(test pine-being-the-compositor-is-a-subclass-not-a-second-protocol
  (let ((c (%managed)))
    (is (typep c 'pine/wm/compositor:compositor))
    (is (typep c 'pine/wm/managed:managed))
    (is (equal '("1" "2" "3")
               (mapcar (lambda (w) (gethash "id" w)) (compositor:windows c))))
    (is (equal "2" (compositor:focused c)))
    (is (equal "browser" (compositor:titled c "2")))))

(test a-window-is-a-place-under-the-compositor
  (%managed)
  (is (equal "browser" (getf (node:contents (tree:at nil "wm/windows/2")) :title)))
  (is (getf (node:contents (tree:at nil "wm/windows/2")) :focused))
  (is (equal '("1" "2" "3") (node:contents (tree:at nil "wm/windows")))))

(test the-layout-is-a-node-and-what-it-says-follows-it
  (%managed)
  (is (equal "tall" (node:contents (tree:at nil "wm/layout"))))
  (is (equal '((1 0 0 640 720) (2 640 0 640 360) (3 640 360 640 360))
             (node:contents (tree:at nil "wm/placement"))))
  (setf (node:contents (tree:at nil "wm/layout")) "wide")
  (is (equal '((1 0 0 1280 360) (2 0 360 640 360) (3 640 360 640 360))
             (node:contents (tree:at nil "wm/placement")))
      "writing the layout works the placement out again")
  (setf (node:contents (tree:at nil "wm/layout")) "full")
  (is (equal '((1 0 0 1280 720))
             (node:contents (tree:at nil "wm/placement")))))

(test what-the-compositor-said-is-what-the-placement-follows
  (%managed)
  (node:contents (tree:at nil "wm/placement"))
  (setf (node:contents (tree:at nil "wm/said"))
        (list :windows '((7 "only" "one")) :outputs '((0 0 0 800 600))
              :focused 7))
  (is (equal '((7 0 0 800 600)) (node:contents (tree:at nil "wm/placement")))))

(test the-area-a-window-gets-is-what-the-bars-left
  "An output says what is left after the furniture took its strip. A window laid
out over the bar is a window laid out wrong."
  (%managed)
  (setf (node:contents (tree:at nil "wm/said"))
        (list :windows '((1 "one" "x") (2 "two" "y"))
              :outputs '((0 64 0 1216 720))
              :focused 1))
  (is (equal '((1 64 0 608 720) (2 672 0 608 720))
             (node:contents (tree:at nil "wm/placement")))))

(test a-layout-is-a-class-so-a-config-can-write-one
  (let ((c (%managed)))
    (is (member "tall" (mapcar (lambda (each)
                                 (string-downcase (symbol-name (class-name each))))
                               (pine/wm/tiles:layouts))
                :test #'equal))
    (let ((l (make-instance 'pine/wm/tiles:tall :share 1/4 :gaps 4)))
      (is (equal '((1 4 4 312 712))
                 (subseq (pine/wm/tiles:arrange l '(1 2) '(0 0 1280 720)) 0 1))
          "the share and the gaps are what the layout was made with"))
    (is (typep (pine/wm/managed:layout-of c) 'pine/wm/tiles:layout))))

(test what-pine-wants-of-the-compositor-is-taken-once
  (%managed)
  (command:run "wm-focus-next")
  (command:run "wm-close-window")
  (let ((wants (node:contents (tree:at nil "wm/wants"))))
    (is (= 2 (length wants)))
    (is (eq :close (first (second wants)))))
  (is (null (node:contents (tree:at nil "wm/wants")))
      "and the next one to look finds nothing"))

(test a-verb-is-a-place
  (%managed)
  (setf (node:contents (tree:at nil "wm/close")) t)
  (is (equal '((:close)) (node:contents (tree:at nil "wm/wants")))))
