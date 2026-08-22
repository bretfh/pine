(in-package :pine/test)

(def-suite* :pine/wm :in :pine)

(defparameter +said+
  (list :windows '((:id 1 :title "term" :app "foot")
                   (:id 2 :title "browser" :app "chrome")
                   (:id 3 :title "notes" :app "pine"))
        :outputs '((:name "eDP-1" :position (0 0) :size (1280 720)
                    :area (0 0 1280 720)))
        :focused 2))

(defun %managed ()
  "A pine that is the window manager, told what a compositor has. Everything about
it is a value, so nothing here needs a compositor."
  (editing)
  (setf (node:contents (tree:ensure nil "wm-manage")) t)
  (unless (system:named "wm") (pine:use :wm))
  (let ((c (pine/wm:current)))
    (setf (node:contents (tree:at nil "wm/said")) +said+)
    c))

(defun %tiled ()
  "The same, with one of the window managers pine ships loaded on top."
  (let ((c (%managed)))
    (when (system:named "tiles") (pine:drop :tiles))
    (pine:use :tiles)
    (setf (node:contents (tree:at nil "wm/said")) +said+)
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

(test what-a-window-says-about-itself-is-a-path-each
  (%managed)
  (is (equal "browser" (node:contents (tree:at nil "wm/windows/2/title"))))
  (is (equal "chrome" (node:contents (tree:at nil "wm/windows/2/app"))))
  (is (node:contents (tree:at nil "wm/windows/2/focused")))
  (is (null (node:contents (tree:at nil "wm/windows/1/focused")))))

(test an-output-is-a-place-and-says-what-the-bars-left
  (%managed)
  (is (equal '("eDP-1") (node:contents (tree:at nil "wm/outputs"))))
  (is (equal '(1280 720) (node:contents (tree:at nil "wm/outputs/eDP-1/size"))))
  (is (equal '(0 0) (node:contents (tree:at nil "wm/outputs/eDP-1/position"))))
  (is (equal '(0 0 1280 720) (node:contents (tree:at nil "wm/outputs/eDP-1/area"))))
  (setf (node:contents (tree:at nil "wm/said"))
        (list :windows nil
              :outputs '((:name "eDP-1" :position (0 0) :size (1280 720)
                          :area (64 0 1216 720)))
              :focused nil))
  (is (equal '(64 0 1216 720) (node:contents (tree:at nil "wm/outputs/eDP-1/area")))
      "what is left after the furniture took its strip"))

(test core-places-nothing-until-something-says-where
  "The substrate has outputs, windows and what has the keyboard. Where they go is
a system you load."
  (%managed)
  (when (system:named "tiles") (pine:drop :tiles))
  (is (null (node:contents (tree:at nil "wm/placement"))))
  (is (null (tree:at nil "wm/layout"))
      "and there is no layout in core to speak of"))

(test a-window-manager-is-something-that-writes-the-placement
  (%managed)
  (when (system:named "tiles") (pine:drop :tiles))
  (setf (node:contents (tree:at nil "wm/placement"))
        '((1 0 0 640 720) (2 640 0 640 720)))
  (is (equal '((1 0 0 640 720) (2 640 0 640 720))
             (node:contents (tree:at nil "wm/placement"))))
  (is (equal '(0 0 640 720) (compositor:rect (pine/wm:current) "1"))
      "and where a window is is what was last placed for it"))

(test tiles-is-one-of-them-and-writes-it-from-what-it-was-told
  (%tiled)
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
  (%tiled)
  (setf (node:contents (tree:at nil "wm/said"))
        (list :windows '((:id 7 :title "only" :app "one"))
              :outputs '((:name "eDP-1" :position (0 0) :size (800 600)
                          :area (0 0 800 600)))
              :focused 7))
  (is (equal '((7 0 0 800 600)) (node:contents (tree:at nil "wm/placement")))))

(test dropping-the-window-manager-takes-its-paths-with-it
  (%tiled)
  (is (tree:at nil "wm/layout"))
  (pine:drop :tiles)
  (is (null (tree:at nil "wm/layout")))
  (is (null (command:named "wm-layout"))))

(test a-layout-is-a-class-so-a-config-can-write-one
  (%tiled)
  (is (member "tall" (mapcar (lambda (each)
                               (string-downcase (symbol-name (class-name each))))
                             (pine/wm/tiles:layouts))
              :test #'equal))
  (let ((l (make-instance 'pine/wm/tiles:tall :share 1/4 :gaps 4)))
    (is (equal '((1 4 4 312 712))
               (subseq (pine/wm/tiles:arrange l '(1 2) '(0 0 1280 720)) 0 1))
        "the share and the gaps are what the layout was made with")))

(test the-compositor-taking-the-windows-over-binds-the-places-again
  "A config names what places the windows before /wm can exist, and a compositor
that asks for a manager says so after. The wm goes away and comes back a
different class in between, so what places the windows has to come with it."
  (editing)
  (when (system:named "tiles") (pine:drop :tiles))
  (when (system:named "wm") (pine:drop :wm))
  (setf (node:contents (tree:ensure nil "wm-places")) "tiles")
  (setf (node:contents (tree:ensure nil "wm-manage")) nil)
  (pine:use :wm)
  (is (system:named "tiles") "the config's answer is used when the wm comes up")
  (setf (node:contents (tree:ensure nil "wm-manage")) t)
  (pine:drop :wm)
  (pine:use :wm)
  (is (typep (pine/wm:current) 'pine/wm/managed:managed))
  (is (tree:at nil "wm/layout")
      "and it is bound again to the wm that replaced the first")
  (setf (node:contents (tree:at nil "wm/said")) +said+)
  (is (equal '((1 0 0 640 720) (2 640 0 640 360) (3 640 360 640 360))
             (node:contents (tree:at nil "wm/placement")))
      "so what it was told still reaches the placement"))

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

(test taking-a-window-off-the-screen-is-writing-that-it-is-off
  (%managed)
  (node:contents (tree:at nil "wm/wants"))
  (setf (node:contents (tree:at nil "wm/windows/2/hidden")) t)
  (is (equal '((:hide "2")) (node:contents (tree:at nil "wm/wants"))))
  (setf (node:contents (tree:at nil "wm/windows/2/hidden")) nil)
  (is (equal '((:show "2")) (node:contents (tree:at nil "wm/wants")))))

(test the-keyboard-goes-where-the-focused-place-says
  (%managed)
  (node:contents (tree:at nil "wm/wants"))
  (setf (node:contents (tree:at nil "wm/focused")) "3")
  (is (equal '((:focus "3")) (node:contents (tree:at nil "wm/wants")))))
