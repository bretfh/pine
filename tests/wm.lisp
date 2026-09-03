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
  (setf (node:contents (tree:ensure "/wm-manages")) :pine)
  (unless (system:named "wm") (pine:use :wm))
  (let ((c (pine/wm:current)))
    (setf (node:contents (tree:at "/wm/said")) +said+)
    c))

(defun %tiled ()
  "The same, with one of the window managers pine ships loaded on top."
  (let ((c (%managed)))
    (when (system:named "tiles") (pine:drop :tiles))
    (pine:use :tiles)
    (setf (node:contents (tree:at "/wm/said")) +said+)
    c))

(test pine-being-the-compositor-is-a-subclass-not-a-second-protocol
  (let ((c (%managed)))
    (is (equal '("1" "2" "3")
               (mapcar (lambda (w) (gethash "id" w)) (compositor:windows c))))
    (is (equal "2" (compositor:focused c)))
    (is (equal "browser" (compositor:titled c "2")))))

(test a-window-is-a-place-under-the-compositor
  (%managed)
  (is (equal "browser" (getf (node:contents (tree:at "/wm/windows/2")) :title)))
  (is (getf (node:contents (tree:at "/wm/windows/2")) :focused))
  (is (equal '("1" "2" "3") (node:contents (tree:at "/wm/windows")))))

(test what-a-window-says-about-itself-is-a-path-each
  (%managed)
  (is (equal "browser" (node:contents (tree:at "/wm/windows/2/title"))))
  (is (equal "chrome" (node:contents (tree:at "/wm/windows/2/app"))))
  (is (node:contents (tree:at "/wm/windows/2/focused")))
  (is (null (node:contents (tree:at "/wm/windows/1/focused")))))

(test an-output-is-a-place-and-says-what-the-bars-left
  (%managed)
  (is (equal '("eDP-1") (node:contents (tree:at "/wm/outputs"))))
  (is (equal '(1280 720) (node:contents (tree:at "/wm/outputs/eDP-1/size"))))
  (is (equal '(0 0) (node:contents (tree:at "/wm/outputs/eDP-1/position"))))
  (is (equal '(0 0 1280 720) (node:contents (tree:at "/wm/outputs/eDP-1/area"))))
  (setf (node:contents (tree:at "/wm/said"))
        (list :windows nil
              :outputs '((:name "eDP-1" :position (0 0) :size (1280 720)
                          :area (64 0 1216 720)))
              :focused nil))
  (is (equal '(64 0 1216 720) (node:contents (tree:at "/wm/outputs/eDP-1/area")))
      "what is left after the furniture took its strip"))

(test core-places-nothing-until-something-says-where
  "The substrate has outputs, windows and what has the keyboard. Where they go is
a system you load."
  (%managed)
  (when (system:named "tiles") (pine:drop :tiles))
  (is (null (node:contents (tree:at "/wm/placement"))))
  (is (null (tree:at "/wm/layout"))
      "and there is no layout in core to speak of"))

(test a-window-manager-is-something-that-writes-the-placement
  (%managed)
  (when (system:named "tiles") (pine:drop :tiles))
  (setf (node:contents (tree:at "/wm/placement"))
        '((1 0 0 640 720) (2 640 0 640 720)))
  (is (equal '((1 0 0 640 720) (2 640 0 640 720))
             (node:contents (tree:at "/wm/placement"))))
  (is (equal '(0 0 640 720) (compositor:rect (pine/wm:current) "1"))
      "and where a window is is what was last placed for it"))

(test tiles-is-one-of-them-and-writes-it-from-what-it-was-told
  (%tiled)
  (is (equal "tall" (node:contents (tree:at "/wm/layout"))))
  (is (equal '((1 0 0 640 720) (2 640 0 640 360) (3 640 360 640 360))
             (node:contents (tree:at "/wm/placement"))))
  (setf (node:contents (tree:at "/wm/layout")) "wide")
  (is (equal '((1 0 0 1280 360) (2 0 360 640 360) (3 640 360 640 360))
             (node:contents (tree:at "/wm/placement")))
      "writing the layout works the placement out again")
  (setf (node:contents (tree:at "/wm/layout")) "full")
  (is (equal '((1 0 0 1280 720))
             (node:contents (tree:at "/wm/placement")))))

(test what-the-compositor-said-is-what-the-placement-follows
  (%tiled)
  (setf (node:contents (tree:at "/wm/said"))
        (list :windows '((:id 7 :title "only" :app "one"))
              :outputs '((:name "eDP-1" :position (0 0) :size (800 600)
                          :area (0 0 800 600)))
              :focused 7))
  (is (until (lambda ()
               (equal '((7 0 0 800 600))
                      (node:contents (tree:at "/wm/placement")))))))

(test dropping-the-window-manager-takes-its-paths-with-it
  (%tiled)
  (is (tree:at "/wm/layout"))
  (pine:drop :tiles)
  (is (null (tree:at "/wm/layout")))
  (is (null (command:named "wm-layout"))))

(test a-layout-is-a-class-so-a-config-can-write-one
  (%tiled)
  (is (member "tall" (mapcar (lambda (each)
                               (string-downcase (symbol-name (class-name each))))
                             (pine/wm/tiles:layouts))
              :test #'equal))
  (let* ((l (make-instance 'pine/wm/tiles:tall :share 1/4 :gaps 4))
         (one (first (pine/wm/tiles:arrange
                      l '(1 2) (pine/wm/tiles:area :wide 1280 :tall 720)))))
    (is (typep one 'pine/wm/tiles:placed) "a layout answers PLACED, not a list")
    (is (equal '(1 4 4 312 712)
               (list (pine/wm/tiles:id-of one) (pine/wm/tiles:x-of one)
                     (pine/wm/tiles:y-of one) (pine/wm/tiles:wide-of one)
                     (pine/wm/tiles:tall-of one)))
        "the share and the gaps are what the layout was made with")))

(defclass %clipped (pine/wm/tiles:layout) ()
  (:documentation "A layout written outside the substrate that clips and stacks."))

(defmethod pine/wm/tiles:arrange ((l %clipped) windows (a pine/wm/tiles:area))
  (declare (ignore a))
  (loop :for id :in windows
        :collect (pine/wm/tiles:placed id :x 0 :y 0 :wide 100 :tall 100
                                          :clip '(0 0 50 50) :stack :bottom)))

(test a-layout-can-clip-and-stack-and-it-reaches-what-shows-a-window
  "What shows a window has always been told how to clip one and where to put it in
the stack: %SHOWN takes both. A layout could not say either, because what ARRANGE
answered was a list of five and the two keywords APPLY-LAYOUT destructures after it
were never written. Nothing caught it, because every shape of that list is a list."
  (%tiled)
  (let* ((out (pine/wm/tiles:arrange (make-instance '%clipped) '(7)
                                     (pine/wm/tiles:area :wide 800 :tall 600)))
         (plain (pine/wm/tiles::%plainly (first out))))
    (is (equal '(7 0 0 100 100 :clip (0 0 50 50) :stack :bottom) plain))
    (destructuring-bind (id x y wide tall &key clip stack) plain
      (declare (ignore id x y wide tall))
      (is (equal '(0 0 50 50) clip) "exactly what APPLY-LAYOUT reads")
      (is (eq :bottom stack)))))

(test the-compositor-taking-the-windows-over-binds-the-places-again
  "A config names what places the windows before /wm can exist, and a compositor
that asks for a manager says so after. The wm goes away and comes back a
different class in between, so what places the windows has to come with it."
  (editing)
  (when (system:named "tiles") (pine:drop :tiles))
  (when (system:named "wm") (pine:drop :wm))
  (setf (node:contents (tree:ensure "/wm-places")) "tiles")
  (setf (node:contents (tree:ensure "/wm-manages")) :compositor)
  (pine:use :wm)
  (is (system:named "tiles") "the config's answer is used when the wm comes up")
  (setf (node:contents (tree:ensure "/wm-manages")) :pine)
  (pine:drop :wm)
  (pine:use :wm)
  (is (typep (pine/wm:current) 'pine/wm/managed:managed))
  (is (tree:at "/wm/layout")
      "and it is bound again to the wm that replaced the first")
  (setf (node:contents (tree:at "/wm/said")) +said+)
  (is (until (lambda ()
               (equal '((1 0 0 640 720) (2 640 0 640 360) (3 640 360 640 360))
                      (node:contents (tree:at "/wm/placement")))))
      "so what it was told still reaches the placement"))

(test what-pine-wants-of-the-compositor-is-taken-once
  (%managed)
  (command:run "wm-focus-next")
  (command:run "wm-close-window")
  (let ((wants (node:contents (tree:at "/wm/wants"))))
    (is (= 2 (length wants)))
    (is (eq :close (first (second wants)))))
  (is (null (node:contents (tree:at "/wm/wants")))
      "and the next one to look finds nothing"))

(test a-verb-is-a-place
  (%managed)
  (setf (node:contents (tree:at "/wm/close")) t)
  (is (equal '((:close)) (node:contents (tree:at "/wm/wants")))))

(test taking-a-window-off-the-screen-is-writing-that-it-is-off
  (%managed)
  (node:contents (tree:at "/wm/wants"))
  (setf (node:contents (tree:at "/wm/windows/2/hidden")) t)
  (is (equal '((:hide "2")) (node:contents (tree:at "/wm/wants"))))
  (setf (node:contents (tree:at "/wm/windows/2/hidden")) nil)
  (is (equal '((:show "2")) (node:contents (tree:at "/wm/wants")))))

(test the-keyboard-goes-where-the-focused-place-says
  (%managed)
  (node:contents (tree:at "/wm/wants"))
  (setf (node:contents (tree:at "/wm/focused")) "3")
  (is (equal '((:focus "3")) (node:contents (tree:at "/wm/wants")))))

(test the-window-manager-has-chords-of-its-own
  "A chord the compositor took was not typed at anything: there is no document in
it, and the mode that answers is the window manager's."
  (%managed)
  (mode:bind 'pine/wm/keys:wm "s-c" "wm-close-window")
  (node:contents (tree:at "/wm/wants"))
  (setf (node:contents (tree:at "/wm/key")) "s-q")
  (is (null (node:contents (tree:at "/wm/wants")))
      "a chord nothing bound does nothing")
  (setf (node:contents (tree:at "/wm/key")) "s-c")
  (is (equal '((:close)) (node:contents (tree:at "/wm/wants")))
      "and one that is bound runs its command"))

(test a-chord-of-the-window-managers-is-not-the-editors
  (%managed)
  (mode:bind 'pine/wm/keys:wm "s-x s-r" "wm-outputs")
  (setf (ui:pending) (ui:chord "C-x"))
  (setf (node:contents (tree:at "/wm/key")) "s-x")
  (is (equal "s-x" (node:contents (tree:at "/wm/key")))
      "the window manager is half way through its own chord")
  (is (equal "C-x" (ui:spelled (ui:pending)))
      "and the editor is still half way through the one somebody was typing")
  (setf (ui:pending) nil)
  (setf (node:contents (tree:at "/wm/key")) "s-r")
  (is (equal "" (node:contents (tree:at "/wm/key")))
      "finishing it clears what was standing"))

(test what-the-compositor-has-to-be-asked-for-is-what-is-bound
  (%managed)
  (mode:bind 'pine/wm/keys:wm "s-Return" "wm-terminal")
  (is (member "s-Return" (pine/wm/keys:chords) :test #'equal)))

(test a-window-is-picked-the-way-a-buffer-is
  "The editor asks for candidates by category and has never heard of a window
manager; what it reads is a path."
  (%managed)
  (edit:ask "Window: " :category :window :must-match t)
  (let ((found (edit:candidates)))
    (is (= 3 (length found)))
    (is (equal "2 browser" (edit:name-of (second found))))
    (is (equal "chrome" (edit:annotation (second found))))
    (is (equal '("2 browser")
               (mapcar #'edit:name-of (edit:matches "brow" found)))
        "and it narrows the way anything else typed at does"))
  (command:run "cancel"))

(test picking-one-gives-it-the-keyboard
  (%managed)
  (node:contents (tree:at "/wm/wants"))
  (command:run "switch-to-window" (list "3 notes"))
  (is (equal '((:focus "3")) (node:contents (tree:at "/wm/wants")))))

(test a-chord-is-spelled-for-the-compositor-the-way-it-spells-one
  "What pine calls s-Return xkb calls Return with mod4. The protocol spells a
bitfield as the list of what is set, not a number."
  (flet ((of (spec) (let ((k (ui:parse spec)))
                      (list (pine/wayland/chords:keysym k)
                            (pine/wayland/chords:mask k)))))
    (is (equal (list (xkb:xkb-keysym-from-name "Return" '()) '(:mod4))
               (of "s-Return")))
    (is (equal (list (xkb:xkb-keysym-from-name "x" '()) '(:ctrl :mod1))
               (of "C-M-x")))
    (is (equal (list (xkb:xkb-keysym-from-name "Tab" '()) '(:shift))
               (of "S-TAB")))
    (is (null (pine/wayland/chords:keysym (ui:parse "NoSuchKeyAtAll")))
        "a name xkb does not know is no chord"))
  (is (equal '("s-x" "s-r") (mapcar (lambda (k) (ui:spelled (list k)))
                                    (pine/wayland/chords:every-key '("s-x s-r"))))
      "and both keys of a chord have to be asked for, not just the first"))

(test a-modes-chords-are-readable-wherever-the-mode-was-written
  "/mode/?name/keys is the keymap in the namespace. A mode is a class anybody can
write, and most of them are not written beside the root one."
  (%managed)
  (mode:bind 'pine/wm/keys:wm "s-c" "wm-close-window")
  (unless (tree:at "/mode") (node:attach (mode:mode-node) (tree:root)))
  (is (typep (mode:mode "wm") 'pine/wm/keys:wm)
      "a mode is found by name whichever package it was written in")
  (is (member "wm" (node:contents (tree:at "/mode")) :test #'equal))
  (let ((n (tree:at "/mode/wm/keys")))
    (is (not (null n)) "the keymap is a place")
    (is (equal "wm-close-window" (cdr (assoc "s-c" (node:contents n)
                                             :test #'equal)))
        "as plain pairs, because a keymap crosses a wire like anything else"))
  (let ((n (tree:at "/mode/prompt/keys")))
    (is (not (null n)))
    (is (assoc "RET" (node:contents n) :test #'equal)
        "and the editor's own modes, which were never readable here either")))
