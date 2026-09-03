(in-package #:pine/ui)

(defclass themes-node (fs:dir) ()
  (:documentation "Every theme there is, and which is on.

A class rather than a PLACE, and the one node in the tree where the two questions
come apart: what is under it is worked out, which is what a place is for, but
/theme/active is a value that persists and a snapshot does not walk into a live
node."))

(defun css-color (role) (color role))

(defun css-glass (role &optional (a (metric :opacity 0.4)))
  (let ((rgb (unhex (color role))))
    (format nil "rgba(~d, ~d, ~d, ~a)" (first rgb) (second rgb) (third rgb) a)))

(defun css-rad () (format nil "~apx" (metric :radius 8)))

(defun css-mono () (format nil "~s, monospace" (metric :font "Maple Mono NF")))

(defgeneric selector (it)
  (:documentation "IT as a selector: a symbol is one class, a list of symbols a
compound, a string the selector as written.")
  (:method ((it string)) it)
  (:method ((it symbol)) (format nil ".~(~a~)" (symbol-name it)))
  (:method ((it cons)) (format nil "~{.~(~a~)~}" (mapcar #'symbol-name it))))

(defun %compound (s &optional (from 0))
  (and (find-if (lambda (c) (member c '(#\Space #\, #\: #\.))) s :start from) t))

(defun %path-segment (sel)
  "The path segment a selector is written at. One class is its own name, so
.editor-view is the path /style/editor-view."
  (let ((s (selector sel)))
    (if (and (plusp (length s)) (char= #\. (char s 0)) (not (%compound s 1)))
        (subseq s 1)
        s)))

(defun %selector (segment)
  (if (%compound segment) segment (format nil ".~a" segment)))

(defun styles ()
  "What is written at /ui/style, as (SELECTOR PROPS), by selector."
  (let ((at (fs:at "/ui/style"))
        (acc nil))
    (when at
      (dolist (each (fs:entries at))
        (let ((props (fs:contents each)))
          (when (consp props)
            (push (list (%selector (fs:name each)) props) acc)))))
    (sort acc #'string< :key #'first)))

(defun style (selector properties)
  "One rule, put on the sheet. What a config says on top of the theme."
  (first (put-rules (list (list selector properties)))))

(defun put-rules (pairs)
  "Put (SELECTOR PROPS) pairs at /ui/style/?selector, replacing what stood there.

What PINE:STYLE calls, so a config saying one rule and a frontend taking a whole
sheet off the wire arrive the same way. This is also the far end of BROADCAST,
where a frontend puts what the daemon sent into its own tree."
  (dolist (each pairs)
    (let ((n (fs:leaf "/ui/style" (%path-segment (first each)))))
      (setf (fs:contents n) (second each))
      (setf (fs:owner n) system:*owner*)))
  (styles))

(defun built-in ()
  (flet ((p (role) (css-color role)) (mono () (css-mono)))
    (list
     (list "*" (list :border-width "0" :border-style "none" :box-shadow "none"
                     :background-color "transparent" :background-image "none"))
     (list ".window" (list :font-family (mono) :font-size "13px" :color (p :fg)))
     (list ".cand" (list :color (p :fg)))
     (list ".cand-annot" (list :color (p :fg-dim)))
     (list ".cand-row" (list :background-color (p :bg-completion)))
     (list ".cand-row.sel" (list :background-color (p :bg-active)))
     (list ".cand-row.sel .cand" (list :color (p :accent-fg)))
     (list ".cand-row.sel .cand-annot" (list :color (p :accent-fg)))
     (list ".dbg-switch" (list :color (p :blue-faint)))
     (list ".dbg-header" (list :color (p :cyan-warmer) :font-weight "bold"))
     (list ".dbg-cond" (list :color (p :red-faint)))
     (list ".dbg-note" (list :color (p :blue-faint)))
     (list ".restart-lbl" (list :color (p :yellow-cooler)))
     (list ".restart.sel" (list :background-color (p :bg-active)))
     (list ".restart.sel .restart-lbl" (list :color (p :accent-fg)))
     (list ".dbg-bt" (list :color (p :blue-faint)))
     (list ".eval-result" (list :color (p :green-cooler) :font-weight "bold"))
     (list ".job-row.sel" (list :background-color (p :bg-active)))
     (list ".help-head" (list :color (p :cyan-warmer) :font-weight "bold"))
     (list ".help-entry" (list :color (p :fg)))
     (list ".field" (list :color (p :fg) :background-color (p :bg-alt)
                          :padding "0 4px"))
     (list ".editor" (list :background-color (p :bg) :color (p :fg)
                           :font-family (mono)))
     (list ".editor-view" (list :background-color (p :bg) :color (p :fg)))
     (list ".modeline" (list :background-color (p :accent) :color (p :accent-fg)))
     (list ".echo" (list :background-color (p :bg) :color (p :fg)))
     (list ".candidates" (list :background-color (p :bg-completion)
                               :color (p :fg))))))

(defmethod fs:works ((s sheet))
  "The stylesheet in cascade order: what pine ships, then what is at /ui/style.
Compiled as it is worked out, and the compiled rules kept beside."
  (let ((cascade (append (built-in) (styles))))
    (setf (compiled s) (%compiled cascade))
    cascade))

(defun %theme-names () (themes))

(defun %theme (n name)
  (fs:child n name
              (lambda ()
                (make-instance 'fs:derived :name name :parent n :live t
                            :reads (lambda ()
                                     (let ((it (theme name)))
                                       (list :palette (palette it)
                                             :metrics (metrics it))))))))

(defun %active (n)
  (fs:child n "active"
              (lambda () (make-instance 'fs:value :name "active" :parent n))))

(defmethod fs:entries ((n themes-node))
  (cons (%active n)
        (loop :for name :in (%theme-names)
              :collect (%theme n (string-downcase (symbol-name name))))))

(defmethod fs:entry ((n themes-node) name)
  (cond ((equal name "active") (%active n))
        ((member name (%theme-names)
                 :key (lambda (each) (string-downcase (symbol-name each)))
                 :test #'equal)
         (%theme n name))))

(defun %attach (root)
  (let* ((ui (fs:ensure root "ui"))
         (themes (fs:attach
                  (make-instance 'themes-node :name "theme"
                                 :describes "every theme there is, and which is on")
                  ui)))
    (fs:attach (make-instance 'face-dir :name "face"
                              :describes "every face in force; write one to change it")
               ui)
    (fs:attach (make-instance 'sheet :name "sheet"
                              :describes "the stylesheet, in cascade order")
               ui)
    (fs:ensure ui "style")
    (fs:ensure ui "surface")
    (let ((active (%active themes)))
      (unless (fs:contents active)
        (setf (fs:contents active) (active))))
    root))


(pine/fs:builder #'%attach)
