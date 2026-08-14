(defpackage #:pine.ui.css
  (:use #:cl #:pine.ui.face)
  (:export #:styles #:stylesheet #:install #:broadcast #:selector
           #:css-color #:css-glass #:css-mono #:css-rad #:*listeners* #:*given*))

(in-package #:pine.ui.css)

(defvar *listeners* nil)

(defvar *given* nil)

(defun %compound-p (s &optional (from 0))
  "Whether S says more than one class: a descendant, a list, a pseudo, or two
classes on one node."
  (and (find-if (lambda (c) (member c '(#\Space #\, #\: #\.))) s :start from) t))

(defun %segment (sel)
  "The path segment SEL is written at.

One class is its own name, so the doc's (write /style/editor-view ..) is the
path it looks like. Anything more than one class has no name of that kind and
is stored as it was written."
  (let ((s (selector sel)))
    (if (and (plusp (length s)) (char= #\. (char s 0)) (not (%compound-p s 1)))
        (subseq s 1)
        s)))

(defun %selector (segment)
  "The selector a segment under /style stands for: the inverse of %SEGMENT."
  (if (%compound-p segment)
      segment
      (format nil ".~a" segment)))

(defun styles ()
  "What is written at /style, as (SELECTOR PROPS) by selector.

The same shape the built-ins are in, since the two are appended into one
stylesheet and whatever compiles it should not have to know which half a style
came from."
  (when (and (null pine.world.world:*world*) *given*)
    (return-from styles *given*))
  (let ((held (and pine.world.world:*world*
                   (pine.world.world:at pine.world.world:*world* "style")))
        (acc nil))
    (when held
      (dolist (each (pine.fs.node:nodes held))
        (let ((props (pine.fs.node:contents each)))
          (when (consp props)
            (push (list (%selector (pine.fs.node:name each)) props) acc)))))
    (sort acc #'string< :key #'first)))

(defgeneric selector (sel)
  (:documentation "SEL as a selector: a symbol is one class (.name), a list of
symbols a compound (.a.b), a string the selector DSL as written."))

(defmethod selector ((sel string)) sel)

(defmethod selector ((sel symbol))
  (format nil ".~(~a~)" (symbol-name sel)))

(defmethod selector ((sel cons))
  (format nil "~{.~(~a~)~}" (mapcar #'symbol-name sel)))

(defun install (styles)
  "Put STYLES (a list of (selector props)) at /style/?selector, replacing
whatever stands there.

This is not the way a config styles anything: a config writes the path. This is
the far end of BROADCAST, where a frontend image puts what the daemon sent it
into its own tree. Selectors may be keywords, symbol lists or selector strings,
and it is reload-safe because a selector names its own path."
  (cond ((null pine.world.world:*world*)
         (setf *given* styles))
        (t (dolist (style styles)
             (setf (pine.fs.node:contents
                    (pine.world.world:ensure pine.world.world:*world*
                                             "style" (%segment (first style))))
                   (first (rest style))))))
  (styles))

(defun broadcast ()
  "Send everything at /style to every attached app, so the pixel painters in
the frontend images restyle too. Called when /style moves, so a config that
writes a path has said all it needs to."
  (let ((all (styles)))
    (dolist (each *listeners*) (ignore-errors (funcall each all)))
    all))

(defun css-color (role) (color role))
(defun css-glass (role &optional (a (metric :opacity 0.4)))
  (multiple-value-bind (r g b) (hex-rgb (color role))
    (format nil "rgba(~d, ~d, ~d, ~a)" r g b a)))
(defun css-rad () (format nil "~apx" (metric :radius 8)))
(defun css-mono () (format nil "~s, monospace" (metric :font "Maple Mono NF")))

(defun stylesheet ()
  "The whole stylesheet in cascade order: what pine ships, then what a config
wrote at /style."
  (flet ((p (role) (css-color role)) (mono () (css-mono)))
    (append
     (list

      (list "*" (list :border-width "0" :border-style "none" :box-shadow "none" :outline-style "none"
            :background-color "transparent" :background-image "none"))
      (list "button" (list :min-width "0" :min-height "0" :padding "0"))
      (list "button, scale" (list :transition "background-color 0.25s, color 0.25s"))
      (list "window, .background, .surface, decoration"
       (list :background-color "transparent" :padding "0" :margin "0"))
      (list "window" (list :font-family (mono) :font-size "13px" :color (p :fg)))
      (list "calendar" (list :color (p :fg)))
      (list "calendar:selected" (list :background-color (p :accent) :color (p :accent-fg)
                                 :border-radius "6px"))

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
      (list ".modeline" (list :background-color (p :accent)
                              :color (p :accent-fg)))
      (list ".echo" (list :background-color (p :bg) :color (p :fg)))
      (list ".candidates" (list :background-color (p :bg-completion)
                                :color (p :fg))))

     (styles))))
