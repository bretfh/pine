(defpackage #:pine.ui.css
  (:use #:cl #:pine.ui.face)
  (:export #:styles #:stylesheet #:install #:broadcast #:selector
           #:css-color #:css-glass #:css-mono #:css-rad))

(in-package #:pine.ui.css)
(named-readtables:in-readtable pine.data:syntax)

;;;; The stylesheet, as data: a list of (selector props) where props is a map of
;;;; CSS property to CSS string. Colours and metrics come from the active theme,
;;;; so restyling is a theme swap. What a config wrote lives at /style/?selector
;;;; and comes last, so it wins the cascade.
;;;;
;;;; There is one way to say what a selector looks like, and it is a write:
;;;; (write /style/editor-view {:color "#fff"}). Nothing here is a verb a config
;;;; calls. What is here is what reads that back out, what compiles it, and what
;;;; carries it to a frontend image that paints in pixels.

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
  (let ((held (pine.ns:held (pine.path:parse "/style")))
        (acc nil))
    (when (fset:map? held)
      (fset:do-map (key props held)
        (when (fset:map? props)
          (push (list (%selector (pine.path:name key)) props) acc))))
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
  (dolist (style styles)
    (pine.ns:write (pine.path:path (pine.path:parse "/style")
                                    (%segment (first style)))
                   (first (rest style))))
  (styles))

(defun broadcast ()
  "Send everything at /style to every attached app, so the pixel painters in
the frontend images restyle too. Called when /style moves, so a config that
writes a path has said all it needs to."
  (let ((all (styles)))
    (dolist (c pine.core.attach:*clients*)
      (pine.err:attempt (lambda () (pine.core.attach:push-to-app c :style :styles all))
                        "style broadcast"))
    all))

(defun css-color (role) (color role))
(defun css-glass (role &optional (a (metric :opacity 0.4)))
  (multiple-value-bind (r g b) (hex-rgb (color role))
    (format nil "rgba(~d, ~d, ~d, ~a)" r g b a)))
(defun css-rad () (format nil "~apx" (metric :radius 8)))
(defun css-mono () (format nil "~s, monospace" (metric :font "Maple Mono NF")))

;;;; What pine styles is what pine draws: the reset every widget starts from,
;;;; the two widgets it ships a look for, and the UI buffers that are its own
;;;; -- the completion popup, the debugger, jobs and the help listings. A bar, a
;;;; panel and their classes belong to whoever declared those surfaces, and that
;;;; is a config: see examples/init.lisp, which writes them with ADD-RULES.

(defun stylesheet ()
  "The whole stylesheet in cascade order: what pine ships, then what a config
wrote at /style."
  (flet ((p (role) (css-color role)) (mono () (css-mono)))
    (append
     (list
      ;; global reset: every widget starts bare -- no borders, shadows, focus
      ;; rings, or backgrounds; each rule below opts back in.
      (list "*" {:border-width "0" :border-style "none" :box-shadow "none" :outline-style "none"
            :background-color "transparent" :background-image "none"})
      (list "button" {:min-width "0" :min-height "0" :padding "0"})
      (list "button, scale" {:transition "background-color 0.25s, color 0.25s"})
      (list "window, .background, .surface, decoration"
       {:background-color "transparent" :padding "0" :margin "0"})
      (list "window" {:font-family (mono) :font-size "13px" :color (p :fg)})
      (list "calendar" {:color (p :fg)})
      (list "calendar:selected" {:background-color (p :accent) :color (p :accent-fg)
                                 :border-radius "6px"})
      ;; UI buffers (the cell render): the completion popup, the debugger,
      ;; jobs, and the help buffers style through the same CSS-as-data as the
      ;; desktop
      (list ".cand" {:color (p :fg)})
      (list ".cand-annot" {:color (p :fg-dim)})
      (list ".cand-row" {:background-color (p :bg-completion)})
      (list ".cand-row.sel" {:background-color (p :bg-active)})
      (list ".cand-row.sel .cand" {:color (p :accent-fg)})
      (list ".cand-row.sel .cand-annot" {:color (p :accent-fg)})
      (list ".dbg-switch" {:color (p :blue-faint)})
      (list ".dbg-header" {:color (p :cyan-warmer) :font-weight "bold"})
      (list ".dbg-cond" {:color (p :red-faint)})
      (list ".dbg-note" {:color (p :blue-faint)})
      (list ".restart-lbl" {:color (p :yellow-cooler)})
      (list ".restart.sel" {:background-color (p :bg-active)})
      (list ".restart.sel .restart-lbl" {:color (p :accent-fg)})
      (list ".dbg-bt" {:color (p :blue-faint)})
      (list ".eval-result" {:color (p :green-cooler) :font-weight "bold"})
      (list ".job-row.sel" {:background-color (p :bg-active)})
      (list ".help-head" {:color (p :cyan-warmer) :font-weight "bold"})
      (list ".help-entry" {:color (p :fg)})
      ;; a field is a place, so it is marked as one
      (list ".field" {:color (p :fg) :background-color (p :bg-alt)
                      :padding "0 4px"}))
     ;; what a config wrote comes last, so it wins the cascade
     (styles))))
