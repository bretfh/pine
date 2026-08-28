(defpackage #:pine/ui/sheet
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:face #:pine/ui/face)
                    (#:style #:pine/ui/style))
  (:export #:put-rules #:styles #:built-in #:selector
           #:css-color #:css-glass #:css-mono #:css-rad))
(in-package #:pine/ui/sheet)

(defclass themes-node (node:node) ()
  (:documentation "Every theme there is, and which is on.

A class rather than a PLACE, and the one node in the tree where the two questions
come apart: what is under it is worked out, which is what a place is for, but
/theme/active is a value that persists and a snapshot does not walk into a live
node."))

(defun css-color (role) (face:color role))

(defun css-glass (role &optional (a (face:metric :opacity 0.4)))
  (let ((rgb (face:unhex (face:color role))))
    (format nil "rgba(~d, ~d, ~d, ~a)" (first rgb) (second rgb) (third rgb) a)))

(defun css-rad () (format nil "~apx" (face:metric :radius 8)))

(defun css-mono () (format nil "~s, monospace" (face:metric :font "Maple Mono NF")))

(defgeneric selector (it)
  (:documentation "IT as a selector: a symbol is one class, a list of symbols a
compound, a string the selector as written.")
  (:method ((it string)) it)
  (:method ((it symbol)) (format nil ".~(~a~)" (symbol-name it)))
  (:method ((it cons)) (format nil "~{.~(~a~)~}" (mapcar #'symbol-name it))))

(defun %compound (s &optional (from 0))
  (and (find-if (lambda (c) (member c '(#\Space #\, #\: #\.))) s :start from) t))

(defun %segment (sel)
  "The path segment a selector is written at. One class is its own name, so
.editor-view is the path /style/editor-view."
  (let ((s (selector sel)))
    (if (and (plusp (length s)) (char= #\. (char s 0)) (not (%compound s 1)))
        (subseq s 1)
        s)))

(defun %selector (segment)
  (if (%compound segment) segment (format nil ".~a" segment)))

(defun styles ()
  "What is written at /style, as (SELECTOR PROPS), by selector."
  (let ((at (and (tree:root) (tree:at nil "style")))
        (acc nil))
    (when at
      (dolist (each (node:nodes at))
        (let ((props (node:contents each)))
          (when (consp props)
            (push (list (%selector (node:name each)) props) acc)))))
    (sort acc #'string< :key #'first)))

(defun put-rules (pairs)
  "Put (SELECTOR PROPS) pairs at /style/?selector, replacing what stood there. A
config does not call this: a config writes the path. This is the far end of
BROADCAST, where a frontend puts what the daemon sent into its own tree."
  (dolist (each pairs)
    (setf (node:contents (tree:ensure nil "style" (%segment (first each))))
          (second each)))
  (refresh)
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

(defun refresh ()
  "The stylesheet in cascade order: what pine ships, then what is at /style."
  (style:sheet (append (built-in) (styles)))
  (style:forget-rules)
  (style:sheet))

(defun %theme-names () (face:themes))

(defun %theme (n name)
  (node:child n name
              (lambda ()
                (node:place name :over n
                            :reads (lambda ()
                                     (let ((it (face:theme name)))
                                       (list :palette (face:palette it)
                                             :metrics (face:metrics it))))))))

(defun %active (n)
  (node:child n "active" (lambda () (node:make "active" :over n))))

(defmethod node:nodes ((n themes-node))
  (cons (%active n)
        (loop :for name :in (%theme-names)
              :collect (%theme n (string-downcase (symbol-name name))))))

(defmethod node:resolve ((n themes-node) name)
  (cond ((equal name "active") (%active n))
        ((member name (%theme-names)
                 :key (lambda (each) (string-downcase (symbol-name each)))
                 :test #'equal)
         (%theme n name))))

(defmethod node:contents ((n themes-node)) (%theme-names))

(defun %key (name)
  "The keyword a face is kept under, without making one that is not already there:
a path nobody named should not grow the keyword package."
  (find-symbol (string-upcase (princ-to-string name)) :keyword))

(defun %face-names ()
  (let (acc)
    (maphash (lambda (name f)
               (declare (ignore f))
               (push (string-downcase (symbol-name name)) acc))
             (face:faces (face:theme (face:active))))
    (sort acc #'string<)))

(defun %face (name)
  (let ((key (%key name)))
    (when (and key (face:in-force key))
      (node:place name
                  :reads (lambda ()
                           (let ((f (face:in-force (%key name))))
                             (when f
                               (list :fg (face:fg f) :bg (face:bg f)
                                     :bold (face:bold f) :italic (face:italic f)
                                     :underline (face:underline f)))))))))

(defun %attach (root)
  (let ((themes (node:attach
                 (make-instance 'themes-node :name "theme"
                                :describes "every theme there is, and which is on")
                 root)))
    (node:attach (node:place "faces" :names #'%face-names :each #'%face
                             :describes "every face in force")
                 root)
    (tree:ensure root "face")
    (tree:ensure root "style")
    (let ((active (%active themes)))
      (unless (node:contents active)
        (setf (node:contents active) (face:active))))
    (refresh)
    root))

(pine/word:lends "css-glass" "css-rad" "css-mono")

(pine/fs/tree:builder #'%attach)
