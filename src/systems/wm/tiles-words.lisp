(defpackage #:pine/wm/tiles-words
  (:use)
  (:import-from #:pine/wm/tiles
   #:arrange #:full #:layout #:stacked #:tall #:wide
   #:area #:placed #:x-of #:y-of #:wide-of #:tall-of)
  (:documentation "What pine laying the windows out offers the language: the
LAYOUT class, its ARRANGE method, the four that ship, and the two classes an
ARRANGE reads and answers."))
(in-package #:pine/wm/tiles-words)

(pine:speaks '#:pine/wm/tiles-words)
