(defpackage #:pine/wm/tiles-words
  (:use)
  (:import-from #:pine/wm/tiles
   #:arrange #:full #:layout #:stacked #:tall #:wide)
  (:documentation "What pine laying the windows out offers the language: the
LAYOUT class, its ARRANGE method, and the four that ship."))
(in-package #:pine/wm/tiles-words)

(pine:speaks '#:pine/wm/tiles-words)
