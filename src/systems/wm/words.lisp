(defpackage #:pine/wm/words
  (:use)
  (:import-from #:pine/wm/compositor #:ids #:outputs)
  (:import-from #:pine/wm/keys #:wm)
  (:documentation "What the window manager offers the language. A layout is
PINE/TILES' and comes with it, because pine laying windows out is a system of its
own that the compositor does not require."))
(in-package #:pine/wm/words)

(pine:speaks '#:pine/wm/words)
