(defpackage #:pine/text/words
  (:use)
  (:import-from #:pine/text
   #:at-col #:at-line #:current #:delete-back #:delete-region #:document
   #:documents #:forget-spans #:goto #:indent-line #:insert #:leading #:line
   #:line-count #:lines #:make-document #:mark #:mode-of #:motion #:move
   #:move-by #:newline #:overlays #:point #:redo #:region-of #:regions
   #:restructure #:revert #:save #:spans #:undo #:visit)
  (:documentation "What the text system offers the language."))
(in-package #:pine/text/words)

(pine:speaks '#:pine/text/words)
