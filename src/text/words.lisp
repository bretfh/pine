(defpackage #:pine/text/words
  (:use)
  (:import-from #:pine/text
   #:at-col #:at-line #:current #:delete-back #:delete-region #:document
   #:documents #:forget-spans #:goto #:indent-line #:insert #:leading #:line
   #:line-count #:lines #:make-document #:mark #:mode-of #:motion #:move
   #:move-by #:newline #:overlays #:point #:redo #:region-of #:regions
   #:restructure #:revert #:save #:spans #:undo #:visit)
  (:documentation "What the text system offers the language.

A vocabulary uses nothing and imports what it offers, so what this package holds
is exactly what it offers. PINE/TEXT exports three times as much, because most of
what a system exports is for the rest of pine and not for somebody writing their
own -- INDENT is PINE/MODE's word in the language, and PINE/TEXT has one too."))
(in-package #:pine/text/words)

(pine:speaks '#:pine/text/words)
