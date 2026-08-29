(defpackage #:pine/edit/words
  (:use)
  (:import-from #:pine/edit
   #:across #:acts #:asked #:askingp #:banner #:cancel #:close-window #:down
   #:focus #:focused #:follow #:indenting #:into #:only #:scrolled #:seed
   #:sideways #:so-far #:split #:windows)
  (:documentation "What the editor offers the language: windows onto documents,
and what a prompt is saying while it asks."))
(in-package #:pine/edit/words)

(pine:speaks '#:pine/edit/words)
