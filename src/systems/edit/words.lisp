(defpackage #:pine/edit/words
  (:use)
  (:import-from #:pine/edit
   #:across #:on-enter #:asked #:askingp #:banner #:cancel #:close-window #:completes
   #:down #:focus #:focused #:follow #:indenting #:show-listing #:only #:scrolled
   #:seed #:sideways #:so-far #:split #:windows)
  (:documentation "What the editor offers the language: windows onto documents,
what a prompt is saying while it asks, and how to answer one."))
(in-package #:pine/edit/words)

(pine:speaks '#:pine/edit/words)
