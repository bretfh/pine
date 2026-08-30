(defpackage #:pine/edit/words
  (:use)
  (:import-from #:pine/edit
   #:across #:on-enter #:asked #:askingp #:banner #:cancel #:close-window #:completes
   #:down #:focus #:focused #:follow #:indenting #:show-listing #:only #:scrolled
   #:seed #:sideways #:so-far #:split #:windows)
  (:documentation "What the editor offers the language: windows onto documents,
what a prompt is saying while it asks, and how to answer one.

COMPLETES is the half that was missing. A command could ask for a category of its
own -- :asks '((:category :note-title)) -- and nothing outside pine could offer the
words for it, so six categories were the only six there could ever be."))
(in-package #:pine/edit/words)

(pine:speaks '#:pine/edit/words)
