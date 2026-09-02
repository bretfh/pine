(defpackage #:pine/mode/words
  (:use)
  (:import-from #:pine/mode
   #:bind #:handles #:code #:complete #:covering #:indent #:lisp #:mode
   #:org #:pine #:press #:prose #:saving #:says #:scheme #:setting
   #:structure #:text #:typing)
  (:documentation "What a mode offers the language: the kinds of text there are,
what each does with a key, and how a chord is bound."))
(in-package #:pine/mode/words)

(pine:speaks '#:pine/mode/words)
