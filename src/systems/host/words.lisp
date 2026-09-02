(defpackage #:pine/host/words
  (:use)
  (:import-from #:pine/host #:device #:defdevice #:defbacking)
  (:import-from #:pine/host/shell #:sh)
  (:documentation "What the machine's own devices offer the language: words to put a
device under /dev and follow it, to declare a new one and say how this machine
answers it, and to ask the machine something."))
(in-package #:pine/host/words)

(pine:speaks '#:pine/host/words)
