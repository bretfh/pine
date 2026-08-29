(defpackage #:pine/host/words
  (:use)
  (:import-from #:pine/host #:attend)
  (:import-from #:pine/host/device #:device)
  (:documentation "What the machine's own devices offer the language: one word to
put a device under /dev and follow it, and one to write a new kind."))
(in-package #:pine/host/words)

(pine:speaks '#:pine/host/words)
