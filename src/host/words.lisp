(defpackage #:pine/host/words
  (:use)
  (:import-from #:pine/host #:device #:defdevice #:defbacking)
  (:import-from #:pine/host/shell #:sh)
  (:documentation "What the machine's own devices offer the language: words to put a
device under /dev and follow it, to declare a new one and say how this machine
answers it, and to ask the machine something.

Asking is here because declaring a device is asking: a backing is a few lines of
shell and what to do with what they said, and a config that can declare one but not
run one would have to reach into pine to finish the job."))
(in-package #:pine/host/words)

(pine:speaks '#:pine/host/words)
