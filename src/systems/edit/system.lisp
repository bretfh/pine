(defpackage #:pine/edit
  (:use #:cl)
  (:local-nicknames (#:actors #:pine/run/actors) (#:command #:pine/run/command)
                    (#:commit #:pine/fs/commit) (#:d #:pine/data)
                    (#:fault #:pine/run/fault) (#:image #:pine/run/image)
                    (#:job #:pine/run/job) (#:log #:pine/fs/log)
                    (#:meter #:pine/run/meter) (#:mode #:pine/mode)
                    (#:node #:pine/fs/node) (#:session #:pine/run/session)
                    (#:system #:pine/run/system) (#:text #:pine/text)
                    (#:tree #:pine/fs/tree) (#:ui #:pine/ui))
  (:export
   #:prompt #:listing #:windows #:focused #:focus
   #:shows #:scrolled #:sideways #:across #:down
   #:split #:close-window #:only #:seed #:show
   #:follow #:name-of #:annotation #:matches #:askingp
   #:ask #:cancel #:so-far #:asked #:candidates
   #:chosen #:matching #:filep #:dispatch #:rows #:completes
   #:indenting #:*cols* #:*lines* #:show-listing #:on-enter
   #:place #:searching #:start #:step-search #:took
   #:banner #:arglist #:type-text))
(in-package #:pine/edit)

(defclass edit (system:system) ()
  (:documentation "Windows onto documents, the chords that act on them, and the
surface pine shows. A system like any other: nothing in the substrate names
it."))

