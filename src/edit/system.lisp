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
   #:prompt #:standing #:listing #:listed
   #:debugger #:offered #:window #:windows
   #:named #:focused #:focus #:root #:shows
   #:scroll #:sideways #:cols #:lines #:runs
   #:weight #:parts #:split #:close-window #:only
   #:seed #:show #:follow #:name-of #:annotation
   #:as-row #:matches #:common-prefix #:expanded #:split-path
   #:entries #:files #:asking #:askingp #:ask
   #:answer #:cancel #:so-far #:asked #:question
   #:answering #:then #:candidates #:chosen #:step-choice
   #:was #:matching #:complete #:source #:sources
   #:*prompt* #:showing #:category #:given #:must-match
   #:history #:remember #:filep #:dispatch #:bindings
   #:drawn #:frame #:modeline #:echo #:rows
   #:indenting #:*cols* #:*lines* #:*font* #:into
   #:acts #:place #:searching #:start #:step-search
   #:took #:took-all #:banner #:needle #:forward
   #:wrapped #:kill #:yank #:times #:counting
   #:+settings+ #:definition #:arglist #:visit #:went
   #:images #:target #:target-was #:standing #:choose
   #:next #:away #:*name* #:edit #:type-text))
(in-package #:pine/edit)

(defclass edit (system:system) ()
  (:documentation "Windows onto documents, the chords that act on them, and the
surface pine shows. A system like any other: nothing in the substrate names
it."))

(system:offers 'edit)
