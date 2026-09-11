(defpackage #:pine/edit
  (:use #:cl)
  (:local-nicknames (#:actors #:pine/run/actors) (#:command #:pine/run/command) (#:d #:pine/data)
                    (#:fault #:pine/run/fault) (#:image #:pine/run/image)
                    (#:job #:pine/run/job) (#:log #:pine/fs/log)
                    (#:meter #:pine/run/meter) (#:mode #:pine/mode)
                    (#:fs #:pine/fs) (#:listener #:pine/run/listener)
                    (#:module #:pine/run/module) (#:text #:pine/text) (#:ui #:pine/ui))
  (:export
   #:prompt #:listing #:panes #:focused #:focus
   #:shows #:scrolled #:sideways #:width #:height
   #:split #:close-pane #:only #:seed #:show
   #:follow #:name-of #:annotation #:matches #:askingp
   #:ask #:cancel #:so-far #:asked #:candidates
   #:chosen #:matching #:filep #:dispatch #:rows #:completes
   #:indenting #:*cols* #:*lines* #:show-listing #:on-enter
   #:place #:searching #:start #:step-search #:took
   #:banner #:arglist #:type-text))
(in-package #:pine/edit)

(defclass edit (module:module) ())

