(defpackage #:pine/ui
  (:use #:cl)
  (:local-nicknames (#:command #:pine/run/command) (#:d #:pine/data)
                    (#:fault #:pine/run/fault) (#:log #:pine/fs/log)
                    (#:meter #:pine/run/meter) (#:fs #:pine/fs)
                    (#:path #:pine/fs/path)
                    (#:module #:pine/run/module))
  (:export
   #:widget #:parts #:label #:rule #:gap
   #:cells #:picture #:calendar #:slider #:ring
   #:column #:row #:stack #:box #:center
   #:centerbox #:scroll #:action #:choice #:on-click #:key
   #:face #:css-class #:hint #:radius #:fill-color
   #:grad #:font #:pad #:chosen #:top
   #:left #:width #:height #:content #:on-change
   #:upright #:held #:thickness #:fixed-width #:fixed-height #:fraction #:rows-of
   #:by-row #:caret #:over #:path #:year
   #:month #:day #:bg #:with-faces #:in-force
   #:unhex #:color #:metric #:resolve #:properties #:property
   #:put-rules #:style #:sheet #:css-glass #:css-mono #:css-rad #:medium
   #:cell-grid #:grid #:make-cell-grid #:put #:put-bg #:ink
   #:measure #:lay #:paint #:text-size #:dress
   #:styled #:with-pass #:under #:clicked #:clicked-at
   #:value-at #:to-wire #:from-wire #:field #:icon
   #:button #:image #:rows #:acting #:here
   #:confirming #:surface #:tree #:defsurface #:surfaces
   #:make-surface #:role #:anchor #:shown
   #:placing #:edges-of #:reserve-of #:margin-of #:inset
   #:shows #:size #:bar #:panel #:overlay
   #:background #:toplevel #:tile #:declared #:make-key
   #:parse #:chord #:spelled #:key= #:selfp
   #:typed #:sym #:ctrl #:meta #:shift
   #:super #:keysym-name #:pending #:last-said #:take-next
   #:reading))
(in-package #:pine/ui)

(defgeneric held (it))
(defgeneric (setf held) (value it))
