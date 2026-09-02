(defpackage #:pine/ui/words
  (:use)
  (:import-from #:pine/ui
   #:acting #:anchor #:background #:bar #:box #:make-surface #:button #:calendar
   #:cells #:center #:centerbox #:choice #:color #:column #:css-glass
   #:css-mono #:css-rad #:defsurface #:field #:gap #:grid #:here #:icon
   #:image #:inset #:label #:measure #:metric #:overlay #:paint
   #:panel #:parts #:placing #:property #:ring #:role #:row #:rows #:rule
   #:scroll #:shown #:shows #:slider #:stack #:style #:surface #:surfaces
   #:tile #:widget #:window)
  (:documentation "What drawing offers the language: the widgets a surface is built
of, where one goes, and the sheet."))
(in-package #:pine/ui/words)

(pine:speaks '#:pine/ui/words)
