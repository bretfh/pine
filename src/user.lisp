(defpackage :pine.user
  (:nicknames :pine-user)
  (:use :cl)
  (:shadowing-import-from :pine.ns #:read #:write #:watch)
  (:import-from :pine.ns #:provider #:preview #:diff #:stir)
  (:import-from :pine.path #:parent #:leaf #:under #:path #:here)
  (:import-from :pine.data #:fn #:keys #:vals)
  (:import-from :pine.ui.build
                #:column #:row #:centerbox #:center #:box #:scroll #:grid
                #:stack #:label #:icon #:image #:rule #:gap #:button #:slider
                #:ring #:choice #:rows #:field #:calendar)
  (:import-from :pine.pane #:window #:terminal #:modeline #:echo)
  (:import-from :pine.echo
                #:candidate #:register-source #:register-actions
                #:candidate-actions #:message)
  (:import-from :pine.ui.face #:color #:metric #:face-fg)
  (:import-from :pine.proc #:emit)
  (:import-from :pine.provider.procfs #:procfs)
  (:import-from :pine.provider.pipewire #:pipewire)
  (:import-from :pine.provider.backlight #:backlight)
  (:import-from :pine.provider.logind #:logind)
  (:import-from :pine.provider.networkmanager #:networkmanager)
  (:import-from :pine.provider.mpris #:mpris)
  (:import-from :pine.provider.niri #:niri)
  (:import-from :pine.host #:pine)
  (:import-from :pine #:frontend)
  (:export
   #:read #:write #:watch #:provider #:preview #:diff #:stir #:fn
   #:keys #:vals
   #:parent #:leaf #:under #:path
   #:column #:row #:centerbox #:center #:box #:scroll #:grid #:stack
   #:label #:icon #:image #:rule #:gap
   #:button #:slider #:ring #:choice #:rows #:field #:here
   #:calendar #:window #:terminal #:modeline #:echo
   #:candidate #:register-source #:register-actions #:candidate-actions
   #:message #:emit
   #:procfs #:pipewire #:backlight #:logind #:networkmanager #:mpris #:niri
   #:pine #:frontend
   #:color #:metric #:face-fg))
