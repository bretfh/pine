(defpackage #:pine/user
  (:use #:cl)
  (:shadowing-import-from #:pine #:read #:write)
  (:shadowing-import-from #:pine/data #:map #:set)
  (:shadowing-import-from #:pine/mode #:structure)
  (:import-from #:pine
   #:at #:blend #:drop #:exclude #:include #:ls #:reach #:spawn #:standsp
   #:style #:toggle #:use #:watch)
  (:import-from #:pine/data
   #:capped #:cas #:emptied #:keys #:lookup #:pairs #:seq #:size #:swap
   #:vals #:with #:without)
  (:import-from #:pine/fs/log #:note)
  (:import-from #:pine/fs/mount #:mount)
  (:import-from #:pine/fs/node
   #:announces #:attach #:child #:contents #:derive #:describes #:detach
   #:full-name #:livep #:name #:node #:nodep #:nodes #:parent #:answers #:lists
   #:refreshes #:resolve #:savedp #:slots #:stir #:verb)
  (:import-from #:pine/fs/path #:leaf #:path)
  (:import-from #:pine/fs/tree #:ensure #:erase #:listing #:root)
  (:import-from #:pine/mode
   #:bind #:handles #:code #:complete #:indent #:lisp #:mode #:org #:pine
   #:press #:prose #:saving #:says #:scheme #:setting #:text
   #:typing)
  (:import-from #:pine/run/command #:command #:commands #:defcommand #:run)
  (:import-from #:pine/run/fault #:attempt)
  (:import-from #:pine/run/job
   #:actor #:alivep #:ask #:job #:program #:start #:stop #:tell #:thread)
  (:import-from #:pine/run/system #:offers #:system)
  (:import-from #:pine/run/watch #:unwatch)
  (:import-from #:pine/ui
   #:acting #:anchor #:background #:bar #:box #:builds #:button #:calendar
   #:cells #:center #:centerbox #:choice #:color #:column #:css-glass
   #:css-mono #:css-rad #:defsurface #:field #:gap #:grid #:here #:icon
   #:image #:label #:measure #:metric #:overlay #:paint #:panel #:parts
   #:ring #:role #:row #:rows #:rule #:scroll #:shown #:shows #:slider
   #:stack #:surface #:surfaces #:tile #:widget #:window)
  (:documentation "The language somebody writes their own pine in.

Common Lisp and every word pine offers, so a package that uses this one and
nothing else can say everything the editor can say.

The words are here as symbols and not as strings, which is the whole of why this
is a DEFPACKAGE and not a registry: a name misspelled is a build that fails rather
than a word that quietly is not in the language. What a system loaded later brings
is its own vocabulary, put here by RUN/SYSTEM:SPEAKS as it loads."))

(in-package #:pine/user)

(let ((p (find-package '#:pine/user))
      (all nil))
  (do-symbols (s p) (pushnew s all))
  (export all p))
