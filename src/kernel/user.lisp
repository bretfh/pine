(defpackage #:pine/user
  (:use #:cl)
  (:shadowing-import-from #:pine #:read #:write)
  (:shadowing-import-from #:pine/data #:map #:set)
  (:import-from #:pine
   #:at #:blend #:drop #:exclude #:include #:ls #:reach #:standsp
   #:toggle #:use #:watch)
  (:import-from #:pine/data #:seq)
  (:import-from #:pine/fs/log #:note)
  (:import-from #:pine/fs/mount #:mount)
  (:import-from #:pine/fs/node
   #:contents #:derived #:describes #:name #:node #:place #:value)
  (:import-from #:pine/fs/tree #:ensure #:erase #:listing #:root)
  (:import-from #:pine/run/command #:defcommand #:run)
  (:import-from #:pine/run/fault #:attempt)
  (:import-from #:pine/run/job #:start #:stop)
  (:import-from #:pine/run/system #:puts #:system)
  (:import-from #:pine/run/watch #:unwatch)
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
