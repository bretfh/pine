(defpackage #:pine/text
  (:use #:cl)
  (:local-nicknames (#:actors #:pine/run/actors) (#:command #:pine/run/command)
                    (#:commit #:pine/fs/commit) (#:d #:pine/data)
                    (#:fault #:pine/run/fault) (#:job #:pine/run/job)
                    (#:meter #:pine/run/meter) (#:mode #:pine/mode)
                    (#:mount #:pine/fs/mount) (#:node #:pine/fs/node)
                    (#:path #:pine/fs/path)
                    (#:system #:pine/run/system) (#:tree #:pine/fs/tree))
  (:export
   #:of #:line #:line-count #:inserted #:region
   #:move-by #:leading #:find-in #:document #:make-document
   #:documents #:named #:kill #:killing #:current
   #:scratch #:asidep #:showing #:lines #:text
   #:point #:at-line #:at-col #:mark #:mode-of
   #:source #:file-of #:origin #:modified #:goto
   #:move #:insert #:delete-back #:newline #:delete-region
   #:region-of #:indent-line #:indent-of #:undo #:redo
   #:span #:spans #:forget-spans #:overlay #:overlays
   #:forget-overlays #:regions #:restructure #:package-of #:readtable-of
   #:reading #:visit #:save #:revert #:recent
   #:make-parse-state #:free-parse-state #:parse-lines! #:parse-highlights #:language
   #:declare-language #:for #:grammar-of #:parser-for #:highlights
   #:note #:forget #:forget-all #:band #:reparsed
   #:indent #:motion #:currentp #:running #:*runtime*))
(in-package #:pine/text)

(defvar *recent* nil)
(defparameter +recent-kept+ 50)

(defclass text (system:system) ()
  (:documentation "Documents, and what their modes make of them."))

