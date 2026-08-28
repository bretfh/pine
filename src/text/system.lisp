(defpackage #:pine/text
  (:use #:cl)
  (:local-nicknames (#:actors #:pine/run/actors) (#:command #:pine/run/command)
                    (#:commit #:pine/fs/commit) (#:d #:pine/data)
                    (#:fault #:pine/run/fault) (#:job #:pine/run/job)
                    (#:meter #:pine/run/meter) (#:mode #:pine/mode)
                    (#:mount #:pine/fs/mount) (#:node #:pine/fs/node)
                    (#:path #:pine/fs/path) (#:pl #:pine/data)
                    (#:system #:pine/run/system) (#:tree #:pine/fs/tree))
  (:export
   #:of #:joined #:line #:line-count #:clamp
   #:inserted #:cut #:region #:move-by #:wordp
   #:leading #:find-in #:foldp #:*word-characters* #:document
   #:make-document #:documents #:named #:kill #:current
   #:root #:scratch #:asidep #:killing #:showing
   #:visiting #:lines #:text #:point #:at-line
   #:at-col #:mark #:mode-of #:source #:file-of
   #:origin #:tick #:past #:edit-of #:changed
   #:modified #:setting #:settings #:visited #:leaving
   #:goto #:move #:insert #:delete-back #:newline
   #:delete-region #:mark-at #:put-mark #:drop-mark #:region-of
   #:indent-line #:indent-of #:undo #:redo #:undoable
   #:redoable #:span #:spans #:forget-spans #:overlay
   #:overlays #:forget-overlays #:covers #:regions #:restructure
   #:fresh-structure #:forms #:head #:bodyp #:package-of
   #:readtable-of #:reading #:visit #:save #:revert
   #:recent #:byte-index #:build-index #:byte-index-lines #:byte-index-pending
   #:index-total #:index-line-count #:line-start #:line-bytes #:byte-line
   #:index-edit #:compact-index #:string-bytes #:line-string #:source-line-col
   #:source-byte #:source-substring #:source-char-at #:ts-runtime #:make-ts-runtime
   #:ts-loaded-p #:ensure-ts #:libs-loaded #:grammars #:ts-entry
   #:entry-parser #:entry-language-ptr #:ensure-language #:grammar-library-candidates #:load-grammar-library
   #:grammar-language-pointer #:load-language-entry #:parse-state #:make-parse-state #:free-parse-state
   #:parse-lines! #:parse-text! #:parse-motion #:ps-language #:ps-syntax
   #:ps-package #:ps-parser #:ps-tree #:ps-byte-index #:ps-lines
   #:ps-scratch #:ps-read-buffer #:ps-band #:ps-band-lines #:ps-offset
   #:call-with-input #:+read-chunk+ #:ps-hl-cache #:ps-hl-lines #:ps-hl-pending
   #:ps-hl-stale #:ps-hl-window #:build-line-index #:line-of-byte #:byte-to-line-col
   #:pos-to-byte #:byte-length #:char-byte-length #:ts-parser-new #:ts-parser-delete
   #:ts-parser-set-language #:ts-parser-parse-string #:ts-tree-delete #:ts-tree-edit #:ts-tree-root-node
   #:ts-tree-get-changed-ranges #:ts-node-type #:ts-node-is-null #:ts-node-parent #:ts-node-start-byte
   #:ts-node-end-byte #:ts-node-named-nth #:ts-node-named-count #:ts-node-by-field-name #:ts-node-named-descendant-for-byte-range
   #:parse-highlights #:walk-highlights #:parse-indent #:body-form-p #:language
   #:make-language #:languagep #:lang-name #:lang-nodes #:lang-heads
   #:lang-otherwise #:lang-indent-width #:lang-raw #:lang-grammar #:lang-constants
   #:lang-infer #:lang-memo #:head-rule #:node-rule #:+roles+
   #:delimiter-face #:lambda-list-keyword-p #:ts-type #:ts-type= #:ts-field
   #:ts-named-nodes #:declare-language #:for #:grammar-of #:languages
   #:for-readtable #:lang-node #:compute-highlights #:hl-dump #:hl-dump-file
   #:infers #:parser #:parser-for #:parsers #:highlights
   #:note #:forget #:forget-all #:state-of #:language-of
   #:document-of #:band #:banded #:parsed #:reparsed
   #:indent #:motion #:currentp #:running #:*runtime*))
(in-package #:pine/text)

(defvar *recent* nil)
(defparameter +recent-kept+ 50)

(defclass text (system:system) ()
  (:documentation "Documents, and what their modes make of them."))

(system:offers 'text)
