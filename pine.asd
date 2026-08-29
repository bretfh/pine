(asdf:defsystem #:pine/value
                :description "What a value is, and how one is replaced"
                :depends-on (#:fset)
                :serial t
                :pathname "src/"
                :components ((:file "word") (:file "data") (:file "said")))

(asdf:defsystem #:pine/place
                :description "The namespace: what a node is, where it stands, what
it says and what outlives the image"
                :depends-on (#:pine/value #:alexandria #:named-readtables #:sqlite
                             #:bordeaux-threads #:uiop)
                :serial t
                :pathname "src/fs/"
                :components ((:file "commit")
                             (:file "node") (:file "graph") (:file "place")
                             (:file "tree") (:file "path")
                             (:file "reader") (:file "mount")
                             (:file "log") (:file "store")))

(asdf:defsystem #:pine/run
                :description "What runs: jobs, actors, faults, commands, sessions,
and other images"
                :depends-on (#:pine/place #:bordeaux-threads #:sento #:sento-remoting
                             #:usocket #:cffi #:cffi-libffi #:sb-posix
                             #:sb-introspect #:uiop)
                :serial t
                :pathname "src/run/"
                :components ((:file "fault") (:file "libs") (:file "meter")
                             (:file "actors") (:file "job") (:file "watch")
                             (:file "command") (:file "image") (:file "peer")
                             (:file "session") (:file "system")))

(asdf:defsystem #:pine/ui
                :description "Widgets, what they are painted with, and the surfaces
they are declared on"
                :depends-on (#:pine/run #:closer-mop #:alexandria #:uiop)
                :serial t
                :pathname "src/ui/"
                :components ((:file "system")
                             (:file "widget") (:file "face")
                             (:file "style") (:file "sheet")
                             (:file "grid") (:file "layout")
                             (:file "hit") (:file "wire")
                             (:file "build") (:file "surface")
                             (:file "key")))

(asdf:defsystem #:pine
                :description "Pine Is Not Emacs"
                :author "Bret Horne"
                :license "GPL"
                :version "0.0.1"
                :depends-on (#:pine/ui #:com.inuoe.jzon #:sb-bsd-sockets)
                :in-order-to ((asdf:test-op (asdf:test-op #:pine/test)))
                :serial t
                :pathname "src/"
                :components ((:file "mode")
                             (:file "serve/json") (:file "serve/wire")
                             (:file "serve/socket")
                             (:file "boot") (:file "cli")))

(asdf:defsystem #:pine/text
                :description "Documents, the structure their modes give them, and
the parse behind it"
                :depends-on (#:pine)
                :serial t
                :pathname "src/text/"
                :components ((:file "system")
                             (:file "lines") (:file "document")
                             (:file "structure")
                             (:file "lisp") (:file "language")
                             (:module "ts"
                                      :serial t
                                      :components ((:file "index")
                                                   (:file "runtime")
                                                   (:file "parse")
                                                   (:file "edit")
                                                   (:file "highlight")
                                                   (:file "walk")
                                                   (:file "indent")
                                                   (:file "syntax")
                                                   (:file "parser")
                                                   (:module "lang"
                                                            :serial t
                                                            :components
                                                            ((:file "commonlisp")
                                                             (:file "pine")
                                                             (:file "scheme")))))
                             (:file "visit")))

(asdf:defsystem #:pine/host
                :description "The machine's own devices, in the namespace"
                :depends-on (#:pine)
                :serial t
                :pathname "src/host/"
                :components ((:file "shell") (:file "device") (:file "system")))

(asdf:defsystem #:pine/edit
                :description "Windows onto documents, and the chords that act on them"
                :depends-on (#:pine/text)
                :serial t
                :pathname "src/edit/"
                :components ((:file "system")
                             (:file "mode") (:file "window") (:file "matching")
                             (:file "prompt") (:file "keys") (:file "render")
                             (:file "listing") (:file "isearch") (:file "commands")
                             (:file "file") (:file "help") (:file "eval")
                             (:file "names") (:file "debugger") (:file "editor")))

(asdf:defsystem #:pine/term
                :description "Programs with screens of their own, as documents"
                :depends-on (#:pine/edit #:pine/vt)
                :serial t
                :pathname "src/term/"
                :components ((:file "terminal") (:file "system")))

(asdf:defsystem #:pine/wm
                :description "The compositor, in the namespace"
                :depends-on (#:pine/host)
                :serial t
                :pathname "src/wm/"
                :components ((:file "compositor") (:file "keys")
                             (:file "managed") (:file "niri") (:file "system")))

(asdf:defsystem #:pine/tiles
                :description "One window manager: where the windows go"
                :depends-on (#:pine/wm)
                :serial t
                :pathname "src/wm/"
                :components ((:file "tiles")))

(asdf:defsystem #:pine/desk
                :description "A bar along the top, and the panels it opens"
                :depends-on (#:pine)
                :serial t
                :pathname "src/desk/"
                :components ((:file "system")))

(asdf:defsystem #:pine/paint
                :description "Pixels, through cairo: the other medium PAINT
dispatches on"
                :depends-on (#:pine #:cl-cairo2)
                :serial t
                :pathname "src/paint/"
                :components ((:file "canvas") (:file "shot")))

(asdf:defsystem #:pine/wayland
                :description "The display pine paints its surfaces on"
                :depends-on (#:pine/paint #:wayflan-client #:posix-shm #:cl-xkb)
                :serial t
                :pathname "src/wayland/"
                :components ((:module "protocol"
                                      :serial t
                                      :components ((:file "wlr-layer-shell")
                                                   (:file "river-wm")
                                                   (:file "river-xkb")
                                                   (:file "river-layer-shell")))
                             (:file "pump") (:file "display") (:file "shell")
                             (:file "pane") (:file "input") (:file "chords")
                             (:file "wm") (:file "screen") (:file "hands")))

(asdf:defsystem #:pine/vt
                :description "Native terminal emulator"
                :depends-on (#:cffi)
                :serial t
                :pathname "vt/"
                :components ((:file "package")
                             (:file "types")
                             (:file "color")
                             (:file "sgr")
                             (:file "ops")
                             (:file "write")
                             (:file "parser")
                             (:file "render")
                             (:file "input")
                             (:file "pty")))

(asdf:defsystem #:pine/all
                :description "Every system pine ships, built in"
                :depends-on (#:pine/edit #:pine/host #:pine/wm #:pine/tiles
                             #:pine/desk #:pine/term #:pine/paint
                             #:pine/wayland))

(asdf:defsystem #:pine/test
                :depends-on (#:pine/all #:fiveam)
                :serial t
                :pathname "tests/"
                :components ((:file "suite") (:file "data") (:file "fs")
                             (:file "run") (:file "serve")
                             (:file "ui") (:file "text")
                             (:file "host") (:file "edit") (:file "term")
                             (:file "wm") (:file "config") (:file "app") (:file "draw")
                             (:file "style"))
                :perform (asdf:test-op (o c)
                                       (unless (uiop:symbol-call :fiveam :run! :pine)
                                         (error "pine tests failed"))))
