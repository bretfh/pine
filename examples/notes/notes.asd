(asdf:defsystem #:notes
                :description "An app: a kind of node, a mode, a surface and its
commands, written in the language and nothing else"
                :depends-on (#:pine/text)
                :components ((:file "notes")))
