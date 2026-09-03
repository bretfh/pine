(in-package :pine/test)

(def-suite* :pine/config :in :pine)

(defun %example ()
  (merge-pathnames "examples/init.lisp"
                   (asdf:system-source-directory :pine)))

(test the-example-config-is-one-you-can-copy
  "It is read the way a daemon reads yours: in PINE/USER, with pine's own
readtable, and a fault in it is a fault like any other rather than a crash."
  (editing)
  (fault:forget-faults)
  (let ((before (length (fault:faults))))
    (is (pine:load-config (%example))
        "~{~%  ~a~}"
        (mapcar (lambda (f) (princ-to-string (fault:condition-of f)))
                (subseq (fault:faults) 0
                        (max 0 (- (length (fault:faults)) before)))))))

(test what-the-example-declared-is-there
  (editing)
  (pine:load-config (%example))
  (is (not (null (command:named "hello"))) "a command it added")
  (is (equal "hello from the config" (command:run "hello")))
  (is (not (null (mode:binding (make-instance 'mode:text) "C-c h")))
      "a chord it bound")
  (is (not (null (tree:at "/surface" "ticker"))) "a surface it declared")
  (is (not (null (tree:at "/surface" "sound"))))
  (is (not (null (find "notes" (mode:modes)
                       :key (lambda (c)
                              (string-downcase (symbol-name (class-name c))))
                       :test #'equal)))
      "and a mode it defined"))

(test a-role-written-in-a-config-says-where-it-goes
  (editing)
  (pine:load-config (%example))
  (let* ((s (tree:at "/surface" "ticker"))
         (where (ui:anchor (ui:role s) 100 20)))
    (is (equal '(:bottom :right) (ui:edges-of where)))
    (is (equal '(0 12 12 0) (ui:margin-of where)))))

(test a-layout-written-in-a-config-lays-windows-out
  (editing)
  (pine:load-config (%example))
  (let ((l (tiles:layout "sidebar")))
    (is (not (null l)) "it is offered like the ones pine ships")
    (is (equal '((1 0 0 320 720) (2 320 0 960 720))
               (tiles:arrange l '(1 2) '(0 0 1280 720))))))

(test a-surface-a-config-declared-crosses-the-wire
  (editing)
  (pine:load-config (%example))
  (let ((form (node:contents (tree:at "/surface/ticker/wire"))))
    (is (not (null form)))
    (is (typep (pine/ui:from-wire form) 'ui:row))))

(defun %reads (text)
  "TEXT read the way a config is read."
  (let ((*readtable* (named-readtables:find-readtable 'pine/fs/reader:syntax))
        (*package* (find-package '#:pine/user)))
    (read-from-string text)))

(test a-path-is-the-whole-of-the-syntax
  "One reader macro. The map and the seq and the set were three more, spelled
{...} [...] and #{...}, with no use anywhere in the tree -- and they cost a stop
list a path had to know about, a style rule, a style test and a clause in the
guide. A map is (map ...), which reads everywhere."
  (is (equal '(pine/fs/path:path "/dev/audio/volume")
             (%reads "/dev/audio/volume")))
  (is (equal '(pine/data:map :v (pine/fs/path:path "/a"))
             (%reads "(map :v /a)"))
      "a path against a closing paren, where a path always ended")
  (is (equal '(pine/data:map :a 1) (%reads "(map :a 1)"))))

(test division-is-writable-where-the-sugar-is-on
  "A bare / answered #'/ , so (/ 1 2) read as ((function /) 1 2), whose head is
neither a symbol nor a lambda. Division was unwritable in a config and in every
file that declares the readtable, which is why pine's own layouts multiply."
  (is (equal '(/ 1 2) (%reads "(/ 1 2)")))
  (is (eql 1/2 (eval (%reads "(/ 1 2)")))))

(test the-prompt-reads-what-a-config-reads
  "What a config teaches has to work where a person types it. The console had the
language and not the syntax, so /dev/audio/volume was an error at the prompt."
  (let ((s (pine:console)))
    (unwind-protect
         (is (eq (named-readtables:find-readtable 'pine/fs/reader:syntax)
                 (pine/run/session::readtable-of s)))
      (pine/run/session:close s))))
