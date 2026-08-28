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
  (is (not (null (ui:named "ticker"))) "a surface it declared")
  (is (not (null (ui:named "sound"))))
  (is (not (null (find "notes" (mode:modes)
                       :key (lambda (c)
                              (string-downcase (symbol-name (class-name c))))
                       :test #'equal)))
      "and a mode it defined"))

(test a-role-written-in-a-config-says-where-it-goes
  (editing)
  (pine:load-config (%example))
  (let* ((s (ui:named "ticker"))
         (where (ui:anchor (ui:role s) 100 20)))
    (is (equal '(:bottom :right) (d:lookup where :edges)))
    (is (equal '(0 12 12 0) (d:lookup where :margin)))))

(test a-layout-written-in-a-config-lays-windows-out
  (editing)
  (pine:load-config (%example))
  (let ((l (tiles:named "sidebar")))
    (is (not (null l)) "it is offered like the ones pine ships")
    (is (equal '((1 0 0 320 720) (2 320 0 960 720))
               (tiles:arrange l '(1 2) '(0 0 1280 720))))))

(test what-a-config-can-name-follows-what-is-loaded
  (editing)
  (pine:load-config (%example))
  (dolist (name '("use" "defsurface" "anchor" "arrange" "layout" "press"
                  "structure" "contents" "column" "label" "note"))
    (is (not (null (find-symbol (string-upcase name) :pine/user)))
        "~a is a word a config can say" name)))

(test a-surface-a-config-declared-crosses-the-wire
  (editing)
  (pine:load-config (%example))
  (let ((form (node:contents (tree:at nil "surface/ticker/wire"))))
    (is (not (null form)))
    (is (typep (pine/ui:from-wire form) 'ui:row))))
