(in-package :pine.test)

(def-suite* :pine.boot :in :pine)

(defmacro with-pine ((&key store) &body body)
  `(unwind-protect (progn (pine:start :store ,store) ,@body)
     (pine:stop)))

(defmacro with-console ((var &key (in "")) &body body)
  `(let* ((out (make-string-output-stream))
          (,var (pine.repl.session:open-session
                 :name "probe" :mode "shell"
                 :node (pine.world.world:root pine.world.world:*world*)
                 :input (make-string-input-stream ,in)
                 :output out
                 :package (find-package :pine))))
     (unwind-protect (progn ,@body) (pine.repl.session:close ,var))))

(defun ran (session form)
  (pine.repl.session:answered (pine.repl.session:evaluate session form)))

(test a-started-pine-has-a-world-a-supervisor-and-its-commands
  (with-pine ()
    (is (typep pine.world.world:*world* 'pine.world.world:world))
    (is (typep pine:*supervisor* 'pine.proc.supervisor:supervisor))
    (is (member "ls" (pine.fs.tree:listing
                      (pine.world.world:at pine.world.world:*world* "cmd"))
                :test #'equal))
    (is (equal "what is under a node"
               (pine.fs.node:contents
                (pine.world.world:at pine.world.world:*world* "cmd/ls"))))))

(test commands-live-in-the-filesystem-and-a-new-one-appears-there
  (with-pine ()
    (let ((cmd (pine.world.world:at pine.world.world:*world* "cmd")))
      (is (null (member "probe-live" (pine.fs.tree:listing cmd) :test #'equal)))
      (pine.repl.command:defcommand "probe-live" () (:describes "made at the repl")
        :ran)
      (unwind-protect
           (progn
             (is (member "probe-live" (pine.fs.tree:listing cmd) :test #'equal)
                 "the tree reflects the commands rather than copying them")
             (is (equal "made at the repl"
                        (pine.fs.node:contents
                         (pine.world.world:at pine.world.world:*world*
                                              "cmd/probe-live")))))
        (pine.repl.command:forget "probe-live")))))

(test the-shell-walks-the-tree
  (with-pine ()
    (with-console (s)
      (is (equal '("/") (ran s 'pwd)))
      (ran s '(mkdir "probe"))
      (is (member "probe" (first (ran s 'ls)) :test #'equal))
      (is (equal '("/probe") (ran s '(cd probe))))
      (is (equal '("/probe") (ran s 'pwd)))
      (ran s '(put "note" "a value"))
      (is (equal '("a value") (ran s '(cat note))))
      (is (equal '("/") (ran s 'cd))))))

(test the-shell-mounts-a-real-directory
  (with-pine ()
    (let ((where (merge-pathnames "pine-probe-shell/" (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (ensure-directories-exist where)
             (with-open-file (out (merge-pathnames "note.txt" where)
                                  :direction :output :if-exists :supersede)
               (write-string "on the disk" out))
             (with-console (s)
               (ran s (list 'mount (namestring where) "disk"))
               (is (member "note.txt" (first (ran s '(ls "/disk"))) :test #'equal))
               (is (equal '("on the disk") (ran s '(cat "/disk/note.txt"))))))
        (uiop:delete-directory-tree where :validate t :if-does-not-exist :ignore)))))

(test the-shell-runs-and-lists-a-process
  (with-pine ()
    (with-console (s)
      (ran s '(spawn "probe-sleep" "sleep 30"))
      (let ((listed (first (ran s 'ps))))
        (is (find "probe-sleep" listed :key #'first :test #'equal)))
      (is (equal '(t) (ran s '(kill "probe-sleep"))))
      (is (null (find "probe-sleep" (first (ran s 'ps)) :key #'first :test #'equal))))))

(test a-world-with-a-store-comes-back-as-it-was
  (let ((file (namestring (merge-pathnames "pine-probe-boot.db"
                                           (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (with-pine (:store file)
             (with-console (s)
               (ran s '(mkdir "kept"))
               (ran s '(put "kept/note" "across sessions"))))
           (with-pine (:store file)
             (with-console (s)
               (is (equal '("across sessions") (ran s '(cat "/kept/note")))))))
      (ignore-errors (delete-file file)))))

(test help-says-what-every-command-is-for
  (with-pine ()
    (with-console (s)
      (is (equal '("where this session is") (ran s '(help pwd))))
      (is (< 10 (length (first (ran s 'help))))))))

(test the-example-config-is-one-a-config-can-be-copied-from
  "What ships as the example has to load and build against the code as it is,
or it is a document about a system that no longer exists."
  (let ((file (merge-pathnames "examples/init.lisp"
                               (asdf:system-source-directory :pine))))
    (unwind-protect
         (progn
           (pine:start)
           (let ((text (uiop:read-file-string file))
                 (scratch (merge-pathnames "pine-probe-example.lisp"
                                           (uiop:temporary-directory))))
             (with-open-file (out scratch :direction :output :if-exists :supersede)
               (write-string (pine.edit.text:text-of
                              (pine/data:as :seq
                                            (remove-if
                                             (lambda (line)
                                               (search "declare-frontends" line))
                                             (uiop:split-string
                                              text :separator '(#\Newline)))))
                             out))
             (pine:load-config scratch)
             (is (member "bar" (mapcar #'pine.fs.node:name
                                       (pine.app.surface:surfaces))
                         :test #'equal)
                 "the surfaces it declares are there")
             (is (plusp (length (pine.ui.css:styles)))
                 "and the styles it writes are installed")
             (is-true (pine.fs.computed:recompute
                       (pine.app.surface:surface-named "bar"))
                      "and its bar builds")
             (ignore-errors (delete-file scratch))))
      (pine:stop))))
