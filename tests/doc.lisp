(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.doc :in :pine)

;;;; Output and documentation are both places to read.

(test a-line-written-to-log-is-the-last-line
  (pine.ns:with-space ()
    (pine.ns:raise :log)
    (let ((*error-output* (make-broadcast-stream)))
      (pine.log:note "the first thing")
      (pine.log:note "the second thing"))
    (is (string= "the second thing" (pine.ns:read /log)))
    (is (string= "the first thing" (pine.ns:read /log/1)))))

(test the-log-is-bounded
  (pine.ns:with-space ()
    (pine.ns:raise :log)
    (let ((*error-output* (make-broadcast-stream)))
      (dotimes (i (+ pine.log:*kept* 20))
        (pine.log:note "line ~d" i)))
    (is (= pine.log:*kept* (fset:size (pine.ns:read /log/*)))
        "the ring grew past its bound")))

(test what-pine-said-is-not-in-the-file
  "Output is what this image said, not what the world holds."
  (pine.ns:with-space ()
    (pine.ns:raise :log)
    (is (null (pine.ns:setting /log :keep t)))))

(test anything-written-to-the-log-becomes-a-line
  (pine.ns:with-space ()
    (pine.ns:raise :log)
    (pine.ns:write /log 42)
    (is (string= "42" (pine.ns:read /log)))))

(test a-provider-says-what-its-paths-are-for
  (pine.ns:with-space ()
    (pine.ns:raise :doc)
    (pine.ns:write /audio
                   (pine.ns:provider
                    (/audio/volume {:read (pine.data:fn [] 40)
                                    :doc "0..100"})
                    (/audio/muted {:read (pine.data:fn [] nil)})))
    (is (string= "0..100" (pine.ns:read /doc/audio/volume)))))

(test a-path-nobody-documented-answers-nothing
  (pine.ns:with-space ()
    (pine.ns:raise :doc)
    (pine.ns:write /tab-width 8)
    (is (null (pine.ns:read /doc/tab-width))
        "a held path has no doc, and saying so is the point")))

(test a-clause-with-no-doc-falls-back-to-its-providers
  (pine.ns:with-space ()
    (pine.ns:raise :doc)
    (pine.ns:write /audio
                   (pine.ns:provider
                    {:doc "pipewire, through wpctl and pactl"}
                    (/audio/muted {:read (pine.data:fn [] nil)})))
    (is (string= "pipewire, through wpctl and pactl"
                 (pine.ns:read /doc/audio/muted)))))

(defun %doc-table-paths ()
  "Every path the sealed API doc names in a table, as text.

The doc is the contract, so it is read rather than transcribed: a path added to
a table there and to nothing else fails here."
  (let ((path (merge-pathnames "../doc/api.org"
                               #.(or *compile-file-truename* *load-truename*)))
        (found nil))
    (with-open-file (f path :if-does-not-exist nil)
      (when f
        (loop :for line = (read-line f nil)
              :while line
              :when (and (plusp (length (string-left-trim " " line)))
                         (char= #\| (char (string-left-trim " " line) 0)))
                :do (loop :with start = 0
                          :for open = (position #\= line :start start)
                          :while open
                          :for close = (position #\= line :start (1+ open))
                          :while close
                          :do (let ((text (subseq line (1+ open) close)))
                                (when (and (plusp (length text))
                                           (char= #\/ (char text 0))
                                           (not (find #\Space text)))
                                  (pushnew text found :test #'string=)))
                              (setf start (1+ close))))))
    (nreverse found)))

(defun %as-a-place (text)
  "A path the doc spells for a reader, as a place to ask about: its binders and
its interpolations stand for one segment each."
  (let ((out (make-string-output-stream))
        (i 0)
        (n (length text)))
    (loop :while (< i n)
          :for ch = (char text i)
          :do (cond
                ;; ${form} and $@{form} name one segment
                ((and (char= ch #\$) (find #\{ text :start i))
                 (write-char #\x out)
                 (setf i (1+ (position #\} text :start i))))
                ;; ?name and ?@name bind one
                ((char= ch #\?)
                 (write-char #\x out)
                 (incf i)
                 (when (and (< i n) (char= (char text i) #\@)) (incf i))
                 (loop :while (and (< i n) (alpha-char-p (char text i)))
                       :do (incf i)))
                (t (write-char ch out) (incf i))))
    (get-output-stream-string out)))

(test every-path-the-doc-names-says-what-it-is
  "The doc is the contract and /doc is the tree answering for itself, so every
path a table in the doc names has to be a path the tree knows about. A leaf that
is in the doc and in nothing else is a promise pine does not keep; one that
answers but says nothing is a provider that never said what it was for."
  (with-fixture substrate ()
    ;; the device providers a machine's config mounts, mounted here instead of
    ;; loading one: this asks what the tree can say, not what a config says
    (pine.ns:write (pine.path:parse "/audio")  (pine.provider.pipewire:pipewire))
    (pine.ns:write (pine.path:parse "/screen") (pine.provider.backlight:backlight))
    (pine.ns:write (pine.path:parse "/power")  (pine.provider.logind:logind))
    (pine.ns:write (pine.path:parse "/net")
                   (pine.provider.networkmanager:networkmanager))
    (pine.ns:write (pine.path:parse "/media")  (pine.provider.mpris:mpris))
    (pine.ns:write (pine.path:parse "/wm")     (pine.provider.niri:niri))
    (let* ((named (%doc-table-paths))
           (silent (remove-if (lambda (text)
                                (or (string= text "/.") (string= text "/..")
                                    (find #\* text)
                                    (pine.ns:doc (pine.path:parse
                                                  (%as-a-place text)))))
                              named)))
      (is (plusp (length named)) "the doc's tables were not read at all")
      (is (null silent) "~d of ~d paths the doc names say nothing:~{~%  ~a~}"
          (length silent) (length named) silent))))

(defun %doc-verb-promises ()
  "Every (path . verb-name) a table in the doc promises. A verb is written in
the doc as [:name ...], so only the name is taken: what its arguments mean is
prose, but that the path takes it at all is a fact the tree can be asked."
  (let ((path (merge-pathnames "../doc/api.org"
                               #.(or *compile-file-truename* *load-truename*)))
        (out nil))
    (with-open-file (f path :if-does-not-exist nil)
      (when f
        (loop :for line = (read-line f nil)
              :while line
              :for trimmed = (string-left-trim " " line)
              :when (and (plusp (length trimmed)) (char= #\| (char trimmed 0)))
                :do (let ((named nil) (verbs nil) (start 0))
                      (loop :for open = (position #\= line :start start)
                            :while open
                            :for close = (position #\= line :start (1+ open))
                            :while close
                            :do (let ((text (subseq line (1+ open) close)))
                                  (cond
                                    ((and (plusp (length text))
                                          (char= #\/ (char text 0))
                                          (not (find #\Space text)))
                                     (push text named))
                                    ((and (> (length text) 2)
                                          (char= #\[ (char text 0))
                                          (char= #\: (char text 1)))
                                     (let* ((body (subseq text 1 (1- (length text))))
                                            (space (position #\Space body)))
                                       (push (subseq body 1 space) verbs)))))
                                (setf start (1+ close)))
                      (when (and named verbs)
                        (dolist (verb verbs)
                          (push (cons (first (last named)) verb) out)))))))
    (nreverse out)))

(test every-verb-the-doc-promises-is-one-the-path-takes
  "A verb column in the doc is a promise, and asking what a path takes is not
the same as doing it: /power says it takes [:poweroff] and nothing here fires
one. A verb in the doc that the tree would refuse is the promise pine breaks
the first time someone reads the table and writes it."
  (with-fixture substrate ()
    (pine.ns:write (pine.path:parse "/audio")  (pine.provider.pipewire:pipewire))
    (pine.ns:write (pine.path:parse "/screen") (pine.provider.backlight:backlight))
    (pine.ns:write (pine.path:parse "/power")  (pine.provider.logind:logind))
    (pine.ns:write (pine.path:parse "/net")
                   (pine.provider.networkmanager:networkmanager))
    (pine.ns:write (pine.path:parse "/media")  (pine.provider.mpris:mpris))
    (pine.ns:write (pine.path:parse "/wm")     (pine.provider.niri:niri))
    (let* ((promises (%doc-verb-promises))
           (broken (remove-if
                    (lambda (promise)
                      (let* ((place (pine.path:parse (%as-a-place (car promise))))
                             (name (intern (string-upcase (cdr promise)) :keyword))
                             (takes (pine.ns:verbs place)))
                        (or (fset:contains? takes name)
                            (fset:contains? takes t))))
                    promises)))
      (is (plusp (length promises)) "the doc's verb columns were not read at all")
      (is (null broken) "~d of ~d verbs the doc promises are refused:~{~%  ~a~}"
          (length broken) (length promises)
          (loop :for (place . verb) :in broken
                :collect (format nil "~a takes no [:~a]" place verb))))))

(test every-provider-the-daemon-mounts-says-what-its-paths-are
  "Documentation is a handler, so an undocumented path is a provider that never
said what it was for. The list is read off the mounts rather than written here,
so a provider added tomorrow is held to it too."
  (with-fixture substrate ()
    (let ((silent (remove-if (lambda (at) (pine.ns:read (pine.path:path /doc at)))
                             (pine.ns:mounts))))
      (is (null silent) "these say nothing about themselves: ~{~a~^ ~}"
          (mapcar #'pine.path:text silent)))))

;;;; Style, as paths.

(test a-theme-is-made-by-writing-its-palette
  "There is no deftheme: a theme that was not there is one that is now, and a
colour written into the active one restyles at once."
  (with-fixture substrate ()
    (pine.ns:write (pine.path:parse "/theme/probe-theme/palette")
                   (fset:map (:accent "#010203") (:fg "#040506")))
    (is (string= "#010203"
                 (pine.ns:read (pine.path:parse "/theme/probe-theme/palette/accent"))))
    (is (eq :probe-theme (pine.ns:read (pine.path:parse "/theme/probe-theme")))
        "a theme written into being is a theme there is")
    (pine.ns:write (pine.path:parse "/theme/probe-theme/metrics")
                   (fset:map (:radius 3)))
    (is (= 3 (pine.ns:read (pine.path:parse "/theme/probe-theme/metrics/radius"))))
    (is (string= "#010203"
                 (pine.ns:read (pine.path:parse "/theme/probe-theme/palette/accent")))
        "writing the metrics kept the colours")
    (let ((was (pine.ns:read (pine.path:parse "/theme"))))
      (pine.ns:write (pine.path:parse "/theme") :probe-theme)
      (is (string= "#010203" (pine.ui.face:color :accent))
          "loading it did not make its colours the ones everything resolves in")
      (pine.ns:write (pine.path:parse (format nil "/theme/probe-theme/palette"))
                     (fset:map (:accent "#0a0b0c")))
      (is (string= "#0a0b0c" (pine.ui.face:color :accent))
          "a colour written into the active theme did not take")
      (pine.ns:write (pine.path:parse "/theme") was))))

(test the-theme-is-a-path-and-writing-one-loads-it
  (with-fixture substrate ()
    (is (eq :ef-dream (pine.ns:read /theme)))
    (is (stringp (pine.ns:read /theme/ef-dream/palette/accent))
        "a theme's colours are readable without loading it")))

(test a-face-reads-as-a-map-and-takes-one
  (with-fixture substrate ()
    (let ((was (pine.ns:read /face/keyword)))
      (is (fset:map? was))
      (pine.ns:write /face/keyword {:fg "#ff0000"})
      (is (string= "#ff0000" (fset:lookup (pine.ns:read /face/keyword) :fg)))
      (pine.ns:write /face/keyword was))))

(test a-style-rule-is-a-write
  (with-fixture substrate ()
    (pine.ns:write /style/probe-class {:opacity "0.5"})
    (is (fset:equal? {:opacity "0.5"} (pine.ns:read /style/probe-class)))))

;;;; Invariant 7, clause three: nothing struck out in "what this replaces" is
;;;; still callable.

(defun %struck-names ()
  "Every name the doc's \"what this replaces\" table strikes out.

Read off the doc rather than transcribed, so a name added to that table is a
name this holds the tree to without anyone remembering to come here."
  (let ((path (merge-pathnames "../doc/api.org"
                               #.(or *compile-file-truename* *load-truename*)))
        (in nil)
        (out nil))
    (with-open-file (f path :if-does-not-exist nil)
      (when f
        (loop :for line = (read-line f nil)
              :while line
              :do (cond
                    ((search "* What this replaces" line) (setf in t))
                    ((and in (> (length line) 1) (char= #\* (char line 0)))
                     (setf in nil))
                    ((and in (plusp (length line))
                          (char= #\| (char (string-left-trim " " line) 0)))
                     ;; the left column only: the right one names what replaced
                     ;; it, and those are the words pine speaks now
                     (let ((bar (position #\| line :start 1)))
                       (when bar
                         (loop :with start = 0
                               :for open = (position #\= line :start start
                                                             :end bar)
                               :while open
                               :for close = (position #\= line :start (1+ open)
                                                              :end bar)
                               :while close
                               :do (let ((text (subseq line (1+ open) close)))
                                     (when (and (plusp (length text))
                                                (not (find #\Space text)))
                                       (pushnew text out :test #'string-equal)))
                                   (setf start (1+ close)))))))))
      (nreverse out))))

(defparameter +struck-but-different+
  '("store" "snapshot" "show" "toggle" "ask" "buffer" "kill" "sh" "execute"
    "var" "ref")
  "Struck names that pine still uses for something else.

The doc strikes a vocabulary, not a spelling: =store= was a world to save and is
now the file :KEEP writes through, =ask= was one of six ways to reach a buffer
and is now how one image asks another, =kill= was killing an agent and is now
killing a buffer. Each is a word pine kept for a different job, and each is
named here rather than left to be argued about twice.")

(test nothing-the-doc-struck-out-is-still-exported
  "The doc's \"what this replaces\" table is a list of what is gone. A name on
it that a pine package still exports is a vocabulary the doc says pine does not
speak and the tree says it does."
  (let ((alive nil))
    (dolist (name (%struck-names))
      (unless (member name +struck-but-different+ :test #'string-equal)
        (dolist (package (pine-packages))
          (multiple-value-bind (symbol status)
              (find-symbol (string-upcase name) package)
            (when (and symbol (eq status :external))
              (pushnew (format nil "~a:~a" (package-name package) name)
                       alive :test #'string=))))))
    (is (null alive)
        "~d name~:p the doc struck out are still exported:~{~%  ~a~}"
        (length alive) (nreverse alive))))

;;;; The doc's own measure: "if a real config against this design is not
;;;; providers, trees and keys, the design is wrong." It ships that config, and
;;;; this is it being run.

(defun %doc-blocks ()
  "Every lisp source block in the doc, in the order it writes them."
  (let ((path (merge-pathnames "../doc/api.org"
                               #.(or *compile-file-truename* *load-truename*)))
        (blocks nil)
        (current nil))
    (with-open-file (f path :if-does-not-exist nil)
      (when f
        (loop :for line = (read-line f nil)
              :while line
              :do (cond
                    ((search "#+begin_src lisp" line) (setf current nil))
                    ((and (search "#+end_src" line) current)
                     (push (format nil "~{~a~^~%~}" (nreverse current)) blocks)
                     (setf current nil))
                    (t (push line current))))))
    (nreverse blocks)))

(defun %doc-block (marker)
  "The block that names MARKER, so a test runs what the doc shows rather than a
transcription of it that can drift."
  (find marker (%doc-blocks) :test #'search))

(defun %doc-config ()
  "The last source block in the doc: the whole init it ships."
  (car (last (%doc-blocks))))

(defparameter +config-helpers+
  "(defun mmss (s) (declare (ignorable s)) \"0:00\")
   (defun signal-class (s) (declare (ignorable s)) \"nm-sig\")
   (defun ring-face (c) (declare (ignorable c)) :yellow)
   (defun uptime-string (s) (declare (ignorable s)) \"up\")"
  "The four formatters the doc's closing paragraph says a config still writes.
They are the config's, not pine's, so they are supplied here rather than
shipped.")

(test the-config-the-doc-ships-runs-as-written
  "api.org states the measure and then ships the config. This reads that
config out of the doc and runs it: no readtable line, no nil guards, nothing
edited to suit the tree. What it writes has to lay out and paint."
  (pine.ns:with-space ()
    (pine.ns:raise-all)
    (let ((*package* (find-package :pine.user))
          (*readtable* (named-readtables:find-readtable 'pine.path:syntax))
          (text (%doc-config)))
      (is (and text (search "surface/bar" text))
          "the doc's config block was not read at all")
      (finishes
        (with-input-from-string (in (concatenate 'string +config-helpers+ text))
          (loop :for form = (read in nil :eof)
                :until (eq form :eof)
                :do (eval form))))
      (is (eq :bar (pine.ns:read (pine.path:parse "/surface/bar/as"))))
      (is (= 2 (pine.ns:read (pine.path:parse "/tab-width"))))
      (dolist (name '("bar" "echo" "audio" "network" "media" "ctl" "calendar"))
        (let ((tree (pine.desktop:surface-tree name)))
          (is-true tree "the doc's config wrote no ~a surface" name)
          (when tree
            (is-true (pine.ui.cells:render tree 80)
                     "~a laid out to no cells" name)))))))

;;;; The one worked mode the doc ships. Its rows are /proc entries, so what the
;;;; keys act on is a process and not a line of text: this is the whole of what
;;;; a mode author gets beyond editing characters, run as the doc writes it.

(test the-mode-the-doc-ships-acts-on-what-its-rows-stand-for
  "api.org's process list, as written. A row stands for a /proc entry, the
selection is the path of the row under point, and the key the doc binds writes a
verb to that path. Nothing anywhere remembers which line was which."
  (with-fixture substrate ()
    (let ((*package* (find-package :pine.user))
          (*readtable* (named-readtables:find-readtable 'pine.path:syntax))
          (text (%doc-block "proc-list")))
      (is (and text (search "selection" text))
          "the doc's process list was not read out of the document")
      (unwind-protect
           (progn
             (finishes
               (with-input-from-string (in text)
                 (loop :for form = (read in nil :eof)
                       :until (eq form :eof)
                       :do (eval form))))
             (pine.ns:write (pine.path:parse "/proc/probe-one")
                            (fset:map (:run "sleep 30")))
             (pine.ns:write (pine.path:parse "/proc/probe-two")
                            (fset:map (:run "sleep 30")))
             (pine.key::call-command "list-processes")
             (sleep 0.2)
             (let ((rows (btext "*proc*")))
               (is-true (and rows (search "probe-one" rows))
                        "the process list showed no process"))
             ;; the selection is a path, so it is the process and not the line
             (let ((chosen (pine.ns:read
                            (pine.path:parse "/buf/*proc*/selection"))))
               (is-true (pine.path:pathp chosen)
                        "the selected row stands for nothing addressable")
               (when (pine.path:pathp chosen)
                 (is (pine.path:prefixp (pine.path:parse "/proc") chosen)
                     "the row stands for ~a, which is not a process" chosen)
                 ;; and a verb written to what the row stands for reaches the
                 ;; process, which is what every key such a mode binds does
                 (let ((before (pine.ns:read (pine.path:path chosen "pid"))))
                   (pine.ns:put chosen (fset:seq :restart))
                   (sleep 0.3)
                   (let ((after (pine.ns:read (pine.path:path chosen "pid"))))
                     ;; not /=, which this readtable reads as a path
                     (is (and before after (not (eql before after)))
                         "the row's path did not reach ~a: pid was ~a and is ~a"
                         chosen before after))))))
        (pine.ns:write (pine.path:parse "/proc/probe-one") nil)
        (pine.ns:write (pine.path:parse "/proc/probe-two") nil)
        ;; the buffer and the mode go too: this runs against the shared
        ;; substrate, and a listing left behind is a listing every later test
        ;; carries
        (pine.buf:kill "*proc*")
        (pine.ns:write (pine.path:parse "/mode/proc-list") nil)
        (pine.ns:write (pine.path:parse "/cmd/list-processes") nil)))))
