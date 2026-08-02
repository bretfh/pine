(defpackage #:pine.cairo.shot
  (:use #:cl)
  (:export #:shot #:frame-shot))
(in-package #:pine.cairo.shot)

;;;; The typed-frame shot: a real editor session on a minimal substrate, fed a
;;;; buffer of TEXT, its rendered frame written to a PNG -- headless eyes for
;;;; the editor frame itself (no window, no daemon).

(defun %shot-substrate ()
  "The daemon's own substrate, without the daemon: the tree raised, a runtime
for the grammars, and the local agent. Raised, because a buffer's text is a
string on the way in and its lines in the tree, and the clause that makes it
lines is /buf's -- without it the renderer would walk a string a character at a
time."
  (let ((srv (pine.core.server:start-server)))
    (setf pine.core.server:*server* srv
          (pine.core.server:ts-runtime srv) (pine.ts.runtime:make-ts-runtime))
    (ignore-errors (pine.ts.runtime:ensure-ts (pine.core.server:ts-runtime srv)))
    (pine.core.actor:start-agent-registry srv)
    (pine.core.actor:start-local-agent srv)
    (pine.ns:raise-all :system (pine.core.server:actor-system srv)
                       :runtime (pine.core.server:ts-runtime srv))
    srv))

(defun frame-shot (&key (path "/tmp/pine-shot.png")
                        (text "(defun frobnicate (x &optional (y 10))
  \"a docstring\"
  (let ((sum 0))
    (dolist (a x) (incf sum (car a)))
    (when (> sum y) (format t \"~a: ~s~%\" x sum))
    (pine.util:combine sum y :key)))"))
  "Render the editor's live tree for TEXT to a PNG at PATH, headless: a real
session, its tree refreshed and cell-rendered through pine.ui.cells:render."
  (unless pine.core.server:*server* (%shot-substrate))
  (let* ((sess (pine.editor.session:make-editor-session
                nil :sink (lambda (&rest args) (declare (ignore args)) nil)))
         (client (pine.editor.session:sess-client sess)))
    (let ((buf (pine.editor.frame:current-buffer)))
      (pine.ns:write (pine.buf:at (pine.buf:name-of buf) :text) text))
    (pine.editor.session:session-feed sess (list :resize :cols 84 :rows 30))
    (sleep 0.8)
    (let* ((pine.editor.frame:*client* client)
           (tree (pine.editor.render:refresh-editor-tree client))
           (f (pine.editor.frame:frame client))
           (rows (and tree
                      (nth-value 0 (pine.ui.cells:render
                                    tree (pine.editor.view-state:frame-cols f)
                                    :height (pine.editor.view-state:frame-rows f))))))
      (cond (rows (pine.cairo.grid:render-grid-to-png rows path)
                  (format t "~&wrote ~a~%" path) path)
            (t (format t "~&shot: no frame captured~%") nil)))))

;;;; Headless eyes for the cairo backend: load the user's init.lisp (the real
;;;; surfaces), seed a few refs with sample data, build each surface tree from
;;;; the registry, and render it to a PNG. Read the PNGs to see what the wayflan
;;;; client will show -- no window, no daemon, no display.

(defun ensure-env (&optional (init (merge-pathnames "pine/init.lisp"
                                                    (uiop:xdg-config-home))))
  "Raise the substrate, then load INIT, which is what declares the surfaces.

The substrate is raised because a config's providers are written on it: /sys
reads /file, /audio runs /sh. Without it every panel would render zeros and the
shot would say the surfaces are broken when it is the tree that is missing. A
config that will not load is reported and the shot goes on with whatever did."
  (unless pine.core.server:*server*
    (setf pine.core.server:*server* (make-instance 'pine.core.server:server)))
  (pine.ns:raise-all)
  (when (probe-file init)
    (handler-case (pine:load-config init)
      (error (e) (format *error-output* "~&shot: ~a: ~a~%" init e)))))

(defun seed-paths ()
  "The paths nothing serves, so a surface reading one gets a value rather than
nothing. Everything a provider answers is left alone: with the substrate up it
reads this machine, which is what a shot is for."
  (flet ((w (path value) (pine.ns:write (pine.path:parse path) value)))
    (w "/hint" "") (w "/wintitle" "")))

(defparameter *shots* '("ctl" "audio" "network" "media" "calendar" "bar"))

(defun sample-editor-rows ()
  (flet ((run (col r g b &optional (br -1) (bg -1) (bb -1) (attr 0))
           (list col r g b br bg bb attr))
         (plain (s) (cons s (list (list 0 205 214 244 -1 -1 -1 0)))))
    (append
     (list
      (cons "(defun greet (name)"
            (list (run 0 205 214 244) (run 1 167 139 250 -1 -1 -1 1) (run 7 137 220 194)
                  (run 12 205 214 244) (run 14 250 179 135) (run 18 205 214 244)))
      (cons "  (format t \"Hello, ~a!\" name))"
            (list (run 0 205 214 244) (run 3 137 180 250) (run 12 137 220 235)
                  (run 25 250 179 135) (run 29 205 214 244)))
      (cons ";; the editor is a surface now" (list (run 0 110 110 130 -1 -1 -1 2)))
      (plain ""))
     (loop repeat 12 collect (plain ""))
     (list
      (cons "> greet          function" (list (run 0 250 210 150 80 50 64)))
      (cons "  greeting       variable" (list (run 0 205 214 244 80 50 64)))
      (cons "  greet-user     command"  (list (run 0 205 214 244 80 50 64)))
      (cons " scratch   Lisp   L1 C7" (list (run 0 205 214 244 42 42 54)))
      (cons " M-x " (list (run 0 166 173 200)))))))

(defun editor-shot (dir)
  "Paint a sample editor tree (window + modeline + echo rows) through the
cairo pass, the same shape the live editor surface ships."
  (let* ((rows (sample-editor-rows))
         (n (length rows))
         (tree (pine.ui.build:column :align :stretch
                 (pine.ui.build:cells (subseq rows 0 (- n 2))
                                     :crow 3 :ccol 8 :expand 1
                                     :font-px 15 :opacity 0.9)
                 (pine.ui.build:cells (list (nth (- n 2) rows)) :font-px 15)
                 (pine.ui.build:cells (list (nth (1- n) rows)) :font-px 15)))
         (w 560) (h 480) (path (format nil "~a/pine-cairo-editor.png" dir)))
    (pine.cairo.paint:with-cairo-layout
      (let ((surface (cairo:create-image-surface :argb32 w h)))
        (cairo:with-context ((cairo:create-context surface))
          (multiple-value-bind (r g b) (pine.ui.face:hex-rgb (pine.ui.face:color :bg-alt))
            (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0)) (cairo:paint))
          (pine.cairo.paint:paint-tree tree w h))
        (cairo:surface-write-to-png surface path)))
    (cons "editor" (list :w w :h h :path path))))

(defun shot (&key (dir "/tmp")
                  (config (merge-pathnames "pine/init.lisp" (uiop:xdg-config-home))))
  "Render every surface CONFIG declares to DIR/pine-cairo-NAME.png, and answer
what was written."
  (ensure-env config)
  (seed-paths)
  (append
   (loop :for name :in *shots*
         :for tree = (pine.desktop:surface-tree name)
         :when tree
           :collect (let ((path (format nil "~a/pine-cairo-~a.png" dir name)))
                      (cons name
                            (pine.cairo.paint:render-tree-to-png
                             tree path
                             :avail-w (if (string= name "bar") 120 440)))))
   (list (ignore-errors (editor-shot dir)))))
