(defpackage #:pine.cairo
  (:use #:cl)
  (:export #:shot #:frame-shot))
(in-package #:pine.cairo)

;;;; The typed-frame shot: a real editor session on a minimal substrate, fed a
;;;; buffer of TEXT, its rendered frame written to a PNG -- headless eyes for
;;;; the editor frame itself (no window, no daemon).

(defun %shot-substrate ()
  (let ((srv (pine.core.server:start-server)))
    (setf pine.core.server:*server* srv
          (pine.core.server:ts-runtime srv) (pine.ts:make-ts-runtime))
    (ignore-errors (pine.ts:ensure-ts (pine.core.server:ts-runtime srv)))
    (pine.core.event:make-event-bus srv)
    (pine.core.actor:start-agent-registry srv)
    (pine.core.actor:start-local-agent srv)
    (pine.buffer:start-buffer-registry srv)
    srv))

(defun frame-shot (&key (path "/tmp/pine-shot.png")
                        (text "(defun frobnicate (x &optional (y 10))
  \"a docstring\"
  (let ((sum 0))
    (dolist (a x) (incf sum (car a)))
    (when (> sum y) (format t \"~a: ~s~%\" x sum))
    (pine.util:combine sum y :key)))"))
  "Render the editor's live tree for TEXT to a PNG at PATH, headless: a real
session, its tree refreshed and cell-rendered through pine.layout:render."
  (unless pine.core.server:*server* (%shot-substrate))
  (let* ((sess (pine.editor:make-editor-session
                nil :sink (lambda (&rest args) (declare (ignore args)) nil)))
         (client (pine.editor::sess-client sess)))
    (let ((buf (pine.client:current-buffer client)))
      (pine.ask:tell buf :replace-content :content text))
    (pine.editor:session-feed sess (list :resize :cols 84 :rows 30))
    (sleep 0.8)
    (let* ((pine.client:*client* client)
           (tree (pine.render:refresh-editor-tree client))
           (f (pine.client:frame client))
           (rows (and tree
                      (nth-value 0 (pine.layout:render
                                    tree (pine.buffer:frame-cols f)
                                    :height (pine.buffer:frame-rows f))))))
      (cond (rows (pine.display:render-frame-to-png rows path)
                  (format t "~&wrote ~a~%" path) path)
            (t (format t "~&shot: no frame captured~%") nil)))))

;;;; Headless eyes for the cairo backend: load the user's init.lisp (the real
;;;; surfaces), seed a few refs with sample data, build each surface tree from
;;;; the registry, and render it to a PNG. Read the PNGs to see what the wayflan
;;;; client will show -- no window, no daemon, no display.

(defun ensure-env ()
  (unless pine.core.server:*server*
    (setf pine.core.server:*server* (make-instance 'pine.core.server:server)))
  (let ((init (merge-pathnames "pine/init.lisp" (uiop:xdg-config-home))))
    (when (probe-file init)
      (let ((*package* (find-package :pine.user))) (load init)))))

(defun seed-refs ()
  (flet ((c (name val) (pine.state.ref:set-ref (pine.state.ref:make-ref :name name) val)))
    (c :sys '(:cpu 42 :ram 68 :disk 55 :temp 47))
    (c :vol 65) (c :muted nil) (c :bri 40)
    (c :user "bfh") (c :host "pine") (c :uptime "up 3:21")
    (c :clock (get-universal-time))
    (c :workspaces '((:idx 1 :focused t) (:idx 2) (:idx 3) (:idx 4)))
    (c :net "home-5g") (c :hint "") (c :wintitle "")
    (c :sinks '((:name "spk" :desc "Speakers" :default t)
                (:name "hp"  :desc "Headphones")))
    (c :netlist '((:ssid "home-5g" :in_use t :sig "hi" :secure t)
                  (:ssid "cafe"    :sig "mid" :secure nil)
                  (:ssid "neighbor" :sig "lo" :secure t)))
    (c :netactions '((:label "Scan" :kind :scan) (:label "Disconnect" :kind :down :style "no")))))

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
         (tree (pine.layout:column :align :stretch
                 (pine.layout:window (subseq rows 0 (- n 2))
                                     :crow 3 :ccol 8 :expand 1
                                     :font-px 15 :opacity 0.9)
                 (pine.layout:window (list (nth (- n 2) rows)) :font-px 15)
                 (pine.layout:window (list (nth (1- n) rows)) :font-px 15)))
         (w 560) (h 480) (path (format nil "~a/pine-cairo-editor.png" dir)))
    (pine.layout:with-cairo-layout
      (let ((surface (cairo:create-image-surface :argb32 w h)))
        (cairo:with-context ((cairo:create-context surface))
          (multiple-value-bind (r g b) (pine.buffer:hex-rgb (pine.buffer:color :bg-alt))
            (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0)) (cairo:paint))
          (pine.layout:paint-tree tree w h))
        (cairo:surface-write-to-png surface path)))
    (cons "editor" (list :w w :h h :path path))))

(defun shot (&key (dir "/tmp"))
  "Render every desktop surface to DIR/pine-cairo-NAME.png; return the results."
  (ensure-env)                                     ; loads init.lisp -> registers surfaces
  (seed-refs)                                     ; sample data over the defrefs
  (let ((surfaces (symbol-value (find-symbol "*SURFACES*" :pine.desktop))))
    (append
     (loop for name in *shots*
           for builder = (gethash name surfaces)
           when builder
             collect (let ((path (format nil "~a/pine-cairo-~a.png" dir name)))
                       (cons name (pine.layout:render-tree-to-png (funcall builder nil) path
                                    :avail-w (if (string= name "bar") 120 440)))))
     (list (ignore-errors (editor-shot dir))))))
