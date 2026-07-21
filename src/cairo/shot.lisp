(defpackage #:pine.cairo
  (:use #:cl)
  (:export #:shot))
(in-package #:pine.cairo)

;;;; Headless eyes for the cairo backend: load the user's init.lisp (the real
;;;; surfaces), seed a few cells with sample data, build each surface tree from
;;;; the registry, and render it to a PNG. Read the PNGs to see what the wayflan
;;;; client will show -- no window, no daemon, no display.

(defun ensure-env ()
  (unless pine.server:*server*
    (setf pine.server:*server* (make-instance 'pine.server:server)))
  (pine.buffer:install-default-faces)
  (let ((init (merge-pathnames "pine/init.lisp" (uiop:xdg-config-home))))
    (when (probe-file init)
      (let ((*package* (find-package :pine.user))) (load init)))))

(defun seed-cells ()
  (flet ((c (name val) (pine.ref:set-ref (pine.ref:make-ref :name name) val)))
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
  (let* ((rows (sample-editor-rows))
         (sess (pine.editor::make-sess :rows rows :crow 3 :ccol 8))
         (builder (gethash "editor" (symbol-value (find-symbol "*SURFACES*" :pine.desktop))))
         (pine.editor::*editor-session* sess)
         (w 560) (h 480) (path (format nil "~a/pine-cairo-editor.png" dir)))
    (when builder
      (pine.layout:with-cairo-layout
        (let ((surface (cairo:create-image-surface :argb32 w h))
              (tree (funcall builder nil)))
          (cairo:with-context ((cairo:create-context surface))
            (multiple-value-bind (r g b) (pine.buffer:hex-rgb (pine.buffer:color :bg-alt))
              (cairo:set-source-rgb (/ r 255.0) (/ g 255.0) (/ b 255.0)) (cairo:paint))
            (pine.layout:paint-tree tree w h))
          (cairo:surface-write-to-png surface path)))
      (cons "editor" (list :w w :h h :path path)))))

(defun shot (&key (dir "/tmp"))
  "Render every desktop surface to DIR/pine-cairo-NAME.png; return the results."
  (ensure-env)                                     ; loads init.lisp -> registers surfaces
  (seed-cells)                                     ; sample data over the defrefs
  (let ((surfaces (symbol-value (find-symbol "*SURFACES*" :pine.desktop))))
    (append
     (loop for name in *shots*
           for builder = (gethash name surfaces)
           when builder
             collect (let ((path (format nil "~a/pine-cairo-~a.png" dir name)))
                       (cons name (pine.layout:render-tree-to-png (funcall builder nil) path
                                    :avail-w (if (string= name "bar") 120 440)))))
     (list (ignore-errors (editor-shot dir))))))
