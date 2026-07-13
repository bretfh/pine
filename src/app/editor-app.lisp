(defpackage #:pine.gtk
  (:use #:cl #:gtk4 #:pine.surface)
  (:export #:run-editor))

(in-package #:pine.gtk)

(defvar *sys* nil)
(defvar *client-ref* nil)   ; the daemon's client-N input actor, over remoting
(defvar *attach-ref* nil)   ; the daemon's attach actor, kept from GC
(defvar *area* nil)
(defvar *grid* nil)
(defvar *cursor-row* 0)
(defvar *cursor-col* 0)
(defvar *px-w* 800) (defvar *px-h* 500)
(defvar *sent-cols* -1) (defvar *sent-rows* -1)

(defvar *cell-w* 9d0) (defvar *cell-h* 18d0) (defvar *ascent* 14d0)
(defparameter *font* 15d0)
(defvar *measured* nil)

(defun measure-font ()
  (multiple-value-bind (xb yb w h xadv) (cairo:text-extents "MMMMMMMMMM")
    (declare (ignore xb yb w h))
    (when (plusp xadv) (setf *cell-w* (/ xadv 10d0))))
  (let ((fe (cairo:get-font-extents)))
    (setf *cell-h* (cairo:font-height fe) *ascent* (cairo:font-ascent fe)))
  (setf *measured* t))

(defun cols () (max 1 (floor *px-w* (max 1d0 *cell-w*))))
(defun rows () (max 1 (floor *px-h* (max 1d0 *cell-h*))))

(defun paint-sink (grid cursor-row cursor-col)
  (setf *grid* grid *cursor-row* cursor-row *cursor-col* cursor-col)
  (when *area* (run-in-main-event-loop () (widget-queue-draw *area*))))

(cffi:defcallback %draw :void ((area :pointer) (cr :pointer)
                               (width :int) (height :int) (data :pointer))
  (declare (ignore area data))
  (setf *px-w* width *px-h* height)
  (let ((cairo:*context* (make-instance 'cairo:context :pointer cr
                                        :width width :height height :pixel-based-p nil)))
    (destructuring-bind (r g b) (pine.buffer:face-bg :window)
      (cairo:set-source-rgb (/ r 255d0) (/ g 255d0) (/ b 255d0)))
    (cairo:paint)
    (cairo:select-font-face *font-family* :normal :normal)
    (cairo:set-font-size *font*)
    (unless *measured* (measure-font))
    (let ((c (cols)) (r (rows)))
      (unless (and (= c *sent-cols*) (= r *sent-rows*))
        (setf *sent-cols* c *sent-rows* r)
        (when *client-ref*
          (sento.actor:tell *client-ref* (list :resize :cols c :rows r)))))
    (let ((grid *grid*))
      (when grid
        (paint-rows grid *cell-w* *cell-h* *ascent* 6d0)
        (destructuring-bind (r g b) (pine.buffer:face-bg :cursor)
          (cairo:set-source-rgb (/ r 255d0) (/ g 255d0) (/ b 255d0)))
        (cairo:rectangle (+ 6d0 (* *cursor-col* *cell-w*))
                         (* *cursor-row* *cell-h*) *cell-w* *cell-h*)
        (cairo:set-line-width 1.5d0) (cairo:stroke)))))

(defun gdk->wire (keyval state)
  (let* ((ctrl  (and (logtest state gdk4:+modifier-type-control-mask+) t))
         (meta  (and (logtest state gdk4:+modifier-type-alt-mask+) t))
         (super (and (logtest state gdk4:+modifier-type-super-mask+) t))
         (shift (and (logtest state gdk4:+modifier-type-shift-mask+) t))
         (uni   (gdk4:keyval-to-unicode keyval))
         (printable (and (plusp uni) (not super)
                         (graphic-char-p (code-char uni))
                         (not (and ctrl (char= (code-char uni) #\Space))))))
    (if printable
        (list :key :key-str (string (code-char uni)) :ctrl ctrl :meta meta :super super)
        (list :key :key-str (gdk4:keyval-name keyval)
              :ctrl ctrl :meta meta :shift shift :super super))))

(defun display-receive (msg)
  (case (first msg)
    (:attached
     (destructuring-bind (&key id client-uri) (rest msg)
       (declare (ignore id))
       (setf *client-ref* (sento.remoting:make-remote-ref *sys* client-uri))
       (sento.actor:tell *client-ref* (list :resize :cols (cols) :rows (rows)))))
    (:frame
     (destructuring-bind (&key rows crow ccol) (rest msg)
       (paint-sink rows crow ccol))))
  nil)

(define-application (:name %app :id "org.pine.editor")
  (define-main-window (window (make-application-window :application *application*))
    (setf (window-title window) "pine")
    (let ((area (make-drawing-area)))
      (setf *area* area
            (drawing-area-content-width area) 800
            (drawing-area-content-height area) 500
            (drawing-area-draw-func area) (list (cffi:callback %draw)
                                                (cffi:null-pointer) (cffi:null-pointer))
            (window-child window) area)
      (let ((kc (make-event-controller-key)))
        (connect kc "key-pressed"
                 (lambda (c keyval keycode state)
                   (declare (ignore c keycode))
                   (when *client-ref*
                     (sento.actor:tell *client-ref* (gdk->wire keyval state)))
                   t))
        (widget-add-controller window kc))
      (window-present window))))

(defun %shot-substrate ()
  "A minimal in-process substrate for headless rendering: no remoting, no window."
  (let ((srv (pine.server:start-server)))
    (setf pine.server:*server* srv
          (pine.server:ts-runtime srv) (pine.ts:make-ts-runtime))
    (ignore-errors (pine.ts:ensure-ts (pine.server:ts-runtime srv)))
    (pine.event:make-event-bus srv)
    (pine.actor:start-agent-registry srv)
    (pine.actor:start-local-agent srv)
    (pine.buffer:start-buffer-registry srv)
    (pine.buffer:install-default-faces)
    (pine.mode:install-default-modes)
    (pine.editor:install-commands)
    (pine.editor:install-bindings)
    (pine.editor:install-editor-sessions)
    srv))

(defun shot (&key (path "/tmp/pine-shot.png")
                  (text "(defun frobnicate (x &optional (y 10))
  \"a docstring\"
  (let ((sum 0))
    (dolist (a x) (incf sum (car a)))
    (when (> sum y) (format t \"~a: ~s~%\" x sum))
    (pine.util:combine sum y :key)))"))
  "Render the editor frame for TEXT to a PNG at PATH, headless."
  (unless pine.server:*server* (%shot-substrate))
  (let* ((frame nil)
         (sess (pine.editor:make-editor-session
                nil :sink (lambda (rows crow ccol)
                            (declare (ignore crow ccol))
                            (setf frame rows)))))
    (let ((buf (pine.client:current-buffer (pine.editor::sess-client sess))))
      (pine.buffer:tell buf :replace-content :content text))
    (pine.editor:session-feed sess (list :resize :cols 84 :rows 30))
    (sleep 0.8)
    (cond (frame (pine.surface:render-frame-to-png frame path)
                 (format t "~&wrote ~a~%" path) path)
          (t (format t "~&shot: no frame captured~%") nil))))

(defun run-editor (&key (host "127.0.0.1") (port 17000))
  "Attach the editor window to the pine daemon on HOST:PORT. The daemon owns the
buffers and runs the eval; this process paints frames it pushes and sends input
back. Faces are installed locally too, since the cairo draw resolves the window
and cursor colours here."
  (unless pine.server:*server* (setf pine.server:*server* (make-instance 'pine.server:server)))
  (ignore-errors (pine.buffer:install-default-faces))
  (setf *sys* (sento.actor-system:make-actor-system
               '(:dispatchers (:shared (:workers 2 :strategy :random)))))
  (sento.remoting:enable-remoting *sys* :host "127.0.0.1" :port 0)
  (sento.actor-context:actor-of *sys* :name "display" :receive #'display-receive)
  (setf *attach-ref*
        (pine.attach:attach-to-daemon
         *sys*
         (format nil "sento://~a:~d/user/attach" host port)
         (format nil "sento://127.0.0.1:~d/user/display" (sento.remoting:remoting-port *sys*))
         :kind :editor))
  (%app))
