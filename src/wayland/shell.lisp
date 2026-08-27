(defpackage #:pine/wayland/shell
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell #:pine/wayland/protocol)
  (:local-nicknames (#:display #:pine/wayland/display))
  (:export #:shell #:bind #:display-of #:compositor #:shm #:layer #:toplevel
           #:seat #:pointer #:keyboard #:on-pointer #:on-keyboard #:chrome
           #:showing #:show #:unshow #:at-surface))
(in-package #:pine/wayland/shell)

(defclass shell ()
  ((display-of :initarg :display :reader display-of)
   (compositor :initform nil :accessor compositor)
   (shm        :initform nil :accessor shm)
   (layer      :initform nil :accessor layer)
   (toplevel   :initform nil :accessor toplevel)
   (seat       :initform nil :accessor seat)
   (pointer    :initform nil :accessor pointer)
   (keyboard   :initform nil :accessor keyboard)
   (on-pointer  :initarg :on-pointer  :accessor on-pointer  :initform nil)
   (on-keyboard :initarg :on-keyboard :accessor on-keyboard :initform nil)
   (chrome     :initform nil :accessor chrome)
   (showing    :initform nil :accessor showing))
  (:documentation "What the compositor advertised, and what is up on it. One
connection: a bar, a panel and a window are surfaces on the same shell, which is
why the editor and the desktop are one program.

CHROME is where furniture comes from on a compositor with no layer shell of its
own: the window manager hands it out, and pine is the window manager there."))

(defun at-surface (s surface)
  "What pine surface a wayland surface is showing."
  (cdr (assoc surface (showing s))))

(defun show (s surface shown) (push (cons surface shown) (showing s)))

(defun unshow (s shown)
  (setf (showing s) (remove shown (showing s) :key #'cdr)))

(defun %seat (s &rest event)
  (event-case event
    (:capabilities (capabilities)
     (when (and (member :pointer capabilities) (null (pointer s)))
       (let ((it (wl-seat.get-pointer (seat s))))
         (setf (pointer s) it)
         (push (lambda (&rest e)
                 (when (on-pointer s) (apply (on-pointer s) s e)))
               (wl-proxy-hooks it))))
     (when (and (member :keyboard capabilities) (null (keyboard s)))
       (let ((it (wl-seat.get-keyboard (seat s))))
         (setf (keyboard s) it)
         (push (lambda (&rest e)
                 (when (on-keyboard s) (apply (on-keyboard s) s e)))
               (wl-proxy-hooks it)))))
    (:name (name) (declare (ignore name)))))

(defun bind (d &key on-pointer on-keyboard)
  "Bind what pine paints with: a compositor, shared memory, the two shells and a
seat. A compositor that has no layer shell can still show a window."
  (let* ((s (make-instance 'shell :display d :on-pointer on-pointer
                                  :on-keyboard on-keyboard))
         (it (display:of d))
         (registry (wl-display.get-registry it)))
    (push (evlambda
            (:global (name interface version)
             (declare (ignore version))
             (case (alexandria:when-let ((found (find-interface-named interface)))
                     (class-name found))
               (wl-compositor
                (setf (compositor s) (wl-registry.bind registry name
                                                       'wl-compositor 4)))
               (wl-shm (setf (shm s) (wl-registry.bind registry name 'wl-shm 1)))
               (zwlr-layer-shell-v1
                (setf (layer s) (wl-registry.bind registry name
                                                  'zwlr-layer-shell-v1 1)))
               (xdg-wm-base
                (let ((base (wl-registry.bind registry name 'xdg-wm-base 1)))
                  (setf (toplevel s) base)
                  (push (evelambda (:ping (serial)
                                    (xdg-wm-base.pong base serial)))
                        (wl-proxy-hooks base))))
               (wl-seat
                (let ((it (wl-registry.bind registry name 'wl-seat 5)))
                  (setf (seat s) it)
                  (push (alexandria:curry #'%seat s) (wl-proxy-hooks it)))))))
          (wl-proxy-hooks registry))
    (wl-display-roundtrip it)
    (wl-display-roundtrip it)
    (unless (compositor s) (error "the compositor advertises no wl_compositor"))
    s))
