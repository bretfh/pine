(in-package #:pine/wm)

(defparameter +actions+
  '(("overview"  "toggle-overview")
    ("close"     "close-window")
    ("expel"     "consume-or-expel-window-right")
    ("consume"   "consume-or-expel-window-left")
    ("workspace" "focus-workspace")
    ("window"    "focus-window" "--id")
    ("exit"      "quit" "--skip-confirmation")))

(defclass niri (compositor) ())

(defmethod fs:notified-by ((c niri)) (list "niri msg --json event-stream"))

(defun json (text)
  (when (and text (plusp (length text)))
    (fault:or-nothing "what the compositor said may not be json"
      (com.inuoe.jzon:parse text))))

(defun %list (command)
  (let ((value (json (sh:sh "niri msg --json ~a" command))))
    (when (vectorp value) (coerce value 'list))))

(defmethod workspaces ((c niri)) (%list "workspaces"))

(defmethod windows ((c niri)) (%list "windows"))

(defmethod outputs ((c niri))
  (let ((said (json (sh:sh "niri msg --json outputs"))))
    (when (hash-table-p said)
      (loop :for name :being :the :hash-keys :of said :using (:hash-value out)
            :for mode := (let ((modes (gethash "modes" out))
                               (at (gethash "current_mode" out)))
                           (when (and (vectorp modes) (numberp at)
                                      (< at (length modes)))
                             (aref modes at)))
            :for size := (list (if mode (gethash "width" mode) 0)
                               (if mode (gethash "height" mode) 0))
            :for at := (let ((p (gethash "logical" out)))
                         (if p
                             (list (gethash "x" p) (gethash "y" p))
                             (list 0 0)))
            :collect (list :name name :position at :size size
                           :area (append at size))))))

(defmethod focused ((c niri))
  (let ((found (find-if (lambda (w) (gethash "is_focused" w))
                        (windows c))))
    (when found (princ-to-string (gethash "id" found)))))

(defmethod rect ((c niri) id)
  (let ((found (find-if (lambda (w)
                          (equal (princ-to-string id)
                                 (princ-to-string (gethash "id" w ""))))
                        (windows c))))
    (when found
      (let ((at (gethash "layout" found)))
        (when at
          (let ((pos (gethash "pos_in_scrolling_layout" at))
                (size (gethash "tile_size" at)))
            (when (and (vectorp pos) (vectorp size) (= 2 (length pos))
                       (= 2 (length size)))
              (list (round (aref pos 0)) (round (aref pos 1))
                    (round (aref size 0)) (round (aref size 1))))))))))

(defmethod titled ((c niri) id)
  (let ((found (find-if (lambda (w)
                          (equal (princ-to-string id)
                                 (princ-to-string (gethash "id" w ""))))
                        (windows c))))
    (when found (gethash "title" found))))

(defmethod focus ((c niri) id)
  (act c "window" id))

(defmethod verbs ((c niri)) (mapcar #'car +actions+))

(defmethod act ((c niri) verb &rest arguments)
  (let ((action (cdr (assoc (princ-to-string verb) +actions+ :test #'equal))))
    (when action
      (apply #'sh:argv "niri" "msg" "action" (append action arguments))
      t)))
