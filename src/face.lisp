(in-package :pine.buffer)


;;;; ================================================================
;;;; Faces
;;;; ================================================================

(defclass face ()
  ((fg        :initarg :fg        :accessor fg        :initform nil)
   (bg        :initarg :bg        :accessor bg        :initform nil)
   (bold      :initarg :bold      :accessor bold      :initform nil)
   (italic    :initarg :italic    :accessor italic    :initform nil)
   (underline :initarg :underline :accessor underline :initform nil)))

(defun faces-table ()
  (let ((srv (pine.client:server-of (pine.client:current-client))))
    (or (pine.server:faces srv)
        (setf (pine.server:faces srv) (make-hash-table :test 'eq)))))

(defun defface (name &key fg bg bold italic underline)
  (setf (gethash name (faces-table))
        (make-instance 'face :fg fg :bg bg :bold bold
                             :italic italic :underline underline)))

(defun find-face (name)
  (gethash name (faces-table)))

(defun face-to-plist (f)
  "Serialize a face to a plist for crossing the QML boundary."
  (when f
    (append
     (when (fg f) (list :fg (fg f)))
     (when (bg f) (list :bg (bg f)))
     (when (bold f) (list :bold t))
     (when (italic f) (list :italic t))
     (when (underline f) (list :underline t)))))


(defun install-default-faces ()
  ;; ef-dream palette (the active theme). Themes both the editor's syntax faces
  ;; and the desktop, since the widgets reuse these faces.
  (defface :default       :fg "#efd5c5")
  (defface :cursor        :bg "#f3c09a")
  (defface :selection     :bg "#544a50")
  (defface :modeline      :fg "#efd5c5" :bg "#322f34")
  (defface :modeline-mode :fg "#57b0ff" :bg "#322f34" :bold t)
  (defface :modeline-dim  :fg "#8f8886" :bg "#322f34")
  (defface :modeline-faint :fg "#635850" :bg "#322f34")
  (defface :border-active :fg "#675072")
  (defface :border-inactive :fg "#3b393e")
  (defface :prompt        :fg "#57b0ff" :bold t)
  (defface :completion    :fg "#efd5c5" :bg "#322f34")
  (defface :completion-selected :fg "#fedeff" :bg "#5b595e")
  (defface :keyword         :fg "#ffaacf")   ; magenta
  (defface :string          :fg "#51b04f")   ; green
  (defface :comment         :fg "#8f8886")   ; fg-dim
  (defface :function-name   :fg "#57b0ff")   ; blue
  (defface :variable        :fg "#b0a0cf")   ; fg-alt
  (defface :variable-param  :fg "#c0b24f")   ; yellow
  (defface :type            :fg "#c0b24f")
  (defface :builtin         :fg "#6fb3c0")   ; cyan
  (defface :constant        :fg "#c0b24f")
  (defface :escape          :fg "#c0b24f")
  (defface :line-number     :fg "#635850")
  ;; desktop accents
  (defface :error           :fg "#ff6f6f")   ; red (destructive actions)
  (defface :accent          :fg "#b0a0cf")   ; fg-alt as a visible accent on glass
  (defface :ws-active       :fg "#fedeff" :bg "#675072" :bold t)   ; accent pill
  (defface :hover           :fg "#fedeff" :bg "#5b595e"))          ; hover: bg-active


;;;; Face runs — attributed text

(defclass face-run ()
  ((start-col :initarg :start :accessor run-start :initform 0)
   (end-col   :initarg :end   :accessor run-end   :initform 0)
   (run-face  :initarg :face  :accessor run-face   :initform :default)))

(defclass display-line ()
  ((text :initarg :text :accessor display-text :initform "")
   (runs :initarg :runs :accessor display-runs :initform nil)))

(defun make-display-line (text &optional runs)
  (make-instance 'display-line
    :text text
    :runs (or runs
              (list (make-instance 'face-run
                      :start 0 :end (length text) :face :default)))))

(defun display-line-to-plist (dl)
  "Serialize a display-line for QML: (:text str :runs ((s e face-plist) ...))."
  (list :text (display-text dl)
        :runs (mapcar (lambda (r)
                        (list (run-start r) (run-end r)
                              (face-to-plist (find-face (run-face r)))))
                      (display-runs dl))))
