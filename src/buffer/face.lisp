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
  ;; server-scoped state; a headless daemon (no client) resolves via *server*.
  ;; read the *client* special directly -- current-client errors when unbound.
  (let* ((cli pine.client:*client*)
         (srv (if cli (pine.client:server-of cli) pine.server:*server*)))
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

(defun face-attr-bits (face)
  "bit 0 bold, bit 1 italic, bit 2 underline."
  (if face
      (logior (if (bold face) 1 0) (if (italic face) 2 0) (if (underline face) 4 0))
      0))


;;;; Themes

(defclass theme ()
  ((name    :initarg :name    :reader theme-name)
   (palette :initarg :palette :reader theme-palette :initform nil)
   (faces   :initarg :faces   :reader theme-faces
            :initform (make-hash-table :test 'eq))))

(defvar *themes* (make-hash-table :test 'eq))
(defvar *active-theme* :ef-dream
  "The installed theme; color and the faces resolve against it.")

(defun theme-key (name)
  (etypecase name
    (keyword name)
    (symbol  (intern (symbol-name name) :keyword))
    (string  (intern (string-upcase name) :keyword))))

(defun register-theme (theme)
  (setf (gethash (theme-name theme) *themes*) theme))

(defun find-theme (name)
  (or (gethash (theme-key name) *themes*)
      (error "unknown theme ~s" name)))

(defun resolve-color (color palette)
  (etypecase color
    (null   nil)
    (string color)
    (symbol (or (cdr (assoc color palette :test #'string=))
                (error "color ~s is not in the palette" color)))))

(defun theme-color (theme-name role)
  (or (cdr (assoc role (theme-palette (find-theme theme-name)) :test #'string=))
      (error "theme ~s has no color ~s" theme-name role)))

(defun color (role)
  "The hex of a palette ROLE in the active theme."
  (theme-color *active-theme* role))

(defun hex-rgb (hex)
  "The (values r g b) 0..255 of a #rrggbb string, or NIL for a non-colour."
  (when (and (stringp hex) (>= (length hex) 7) (char= (char hex 0) #\#))
    (values (parse-integer hex :start 1 :end 3 :radix 16)
            (parse-integer hex :start 3 :end 5 :radix 16)
            (parse-integer hex :start 5 :end 7 :radix 16))))

(defun face-fg (name)
  "FACE NAME's foreground as an (r g b) list, falling back to the default face."
  (let* ((f (find-face name))
         (hex (or (and f (fg f))
                  (let ((d (find-face :default))) (and d (fg d))))))
    (multiple-value-bind (r g b) (hex-rgb hex)
      (if r (list r g b) '(205 214 244))))) ; only reached before a theme loads

(defun face-bg (name)
  "FACE NAME's background as an (r g b) list, or NIL when it has none."
  (let ((f (find-face name)))
    (when (and f (bg f))
      (multiple-value-bind (r g b) (hex-rgb (bg f)) (list r g b)))))

(defun build-theme (name palette-plist face-specs)
  (let ((palette (loop for (role hex) on palette-plist by #'cddr
                       collect (cons role hex)))
        (faces   (make-hash-table :test 'eq)))
    (dolist (spec face-specs)
      (destructuring-bind (fname &key fg bg bold italic underline) spec
        (setf (gethash fname faces)
              (make-instance 'face
                             :fg (resolve-color fg palette)
                             :bg (resolve-color bg palette)
                             :bold bold :italic italic :underline underline))))
    (make-instance 'theme :name (theme-key name) :palette palette :faces faces)))

(defmacro deftheme (name &key palette faces)
  `(register-theme (build-theme ',name ',palette ',faces)))

(defun %faces-server ()
  (let ((cli pine.client:*client*))
    (if cli (pine.client:server-of cli) pine.server:*server*)))

(defun load-theme (name)
  (setf (pine.server:faces (%faces-server)) (theme-faces (find-theme name))
        *active-theme* (theme-key name))
  *active-theme*)

(defun install-default-faces ()
  (load-theme :ef-dream))

(deftheme ef-dream
  :palette (bg        "#232025"   bg-dim    "#322f34"   bg-alt "#3b393e"
            bg-active "#5b595e"   fg        "#efd5c5"   fg-dim "#8f8886"
            fg-alt    "#b0a0cf"   border    "#635850"   accent "#675072"
            accent-fg "#fedeff"   red       "#ff6f6f"   green  "#51b04f"
            yellow    "#c0b24f"   blue      "#57b0ff"   magenta "#ffaacf"
            cyan      "#6fb3c0"
            ;; ef-dream syntax variants
            red-faint     "#f3a0a0" blue-faint    "#a0a0cf" yellow-faint  "#caa89f"
            yellow-cooler "#deb07a" magenta-faint "#e3b0c0" blue-warmer   "#80aadf"
            cyan-warmer   "#8fcfd0" cyan-faint    "#99bfcf" green-faint   "#a9c99f"
            cyan-cooler   "#65c5a8" red-cooler    "#e47980" magenta-cooler "#d0b0ff"
            green-cooler  "#3fc489"
            ;; ef-dream chrome
            cursor "#f3c09a" region "#544a50" bg-completion "#503240"
            shadow "#0a0a10")
  :faces ((:default        :fg fg)
          (:window         :bg bg)
          (:echo           :fg fg)
          (:cursor         :bg cursor)
          (:selection      :bg region)
          (:modeline       :fg accent-fg :bg accent)
          (:modeline-mode  :fg accent-fg :bg accent :bold t)
          (:modeline-dim   :fg fg-alt    :bg accent)
          (:modeline-faint :fg fg-dim    :bg accent)
          (:border-active   :fg accent)
          (:border-inactive :fg bg-alt)
          (:prompt         :fg magenta :bold t)
          (:completion     :fg fg        :bg bg-completion)
          (:completion-selected :fg accent-fg :bg bg-active)
          ;; syntax -- ef-dream's own role mappings
          (:keyword        :fg yellow-cooler :bold t)
          (:string         :fg red-faint)
          (:comment        :fg blue-faint :italic t)
          (:function-name  :fg cyan-warmer :bold t)
          (:function-call  :fg cyan-faint)
          (:variable       :fg magenta)
          (:variable-param :fg magenta-faint)
          (:number         :fg fg)
          (:builtin        :fg magenta-faint)
          (:constant       :fg blue-warmer)
          (:character      :fg blue-warmer)
          (:type           :fg green-faint)
          (:namespace      :fg fg-alt)
          (:quote          :fg fg-dim)
          (:escape         :fg cyan-cooler)
          (:line-number    :fg fg-dim)
          ;; rainbow delimiters by depth (ef-dream rainbow palette)
          (:delimiter.0    :fg yellow-cooler)
          (:delimiter.1    :fg magenta)
          (:delimiter.2    :fg blue-warmer)
          (:delimiter.3    :fg red-cooler)
          (:delimiter.4    :fg magenta-cooler)
          (:delimiter.5    :fg green-cooler)
          ;; desktop accents
          (:error          :fg red)
          (:accent         :fg fg-alt)
          (:ws-active      :fg accent-fg :bg accent :bold t)
          (:hover          :fg accent-fg :bg bg-active)))


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
