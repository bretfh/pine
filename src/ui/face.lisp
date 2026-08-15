(defpackage #:pine.ui.face
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data))
  (:export

   #:face-run #:run-start #:run-end #:run-face
   #:display-line #:make-display-line #:display-text #:display-runs

   #:face #:fg #:bg #:bold #:italic #:underline
   #:faces #:with-faces #:find-face #:face-attr-bits #:face-fg #:face-bg

   #:theme #:theme-name #:theme-palette #:theme-faces #:theme-metrics
   #:theme-key #:register-theme #:find-theme #:active
   #:*themes* #:+default-theme+
   #:theme-color #:color #:theme-metric #:metric #:resolve-color #:hex-rgb

   #:memo))

(in-package #:pine.ui.face)

(defvar *in-force* nil
  "The face table for the render running on this thread. Bound for the extent
of one render and never assigned: finding the table is three reads and finding a
face in it is one, so a paint that asks per cell spends most of its time asking
where to look.")

(defvar *themes* (d:table)
  "Theme name to theme: what the files said as they loaded, read from every
thread and added to by nothing else.")

(defparameter +default-theme+ :ef-dream
  "The theme a space that has not said resolves in.")

(defclass face-run ()
  ((start-col :initarg :start :accessor run-start :initform 0)
   (end-col   :initarg :end   :accessor run-end   :initform 0)
   (run-face  :initarg :face  :accessor run-face   :initform :default)))

(defclass display-line ()
  ((text :initarg :text :accessor display-text :initform "")
   (runs :initarg :runs :accessor display-runs :initform nil)))

(defclass face ()
  ((fg        :initarg :fg        :accessor fg        :initform nil)
   (bg        :initarg :bg        :accessor bg        :initform nil)
   (bold      :initarg :bold      :accessor bold      :initform nil)
   (italic    :initarg :italic    :accessor italic    :initform nil)
   (underline :initarg :underline :accessor underline :initform nil)))

(defclass theme ()
  ((name    :initarg :name    :reader theme-name)
   (palette :initarg :palette :reader theme-palette :initform nil)
   (metrics :initarg :metrics :reader theme-metrics :initform nil)
   (faces   :initarg :faces   :reader theme-faces
            :initform (make-hash-table :test 'eq))))

(defun memo (which thunk)
  "The cell the :THEME server keeps for WHICH, or NIL before it is raised. Only
read here: making one is a write to the space, and this runs while a cell is
being painted."

  (let ((root (and pine/world/world:*world*
                   (pine/world/world:root pine/world/world:*world*))))
    (if (null root)
        (funcall thunk)
        (let* ((name (string-downcase (symbol-name which)))
               (under (or (pine/fs/node:resolve root "memo")
                          (pine/fs/node:attach
                           (pine/fs/node:make-node "memo" :class 'pine/fs/node:node)
                           root)))
               (n (pine/fs/node:resolve under name)))
          (unless n
            (setf n (pine/fs/computed:computed name thunk))
            (pine/fs/node:attach n under))
          (pine/fs/node:contents n)))))

(defgeneric theme-key (name)
  (:documentation "NAME as the keyword a theme is known by."))

(defmethod theme-key ((name symbol))
  (if (keywordp name) name (intern (symbol-name name) :keyword)))

(defmethod theme-key ((name string))
  (intern (string-upcase name) :keyword))

(defun register-theme (theme)
  (d:keep! *themes* (theme-name theme) theme))

(defun find-theme (name)
  (or (d:at (d:all *themes*) (theme-key name))
      (error "unknown theme ~s" name)))

(defun active ()
  "The theme in force here: /theme, which is a value like any other."
  (or (and pine/world/world:*world*
           (let ((n (pine/world/world:at pine/world/world:*world* "active-theme")))
             (and n (pine/fs/node:contents n))))
      +default-theme+))

(defgeneric resolve-color (color palette)
  (:documentation "COLOR as the hex it stands for: a literal, or a role PALETTE
names."))

(defmethod resolve-color ((color null) palette)
  (declare (ignore palette))
  nil)

(defmethod resolve-color ((color string) palette)
  (declare (ignore palette))
  color)

(defmethod resolve-color ((color symbol) palette)
  (or (cdr (assoc color palette :test #'string=))
      (error "color ~s is not in the palette" color)))

(defun build-theme (name palette-plist metrics-plist face-specs)
  (let ((palette (loop for (role hex) on palette-plist by #'cddr
                       collect (cons role hex)))
        (metrics (loop for (key val) on metrics-plist by #'cddr
                       collect (cons key val)))
        (faces   (make-hash-table :test 'eq)))
    (dolist (spec face-specs)
      (destructuring-bind (fname &key fg bg bold italic underline) spec
        (setf (gethash fname faces)
                       (make-instance 'face
                                      :fg (resolve-color fg palette)
                                      :bg (resolve-color bg palette)
                                      :bold bold :italic italic
                                      :underline underline))))
    (make-instance 'theme :name (theme-key name) :palette palette
                          :metrics metrics :faces faces)))

(defun %as-face (m)
  "A written {:fg .. :bg ..} as a face, or NIL when it is not one."
  (when (and (consp m) (keywordp (first m)))
    (make-instance 'face :fg (getf m :fg) :bg (getf m :bg)
                         :bold (getf m :bold)
                         :italic (getf m :italic)
                         :underline (getf m :underline))))

(defun %resolve ()
  "The active theme's faces with whatever was written at /face/?name on top.
HELD rather than READ, because /face is served: what is asked for here is what
someone put there, and the provider answers by asking this."
  (let ((out (make-hash-table :test 'eq))
        (written (and pine/world/world:*world*
                      (pine/world/world:at pine/world/world:*world* "face"))))
    (maphash (lambda (k v) (setf (gethash k out) v))
             (theme-faces (find-theme (active))))
    (when written
      (dolist (each (pine/fs/node:nodes written))
        (let ((f (%as-face (pine/fs/node:contents each))))
          (when f
            (setf (gethash (intern (string-upcase (pine/fs/node:name each)) :keyword)
                           out)
                  f)))))
    out))

(defun faces ()
  "Face name to face, for the theme in force and the overrides on it. Worked out
once and kept until the tree moves: FIND-FACE is on the path every painted cell
takes."
  (or *in-force* (memo :faces #'%resolve)))

(defmacro with-faces (&body body)
  "Run BODY with the faces in force worked out once."
  `(let ((*in-force* (faces))) ,@body))

(defun find-face (name)
  (gethash name (faces)))

(defun face-attr-bits (face)
  "bit 0 bold, bit 1 italic, bit 2 underline."
  (if face
      (logior (if (bold face) 1 0) (if (italic face) 2 0) (if (underline face) 4 0))
      0))

(defun theme-color (theme-name role)
  (or (cdr (assoc role (theme-palette (find-theme theme-name)) :test #'string=))
      (error "theme ~s has no color ~s" theme-name role)))

(defun color (role)
  "The hex of a palette ROLE in the active theme."
  (theme-color (active) role))

(defun theme-metric (theme-name key &optional default)
  (let ((cell (assoc key (theme-metrics (find-theme theme-name)) :test #'string=)))
    (if cell (cdr cell) default)))

(defun metric (key &optional default)
  "A layout metric (:radius :border :opacity :font ...) from the active theme."
  (theme-metric (active) key default))

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
      (if r (list r g b) '(205 214 244)))))

(defun face-bg (name)
  "FACE NAME's background as an (r g b) list, or NIL when it has none."
  (let ((f (find-face name)))
    (when (and f (bg f))
      (multiple-value-bind (r g b) (hex-rgb (bg f)) (list r g b)))))

(defun make-display-line (text &optional runs)
  (make-instance 'display-line
    :text text
    :runs (or runs
              (list (make-instance 'face-run
                      :start 0 :end (length text) :face :default)))))

(register-theme
 (build-theme
  :ef-dream
  '(bg        "#232025"   bg-dim    "#322f34"   bg-alt "#3b393e"
    bg-active "#5b595e"   fg        "#efd5c5"   fg-dim "#8f8886"
    fg-alt    "#b0a0cf"   border    "#635850"   accent "#675072"
    accent-fg "#fedeff"   red       "#ff6f6f"   green  "#51b04f"
    yellow    "#c0b24f"   blue      "#57b0ff"   magenta "#ffaacf"
    cyan      "#6fb3c0"
    red-faint     "#f3a0a0" blue-faint    "#a0a0cf" yellow-faint  "#caa89f"
    yellow-cooler "#deb07a" magenta-faint "#e3b0c0" blue-warmer   "#80aadf"
    cyan-warmer   "#8fcfd0" cyan-faint    "#99bfcf" green-faint   "#a9c99f"
    cyan-cooler   "#65c5a8" red-cooler    "#e47980" magenta-cooler "#d0b0ff"
    green-cooler  "#3fc489"
    cursor "#f3c09a" region "#544a50" bg-completion "#503240"
    shadow "#0a0a10")
  '(radius 8 border 2 opacity 0.4 font "Maple Mono NF" font-px 15)
  '((:default        :fg fg)
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
    (:delimiter.0    :fg yellow-cooler)
    (:delimiter.1    :fg magenta)
    (:delimiter.2    :fg blue-warmer)
    (:delimiter.3    :fg red-cooler)
    (:delimiter.4    :fg magenta-cooler)
    (:delimiter.5    :fg green-cooler)
    (:error          :fg red)
    (:accent         :fg fg-alt)
    (:ws-active      :fg accent-fg :bg accent :bold t)
    (:hover          :fg accent-fg :bg bg-active)
    (:ring-cpu       :fg red)
    (:ring-ram       :fg blue)
    (:ring-disk      :fg green)
    (:ring-temp      :fg yellow)
    (:ring-track     :fg bg-active))))
