(defpackage #:pine/ui/face
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree))
  (:export #:face #:fg #:bg #:bold #:italic #:underline #:attrs
           #:faces #:with-faces #:named #:rgb
           #:theme #:name #:palette #:metrics #:themes #:register #:active
           #:color #:metric #:hex #:memo #:build #:*themes* #:+plain+))
(in-package #:pine/ui/face)

(defvar *in-force* nil
  "The face table for the render running on this thread. Bound for the extent of one
render: finding the table is three reads and finding a face in it is one, so a paint
that asks per cell spends most of its time asking where to look.")

(defvar *themes* (d:table))

(defparameter +plain+ :default
  "The face a space that has not said resolves in.")

(defparameter +theme+ :ef-dream)

(defclass face ()
  ((fg        :initarg :fg        :accessor fg        :initform nil)
   (bg        :initarg :bg        :accessor bg        :initform nil)
   (bold      :initarg :bold      :accessor bold      :initform nil)
   (italic    :initarg :italic    :accessor italic    :initform nil)
   (underline :initarg :underline :accessor underline :initform nil)))

(defclass theme ()
  ((name    :initarg :name    :reader name)
   (palette :initarg :palette :reader palette :initform nil)
   (metrics :initarg :metrics :reader metrics :initform nil)
   (faces   :initarg :faces   :reader faces
            :initform (make-hash-table :test 'eq))))

(defun %key (name)
  (etypecase name
    (keyword name)
    (symbol (intern (symbol-name name) :keyword))
    (string (intern (string-upcase name) :keyword))))

(defun register (theme) (d:keep! *themes* (name theme) theme))

(defun themes ()
  (sort (d:keys (d:all *themes*)) #'string< :key #'symbol-name))

(defun theme (name)
  (or (d:lookup (d:all *themes*) (%key name))
      (error "no theme called ~s" name)))

(defun active ()
  "The theme in force here: /theme/active, which is a value like any other."
  (or (let ((n (and (tree:root) (tree:at nil "theme" "active"))))
        (and n (node:contents n)))
      +theme+))

(defun memo (which thunk)
  "What WHICH worked out to, kept in the tree so it is thrown away when anything it
read moves. Before there is a tree, it is worked out every time."
  (if (null (tree:root))
      (funcall thunk)
      (let* ((name (string-downcase (symbol-name which)))
             (under (or (node:resolve (tree:root) "memo")
                        (node:attach (node:make "memo" :savedp nil)
                                     (tree:root))))
             (n (node:resolve under name)))
        (unless n
          (setf n (node:derive name thunk))
          (node:attach n under))
        (node:contents n))))

(defgeneric hex (color palette)
  (:documentation "COLOR as the hex it stands for: a literal, or a role the palette
names.")
  (:method ((color null) palette) (declare (ignore palette)) nil)
  (:method ((color string) palette) (declare (ignore palette)) color)
  (:method ((color symbol) palette)
    (or (cdr (assoc color palette :test #'string=))
        (error "color ~s is not in the palette" color))))

(defun build (name palette-plist metrics-plist specs)
  (let ((palette (loop :for (role h) :on palette-plist :by #'cddr
                       :collect (cons role h)))
        (metrics (loop :for (key v) :on metrics-plist :by #'cddr
                       :collect (cons key v)))
        (faces (make-hash-table :test 'eq)))
    (dolist (spec specs)
      (destructuring-bind (fname &key fg bg bold italic underline) spec
        (setf (gethash fname faces)
              (make-instance 'face :fg (hex fg palette) :bg (hex bg palette)
                                   :bold bold :italic italic
                                   :underline underline))))
    (make-instance 'theme :name (%key name) :palette palette :metrics metrics
                          :faces faces)))

(defun %as-face (m)
  (when (and (consp m) (keywordp (first m)))
    (make-instance 'face :fg (getf m :fg) :bg (getf m :bg)
                         :bold (getf m :bold) :italic (getf m :italic)
                         :underline (getf m :underline))))

(defun %resolve ()
  "The active theme's faces with whatever was written at /face/?name on top."
  (let ((out (make-hash-table :test 'eq))
        (written (and (tree:root) (tree:at nil "face"))))
    (maphash (lambda (k v) (setf (gethash k out) v)) (faces (theme (active))))
    (when written
      (dolist (each (node:nodes written))
        (let ((f (%as-face (node:contents each))))
          (when f (setf (gethash (%key (node:name each)) out) f)))))
    out))

(defun faces-in-force ()
  "Face name to face, worked out once and kept until the tree moves: NAMED is on the
path every painted cell takes."
  (or *in-force* (memo :faces #'%resolve)))

(defmacro with-faces (&body body)
  "Run BODY with the faces in force worked out once."
  `(let ((*in-force* (faces-in-force))) ,@body))

(defun named (name) (gethash name (faces-in-force)))

(defun attrs (f)
  "bit 0 bold, bit 1 italic, bit 2 underline."
  (if f
      (logior (if (bold f) 1 0) (if (italic f) 2 0) (if (underline f) 4 0))
      0))

(defun color (role)
  "The hex of a palette ROLE in the active theme."
  (or (cdr (assoc role (palette (theme (active))) :test #'string=))
      (error "the active theme has no color ~s" role)))

(defun metric (key &optional default)
  (let ((cell (assoc key (metrics (theme (active))) :test #'string=)))
    (if cell (cdr cell) default)))

(defun rgb (h)
  "A #rrggbb string as (r g b), or nothing for anything else. A face's FG and BG are
hex; this is how a canvas reads one."
  (when (and (stringp h) (>= (length h) 7) (char= (char h 0) #\#))
    (list (parse-integer h :start 1 :end 3 :radix 16)
          (parse-integer h :start 3 :end 5 :radix 16)
          (parse-integer h :start 5 :end 7 :radix 16))))

(register
 (build
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
    (:match          :fg accent-fg :bg bg-active)
    (:ws-active      :fg accent-fg :bg accent :bold t)
    (:hover          :fg accent-fg :bg bg-active)
    (:ring-cpu       :fg red)
    (:ring-ram       :fg blue)
    (:ring-disk      :fg green)
    (:ring-temp      :fg yellow)
    (:ring-track     :fg bg-active))))

(pine/word:lends "color" "metric")
