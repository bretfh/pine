(in-package #:pine/ui)

(defvar *in-force* nil
  "The face table for the render running on this thread. Bound for the extent of one
render: finding the table is three reads and finding a face in it is one, so a paint
that asks per cell spends most of its time asking where to look.")

(defparameter +plain+ :default
  "The face a space that has not said resolves in.")

(defparameter +theme+ :ef-dream)

(defclass face ()
  ((fg        :initarg :fg        :accessor fg        :initform nil)
   (bg        :initarg :bg        :accessor bg        :initform nil)
   (bold      :initarg :bold      :accessor bold      :initform nil)
   (italic    :initarg :italic    :accessor italic    :initform nil)
   (underline :initarg :underline :accessor underline :initform nil)))

(defclass theme (fs:value) ()
  (:documentation "A theme at /ui/theme/<name>: (:palette … :metrics … :faces …),
the faces by name, each as a plist."))

(defmethod fs:savedp ((th theme)) nil)

(defun palette (th) (getf (fs:contents th) :palette))
(defun metrics (th) (getf (fs:contents th) :metrics))
(defun faces (th) (getf (fs:contents th) :faces))

(defun %as-keyword (name)
  (etypecase name
    (keyword name)
    (symbol (intern (symbol-name name) :keyword))
    (string (intern (string-upcase name) :keyword))))

(defun %themes () (fs:at "/ui/theme"))

(defun themes ()
  (sort (loop :for each :in (fs:entries (%themes))
              :when (typep each 'theme) :collect (%as-keyword (fs:name each)))
        #'string< :key #'symbol-name))

(defun theme (name)
  (let ((it (fs:entry (%themes) (string-downcase (symbol-name (%as-keyword name))))))
    (if (typep it 'theme)
        it
        (error "no theme called ~s" name))))

(defun active ()
  "The theme in force here: /ui/theme/active, which is a value like any other."
  (or (let ((n (fs:at "/ui/theme/active")))
        (and n (fs:contents n)))
      +theme+))

(defun %role (plist name)
  "What PLIST says for NAME, by name: a role reads the same whichever package
spelled it."
  (loop :for (k v) :on plist :by #'cddr
        :when (string= k name) :return (values v t)))

(defgeneric hex (color palette)
  (:documentation "COLOR as the hex it stands for: a literal, or a role the palette
names.")
  (:method ((color null) palette) (declare (ignore palette)) nil)
  (:method ((color string) palette) (declare (ignore palette)) color)
  (:method ((color symbol) palette)
    (or (%role palette color)
        (error "color ~s is not in the palette" color))))

(defun build (name palette-plist metrics-plist specs)
  "Declare a theme at /ui/theme/<name>."
  (let* ((palette (loop :for (role h) :on palette-plist :by #'cddr
                        :append (list (%as-keyword role) h)))
         (metrics (loop :for (key v) :on metrics-plist :by #'cddr
                        :append (list (%as-keyword key) v)))
         (faces (loop :for (fname . spec) :in specs
                      :append (destructuring-bind (&key fg bg bold italic underline) spec
                                (list fname (list :fg (hex fg palette) :bg (hex bg palette)
                                                  :bold bold :italic italic
                                                  :underline underline)))))
         (held (list :palette palette :metrics metrics :faces faces)))
    (fs:mount (lambda () (make-instance 'theme :held held))
              (format nil "/ui/theme/~a" (string-downcase (symbol-name (%as-keyword name)))))))

(defun %as-face (m)
  (when (and (consp m) (keywordp (first m)))
    (make-instance 'face :fg (getf m :fg) :bg (getf m :bg)
                         :bold (getf m :bold) :italic (getf m :italic)
                         :underline (getf m :underline))))

(defun %themed (name)
  "The active theme's face called NAME as a plist, or nothing."
  (let ((key (find-symbol (string-upcase name) :keyword)))
    (and key (getf (faces (theme (active))) key))))

(defclass face-node (fs:value)
  ((written :initform nil :accessor written))
  (:documentation "One face in force at /ui/face/<name>: the active theme's until
something is written here, and what was written after. Saved only once written."))

(defmethod fs:savedp ((n face-node)) (written n))

(defmethod (setf fs:contents) :after (value (n face-node))
  (declare (ignore value))
  (setf (written n) t))

(defmethod fs:contents ((n face-node))
  (if (written n)
      (call-next-method)
      (%themed (fs:name n))))

(defclass face-dir (fs:mount)
  ((in-force :accessor in-force-of))
  (:documentation "Every face in force, one entry each."))

(defun %face-node (d name)
  (fs:child d name (lambda () (make-instance 'face-node :name name :parent d))))

(defun %in-force (&optional d)
  "Face name to face: what is at D, or the active theme's where there is no D."
  (let ((out (make-hash-table :test 'eq)))
    (if d
        (dolist (n (fs:entries d))
          (let ((f (%as-face (fs:contents n))))
            (when f (setf (gethash (%as-keyword (fs:name n)) out) f))))
        (loop :for (k plist) :on (faces (theme (active))) :by #'cddr
              :do (setf (gethash k out) (%as-face plist))))
    out))

(defclass in-force (fs:derived) ()
  (:documentation "Every face in force, by name, worked out once and kept until a
face or the theme moves."))

(defmethod fs:works ((n in-force)) (%in-force (fs:of n)))

(defmethod initialize-instance :after ((d face-dir) &key)
  (setf (in-force-of d)
        (make-instance 'in-force :name "in force" :parent d :of d)))

(defmethod fs:entries ((d face-dir))
  (let ((had (call-next-method)))
    (append had
            (loop :for (key) :on (faces (theme (active))) :by #'cddr
                  :for name := (string-downcase (symbol-name key))
                  :unless (find name had :key #'fs:name :test #'equal)
                    :collect (%face-node d name)))))

(defmethod fs:entry ((d face-dir) name)
  (or (call-next-method)
      (let ((name (princ-to-string name)))
        (and (%themed name) (%face-node d name)))))

(defun faces-in-force ()
  "Face name to face, worked out once and kept until what it read moves: NAMED is
on the path every painted cell takes."
  (or *in-force*
      (let ((d (fs:at "/ui/face")))
        (if d (fs:contents (in-force-of d)) (%in-force)))))

(defmacro with-faces (&body body)
  "Run BODY with the faces in force worked out once."
  `(let ((*in-force* (faces-in-force))) ,@body))

(defun in-force (name)
  "The face called NAME, as the render running here sees it. The class and the
look-up are one word because they are one idea."
  (gethash name (faces-in-force)))

(defun attrs (f)
  "bit 0 bold, bit 1 italic, bit 2 underline."
  (if f
      (logior (if (bold f) 1 0) (if (italic f) 2 0) (if (underline f) 4 0))
      0))

(defun color (role)
  "The hex of a palette ROLE in the active theme."
  (or (%role (palette (theme (active))) role)
      (error "the active theme has no color ~s" role)))

(defun metric (key &optional default)
  (multiple-value-bind (v found) (%role (metrics (theme (active))) key)
    (if found v default)))

(defun unhex (h)
  "A #rrggbb string as (r g b), or nothing for anything else. A face's FG and BG are
hex; this is how a canvas reads one."
  (when (and (stringp h) (>= (length h) 7) (char= (char h 0) #\#))
    (list (parse-integer h :start 1 :end 3 :radix 16)
          (parse-integer h :start 3 :end 5 :radix 16)
          (parse-integer h :start 5 :end 7 :radix 16))))

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
    (:ring-track     :fg bg-active)))

