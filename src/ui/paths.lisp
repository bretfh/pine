(defpackage #:pine.ui.paths
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:face #:pine.ui.face)
                    (#:css #:pine.ui.css))
  (:export))

(in-package #:pine.ui.paths)
(named-readtables:in-readtable pine.path:syntax)

;;;; Style, as paths.
;;;;
;;;;   (write /theme :ef-dream)               the theme everything resolves in
;;;;   (read  /theme/*)                       the themes there are
;;;;   (read  /theme/ef-dream/palette/accent) one colour of one theme
;;;;   (read  /face/keyword)                  {:fg .. :bg .. :bold ..}
;;;;   (write /face/keyword {:fg "#ff0000"})  restyle one face
;;;;   (write /style/editor-view {:opacity 0.9})
;;;;
;;;; A theme is data and loading one is a write, so restyling is the same act
;;;; as anything else and a surface that read a colour is built again when the
;;;; theme moves.

(defun %themes ()
  (sort (mapcar (lambda (name) (string-downcase (symbol-name name)))
                (pine.data:keys (pine.data:all face:*themes*)))
        #'string<))

(defun %theme-of (name)
  "The theme NAME names, or NIL. A theme that is not there is nothing rather
than an error, the way a path that holds nothing is: reading the palette of a
theme is also how the write that makes one asks what was there before."
  (pine.data:at face:*themes* (face:theme-key name)))

(defun %face-map (f)
  (when f
    (fset:map (:fg (face:fg f)) (:bg (face:bg f))
              (:bold (and (face:bold f) t))
              (:italic (and (face:italic f) t))
              (:underline (and (face:underline f) t)))))

(defun %faces ()
  (sort (mapcar (lambda (name) (string-downcase (symbol-name name)))
                (pine.data:keys (face:faces)))
        #'string<))

(defun %storage-key (name)
  "NAME as the key the tree stores a segment under."
  (pine.path:key (pine.path:name name)))

(defun %keyword (segment)
  (intern (string-upcase segment) :keyword))

(defun %alist (map)
  "A map of role to value as the alist a theme holds."
  (let ((out nil))
    (when (fset:map? map)
      (fset:do-map (key value map)
        (push (cons (%keyword (pine.path:name key)) value) out)))
    (nreverse out)))

(defun %theme-put (name which map)
  "Define or override the theme NAME's colours or its metrics.

Writing a palette is how a config makes a theme: there is no deftheme, and a
theme that was not there is one that is now. The roles written win over the ones
that were there, and moving a colour of the active theme restyles at once because
everything that read one is computed again.

A theme is its palette and its metrics. Its faces are their own paths, at
/face/?name, so a new theme starts from the faces in use rather than from none."
  (let* ((key (face:theme-key (%keyword name)))
         (old (pine.data:at face:*themes* key))
         (was-palette (and old (face:theme-palette old)))
         (was-metrics (and old (face:theme-metrics old))))
    (face:register-theme
     (make-instance 'face:theme
                    :name key
                    :palette (if (eq which :palette)
                                 (append (%alist map) was-palette)
                                 was-palette)
                    :metrics (if (eq which :metrics)
                                 (append (%alist map) was-metrics)
                                 was-metrics)
                    ;; a theme nobody has defined the faces of starts from the
                    ;; ones in use, so writing a palette gives a theme that
                    ;; renders. Faces are their own paths: /face/?name
                    :faces (face:theme-faces
                            (or old (face:find-theme (face:active))))))
    map))

(defun theme ()
  (ns:provider
   (/theme/?name/palette/?role
    {:read (pine.data:fn []
             (let ((it (%theme-of name)))
               (when it (cdr (assoc (%keyword role) (face:theme-palette it)
                                    :test #'string=)))))
     :doc "one colour of one theme"})
   (/theme/?name/palette
    {:read (pine.data:fn []
             (let ((it (%theme-of name)))
               (when it (pine.data:cells (face:theme-palette it)
                                         :key #'%storage-key))))
     :write (pine.data:fn [v] (%theme-put name :palette v))
     :doc "a theme's colours; write a map to define or override them"})
   (/theme/?name/metrics/?key
    {:read (pine.data:fn []
             (let ((it (%theme-of name)))
               (when it (cdr (assoc (%keyword key) (face:theme-metrics it)
                                    :test #'string=)))))
     :doc "one metric of one theme"})
   (/theme/?name/metrics
    {:read (pine.data:fn []
             (let ((it (%theme-of name)))
               (when it (pine.data:cells (face:theme-metrics it)
                                         :key #'%storage-key))))
     :write (pine.data:fn [v] (%theme-put name :metrics v))
     :doc "a theme's radius, border, opacity and font; write a map to set them"})
   (/theme/?name
    {:read (pine.data:fn [] (and (%theme-of name) (%keyword name)))
     :doc "a theme there is"})
   ;; the theme in force is a value, so the tree holds it: :IN says what a
   ;; written name becomes and the clause says nothing about reading, which is
   ;; what leaves it a place. Everything that resolves against it reads it, so
   ;; writing another restyles at once.
   (/theme
    ;; nil takes a path away, so it stays nil: a theme named NIL is what
    ;; normalizing it into a key would make of clearing it
    {:in (pine.data:fn [v] (and v (face:theme-key v)))
     :ls (pine.data:fn [] (%themes))
     :doc "the theme everything resolves in; write another to load it"})))

(defun faces ()
  (ns:provider
   ;; a face reads as the one in force -- the theme's, or the one written over
   ;; it -- and a write lands in the tree, which is what makes restyling one an
   ;; ordinary write that is undone and stored like any other
   (/face/?name
    {:read (pine.data:fn [] (%face-map (face:find-face (%keyword name))))
     :in (pine.data:fn [v] (and (fset:map? v) v))
     :doc "{:fg .. :bg .. :bold .. :italic .. :underline ..}"})
   (/face
    {:ls (pine.data:fn [] (%faces))
     :doc "every face the active theme has"})))

(defun style ()
  (ns:provider
   ;; a rule is a place: the tree holds it, so it is undone, watched and stored
   ;; like anything else, and what compiled the stylesheet notices it moved the
   ;; same way everything else notices
   (/style/?class
    {:doc "one class's style rule, as {:prop value}"})
   (/style
    {:doc "what a config styled, which wins the cascade"})))

(ns:serve :theme
  {:at [/theme /face /style]
   :doc "style, as paths: the theme everything resolves in, the faces it has,
and what a config styled"
   ;; what it keeps is the cells for what is worked out from the three.
   ;; Resolving the faces and compiling the stylesheet is what every painted
   ;; cell and every styled node asks for, and both are built from paths. They
   ;; are worked out once and kept until the tree moves; the cells are made
   ;; here because making one is a write, and a paint may not write.
   :up (lambda ()
         (ns:write /theme (theme))
         (ns:write /face (faces))
         (ns:write /style (style))
         ;; the theme pine ships, unless this space already said otherwise
         (unless (ns:read /theme) (ns:write /theme face:+default-theme+))
         ;; a config styles a selector by writing its path, so this is where a
         ;; frontend image that paints in pixels hears about it. Nothing a
         ;; config calls: the write is the whole of it.
         (ns:watch /style/*
                   (pine.data:fn [v] (declare (ignore v)) (css:broadcast) {})
                   :as :style-broadcast)
         {:faces (sento.atomic:make-atomic-reference :value nil)
          :stylesheet (sento.atomic:make-atomic-reference :value nil)})})
