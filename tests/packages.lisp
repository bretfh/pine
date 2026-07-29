(in-package :pine.test)

(def-suite* :pine.packages :in :pine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(defparameter +generated-packages+ '("PINE.WAYLAND.PROTOCOL")
  "Packages whose contents a generator interns and exports: wayflan's scanner
reads the compositor's XML with :export t, so their export list is not written
here and is not this suite's to judge.")

(defun pine-packages ()
  (sort (remove-if (lambda (p) (member (package-name p) +generated-packages+
                                       :test #'string=))
                   (remove-if-not (lambda (p)
                                    (pine.provider.out:starts-with (package-name p) "PINE"))
                                  (list-all-packages)))
        #'string< :key #'package-name))

(defun own-symbols (package)
  (let ((acc nil))
    (do-symbols (s package acc)
      (when (eq (symbol-package s) package) (push s acc)))))

(defun external-symbols (package)
  (let ((acc nil))
    (do-external-symbols (s package acc) (push s acc))))

(defun names-something-p (symbol)
  (or (fboundp symbol)
      (boundp symbol)
      (macro-function symbol)
      (special-operator-p symbol)
      (find-class symbol nil)
      (fboundp (list 'setf symbol))
      (sb-int:info :type :kind symbol)
      (named-readtables:find-readtable symbol)))

;;;; "A file may only name packages that load before it." Read off the image's
;;;; cross-reference tables and the systems' own component order, so a name
;;;; reached through :use counts the same as one written pkg:sym, and a method
;;;; is credited to the file that defines it rather than to the package that
;;;; owns its generic function.

(defparameter +graphed-systems+ '("pine/vt" "pine")
  "The systems whose files make up the load order, in dependency order.")

(defun load-order ()
  "A vector of every graphed source pathname, in the order the systems load."
  (let ((files nil))
    (dolist (system +graphed-systems+ (coerce (nreverse files) 'vector))
      (labels ((walk (component)
                 (cond ((typep component 'asdf:cl-source-file)
                        (let ((path (probe-file (asdf:component-pathname component))))
                          (when path (push path files))))
                       ((typep component 'asdf:parent-component)
                        (mapc #'walk (asdf:component-children component))))))
        (let ((system (asdf:find-system system nil)))
          (when system (walk system)))))))

(defun file-positions (order)
  (let ((table (make-hash-table :test 'equal)))
    (loop :for path :across order
          :for i :from 0
          :do (setf (gethash (namestring path) table) i))
    table))

(defparameter +definition-kinds+
  '(:function :macro :generic-function :variable :constant :class :type
    :structure :condition)
  "The kinds a pine symbol can be defined as, tried in this order.")

(defun definition-file (symbol)
  (loop :for kind :in +definition-kinds+
        :thereis (loop :for source :in (sb-introspect:find-definition-sources-by-name
                                        symbol kind)
                       :for path = (sb-introspect:definition-source-pathname source)
                       :when path :return (namestring path))))

(defun reference-sites (symbol)
  "The files whose code calls or reads SYMBOL."
  (let ((acc nil))
    (dolist (entry (append (sb-introspect:who-calls symbol)
                           (sb-introspect:who-references symbol))
                   acc)
      (let ((path (sb-introspect:definition-source-pathname (cdr entry))))
        (when path (pushnew (namestring path) acc :test #'string=))))))

(defun forward-references ()
  "Every place a graphed file names something a later graphed file defines."
  (let ((positions (file-positions (load-order)))
        (violations nil))
    (dolist (package (pine-packages) violations)
      (dolist (symbol (own-symbols package))
        (let* ((home (definition-file symbol))
               (defined-at (and home (gethash home positions))))
          (when defined-at
            (dolist (site (reference-sites symbol))
              (let ((used-at (gethash site positions)))
                (when (and used-at (< used-at defined-at))
                  (pushnew (list (file-namestring site) symbol
                                 (file-namestring home))
                           violations :test #'equal))))))))))

(test a-file-only-names-what-loads-before-it
  (let ((violations (forward-references)))
    (is (null violations)
        "~{~%  ~a names ~s, defined later in ~a~}"
        (loop :for (site symbol home) :in violations
              :append (list site symbol home)))))

(test every-exported-symbol-names-something
  (dolist (package (pine-packages))
    (let ((empty (sort (remove-if #'names-something-p (external-symbols package))
                       #'string< :key #'symbol-name)))
      (is (null empty)
          "~a exports ~d symbol~:p that name nothing: ~{~a~^ ~}"
          (package-name package) (length empty) empty))))

(test no-package-exports-an-internal
  "A name spelled %foo is internal by convention here, so exporting one says
the interface and the convention disagree."
  (dolist (package (pine-packages))
    (let ((internal (sort (remove-if-not
                           (lambda (s) (pine.provider.out:starts-with (symbol-name s) "%"))
                           (external-symbols package))
                          #'string< :key #'symbol-name)))
      (is (null internal)
          "~a exports ~d internal name~:p: ~{~a~^ ~}"
          (package-name package) (length internal) internal))))

(test no-package-exports-a-symbol-it-does-not-own-or-inherit
  (dolist (package (pine-packages))
    (let ((foreign nil))
      (dolist (symbol (external-symbols package))
        (unless (or (eq (symbol-package symbol) package)
                    (member (symbol-package symbol) (package-use-list package))
                    (nth-value 1 (find-symbol (symbol-name symbol) package)))
          (push symbol foreign)))
      (is (null foreign)
          "~a exports symbols from elsewhere: ~{~a~^ ~}"
          (package-name package) foreign))))

(test the-user-language-vocabulary-is-bound
  (dolist (symbol (external-symbols (find-package :pine.user)))
    (is (names-something-p symbol)
        "pine.user:~a is exported but names nothing" (symbol-name symbol))))

(test every-file-a-system-names-is-on-disk
  (let ((order (load-order)))
    (is (plusp (length order)))
    (loop :for path :across order
          :do (is (probe-file path) "~a is named by a system but missing" path))))

;;;; The tree against the doc
;;;;
;;;; doc/api.org says what a buffer is. A leaf the code keeps under one and
;;;; the doc does not name is drift, and drift is how an API becomes a port of
;;;; whatever was there before.

(defun %doc-text ()
  (uiop:read-file-string
   (merge-pathnames "doc/api.org" (asdf:system-source-directory :pine))))

(defun %doc-paths (under)
  "Every path the doc names under UNDER, as text. A path is written =/like/this=."
  (let ((text (%doc-text))
        (found nil)
        (i 0))
    (loop :for start = (search (concatenate 'string "=" under) text :start2 i)
          :while start
          :for end = (position #\= text :start (1+ start))
          :while end
          :do (push (subseq text (1+ start) end) found)
              (setf i (1+ end)))
    (remove-duplicates (nreverse found) :test #'string=)))

(defun %doc-buffer-leaves ()
  "The leaf of every /buf/?name/... path the doc's table names."
  (let ((prefix "/buf/?name/"))
    (remove-duplicates
     (loop :for path :in (%doc-paths "/buf/")
           :when (and (> (length path) (length prefix))
                      (string= prefix path :end2 (length prefix)))
             :collect (let ((rest (subseq path (length prefix))))
                        (subseq rest 0 (or (position #\/ rest) (length rest)))))
     :test #'string=)))

(defparameter +leaves-with-another-home-coming+ '("tick")
  "Leaves still under a buffer that the doc does not name. The tick belongs to
what the buffer and its parser say to each other and leaves with the buffer
actor; nothing else is allowed to join it.")

(test the-doc-names-a-buffers-leaves
  (let ((named (%doc-buffer-leaves)))
    (dolist (leaf '("text" "line" "face" "point" "mark" "file" "mode" "minor"
                    "modified" "tree" "view"))
      (is (member leaf named :test #'string=)
          "the doc no longer names /buf/?name/~a, but the code serves it" leaf))))

(test a-buffer-carries-only-the-leaves-the-doc-names
  (pine.ns:with-space ()
    (pine.buf:mount)
    (pine.ns:write (pine.buf:at "probe" :text) "hello")
    (pine.ns:write (pine.buf:at "probe" :text) (fset:seq :insert "!"))
    (pine.ns:write (pine.buf:at "probe" :mode) :text)
    (let* ((named (append (%doc-buffer-leaves) +leaves-with-another-home-coming+))
           (leaves (let ((acc nil))
                     (fset:do-map (key value (pine.ns:read (pine.buf:at "probe")))
                       (declare (ignore value))
                       (push (pine.path:name key) acc))
                     acc))
           (extra (remove-if (lambda (leaf) (member leaf named :test #'string=))
                             leaves)))
      (is (null extra)
          "a buffer carries leaves the doc does not name: ~{~a~^ ~}" extra))))
