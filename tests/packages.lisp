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
                                    (pine.source:starts-with (package-name p) "PINE"))
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
      (sb-int:info :type :kind symbol)))

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
                           (lambda (s) (pine.source:starts-with (symbol-name s) "%"))
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
