(in-package :pine.test)

(def-suite :pine.deps :in :pine)
(in-suite :pine.deps)

;;;; The package graph, read off the source rather than the manifest. A file
;;;; may only name packages that load before it, so the graph must be acyclic;
;;;; nothing else enforces that, and a cycle is invisible while every package
;;;; is declared up front.

(defun %source-files (system)
  "Every source file of SYSTEM, in load order. Nil when the system is absent."
  (let ((sys (asdf:find-system system nil)))
    (when sys
      (labels ((walk (c)
                 (cond ((typep c 'asdf:cl-source-file)
                        (list (asdf:component-pathname c)))
                       ((typep c 'asdf:parent-component)
                        (loop :for child :in (asdf:component-children c)
                              :append (walk child))))))
        (walk sys)))))

(defun %code-text (path)
  "PATH's text with comments, block comments and string literals blanked out,
so a scan over it sees code and nothing else."
  (let* ((raw (uiop:read-file-string path))
         (n (length raw))
         (out (make-string n :initial-element #\Space))
         (i 0))
    (flet ((at (k) (and (< k n) (char raw k))))
      (loop :while (< i n)
            :for ch = (char raw i)
            :do (cond
                  ((char= ch #\;)
                   (loop :while (and (< i n) (char/= (char raw i) #\Newline))
                         :do (incf i)))
                  ((and (char= ch #\#) (eql (at (1+ i)) #\|))
                   (let ((depth 1))
                     (incf i 2)
                     (loop :while (and (< i n) (plusp depth))
                           :do (cond ((and (char= (char raw i) #\|) (eql (at (1+ i)) #\#))
                                      (decf depth) (incf i 2))
                                     ((and (char= (char raw i) #\#) (eql (at (1+ i)) #\|))
                                      (incf depth) (incf i 2))
                                     (t (incf i))))))
                  ((char= ch #\")
                   (incf i)
                   (loop :while (< i n)
                         :do (cond ((char= (char raw i) #\\) (incf i 2))
                                   ((char= (char raw i) #\") (incf i) (loop-finish))
                                   (t (incf i)))))
                  ((char= ch #\\)
                   (setf (char out i) ch)
                   (when (at (1+ i)) (setf (char out (1+ i)) (char raw (1+ i))))
                   (incf i 2))
                  (t (setf (char out i) ch) (incf i)))))
    out))

(defun %name-char-p (ch)
  (or (alphanumericp ch) (find ch ".-/")))

(defun %name-at (text start)
  "The package-name run of TEXT at START, and the index just past it."
  (let ((j start) (n (length text)))
    (loop :while (and (< j n) (%name-char-p (char text j)))
          :do (incf j))
    (values (string-downcase (subseq text start j)) j)))

(defun %file-package (text)
  "The package TEXT's IN-PACKAGE names, downcased."
  (let ((p (search "(in-package" text :test #'char-equal)))
    (when p
      (let ((i (+ p (length "(in-package"))) (n (length text)))
        (loop :while (and (< i n) (find (char text i) " #:"))
              :do (incf i))
        (%name-at text i)))))

(defun %referenced-packages (text)
  "Every pine package TEXT names with a package marker, downcased."
  (let ((acc '()) (i 0) (n (length text)))
    (loop :for p = (search "pine." text :start2 i :test #'char-equal)
          :while p
          :do (multiple-value-bind (name j) (%name-at text p)
                (when (and (or (zerop p) (not (%name-char-p (char text (1- p)))))
                           (< j n)
                           (char= (char text j) #\:))
                  (pushnew name acc :test #'string=))
                (setf i (max (1+ p) j))))
    acc))

(defparameter +graphed-systems+
  '("pine" "pine/vt" "pine/cairo" "pine/wayland")
  "The systems whose sources make up the graph. Their files are read from disk,
so a system that is not loaded is still checked.")

(defun package-graph ()
  "PACKAGE -> the packages its files name, over every graphed system."
  (let ((graph (make-hash-table :test 'equal)))
    (dolist (system +graphed-systems+ graph)
      (dolist (path (%source-files system))
        (let* ((text (%code-text path))
               (home (%file-package text)))
          (when home
            (dolist (dep (%referenced-packages text))
              (unless (string= dep home)
                (pushnew dep (gethash home graph) :test #'string=)))))))))

(defun graph-cycles (graph)
  "Every strongly connected component of GRAPH holding more than one package."
  (let ((index (make-hash-table :test 'equal))
        (low (make-hash-table :test 'equal))
        (on-stack (make-hash-table :test 'equal))
        (stack '()) (counter 0) (cycles '()))
    (labels ((visit (v)
               (setf (gethash v index) counter
                     (gethash v low) counter)
               (incf counter)
               (push v stack)
               (setf (gethash v on-stack) t)
               (dolist (w (gethash v graph))
                 (cond ((not (gethash w index))
                        (visit w)
                        (setf (gethash v low) (min (gethash v low) (gethash w low))))
                       ((gethash w on-stack)
                        (setf (gethash v low) (min (gethash v low) (gethash w index))))))
               (when (= (gethash v low) (gethash v index))
                 (let ((component '()))
                   (loop :for w = (pop stack)
                         :do (remhash w on-stack)
                             (push w component)
                         :until (string= w v))
                   (when (rest component)
                     (push (sort component #'string<) cycles))))))
      (loop :for v :being :the :hash-keys :of graph
            :unless (gethash v index) :do (visit v))
      cycles)))

(defun cycle-report (cycles)
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (c)
                    (format nil "~d packages: ~{~a~^ ~}" (length c) c))
                  cycles)))

(test package-graph-is-acyclic
  "No package may name one that names it back, directly or through others.
A file can only use what loads before it, and every package declaring itself
is what makes that checkable."
  (let ((cycles (graph-cycles (package-graph))))
    (is (null cycles) "~a" (cycle-report cycles))))
