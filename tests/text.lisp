(in-package :pine/test)

(def-suite* :pine/text :in :pine)

(test lines-are-immutable-with-sharing
  (let* ((had (lines:of (format nil "one~%two")))
         (now (nth-value 0 (lines:insert had 0 3 "!"))))
    (is (equal "one" (lines:line had 0)))
    (is (equal "one!" (lines:line now 0)))
    (is (= 2 (lines:line-count now)))))

(test a-region-of-lines-is-the-text-it-covers
  (let ((had (lines:of (format nil "one~%two~%three"))))
    (is (equal "ne" (lines:region had 0 1 0 3)))
    (is (equal (format nil "wo~%th") (lines:region had 1 1 2 2)))))

(test moving-by-word-and-by-line
  (let ((had (lines:of (format nil "one two~%three"))))
    (is (equal '(0 3) (multiple-value-list (lines:move-by :word had 0 0 1))))
    (is (equal '(1 0) (multiple-value-list (lines:move-by :line had 0 0 1))))
    (is (equal '(1 5) (multiple-value-list (lines:move-by :text had 0 0 1))))))

(test a-mode-is-a-class-and-inheritance-is-the-fallback
  (let ((m (make-instance 'mode:pine)))
    (is (eql 2 (mode:setting m :indent)) "from code")
    (is (eql 8 (mode:setting m :tab-width)) "from text")
    (is (eq :pine (mode:setting m :grammar)) "its own")
    (is (equal ";" (mode:setting m :comment)))))

(test a-mode-claims-the-paths-it-is-for
  (is (typep (mode:mode-for "/tmp/thing.lisp") 'mode:lisp))
  (is (typep (mode:mode-for "/tmp/thing.scm") 'mode:scheme))
  (is (typep (mode:mode-for "/tmp/notes.org") 'mode:org))
  (is (null (mode:mode-for "/tmp/thing.unknown"))))

(test a-chord-is-inherited-the-way-a-method-is
  (command:defcommand "probe-nothing" () (:describes "nothing") nil)
  (unwind-protect
       (progn
         (mode:bind 'mode:text "C-probe" "probe-nothing")
         (is (command:named "probe-nothing"))
         (is (eq (command:named "probe-nothing")
                 (mode:binding (make-instance 'mode:lisp) "C-probe"))
             "a lisp document inherits what text binds"))
    (command:forget "probe-nothing")))

(test a-document-holds-text-and-a-point
  (with-tree
    (let ((doc (doc:make-document "probe")))
      (setf (node:contents doc) (format nil "one~%two"))
      (is (equal (format nil "one~%two") (doc:text doc)))
      (is (= 2 (doc:line-count doc)))
      (doc:goto doc 1 1)
      (doc:insert doc "X")
      (is (equal "tXwo" (doc:line doc 1)))
      (doc:undo doc)
      (is (equal "two" (doc:line doc 1))))))

(test the-structure-a-mode-gives-text-is-in-the-namespace
  (with-tree
    (let ((doc (doc:make-document "probe" :mode (make-instance 'mode:lisp))))
      (setf (node:contents doc)
            (format nil "(defun hello () 1)~%(defun goodbye () 2)"))
      (doc:restructure doc)
      (let ((form (tree:at doc "defun/hello")))
        (is (not (null form)) "a form is a place under the document")
        (is (search "defun hello" (node:contents form)))
        (setf (node:contents form) "(defun hello () 3)")
        (is (search "3" (doc:text doc)) "writing a region replaces that span")
        (is (search "goodbye" (doc:text doc)) "and leaves the rest alone")))))

(test a-region-keeps-its-identity-across-a-restructure
  (with-tree
    (let ((doc (doc:make-document "probe" :mode (make-instance 'mode:lisp))))
      (setf (node:contents doc) "(defun hello () 1)")
      (doc:restructure doc)
      (let ((was (tree:at doc "defun/hello")))
        (doc:restructure doc)
        (is (eq was (tree:at doc "defun/hello"))
            "a watcher on it goes on watching")))))

(test visiting-a-file-opens-it-and-saving-writes-it-back
  (booted)
  (with-tree
    (let ((file (merge-pathnames "pine-test-visit.lisp"
                                 (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (with-open-file (o file :direction :output :if-exists :supersede)
               (write-string "(defun probe () 1)" o))
             (mount:mount #p"/" (tree:root) "file")
             (let ((doc (doc:make-document "visit")))
               (text:visit doc (namestring file))
               (is (equal "(defun probe () 1)" (doc:text doc)))
               (is (typep (doc:mode-of doc) 'mode:lisp)
                   "the mode comes from what it claims")
               (setf (node:contents doc) "(defun probe () 2)")
               (text:save doc)
               (is (equal "(defun probe () 2)" (uiop:read-file-string file)))))
        (ignore-errors (delete-file file))))))
