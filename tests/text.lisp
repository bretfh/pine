(in-package :pine/test)

(def-suite* :pine/text :in :pine)

(test lines-are-immutable-with-sharing
  (let* ((had (text:of (format nil "one~%two")))
         (now (nth-value 0 (text:inserted had 0 3 "!"))))
    (is (equal "one" (text:line had 0)))
    (is (equal "one!" (text:line now 0)))
    (is (= 2 (text:line-count now)))))

(test a-region-of-lines-is-the-text-it-covers
  (let ((had (text:of (format nil "one~%two~%three"))))
    (is (equal "ne" (text:region had 0 1 0 3)))
    (is (equal (format nil "wo~%th") (text:region had 1 1 2 2)))))

(test moving-by-word-and-by-line
  (let ((had (text:of (format nil "one two~%three"))))
    (is (equal '(0 3) (multiple-value-list (text:move-by :word had 0 0 1))))
    (is (equal '(1 0) (multiple-value-list (text:move-by :line had 0 0 1))))
    (is (equal '(1 5) (multiple-value-list (text:move-by :text had 0 0 1))))))

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
    (let ((doc (text:make-document "probe")))
      (setf (node:contents doc) (format nil "one~%two"))
      (is (equal (format nil "one~%two") (text:text doc)))
      (is (= 2 (text:line-count doc)))
      (text:goto doc 1 1)
      (text:insert doc "X")
      (is (equal "tXwo" (text:line doc 1)))
      (text:undo doc)
      (is (equal "two" (text:line doc 1))))))

(test the-structure-a-mode-gives-text-is-in-the-namespace
  (with-tree
    (let ((doc (text:make-document "probe" :mode (make-instance 'mode:lisp))))
      (setf (node:contents doc)
            (format nil "(defun hello () 1)~%(defun goodbye () 2)"))
      (text:restructure doc)
      (let ((form (tree:at doc "defun/hello")))
        (is (not (null form)) "a form is a place under the document")
        (is (search "defun hello" (node:contents form)))
        (setf (node:contents form) "(defun hello () 3)")
        (is (search "3" (text:text doc)) "writing a region replaces that span")
        (is (search "goodbye" (text:text doc)) "and leaves the rest alone")))))

(test a-region-keeps-its-identity-across-a-restructure
  (with-tree
    (let ((doc (text:make-document "probe" :mode (make-instance 'mode:lisp))))
      (setf (node:contents doc) "(defun hello () 1)")
      (text:restructure doc)
      (let ((was (tree:at doc "defun/hello")))
        (text:restructure doc)
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
             (let ((doc (text:make-document "visit")))
               (text:visit doc (namestring file))
               (is (equal "(defun probe () 1)" (text:text doc)))
               (is (typep (text:mode-of doc) 'mode:lisp)
                   "the mode comes from what it claims")
               (setf (node:contents doc) "(defun probe () 2)")
               (text:save doc)
               (is (equal "(defun probe () 2)" (uiop:read-file-string file)))))
        (ignore-errors (delete-file file))))))

(test a-region-still-covers-its-own-span-after-an-edit
  "Regions are worked out from the text. One worked out before an edit covers the
wrong stretch, so writing it replaces something it was never standing for."
  (editing)
  (let ((doc (text:current)))
    (setf (node:contents doc) (format nil "(defun a () 1)~%(defun b () 2)"))
    (is (equal "(defun a () 1)"
               (node:contents (tree:at doc "defun/a"))))
    (text:goto doc 0 0)
    (setf (node:contents (tree:at nil "key")) "C-e")
    (setf (node:contents (tree:at doc "defun/b")) "(defun b () 99)")
    (is (equal (format nil "(defun a () 1)~%(defun b () 99)") (text:text doc)))))

(test the-structure-command-answers-what-the-mode-made
  (editing)
  (let ((doc (text:current)))
    (setf (node:contents doc) (format nil "(defun a () 1)~%(defun b () 2)"))
    (is (equal '("defun") (command:run "structure")))))

(test two-regions-with-one-name-are-two-places
  "A region is kept under the name its mode gave it, so two functions of one name,
or two headings of one text, were one node covering the last of them -- and writing
it replaced text it was never standing for."
  (with-tree
    (let ((doc (text:make-document "test-regions")))
      (setf (node:contents doc) "one
two
three
four")
      (pine/text::%build doc '(("dup" (0 . 0) (0 . 3))
                               ("dup" (2 . 0) (2 . 5))
                               ("other" (1 . 0) (1 . 3))))
      (let ((names (sort (mapcar #'node:name
                                 (remove-if-not
                                  (lambda (n) (typep n 'pine/text::region))
                                  (d:as :list (pine/fs/node::beneath doc))))
                         #'string<)))
        (is (equal '("dup" "dup<2>" "other") names))))))

(test a-region-that-is-still-there-is-the-node-it-was
  "Built again, a region keeps its identity, so a watcher on one goes on watching."
  (with-tree
    (let ((doc (text:make-document "test-region-identity")))
      (setf (node:contents doc) "one two three")
      (pine/text::%build doc '(("only" (0 . 0) (0 . 3))))
      (let ((first-time (d:lookup (d:all (node:memo doc)) "only")))
        (pine/text::%build doc '(("only" (0 . 4) (0 . 7))))
        (let ((now (d:lookup (d:all (node:memo doc)) "only")))
          (is (eq first-time now) "the same node")
          (is (equal '((0 . 4) (0 . 7)) (pine/text::covers now))
              "standing over where it stands now"))))))

(test word-motion-crosses-a-line-the-way-character-motion-does
  "Stepping a word stayed on the line it started on, so M-f at the end of one and
M-b at the start of one were motions that did nothing however often they were asked
for."
  (let ((lines (text:of "alpha beta
gamma delta")))
    (is (equal '(1 0) (multiple-value-list (text:move-by :char lines 0 10 1))))
    (is (equal '(1 5) (multiple-value-list (text:move-by :word lines 0 10 1)))
        "forward off the end of a line lands in the next word")
    (is (equal '(0 10) (multiple-value-list (text:move-by :char lines 1 0 -1))))
    (is (equal '(0 6) (multiple-value-list (text:move-by :word lines 1 0 -1)))
        "and back off the front lands in the last word above")))

(test word-motion-still-walks-a-line-a-word-at-a-time
  (let ((lines (text:of "alpha beta gamma")))
    (is (equal '(0 5) (multiple-value-list (text:move-by :word lines 0 0 1))))
    (is (equal '(0 10) (multiple-value-list (text:move-by :word lines 0 0 2))))
    (is (equal '(0 6) (multiple-value-list (text:move-by :word lines 0 10 -1))))))
