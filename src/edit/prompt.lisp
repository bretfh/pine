(in-package #:pine/edit)

(defvar *prompt* nil)
(defvar *sources* (d:table))
(defvar *shown* 12)
(defvar *history-kept* 200)
(defparameter +document+ "*prompt*")

(defclass standing ()
  ((question   :initarg :question   :reader question)
   (was        :initarg :was        :reader was        :initform nil)
   (then       :initarg :then       :reader then       :initform nil)
   (category   :initarg :category   :reader category   :initform nil)
   (given      :initarg :candidates :reader given      :initform nil)
   (must-match :initarg :must-match :reader must-match :initform nil)
   (history    :initarg :history    :reader history    :initform nil)
   (walking    :initform nil        :accessor walking)
   (walked     :initform nil        :accessor walked)
   (seen       :initform nil        :accessor seen))
  (:documentation "A question standing, and what will be done with the answer."))

(defmethod print-object ((p standing) stream)
  (print-unreadable-object (p stream :type t)
    (write-string (question p) stream)))

(defun asking () *prompt*)

(defun askingp () (and *prompt* t))

(defun answering ()
  (or (text:named +document+)
      (text:make-document +document+ :mode (make-instance 'prompt))))

(defun %under () (tree:ensure "/prompt"))

(defun %place (name builder)
  (let ((under (%under)))
    (or (node:resolve under name)
        (node:attach (funcall builder) under))))

(defun %question-node () (%place "question" (lambda () (node:make "question"))))

(defun %chose-node () (%place "chose" (lambda () (node:make "chose"))))

(defun %typed ()
  (let ((d (text:named +document+)))
    (if d (node:contents d) "")))

(defun %said-node ()
  (%place "said" (lambda () (node:derive "said" #'%typed))))

(defun %matching-node ()
  (%place "matching"
          (lambda ()
            (node:derive "matching"
                         (lambda ()
                           (let ((p *prompt*))
                             (when p
                               (let ((text (node:contents (%said-node))))
                                 (if (filep p)
                                     (candidates p text)
                                     (matches text (candidates p text)))))))))))

(defun so-far ()
  "What has been typed into the prompt so far."
  (if (tree:root) (node:contents (%said-node)) (%typed)))

(defun asked ()
  (and (tree:root) (node:contents (%question-node))))

(defun source (category function)
  (d:keep! *sources* category function)
  category)

(defun sources () (d:keys (d:all *sources*)))

(defun candidates (&optional (p *prompt*) (text (so-far)))
  (when p
    (or (given p)
        (let ((fn (d:lookup (d:all *sources*) (category p))))
          (when fn (fault:attempt (lambda () (funcall fn text))
                                  "the candidates"))))))

(defun %sync (p)
  (let ((text (so-far)))
    (unless (equal text (seen p))
      (setf (seen p) text)
      (setf (node:contents (%chose-node)) 0)))
  p)

(defun chosen (&optional (p *prompt*))
  (when p
    (%sync p)
    (or (node:contents (%chose-node)) 0)))

(defun (setf chosen) (value p)
  (%sync p)
  (setf (node:contents (%chose-node)) value))

(defun matching (&optional (p *prompt*))
  "What answers the question as it stands. A file question is already narrowed by
the directory it is in, so what it offers is what is there."
  (when p
    (%sync p)
    (if (and (eq p *prompt*) (tree:root))
        (node:contents (%matching-node))
        (if (filep p)
            (candidates p)
            (matches (so-far) (candidates p))))))

(defun here-directory ()
  (let* ((d (text:current))
         (file (and d (typep d 'text:document) (text:file-of d))))
    (if file
        (directory-namestring (pathname file))
        (namestring (uiop:getcwd)))))

(defun %standing (&optional (p *prompt*))
  "What is being asked, at a place. A question, what has been typed at it and which
candidate is chosen are what the frame shows, so they are nodes: a surface follows
what it read, and a slot nobody reads is a thing nothing can follow."
  (when (tree:root)
    (setf (node:contents (%question-node)) (and p (question p)))
    (setf (node:contents (%chose-node)) 0)
    (node:stir (%said-node))
    (%under)))

(defun %where-from (had)
  "Where answering goes back to. Never the prompt itself: a question asked from
inside another one would otherwise leave the prompt as what every key edits."
  (let ((it (and had (text:named (node:name had)))))
    (cond ((and it (not (eq it (text:named +document+)))) it)
          (t (find-if-not #'text:asidep (text:documents))))))

(defun ask (question &key then category initial must-match candidates history)
  (let ((d (answering))
        (seed (or initial (when (eq category :file) (here-directory)) ""))
        (back (or (and *prompt* (was *prompt*)) (text:current))))
    (setf (node:contents d) seed)
    (text:move d :text 1)
    (setf *prompt* (make-instance 'standing :question question :then then
                                          :category category
                                          :must-match must-match
                                          :candidates candidates
                                          :history history
                                          :was (%where-from back))
          (text:current) d)
    (setf (seen *prompt*) (so-far)))
  (%standing)
  *prompt*)

(defun step-choice (delta)
  (let* ((p *prompt*)
         (found (and p (matching p)))
         (n (length found)))
    (when (plusp n)
      (setf (chosen p) (mod (+ (chosen p) delta) n))
      (nth (chosen p) found))))

(defun filep (&optional (p *prompt*)) (and p (eq :file (category p))))

(defun %put (text)
  (let ((d (answering)))
    (setf (node:contents d) text)
    (text:move d :text 1)
    (when *prompt* (setf (seen *prompt*) (so-far)))
    text))

(defun %complete-file (found)
  (multiple-value-bind (where base) (split-path (expanded (so-far)))
    (declare (ignore base))
    (let ((shared (common-prefix (mapcar #'name-of found))))
      (when (plusp (length shared))
        (%put (concatenate 'string where shared)))
      (first found))))

(defun complete ()
  "Fill in as much as every match agrees on. With one match, that is the whole of
it; with several, the part they share, so the next keystroke narrows rather than
starting over."
  (let* ((p *prompt*)
         (found (and p (matching p))))
    (when found
      (if (filep p)
          (%complete-file found)
          (let ((shared (common-prefix (mapcar #'name-of found))))
            (cond ((and (= 1 (length found))
                        (equal (so-far) (name-of (first found))))
                   (answer))
                  ((and (plusp (length shared)) (> (length shared) (length (so-far))))
                   (%put shared))
                  (t (%put (name-of (nth (min (chosen p) (1- (length found)))
                                         found)))))
            (first found))))))

(defun %history-node (name)
  (when (and name (tree:root))
    (tree:ensure "/prompt/history" (string-downcase (string name)))))

(defun history-of (name)
  (let ((n (%history-node name)))
    (and n (node:contents n))))

(defun remember (name text)
  (let ((n (%history-node name)))
    (when (and n (stringp text) (plusp (length text)))
      (setf (node:contents n)
            (d:capped (remove text (node:contents n) :test #'equal)
                      text *history-kept*))))
  text)

(defun walk-history (by &optional (p *prompt*))
  "Step through what was answered here before. The first step remembers what was
typed, so walking back to the end gives it back."
  (when (and p (history p))
    (let ((all (or (walking p) (setf (walking p) (history-of (history p))))))
      (when all
        (let* ((at (walked p))
               (next (cond ((null at) (when (plusp by) 0))
                           (t (let ((to (+ at by)))
                                (cond ((minusp to) nil)
                                      ((>= to (length all)) (1- (length all)))
                                      (t to)))))))
          (setf (walked p) next)
          (%put (if next (nth next all) ""))
          (nth (or next 0) all))))))

(defun %close ()
  (setf (text:current)
        (or (%where-from (and *prompt* (was *prompt*)))
            (text:named "scratch")
            (text:scratch)))
  (setf *prompt* nil)
  (%standing nil)
  (let ((d (text:named +document+)))
    (when d (setf (node:contents d) "")))
  nil)

(defun descendsp (p)
  (and (filep p)
       (let ((typed (expanded (so-far))))
         (and (plusp (length typed))
              (uiop:directory-exists-p typed)
              (not (eql #\/ (char typed (1- (length typed)))))))))

(defun descend (&optional (p *prompt*))
  (declare (ignore p))
  (let ((typed (expanded (so-far))))
    (%put (concatenate 'string (string-right-trim "/" typed) "/"))))

(defun %answered (p)
  (let ((said (if (filep p) (expanded (so-far)) (so-far))))
    (if (and p (must-match p))
        (let* ((found (matching p))
               (pick (nth (min (chosen p) (max 0 (1- (length found)))) found)))
          (if pick (name-of pick) said))
        said)))

(defun answer (&optional text)
  (let* ((p *prompt*)
         (answer (or text (%answered p)))
         (fn (and p (then p))))
    (when (and p (history p)) (remember (history p) answer))
    (%close)
    (when fn (fault:attempt (lambda () (funcall fn answer)) "answering a prompt"))
    answer))

(defun cancel ()
  (%close)
  (log:note "cancelled")
  nil)

(defun showing ()
  (if (askingp)
      (format nil "~a~a" (question *prompt*) (so-far))
      (or (log:last-said) "")))

(command:defcommand "run-command" ()
    (:describes "run a command by name" :on '(text "M-x"))
  (ask "M-x " :category :command :must-match t :history :commands
              :then (lambda (name)
                      (let ((c (command:named name)))
                        (if c
                            (command:run c)
                            (log:note "no command named ~a" name)))))
  :asking)

(command:defcommand "answer" ()
    (:describes "accept what is typed at the prompt" :on '(prompt "RET"))
  (if (descendsp (asking))
      (descend)
      (answer)))

(command:defcommand "cancel" ()
    (:describes "put the prompt away" :on '(prompt "C-g" "Escape"))
  (cancel))

(command:defcommand "complete" ()
    (:describes "fill the prompt from the candidates" :on '(prompt "TAB"))
  (complete))

(command:defcommand "next-candidate" ()
    (:describes "the next candidate" :on '(prompt "C-n" "Down"))
  (step-choice 1))

(command:defcommand "previous-candidate" ()
    (:describes "the candidate before" :on '(prompt "C-p" "Up"))
  (step-choice -1))

(command:defcommand "history-previous" ()
    (:describes "what was answered here before" :on '(prompt "M-p"))
  (walk-history 1))

(command:defcommand "history-next" ()
    (:describes "the answer after that one" :on '(prompt "M-n"))
  (walk-history -1))

