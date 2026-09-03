(in-package #:pine/edit)

(defvar *prompt* nil)
(defvar *shown* 12)
(defvar *asked* 0)
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
   (walked     :initform nil        :accessor walked))
  (:documentation "A question standing, and what will be done with the answer."))

(defmethod print-object ((p standing) stream)
  (print-unreadable-object (p stream :type t)
    (write-string (question p) stream)))

(defun asking () *prompt*)

(defun askingp () (and *prompt* t))

(defun answering ()
  (or (fs:at "/text" +document+)
      (text:make-document +document+ :mode (make-instance 'prompt))))

(defun %under () (fs:ensure "/edit/prompt"))

(defun %place (name builder)
  (let ((under (%under)))
    (or (fs:entry under name)
        (fs:attach (funcall builder) under))))

(defun %question-node ()
  (%place "question" (lambda () (make-instance 'fs:value :name "question"))))

(defun %chose-node ()
  (%place "chose" (lambda () (make-instance 'fs:value :name "chose"))))

(defun %asking-node ()
  "Which question is standing, as a number no other question answers to. What
MATCHING reads, so that it reads nodes only: *PROMPT* is not one, and the graph
cannot see a thing it cannot see move."
  (%place "asking" (lambda () (make-instance 'fs:value :name "asking"))))

(defun %typed ()
  (let ((d (fs:at "/text" +document+)))
    (if d (text:text d) "")))

(defun %said-node ()
  (%place "said"
          (lambda () (make-instance 'fs:derived :name "said" :reads #'%typed))))

(defun %matching-node ()
  (%place "matching"
          (lambda ()
            (make-instance 'fs:derived :name "matching"
                         :reads (lambda ()
                           (let ((p (and (fs:contents (%asking-node)) *prompt*)))
                             (when p
                               (let ((text (fs:contents (%said-node))))
                                 (if (filep p)
                                     (candidates p)
                                     (matches text (candidates p)))))))))))

(defun so-far ()
  "What has been typed into the prompt so far."
  (if (fs:root) (fs:contents (%said-node)) (%typed)))

(defun asked ()
  (and (fs:root) (fs:contents (%question-node))))

(defun %completes () (fs:ensure (%under) "completes"))

(defun %category (category) (string-downcase (string category)))

(defun completes (category function)
  "Say how to answer a prompt asking for CATEGORY: FUNCTION of what has been typed.
A command asks for one by name -- :asks '((:prompt \"Note: \" :category
:note-title)) -- and this is what offers the words, so a system brings its own kind
of question and its own answers to it. It stands at /prompt/completes/<category>."
  (let ((n (make-instance 'fs:derived :name (%category category) :live t
                                      :reads (lambda () (funcall function (so-far)))
                                      :describes "what answers this kind of question")))
    (setf (fs:owner n) fs:*owner*)
    (fs:attach n (%completes))
    category))

(defun forget-completes (category)
  (fs:erase (%completes) (%category category))
  category)

(defun sources ()
  (mapcar (lambda (e) (intern (string-upcase (fs:name e)) :keyword))
          (fs:entries (%completes))))

(defun candidates (&optional (p *prompt*))
  (when p
    (or (given p)
        (let ((n (fs:entry (%completes) (%category (category p)))))
          (when n (fault:attempt (lambda () (fs:contents n)) "the candidates"))))))

(defun chosen (&optional (p *prompt*))
  "Which candidate is picked, or nought where what has been typed has moved since
anybody picked one.

Nought is worked out and not written down. What is written down is the answer
together with the text it was an answer to, so asking is only asking: reading it
used to put the nought there, and the frame reads it while it is working itself
out -- so drawing the prompt wrote to a place the drawing depended on, and the
frame gave itself up and was drawn again for every keystroke."
  (when p
    (let ((said (fs:contents (%chose-node))))
      (if (and (consp said) (equal (car said) (so-far))) (cdr said) 0))))

(defun (setf chosen) (value p)
  (declare (ignore p))
  (setf (fs:contents (%chose-node)) (cons (so-far) value))
  value)

(defun matching (&optional (p *prompt*))
  "What answers the question as it stands. A file question is already narrowed by
the directory it is in, so what it offers is what is there."
  (when p
    (if (and (eq p *prompt*) (fs:root))
        (fs:contents (%matching-node))
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
  (when (fs:root)
    (setf (fs:contents (%question-node)) (and p (question p)))
    (setf (fs:contents (%asking-node)) (and p (incf *asked*)))
    (setf (fs:contents (%chose-node)) nil)
    (fs:moved (%said-node))
    (%under)))

(defun %where-from (had)
  "Where answering goes back to. Never the prompt itself: a question asked from
inside another one would otherwise leave the prompt as what every key edits."
  (let ((it (and had (fs:at "/text" (fs:name had)))))
    (cond ((and it (not (eq it (fs:at "/text" +document+)))) it)
          (t (find-if-not #'text:asidep (text:documents))))))

(defun ask (question &key then category initial must-match candidates history)
  (let ((d (answering))
        (seed (or initial (when (eq category :file) (here-directory)) ""))
        (back (or (and *prompt* (was *prompt*)) (text:current))))
    (setf (text:text d) seed)
    (text:move d :text 1)
    (setf *prompt* (make-instance 'standing :question question :then then
                                          :category category
                                          :must-match must-match
                                          :candidates candidates
                                          :history history
                                          :was (%where-from back))
          (text:current) d))
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
    (setf (text:text d) text)
    (text:move d :text 1)
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
  (when (and name (fs:root))
    (fs:leaf "/edit/prompt/history" (string-downcase (string name)))))

(defun history-of (name)
  (let ((n (%history-node name)))
    (and n (fs:contents n))))

(defun remember (name text)
  (let ((n (%history-node name)))
    (when (and n (stringp text) (plusp (length text)))
      (setf (fs:contents n)
            (d:capped (remove text (fs:contents n) :test #'equal)
                      text *history-kept*))))
  text)

(defun walk-history (by &optional (p *prompt*))
  "Step through what was answered here before. The first step remembers what was
so-far, so walking back to the end gives it back."
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
            (fs:at "/text" "scratch")
            (text:scratch)))
  (setf *prompt* nil)
  (%standing nil)
  (let ((d (fs:at "/text" +document+)))
    (when d (setf (text:text d) "")))
  nil)

(defun descendsp (p)
  (and (filep p)
       (let ((so-far (expanded (so-far))))
         (and (plusp (length so-far))
              (uiop:directory-exists-p so-far)
              (not (eql #\/ (char so-far (1- (length so-far)))))))))

(defun descend (&optional (p *prompt*))
  (declare (ignore p))
  (let ((so-far (expanded (so-far))))
    (%put (concatenate 'string (string-right-trim "/" so-far) "/"))))

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
    (:describes "accept what is so-far at the prompt" :on '(prompt "RET"))
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

