(defpackage #:pine/edit/prompt
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node) (#:world #:pine/world/world)
                    (#:buffer #:pine/edit/buffer) (#:log #:pine/run/log)
                    (#:fault #:pine/run/fault))
  (:export #:prompt #:asking #:asking-p #:ask #:answer! #:cancel! #:said
           #:question #:answer-buffer #:then #:candidates #:chosen #:choose! #:was
           #:matching #:complete! #:source #:sources #:install #:*prompt*
           #:showing #:category #:annotation #:name-of #:shows #:given
           #:must-match #:history #:remember #:walk-history #:filep
           #:common-prefix #:matches #:descends-p #:descend! #:expanded #:files
           #:history-of #:*shown* #:*history-kept*))
(in-package #:pine/edit/prompt)

(defvar *prompt* nil)
(defvar *sources* (d:table))
(defvar *shown* 12)
(defvar *history-kept* 200)
(defparameter +buffer+ "*prompt*")
(defparameter +separator+ #\Space)

(defclass prompt ()
  ((question   :initarg :question   :reader question)
   (was        :initarg :was        :reader was        :initform nil)
   (then       :initarg :then       :reader then       :initform nil)
   (category   :initarg :category   :reader category   :initform nil)
   (given      :initarg :candidates :reader given      :initform nil)
   (must-match :initarg :must-match :reader must-match :initform nil)
   (history    :initarg :history    :reader history    :initform nil)
   (walking    :initform nil        :accessor walking)
   (walked     :initform nil        :accessor walked)
   (seen       :initform nil        :accessor seen)
   (chose      :initform 0          :accessor chose)))

(defmethod print-object ((p prompt) stream)
  (print-unreadable-object (p stream :type t)
    (write-string (question p) stream)))

(defun asking () *prompt*)

(defun asking-p () (and *prompt* t))

(defun answer-buffer ()
  (or (buffer:buffer-named +buffer+)
      (let ((b (buffer:make-buffer +buffer+)))
        (setf (buffer:setting b :aside) t)
        b)))

(defun said ()
  (let ((b (buffer:buffer-named +buffer+)))
    (if b (buffer:text-of b) "")))

(defun source (category function)
  (d:keep! *sources* category function)
  category)

(defun sources () (d:keys (d:all *sources*)))

(defun candidates (&optional (p *prompt*))
  (when p
    (or (given p)
        (let ((fn (d:at (d:all *sources*) (category p))))
          (when fn (fault:attempt (lambda () (funcall fn (said))) "the candidates"))))))

(defun name-of (each) (princ-to-string (if (consp each) (first each) each)))

(defun %sync (p)
  (let ((text (said)))
    (unless (equal text (seen p))
      (setf (seen p) text (chose p) 0)))
  p)

(defun chosen (&optional (p *prompt*))
  (when p (chose (%sync p))))

(defun (setf chosen) (value p)
  (setf (chose (%sync p)) value))

(defun %tokens (text)
  (remove "" (uiop:split-string (or text "") :separator (list +separator+))
          :test #'string=))

(defun %scored (tokens name)
  "How well NAME answers TOKENS: every token must be somewhere in it, in any
order and in any case. The score is where they were found, so what matches
earliest is offered first, and a name that begins with the first token beats one
that merely contains it. Nil when a token is missing."
  (let ((score 0) (from-the-start nil))
    (loop :for token :in tokens
          :for first := t :then nil
          :for at := (search token name :test #'char-equal)
          :do (unless at (return-from %scored (values nil nil)))
              (when (and first (zerop at)) (setf from-the-start t))
              (incf score at))
    (values (if from-the-start score (+ score 1000)) t)))

(defun matches (text all)
  (let ((tokens (%tokens text)))
    (if (null tokens)
        (stable-sort (copy-list all) #'string-lessp :key #'name-of)
        (let (scored)
          (dolist (each all)
            (multiple-value-bind (score hit) (%scored tokens (name-of each))
              (when hit (push (cons score each) scored))))
          (mapcar #'cdr
                  (stable-sort (nreverse scored)
                               (lambda (a b)
                                 (let ((sa (car a)) (sb (car b))
                                       (na (name-of (cdr a))) (nb (name-of (cdr b))))
                                   (cond ((/= sa sb) (< sa sb))
                                         ((/= (length na) (length nb))
                                          (< (length na) (length nb)))
                                         (t (string-lessp na nb)))))))))))

(defun matching (&optional (p *prompt*))
  "What answers the question as it stands. A file question is already narrowed
by the directory it is in, so what it offers is what is there."
  (when p
    (%sync p)
    (if (filep p)
        (candidates p)
        (matches (said) (candidates p)))))

(defun here-directory ()
  (let* ((b (buffer:current))
         (file (and b (typep b 'buffer:buffer) (buffer:file-of b))))
    (if file
        (directory-namestring (pathname file))
        (namestring (uiop:getcwd)))))

(defun ask (question &key then category initial must-match candidates history)
  (let ((b (answer-buffer))
        (seed (or initial (when (eq category :file) (here-directory)) "")))
    (setf (node:contents b) seed)
    (buffer:move! b :buffer 1)
    (setf *prompt* (make-instance 'prompt :question question :then then
                                          :category category
                                          :must-match must-match
                                          :candidates candidates
                                          :history history
                                          :was (buffer:current))
          (buffer:current) b)
    (setf (seen *prompt*) (said)))
  (when world:*world*
    (setf (node:contents (world:ensure world:*world* "prompt")) question))
  *prompt*)

(defun choose! (delta)
  (let* ((p *prompt*)
         (found (and p (matching p)))
         (n (length found)))
    (when (plusp n)
      (setf (chosen p) (mod (+ (chosen p) delta) n))
      (nth (chosen p) found))))

(defun filep (&optional (p *prompt*)) (and p (eq :file (category p))))

(defun expanded (text)
  "TEXT as a path a person typed: ~ is home, and a second / starts again from
the root, so an absolute path typed over a directory means that path."
  (let* ((text (or text ""))
         (over (search "//" text :from-end t))
         (text (if over (subseq text (1+ over)) text)))
    (cond ((equal text "~") (namestring (user-homedir-pathname)))
          ((and (> (length text) 1) (string= "~/" (subseq text 0 2)))
           (namestring (merge-pathnames (subseq text 2) (user-homedir-pathname))))
          (t text))))

(defun split-path (text)
  (let ((slash (position #\/ text :from-end t)))
    (if slash
        (values (subseq text 0 (1+ slash)) (subseq text (1+ slash)))
        (values "" text))))

(defun entries (where)
  (handler-case
      (append (sort (mapcar (lambda (p)
                              (concatenate 'string
                                           (car (last (pathname-directory p))) "/"))
                            (uiop:subdirectories where))
                    #'string<)
              (sort (mapcar #'file-namestring (uiop:directory-files where)) #'string<))
    (error () nil)))

(defun files (typed)
  (multiple-value-bind (where base) (split-path (expanded typed))
    (let ((all (entries (if (equal where "") "./" where))))
      (if (equal base "")
          all
          (remove-if-not (lambda (name)
                           (and (>= (length name) (length base))
                                (string= base name :end2 (length base))))
                         all)))))

(defun %complete-file (p found)
  (declare (ignore p))
  (multiple-value-bind (where base) (split-path (expanded (said)))
    (declare (ignore base))
    (let ((shared (common-prefix (mapcar #'name-of found))))
      (when (plusp (length shared))
        (%put (concatenate 'string where shared)))
      (first found))))

(defun common-prefix (strings)
  (if (null strings)
      ""
      (let ((shortest (first strings)))
        (dolist (each (rest strings))
          (let ((at (or (mismatch shortest each :test #'char=) (length each))))
            (setf shortest (subseq shortest 0 (min at (length shortest))))))
        shortest)))

(defun %put (text)
  (let ((b (answer-buffer)))
    (setf (node:contents b) text)
    (buffer:move! b :buffer 1)
    (when *prompt* (setf (seen *prompt*) (said)))
    text))

(defun complete! ()
  "Fill in as much as every match agrees on. With one match, that is the whole
of it; with several, the part they share, so the next keystroke narrows rather
than starting over."
  (let* ((p *prompt*)
         (found (and p (matching p))))
    (when found
      (if (filep p)
          (%complete-file p found)
          (let ((shared (common-prefix (mapcar #'name-of found))))
            (cond ((and (= 1 (length found))
                        (equal (said) (name-of (first found))))
                   (answer!))
                  ((and (plusp (length shared)) (> (length shared) (length (said))))
                   (%put shared))
                  (t (%put (name-of (nth (min (chosen p) (1- (length found)))
                                         found)))))
            (first found))))))

(defun %history-node (name)
  (when (and name world:*world*)
    (world:ensure world:*world* "prompt" "history"
                  (string-downcase (string name)))))

(defun history-of (name)
  (let ((n (%history-node name)))
    (and n (node:contents n))))

(defun remember (name text)
  (let ((n (%history-node name)))
    (when (and n (stringp text) (plusp (length text)))
      (setf (node:contents n)
            (let ((kept (cons text (remove text (node:contents n) :test #'equal))))
              (if (> (length kept) *history-kept*)
                  (subseq kept 0 *history-kept*)
                  kept)))))
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
  (let ((was (and *prompt* (was *prompt*))))
    (setf (buffer:current)
          (if (and was (buffer:buffer-named (node:name was)))
              was
              (or (buffer:current) (buffer:scratch)))))
  (setf *prompt* nil)
  (when world:*world*
    (setf (node:contents (world:ensure world:*world* "prompt")) nil))
  (let ((b (buffer:buffer-named +buffer+)))
    (when b (setf (node:contents b) "")))
  nil)

(defun descends-p (p)
  (and (filep p)
       (let ((typed (expanded (said))))
         (and (plusp (length typed))
              (uiop:directory-exists-p typed)
              (not (eql #\/ (char typed (1- (length typed)))))))))

(defun descend!(&optional (p *prompt*))
  (declare (ignore p))
  (let ((typed (expanded (said))))
    (%put (concatenate 'string (string-right-trim "/" typed) "/"))))

(defun %answered (p)
  (let ((said (if (filep p) (expanded (said)) (said))))
    (if (and p (must-match p))
        (let* ((found (matching p))
               (pick (nth (min (chosen p) (max 0 (1- (length found)))) found)))
          (if pick (name-of pick) said))
        said)))

(defun answer! (&optional text)
  (let* ((p *prompt*)
         (answer (or text (%answered p)))
         (fn (and p (then p))))
    (when (and p (history p)) (remember (history p) answer))
    (%close)
    (when fn (fault:attempt (lambda () (funcall fn answer)) "answering a prompt"))
    answer))

(defun cancel! ()
  (%close)
  (log:note "cancelled")
  nil)

(defun showing ()
  (if (asking-p)
      (format nil "~a~a" (question *prompt*) (said))
      (or (log:last-said) "")))

(defun annotation (each)
  (if (consp each) (princ-to-string (rest each)) ""))

(defun shows (each width)
  (let* ((name (name-of each))
         (note (annotation each))
         (gap (- width (length name) (length note))))
    (if (and (plusp (length note)) (> gap 1))
        (concatenate 'string name (make-string gap :initial-element #\space) note)
        name)))
