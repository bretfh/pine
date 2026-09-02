(in-package #:pine/edit)

(defvar *went* nil)
(defvar *evaluating* (d:table)
  "The session each document's forms are read and evaluated in, by its name.")
(defparameter +kinds+ '(:function :macro :generic-function :variable :class))
(defparameter +delimiters+ (format nil "~c()'`,;\"" #\Newline))

(defun delimiterp (ch) (find ch +delimiters+))

(defun offset-of (document &optional (line (text:at-line document))
                                     (col (text:at-col document)))
  (let ((at 0))
    (dotimes (i line)
      (incf at (1+ (length (text:line document i)))))
    (+ at col)))

(defun line-col (text offset)
  (let ((line 0) (col 0))
    (dotimes (i (min offset (length text)) (values line col))
      (if (char= #\Newline (char text i))
          (setf line (1+ line) col 0)
          (incf col)))))

(defun token-at (text offset)
  (let ((n (length text)))
    (let ((from (min offset n)) (to (min offset n)))
      (loop :while (and (plusp from) (not (delimiterp (char text (1- from)))))
            :do (decf from))
      (loop :while (and (< to n) (not (delimiterp (char text to))))
            :do (incf to))
      (when (< from to) (subseq text from to)))))

(defun token-start (text offset)
  (let ((from (min offset (length text))))
    (loop :while (and (plusp from) (not (delimiterp (char text (1- from)))))
          :do (decf from))
    from))

(defun symbol-at (document &optional of)
  (let* ((text (text:text document))
         (token (or of (token-at text (offset-of document)))))
    (when token
      (values (multiple-value-bind (*package* *readtable*)
                  (text:reading document)
                (fault:or-nothing "a token that is not a form is just a token"
                  (read-from-string token)))
              token))))

(defun quoted (text)
  "A bit per place in TEXT: whether a paren there is inside a string.

Walked once, and the walk is the only one. Asked place by place instead, every ask
walked the text from the beginning again -- so finding the form before point cost
the length of the document squared, which on a hundred kilobytes is thousands of
millions of steps for one C-x C-e.

A comment is followed but not marked: a quote inside one opens no string, and what
this answers is only whether a place is inside one."
  (let* ((n (length text))
         (mask (make-array (1+ n) :element-type 'bit :initial-element 0))
         (in nil) (escaped nil) (comment nil))
    (dotimes (i n)
      (setf (sbit mask i) (if in 1 0))
      (let ((ch (char text i)))
        (cond (escaped (setf escaped nil))
              ((char= ch #\\) (setf escaped t))
              (comment (when (char= ch #\Newline) (setf comment nil)))
              ((char= ch #\") (setf in (not in)))
              ((and (not in) (char= ch #\;)) (setf comment t)))))
    (setf (sbit mask n) (if in 1 0))
    mask))

(defun quotedp (mask at) (plusp (sbit mask (min at (1- (length mask))))))

(defun form-before (text offset)
  (let ((at (min offset (length text)))
        (mask (quoted text)))
    (loop :while (and (plusp at)
                      (member (char text (1- at)) '(#\Space #\Tab #\Newline)))
          :do (decf at))
    (when (and (plusp at) (char= #\) (char text (1- at)))
               (not (quotedp mask (1- at))))
      (let ((depth 0))
        (loop :for i :downfrom (1- at) :to 0
              :for ch := (char text i)
              :do (unless (quotedp mask i)
                    (case ch
                      (#\) (incf depth))
                      (#\( (decf depth)
                           (when (zerop depth)
                             (return-from form-before (values i at)))))))))
    (when (plusp at)
      (let ((from (token-start text at)))
        (when (< from at) (values from at))))))

(defun form-around (text offset)
  (let ((at (min offset (length text)))
        (mask (quoted text)))
    (let ((from (loop :for i :downfrom (min at (1- (length text))) :to 0
                      :when (and (char= #\( (char text i))
                                 (or (zerop i) (char= #\Newline (char text (1- i))))
                                 (not (quotedp mask i)))
                        :do (return i))))
      (when from
        (let ((depth 0))
          (loop :for i :from from :below (length text)
                :for ch := (char text i)
                :do (unless (quotedp mask i)
                      (case ch
                        (#\( (incf depth))
                        (#\) (decf depth)
                             (when (zerop depth)
                               (return-from form-around (values from (1+ i)))))))))))))

(defun went ()
  (let ((back (first *went*)))
    (when back (d:swap *went* #'rest))
    back))

(defun %remember (document)
  (d:swap *went*
           (lambda (all)
             (cons (list (node:name document) (text:at-line document)
                         (text:at-col document))
                   all))))

(defun visit (place)
  (destructuring-bind (file line col &optional kind) place
    (declare (ignore kind))
    (%remember (text:current))
    (command:run "find-file" (list file))
    (text:goto (text:current) line col)
    (log:note "~a:~d" (file-namestring file) (1+ line))
    place))

(defun images ()
  "Every image work can be done in: the children this pine runs, and the pines it
has reached. Two relationships, one protocol."
  (remove-if-not (lambda (j) (typep j 'image:image)) (job:jobs)))

(defun image-named (name)
  (find (princ-to-string name) (images) :key #'job:name :test #'equal))

(defun %at (name) (tree:ensure "/eval" name))

(defun target () (and (tree:root) (node:contents (%at "target"))))

(defun (setf target) (name)
  (setf (node:contents (%at "target")) name))

(defun target-was () (and (tree:root) (node:contents (%at "was"))))

(defun (setf target-was) (name)
  (setf (node:contents (%at "was")) name))

(defun evaluating (document)
  "The session this document's forms are evaluated in.

One per document, and what it reads in is what the document says it is written in,
asked again each time because the document may have said something else since. One
session for the image took whichever document asked first and kept its package for
ever, so M-: in a second file read its names in the first file's."
  (let* ((name (node:name document))
         (s (or (d:lookup (d:all *evaluating*) name)
                (d:claim *evaluating* name
                         (session:open-session :name name)))))
    (setf (session:package-of s) (text:package-of document)
          (session:readtable-of s) (text:readtable-of document))
    s))

(defun %there (document text)
  "Evaluate in the image the target names, and say what it said the way a session
here would."
  (let* ((where (target)) (i (image-named where)))
    (cond ((null i) (format nil "no image named ~a" where))
          (t (multiple-value-bind (*package* *readtable*)
                 (text:reading document)
               (multiple-value-bind (answered broke)
                   (image:evaluate i (read-from-string text))
                 (if broke
                     (format nil "~a" broke)
                     (format nil "~{~s~^, ~}" answered))))))))

(defun evaluate (document text at)
  (let* ((where (target))
         (s (unless where (evaluating document)))
         (e (when s (session:evaluate s (session:read s text))))
         (said (cond (where (%there document text))
                     ((and e (session:fault e)) (format nil "~a" (session:fault e)))
                     (t (format nil "~{~s~^, ~}" (session:answered e))))))
    (log:note "~a" said)
    (text:forget-overlays document)
    (text:overlay document (line-col (text:text document) at)
                    (format nil "=> ~a" said)
                    (if (and e (session:fault e)) :error :comment))
    (or e said)))

(command:defcommand "find-definition" ()
    (:describes "go to where what is at point is defined" :on '(code "M-."))
  (let ((found (definition (text:mode-of (text:current)) (text:current))))
    (if found (visit (first found)) (log:note "no definition"))))

(command:defcommand "go-back" ()
    (:describes "back to where the last jump started" :on '(code "M-,"))
  (let ((back (went)))
    (when back
      (destructuring-bind (name line col) back
        (let ((document (text:named name)))
          (when document
            (setf (text:current) document)
            (show (focused) document)
            (text:goto document line col)))))))

(command:defcommand "find-references" ()
    (:describes "every place that mentions what is at point" :on '(code "M-?"))
  (flet ((says (p) (format nil "~a:~d" (file-namestring (first p))
                           (1+ (second p)))))
    (let ((found (references (text:mode-of (text:current)) (text:current))))
      (cond ((null found) (log:note "no references"))
            ((null (rest found)) (visit (first found)))
            (t (ask "Reference: " :must-match t
                           :candidates (mapcar (lambda (p)
                                                 (cons (says p)
                                                       (princ-to-string (fourth p))))
                                               found)
                           :then (lambda (said)
                                   (let ((pick (find said found :test #'equal
                                                               :key #'says)))
                                     (when pick (visit pick)))))
               :asking)))))

(command:defcommand "complete-symbol" ()
    (:describes "finish the name at point" :on '(code "M-TAB" "C-M-i"))
  (let* ((document (text:current))
         (prefix (prefix-at document))
         (found (and (plusp (length prefix))
                     (mode:complete (text:mode-of (text:current)) document prefix))))
    (cond ((null found) (log:note "no completions"))
          ((null (rest found)) (put-completion document prefix (first found)))
          (t (ask "Complete: " :must-match t :candidates found
                         :then (lambda (choice)
                                 (put-completion document prefix choice)))
             :asking))))

(command:defcommand "arglist" ()
    (:describes "what the call at point takes" :on '(code "C-c C-a"))
  (log:note "~a" (or (arglist (text:mode-of (text:current)) (text:current))
                     "nothing at point takes arguments")))

(command:defcommand "describe-symbol" ()
    (:describes "what the name at point is" :on '(code "C-c C-d"))
  (log:note "~a" (or (explains (text:mode-of (text:current)) (text:current)) "")))

(command:defcommand "eval-last-expression" ()
    (:describes "evaluate the form before point" :on '(code "C-x C-e"))
  (let* ((document (text:current)) (text (text:text document)))
    (multiple-value-bind (from to) (form-before text (offset-of document))
      (if from
          (evaluate document (subseq text from to) to)
          (log:note "no form before point")))))

(command:defcommand "eval-defun" ()
    (:describes "evaluate the definition point is in" :on '(code "C-M-x"))
  (let* ((document (text:current)) (text (text:text document)))
    (multiple-value-bind (from to) (form-around text (offset-of document))
      (if from
          (evaluate document (subseq text from to) to)
          (log:note "point is in no definition")))))

(command:defcommand "load-file" ()
    (:describes "compile this document's file and load it" :on '(code "C-c C-l"))
  (let* ((document (text:current)) (file (text:file-of document)))
    (cond ((null file) (log:note "~a has no file" (node:name document)))
          (t (fault:attempt
              (lambda ()
                (multiple-value-bind (*package* *readtable*)
                    (text:reading document)
                  (load (compile-file file))))
              (format nil "loading ~a" file))
             (log:note "loaded ~a" file)
             file))))

(command:defcommand "set-eval-target" ()
    (:describes "which image a form is evaluated in" :on '(code "C-c C-t"))
  (let ((names (cons "here" (mapcar #'job:name (images)))))
    (ask "Eval in: " :must-match t :candidates names
                :then (lambda (said)
                        (setf (target) (unless (equal said "here") said))
                        (log:note "evaluating in ~a"
                                  (or (target) "this image"))))
    :asking))

(command:defcommand "eval-expression" (form)
    (:describes "read a form and evaluate it"
     :asks '((:prompt "Eval: " :history :eval))
     :on '(text "M-:"))
  (let* ((s (evaluating (text:current)))
         (e (session:evaluate s (session:read s (princ-to-string form)))))
    (if (session:fault e)
        (log:note "~a" (session:fault e))
        (log:note "~{~s~^, ~}" (session:answered e)))
    (first (session:answered e))))

(command:defcommand "eval-document" ()
    (:describes "evaluate every form in this document" :on '(code "C-c C-k"))
  (let* ((document (text:current)) (text (text:text document)) (n 0))
    (fault:attempt
     (lambda ()
       (multiple-value-bind (*package* *readtable*) (text:reading document)
         (let ((at 0))
           (loop (multiple-value-bind (form next)
                     (read-from-string text nil :eof :start at)
                   (when (eq form :eof) (return))
                   (eval form)
                   (incf n)
                   (setf at next))))))
     (format nil "evaluating ~a" (node:name document)))
    (log:note "~d form~:p" n)
    n))
