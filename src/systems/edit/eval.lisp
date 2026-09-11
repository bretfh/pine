(in-package #:pine/edit)

(defvar *went* nil)
(defparameter +kinds+ '(:function :macro :generic-function :variable :class))
(defparameter +delimiters+ (format nil "~c()'`,;\"" #\Newline))

(defun delimiterp (ch) (find ch +delimiters+))

(defun offset-of (buffer &optional (line (text:at-line buffer))
                                     (col (text:at-col buffer)))
  (let ((at 0))
    (dotimes (i line)
      (incf at (1+ (length (text:line buffer i)))))
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

(defun symbol-at (buffer &optional of)
  (let* ((text (text:text buffer))
         (token (or of (token-at text (offset-of buffer)))))
    (when token
      (values (multiple-value-bind (*package* *readtable*)
                  (text:reading buffer)
                (fault:or-nothing "a token that is not a form is just a token"
                  (read-from-string token)))
              token))))

(defun quoted (text)
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
    (when back (sb-ext:atomic-update *went* (lambda (old) (rest old))))
    back))

(defun %remember (buffer)
  (sb-ext:atomic-update *went*
           (lambda (all)
             (cons (list (fs:name buffer) (text:at-line buffer)
                         (text:at-col buffer))
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
  (remove-if-not (lambda (j) (typep j 'image:image)) (job:jobs)))

(defun image-named (name)
  (find (princ-to-string name) (images) :key #'job:name :test #'equal))

(defun %at (name)
  (fs:mount (make-instance 'fs:value) (format nil "/edit/eval/~a" name)))

(defun target () (and (fs:root) (fs:contents (%at "target"))))

(defun (setf target) (name)
  (setf (fs:contents (%at "target")) name))

(defun target-was () (and (fs:root) (fs:contents (%at "was"))))

(defun (setf target-was) (name)
  (setf (fs:contents (%at "was")) name))

(defun evaluating (buffer)
  (let ((s (or (text:listener buffer)
               (setf (text:listener buffer)
                     (listener:open-listener :name (fs:name buffer))))))
    (setf (listener:package-of s) (text:package-of buffer)
          (listener:readtable-of s) (text:readtable-of buffer))
    s))

(defun %there (buffer text)
  (let* ((where (target)) (i (image-named where)))
    (cond ((null i) (format nil "no image named ~a" where))
          (t (multiple-value-bind (*package* *readtable*)
                 (text:reading buffer)
               (multiple-value-bind (answered broke)
                   (image:evaluate i (read-from-string text))
                 (if broke
                     (format nil "~a" broke)
                     (format nil "~{~s~^, ~}" answered))))))))

(defun evaluate (buffer text at)
  (let* ((where (target))
         (s (unless where (evaluating buffer)))
         (e (when s (listener:evaluate s (listener:read s text))))
         (said (cond (where (%there buffer text))
                     ((and e (listener:fault e)) (format nil "~a" (listener:fault e)))
                     (t (format nil "~{~s~^, ~}" (listener:answered e))))))
    (log:note "~a" said)
    (text:forget-overlays buffer)
    (text:overlay buffer (line-col (text:text buffer) at)
                    (format nil "=> ~a" said)
                    (if (and e (listener:fault e)) :error :comment))
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
        (let ((buffer (fs:at "/text" name)))
          (when buffer
            (setf (text:current) buffer)
            (show (focused) buffer)
            (text:goto buffer line col)))))))

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
  (let* ((buffer (text:current))
         (prefix (prefix-at buffer))
         (found (and (plusp (length prefix))
                     (mode:complete (text:mode-of (text:current)) buffer prefix))))
    (cond ((null found) (log:note "no completions"))
          ((null (rest found)) (put-completion buffer prefix (first found)))
          (t (ask "Complete: " :must-match t :candidates found
                         :then (lambda (choice)
                                 (put-completion buffer prefix choice)))
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
  (let* ((buffer (text:current)) (text (text:text buffer)))
    (multiple-value-bind (from to) (form-before text (offset-of buffer))
      (if from
          (evaluate buffer (subseq text from to) to)
          (log:note "no form before point")))))

(command:defcommand "eval-defun" ()
    (:describes "evaluate the definition point is in" :on '(code "C-M-x"))
  (let* ((buffer (text:current)) (text (text:text buffer)))
    (multiple-value-bind (from to) (form-around text (offset-of buffer))
      (if from
          (evaluate buffer (subseq text from to) to)
          (log:note "point is in no definition")))))

(command:defcommand "load-file" ()
    (:describes "compile this buffer's file and load it" :on '(code "C-c C-l"))
  (let* ((buffer (text:current)) (file (text:file-of buffer)))
    (cond ((null file) (log:note "~a has no file" (fs:name buffer)))
          (t (fault:attempt
              (lambda ()
                (multiple-value-bind (*package* *readtable*)
                    (text:reading buffer)
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
         (e (listener:evaluate s (listener:read s (princ-to-string form)))))
    (if (listener:fault e)
        (log:note "~a" (listener:fault e))
        (log:note "~{~s~^, ~}" (listener:answered e)))
    (first (listener:answered e))))

(command:defcommand "eval-buffer" ()
    (:describes "evaluate every form in this buffer" :on '(code "C-c C-k"))
  (let* ((buffer (text:current)) (text (text:text buffer)) (n 0))
    (fault:attempt
     (lambda ()
       (multiple-value-bind (*package* *readtable*) (text:reading buffer)
         (let ((at 0))
           (loop (multiple-value-bind (form next)
                     (read-from-string text nil :eof :start at)
                   (when (eq form :eof) (return))
                   (eval form)
                   (incf n)
                   (setf at next))))))
     (format nil "evaluating ~a" (fs:name buffer)))
    (log:note "~d form~:p" n)
    n))
