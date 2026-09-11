(in-package #:pine/edit)

(defun %listing (buffer)
  (let ((m (text:mode-of buffer))) (and (typep m 'listing) m)))

(defun said (row) (if (consp row) (car row) (princ-to-string row)))

(defun row-at (buffer &optional (line (text:at-line buffer)))
  (let ((l (%listing buffer)))
    (when l (nth line (shown-rows l)))))

(defun place (&optional (buffer (text:current)))
  (let ((row (row-at buffer)))
    (and (consp row) (cdr row))))

(defun %mark (buffer)
  (setf (mode:setting buffer :selection) (place buffer))
  buffer)

(defun show-listing (name shown-rows &optional on-enter)
  (let ((buffer (or (fs:at "/text" name)
                      (text:make-buffer name :mode (make-instance 'listing))))
        (shown-rows (if (stringp shown-rows)
                  (uiop:split-string shown-rows :separator '(#\Newline))
                  shown-rows)))
    (setf (text:text buffer)
          (format nil "~{~a~^~%~}" (mapcar #'said shown-rows)))
    (text:goto buffer 0 0)
    (setf (text:mode-of buffer)
          (make-instance 'listing :shown-rows shown-rows :on-enter on-enter))
    (setf (text:current) buffer)
    (%mark buffer)
    (fs:name buffer)))

(defun activate ()
  (let* ((buffer (text:current))
         (l (%listing buffer)))
    (when (and l (on-enter l))
      (funcall (on-enter l) (place buffer)))))

(defun step-row (delta &optional (buffer (text:current)))
  (let* ((l (%listing buffer))
         (n (length (and l (shown-rows l)))))
    (when (plusp n)
      (text:goto buffer (mod (+ (text:at-line buffer) delta) n) 0)
      (%mark buffer)
      (log:note "~a" (said (row-at buffer)))
      (text:at-line buffer))))

(command:defcommand "list-activate" ()
    (:describes "open what this row stands for" :on '(listing "RET"))
  (activate))

(command:defcommand "list-next" ()
    (:describes "the row after this one" :on '(listing "n" "C-n" "Down"))
  (step-row 1))

(command:defcommand "list-previous" ()
    (:describes "the row before this one" :on '(listing "p" "C-p" "Up"))
  (step-row -1))

(command:defcommand "list-place" ()
    (:describes "what the row point is on stands for" :on '(listing "."))
  (let ((it (place)))
    (log:note "~a" (cond ((null it) "this row stands for nothing")
                         ((fs:nodep it) (fs:full-name it))
                         (t it)))
    it))

