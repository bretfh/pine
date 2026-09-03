(in-package #:pine/edit)

(defun %listing (document)
  (let ((m (text:mode-of document))) (and (typep m 'listing) m)))

(defun said (row) (if (consp row) (car row) (princ-to-string row)))

(defun row-at (document &optional (line (text:at-line document)))
  "The row point is on: what it says, and what it stands for."
  (let ((l (%listing document)))
    (when l (nth line (shown-rows l)))))

(defun place (&optional (document (text:current)))
  (let ((row (row-at document)))
    (and (consp row) (cdr row))))

(defun %mark (document)
  (setf (mode:setting document :selection) (place document))
  document)

(defun show-listing (name shown-rows &optional on-enter)
  "Put ROWS in a document of its own and show it. With ON-ENTER, RET on a row hands
it the place that row stands for."
  (let ((document (or (fs:at "/text" name)
                      (text:make-document name :mode (make-instance 'listing))))
        (shown-rows (if (stringp shown-rows)
                  (uiop:split-string shown-rows :separator '(#\Newline))
                  shown-rows)))
    (setf (text:text document)
          (format nil "~{~a~^~%~}" (mapcar #'said shown-rows)))
    (text:goto document 0 0)
    (setf (text:mode-of document)
          (make-instance 'listing :shown-rows shown-rows :on-enter on-enter))
    (setf (text:current) document)
    (%mark document)
    (fs:name document)))

(defun activate ()
  (let* ((document (text:current))
         (l (%listing document)))
    (when (and l (on-enter l))
      (funcall (on-enter l) (place document)))))

(defun step-row (delta &optional (document (text:current)))
  "The row after this one, or before it, wrapping round the ends."
  (let* ((l (%listing document))
         (n (length (and l (shown-rows l)))))
    (when (plusp n)
      (text:goto document (mod (+ (text:at-line document) delta) n) 0)
      (%mark document)
      (log:note "~a" (said (row-at document)))
      (text:at-line document))))

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
                         ((fs:kind it) (fs:full-name it))
                         (t it)))
    it))

