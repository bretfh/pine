(in-package #:pine/edit/eval)

(defgeneric definition (mode document &optional of)
  (:documentation "Where what is at point is defined, as (FILE LINE COL KIND).")
  (:method ((m mode:mode) document &optional of)
    (declare (ignore document of))
    nil))

(defgeneric references (mode document &optional of)
  (:documentation "Every place that mentions what is at point.")
  (:method ((m mode:mode) document &optional of)
    (declare (ignore document of))
    nil))

(defgeneric arglist (mode document &optional of)
  (:documentation "What the call at point takes.")
  (:method ((m mode:mode) document &optional of)
    (declare (ignore document of))
    nil))

(defgeneric explains (mode document &optional of)
  (:documentation "What the name at point is.")
  (:method ((m mode:mode) document &optional of)
    (declare (ignore document of))
    nil))

(defun %placed (source kind)
  (let ((file (sb-introspect:definition-source-pathname source))
        (at (sb-introspect:definition-source-character-offset source)))
    (when (and file (probe-file file))
      (multiple-value-bind (line col)
          (if at
              (line-col (fault:or-nothing "the source may not be on this machine"
                          (uiop:read-file-string file))
                        at)
              (values 0 0))
        (list (namestring file) line col kind)))))

(defmethod definition ((m mode:lisp) document &optional of)
  (let ((s (symbol-at document of)))
    (when (symbolp s)
      (loop :for kind :in +kinds+
            :append (loop :for source
                            :in (fault:or-nothing
                                    "nothing was compiled from a source here"
                                  (sb-introspect:find-definition-sources-by-name
                                   s kind))
                          :for placed := (%placed source kind)
                          :when placed :collect placed)))))

(defmethod references ((m mode:lisp) document &optional of)
  (let ((s (symbol-at document of)))
    (when (and s (symbolp s))
      (loop :for (nil . source) :in (fault:or-nothing "nothing may call it"
                                      (sb-introspect:who-calls s))
            :for placed := (%placed source :caller)
            :when placed :collect placed))))

(defun %qualified (prefix)
  "A name written with its package: the package, whether it was written with one
colon or two, and the part after them. Nothing when it is a plain name."
  (let ((at (position #\: prefix)))
    (when at
      (let* ((twice (and (< (1+ at) (length prefix))
                         (char= #\: (char prefix (1+ at)))))
             (where (find-package (string-upcase (subseq prefix 0 at)))))
        (when where
          (list where twice (subseq prefix (+ at (if twice 2 1)))))))))

(defun %matching (prefix names)
  (let ((up (string-upcase prefix)))
    (remove-if-not (lambda (name)
                     (and (>= (length name) (length prefix))
                          (string-equal up name :end2 (length up))))
                   names)))

(defmethod mode:complete ((m mode:lisp) document prefix)
  "Every name that starts this way. A name written with its package completes in
that package -- pine's own source is written that way, so a completion that only
knew the document's package would be no use in it."
  (when (plusp (length prefix))
    (let ((qualified (%qualified prefix))
          (out nil))
      (cond
        (qualified
         (destructuring-bind (where twice rest) qualified
           (let ((said (package-name where))
                 (seen nil))
             (if twice
                 (do-symbols (s where) (pushnew s seen))
                 (do-external-symbols (s where) (pushnew s seen)))
             (setf out
                   (mapcar (lambda (name)
                             (format nil "~(~a~a~a~)" said (if twice "::" ":") name))
                           (%matching rest
                                      (mapcar (lambda (s)
                                                (string-downcase (symbol-name s)))
                                              seen)))))))
        (t
         (let ((seen nil))
           (do-symbols (s (language:package-of document))
             (pushnew (string-downcase (symbol-name s)) seen :test #'string=))
           (setf out (%matching prefix seen)))))
      (sort (remove-duplicates out :test #'string=) #'string<))))

(defmethod arglist ((m mode:lisp) document &optional of)
  (multiple-value-bind (s token) (symbol-at document of)
    (when (and s (symbolp s) (fboundp s))
      (format nil "~(~a ~a~)" token
              (or (fault:or-nothing "a function may have no lambda list kept"
                    (sb-introspect:function-lambda-list s))
                  "()")))))

(defmethod explains ((m mode:lisp) document &optional of)
  (multiple-value-bind (s token) (symbol-at document of)
    (when (and s (symbolp s))
      (let ((said (or (documentation s 'function) (documentation s 'variable)))
            (args (arglist m document of)))
        (cond ((and args said) (format nil "~a  ~a" args said))
              (args args)
              (said (format nil "~a: ~a" token said))
              (t (format nil "~a is not defined" token)))))))

(defun prefix-at (document)
  (let* ((text (doc:text document))
         (at (offset-of document))
         (from (token-start text at)))
    (subseq text from (min at (length text)))))

(defun put-completion (document prefix choice)
  (let ((line (doc:at-line document)) (col (doc:at-col document)))
    (doc:delete-region document line (max 0 (- col (length prefix))) line col)
    (doc:insert document choice)))
