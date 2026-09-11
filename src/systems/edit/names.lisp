(in-package #:pine/edit)

(defgeneric definition (mode buffer &optional of)
  (:method ((m mode:mode) buffer &optional of)
    (declare (ignore buffer of))
    nil))

(defgeneric references (mode buffer &optional of)
  (:method ((m mode:mode) buffer &optional of)
    (declare (ignore buffer of))
    nil))

(defgeneric arglist (mode buffer &optional of)
  (:method ((m mode:mode) buffer &optional of)
    (declare (ignore buffer of))
    nil))

(defgeneric explains (mode buffer &optional of)
  (:method ((m mode:mode) buffer &optional of)
    (declare (ignore buffer of))
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

(defmethod definition ((m mode:lisp) buffer &optional of)
  (let ((s (symbol-at buffer of)))
    (when (symbolp s)
      (loop :for kind :in +kinds+
            :append (loop :for source
                            :in (fault:or-nothing
                                    "nothing was compiled from a source here"
                                  (sb-introspect:find-definition-sources-by-name
                                   s kind))
                          :for placed := (%placed source kind)
                          :when placed :collect placed)))))

(defmethod references ((m mode:lisp) buffer &optional of)
  (let ((s (symbol-at buffer of)))
    (when (and s (symbolp s))
      (loop :for (nil . source) :in (fault:or-nothing "nothing may call it"
                                      (sb-introspect:who-calls s))
            :for placed := (%placed source :caller)
            :when placed :collect placed))))

(defun %qualified (prefix)
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

(defun %names-in (where &key externals)
  (let ((seen (make-hash-table :test 'equal)))
    (flet ((note (s) (setf (gethash (string-downcase (symbol-name s)) seen) t)))
      (if externals
          (do-external-symbols (s where) (note s))
          (do-symbols (s where) (note s))))
    (loop :for name :being :the :hash-keys :of seen :collect name)))

(defmethod mode:complete ((m mode:lisp) buffer prefix)
  (when (plusp (length prefix))
    (let ((qualified (%qualified prefix))
          (out nil))
      (cond
        (qualified
         (destructuring-bind (where twice rest) qualified
           (let ((said (package-name where)))
             (setf out
                   (mapcar (lambda (name)
                             (format nil "~(~a~a~a~)" said (if twice "::" ":") name))
                           (%matching rest (%names-in where :externals
                                                      (not twice))))))))
        (t (setf out (%matching prefix (%names-in (text:package-of buffer))))))
      (sort out #'string<))))

(defmethod arglist ((m mode:lisp) buffer &optional of)
  (multiple-value-bind (s token) (symbol-at buffer of)
    (when (and s (symbolp s) (fboundp s))
      (format nil "~(~a ~a~)" token
              (or (fault:or-nothing "a function may have no lambda list kept"
                    (sb-introspect:function-lambda-list s))
                  "()")))))

(defmethod explains ((m mode:lisp) buffer &optional of)
  (multiple-value-bind (s token) (symbol-at buffer of)
    (when (and s (symbolp s))
      (let ((said (or (documentation s 'function) (documentation s 'variable)))
            (args (arglist m buffer of)))
        (cond ((and args said) (format nil "~a  ~a" args said))
              (args args)
              (said (format nil "~a: ~a" token said))
              (t (format nil "~a is not defined" token)))))))

(defun prefix-at (buffer)
  (let* ((text (text:text buffer))
         (at (offset-of buffer))
         (from (token-start text at)))
    (subseq text from (min at (length text)))))

(defun put-completion (buffer prefix choice)
  (let ((line (text:at-line buffer)) (col (text:at-col buffer)))
    (text:delete-region buffer line (max 0 (- col (length prefix))) line col)
    (text:insert buffer choice)))
