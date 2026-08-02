(defpackage #:pine.provider.file
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:server))

(in-package #:pine.provider.file)
(named-readtables:in-readtable pine.path:syntax)

;;;; The filesystem, mirrored under /file, so it is one path and needs no
;;;; quoting: /file/proc/uptime is /proc/uptime.
;;;;
;;;; nil is nothing here as it is everywhere: writing it deletes.

(defun %native (segments)
  (format nil "/~{~a~^/~}" segments))

(defun %directoryp (path)
  (let ((truth (probe-file path)))
    (and truth (null (pathname-name truth)))))

(defun %name (entry)
  (if (pathname-name entry)
      (file-namestring entry)
      (car (last (pathname-directory entry)))))

(defun %read (segments)
  (let ((native (%native segments)))
    (cond ((null segments) nil)
          ((%directoryp (concatenate 'string native "/"))
           (let ((out (fset:empty-map)))
             (dolist (entry (append (uiop:directory-files
                                     (concatenate 'string native "/"))
                                    (uiop:subdirectories
                                     (concatenate 'string native "/")))
                            out)
               (setf out (fset:with out (p:key (%name entry)) t)))))
          ((probe-file native)
           (uiop:read-file-string native))
          (t nil))))

(defun %write (segments value)
  (let ((native (%native segments)))
    (cond ((null value)
           (uiop:delete-file-if-exists native))
          (t (ensure-directories-exist native)
             (with-open-file (out native :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
               (write-string (princ-to-string value) out))))
    value))

(defun %ls (segments)
  (let ((value (%read segments)))
    (when (fset:map? value) (pine.data:keys value))))

(defun provider ()
  (ns:provider
   (/file/?@rest
    {:read (pine.data:fn [] (%read rest))
     :write (pine.data:fn [v] (%write rest v))
     :ls (pine.data:fn [] (mapcar #'p:name (%ls rest)))
     :doc "a file's contents, or a directory's entries; nil deletes"})))

(defclass server (ns:server) ()
  (:default-initargs :name :file :serves (list /file))
  (:documentation "The filesystem, as a subtree."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  (ns:write /file (provider)))

(ns:register (make-instance 'server))
