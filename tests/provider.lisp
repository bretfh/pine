(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.provider :in :pine)

(defmacro with-providers (&body body)
  `(pine.ns:with-space ()
     (pine.ns:up :file)
     (pine.ns:up :sh)
     (pine.ns:up :env)
     (pine.ns:up :sys)
     ,@body))

;;;; /file

(test a-file-reads-as-its-contents
  (with-providers
    (is (search "cpu" (pine.ns:read /file/proc/stat))
        "the filesystem is mirrored, so it is one path and needs no quoting")))

(test a-file-is-written-and-deleted
  (with-providers
    (let ((where (pine.path:path /file
                                 (pine.path:spliced (uiop:temporary-directory))
                                 "pine-probe")))
      (pine.ns:write where "hello")
      (is (string= "hello" (pine.ns:read where)))
      (pine.ns:write where nil)
      (is (null (pine.ns:read where)) "nil is nothing, here as everywhere"))))

(test a-directory-reads-as-its-entries
  (with-providers
    (let ((entries (pine.ns:read /file/proc)))
      (is (fset:map? entries))
      (is-true (fset:lookup entries :stat)))))

;;;; /sh

(test a-command-reads-as-what-it-said
  (with-providers
    (is (string= "hello" (pine.ns:read /sh/${"echo hello"})))))

(test running-a-command-is-a-verb-and-takes-argv
  (with-providers
    (let ((where (namestring (merge-pathnames "pine-sh-probe"
                                              (uiop:temporary-directory)))))
      (uiop:delete-file-if-exists where)
      (pine.ns:write /sh [:run "touch" where])
      (is-true (loop :repeat 50
                     :when (probe-file where) :return t
                     :do (sleep 0.05)))
      (uiop:delete-file-if-exists where))))

(test what-ran-is-remembered
  (with-providers
    (pine.ns:write /sh [:sh "true"])
    (is (string= "true" (fset:lookup (pine.ns:read /sh) 0)))))

;;;; /env

(test the-environment-reads-and-is-written
  (with-providers
    (is (stringp (pine.ns:read /env/PATH)))
    (pine.ns:write /env/PINE_PROBE "here")
    (is (string= "here" (pine.ns:read /env/PINE_PROBE)))
    (is (string= "here" (uiop:getenv "PINE_PROBE")))
    (pine.ns:write /env/PINE_PROBE nil)
    (is (null (pine.ns:read /env/PINE_PROBE)))))

;;;; /sys

(test the-machine-answers
  (with-providers
    (is (typep (pine.ns:read /sys/cpu) '(integer 0 100)))
    (is (typep (pine.ns:read /sys/ram) '(integer 0 100)))
    (is (typep (pine.ns:read /sys/disk) '(integer 0 100)))
    (is (integerp (pine.ns:read /sys/uptime)))
    (is (= 3 (fset:size (pine.ns:read /sys/load))))
    (is (stringp (pine.ns:read /sys/host)))))

(test a-mount-point-is-part-of-the-path
  (with-providers
    (is (typep (pine.ns:read /sys/disk/tmp) '(integer 0 100)))))

(test the-machine-is-live-so-nothing-stores-it
  (with-providers
    (is (eq :live (pine.ns:kind /sys/cpu)))
    (is (eq :live (pine.ns:kind /file/proc/stat)))))

;;;; /clock

(test the-clock-reads-as-parts
  (pine.ns:with-space ()
    (pine.provider.clock:tick)
    (is (= 2 (length (pine.ns:read /clock/hour))))
    (is (integerp (pine.ns:read /clock/at)))
    (is (integerp (pine.ns:read /clock/year)))))

(test a-surface-that-shows-the-hour-is-rebuilt-when-the-hour-moves
  (pine.ns:with-space ()
    (pine.provider.clock:tick)
    (pine.ns:write /surface/bar (format nil "it is ~a" (pine.ns:read /clock/hour)))
    (is (search (pine.ns:read /clock/hour) (pine.ns:read /surface/bar)))
    (pine.ns:write /clock/hour "23")
    (is (string= "it is 23" (pine.ns:read /surface/bar)))))
