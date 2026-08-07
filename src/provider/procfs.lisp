(defpackage #:pine.provider.procfs
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns))
  (:export #:procfs))

(in-package #:pine.provider.procfs)
(named-readtables:in-readtable pine.path:syntax)

;;;; The machine, from /proc and sysfs. Nothing above /sys learns which file
;;;; any of it came out of.

(defun %lines (text)
  (uiop:split-string (string-right-trim '(#\newline) text) :separator '(#\newline)))

(defun %first-number (text)
  (let ((start (position-if #'digit-char-p text)))
    (when start
      (let ((end (or (position-if-not #'digit-char-p text :start start)
                     (length text))))
        (parse-integer text :start start :end end)))))

(defun %file (path)
  (pine.provider.out:file path))

(defun %starts-with (text prefix)
  (and (>= (length text) (length prefix))
       (string= prefix text :end2 (length prefix))))

;;;; cpu is the delta since the last look. The (total . idle) it is measured
;;;; against is the server's, so two spaces reading /sys/cpu do not take each
;;;; other's sample, and an atomic swap, so two threads each get a delta rather
;;;; than a torn pair.

(defun %sampled (total idle)
  "Note (TOTAL . IDLE) as the latest sample and answer the one before it."
  (let ((cell (ns:kept :sys)))
    (when cell
      (car (sento.atomic:atomic-swap
            cell
            (lambda (had) (cons (cons total idle) (car had))))))))

(defun cpu ()
  (let* ((line (first (%lines (or (%file "/proc/stat") ""))))
         (numbers (loop :for word :in (rest (uiop:split-string
                                             (string-trim " " (or line ""))
                                             :separator '(#\space)))
                        :for n = (and (plusp (length word))
                                      (every #'digit-char-p word)
                                      (parse-integer word))
                        :when n :collect n))
         (total (reduce #'+ numbers :initial-value 0))
         (idle (+ (or (nth 3 numbers) 0) (or (nth 4 numbers) 0))))
    (let ((before (%sampled total idle)))
      (if before
          (let ((moved (- total (car before)))
                (still (- idle (cdr before))))
            (if (plusp moved)
                (max 0 (min 100 (round (* 100 (- moved still)) moved)))
                0))
          0))))

(defun ram ()
  (let ((total 0) (available 0))
    (dolist (line (%lines (or (%file "/proc/meminfo") "")))
      (cond ((%starts-with line "MemTotal:") (setf total (or (%first-number line) 0)))
            ((%starts-with line "MemAvailable:")
             (setf available (or (%first-number line) 0)))))
    (if (plusp total) (round (* 100 (- total available)) total) 0)))

(defun temp ()
  (dolist (sensor (directory "/sys/class/hwmon/*/") 0)
    (let ((name (string-trim '(#\newline)
                             (or (%file (namestring (merge-pathnames "name" sensor))) ""))))
      (when (member name '("k10temp" "coretemp" "zenpower") :test #'equal)
        (let ((raw (%first-number (or (%file (namestring (merge-pathnames "temp1_input" sensor)))
                                      ""))))
          (when raw (return (round raw 1000))))))))

(defun disk (&optional (mount "/"))
  (let ((line (second (%lines (pine.provider.out:sh "df --output=pcent ~a" mount)))))
    (or (and line (%first-number line)) 0)))

(defun uptime ()
  (round (or (%first-number (or (%file "/proc/uptime") "")) 0)))

(defun load-average ()
  (let ((words (uiop:split-string (string-trim '(#\newline)
                                               (or (%file "/proc/loadavg") ""))
                                  :separator '(#\space))))
    (fset:convert 'fset:seq
                  (mapcar (lambda (w) (or (ignore-errors (read-from-string w)) 0))
                          (subseq words 0 (min 3 (length words)))))))

(defun procfs ()
  "The machine, out of /proc and sysfs."
  (ns:provider
   (/sys/disk/?@mount {:read (pine.data:fn [] (disk (format nil "/~{~a~^/~}" mount)))
                       :doc "percent used of the filesystem holding it"})
   (/sys/cpu    {:read #'cpu    :doc "percent busy since the last look"})
   (/sys/ram    {:read #'ram    :doc "percent used"})
   (/sys/temp   {:read #'temp   :doc "degrees C"})
   (/sys/disk   {:read (pine.data:fn [] (disk "/")) :doc "percent used of /"})
   (/sys/uptime {:read #'uptime :doc "seconds since boot"})
   (/sys/load   {:read #'load-average :doc "1, 5 and 15 minute load"})
   (/sys/user   {:read (pine.data:fn [] (uiop:getenv "USER")) :doc "who is logged in"})
   (/sys/host   {:read (pine.data:fn [] (uiop:hostname)) :doc "this machine's name"})
   (/sys {:ls (pine.data:fn []
                '("cpu" "ram" "temp" "disk" "uptime" "load" "user" "host"))
          :doc "the machine"})))

(ns:serve :sys
  {:at [/sys]
   :doc "the machine: cpu, ram, temperature, disk, uptime, load"
   ;; the cell is what the cpu delta is measured against
   :up (lambda ()
         (ns:write /sys (procfs))
         (sento.atomic:make-atomic-reference :value nil))})
