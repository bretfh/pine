(defpackage #:pine.provider.sys
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:c #:pine.run.cell)
                    (#:sh #:pine.provider.sh))
  (:export #:sys-node #:install #:cpu #:ram #:temp #:disk #:uptime #:load-average
           #:host #:user))

(in-package #:pine.provider.sys)

(defvar *sampled* (c:cell nil))

(defparameter +parts+
  '("cpu" "ram" "temp" "disk" "uptime" "load" "user" "host"))

(defclass sys-node (node:node) ())

(defclass reading-node (node:node) ())

(defun %file (path)
  (when (probe-file path)
    (ignore-errors (uiop:read-file-string path))))

(defun %lines (text)
  (uiop:split-string (string-right-trim '(#\Newline) (or text ""))
                     :separator '(#\Newline)))

(defun %first-number (text)
  (let ((start (position-if #'digit-char-p (or text ""))))
    (when start
      (let ((end (or (position-if-not #'digit-char-p text :start start)
                     (length text))))
        (parse-integer text :start start :end end)))))

(defun %starts-with (text prefix)
  (and (>= (length text) (length prefix))
       (string= prefix text :end2 (length prefix))))

(defun cpu ()
  (let* ((line (first (%lines (%file "/proc/stat"))))
         (numbers (loop :for word :in (rest (uiop:split-string
                                             (string-trim " " (or line ""))
                                             :separator '(#\Space)))
                        :for n := (and (plusp (length word))
                                       (every #'digit-char-p word)
                                       (parse-integer word))
                        :when n :collect n))
         (total (reduce #'+ numbers :initial-value 0))
         (idle (+ (or (nth 3 numbers) 0) (or (nth 4 numbers) 0)))
         (before (c:swap *sampled* (lambda (had)
                                     (declare (ignore had))
                                     (cons total idle)))))
    (declare (ignore before))
    (let ((had (c:held *sampled*)))
      (if (and had (consp had) (plusp (- total (car had))))
          (let ((moved (- total (car had)))
                (still (- idle (cdr had))))
            (max 0 (min 100 (round (* 100 (- moved still)) moved))))
          0))))

(defun ram ()
  (let ((total 0) (available 0))
    (dolist (line (%lines (%file "/proc/meminfo")))
      (cond ((%starts-with line "MemTotal:")
             (setf total (or (%first-number line) 0)))
            ((%starts-with line "MemAvailable:")
             (setf available (or (%first-number line) 0)))))
    (if (plusp total) (round (* 100 (- total available)) total) 0)))

(defun temp ()
  (dolist (sensor (directory "/sys/class/hwmon/*/") 0)
    (let ((name (string-trim '(#\Newline)
                             (or (%file (merge-pathnames "name" sensor)) ""))))
      (when (member name '("k10temp" "coretemp" "zenpower") :test #'equal)
        (let ((raw (%first-number (%file (merge-pathnames "temp1_input" sensor)))))
          (when raw (return (round raw 1000))))))))

(defun disk (&optional (mount "/"))
  (let ((line (second (%lines (sh:output-of (format nil "df --output=pcent ~a" mount))))))
    (or (and line (%first-number line)) 0)))

(defun uptime ()
  (round (or (%first-number (%file "/proc/uptime")) 0)))

(defun load-average ()
  (let ((words (uiop:split-string
                (string-trim '(#\Newline) (or (%file "/proc/loadavg") ""))
                :separator '(#\Space))))
    (mapcar (lambda (w) (or (ignore-errors (read-from-string w)) 0))
            (subseq words 0 (min 3 (length words))))))

(defun user () (uiop:getenv "USER"))

(defun host () (uiop:hostname))

(defun %reading (name)
  (cond ((equal name "cpu") (cpu))
        ((equal name "ram") (ram))
        ((equal name "temp") (temp))
        ((equal name "disk") (disk))
        ((equal name "uptime") (uptime))
        ((equal name "load") (load-average))
        ((equal name "user") (user))
        ((equal name "host") (host))))

(defmethod node:nodes ((n sys-node))
  (loop :for name :in +parts+
        :collect (make-instance 'reading-node :name name :parent n)))

(defmethod node:resolve ((n sys-node) name)
  (when (member name +parts+ :test #'equal)
    (make-instance 'reading-node :name name :parent n)))

(defmethod node:contents ((n sys-node)) +parts+)
(defmethod node:contents ((n reading-node)) (%reading (node:name n)))
(defmethod node:leafp ((n reading-node)) t)
(defmethod node:persistp ((n sys-node)) nil)
(defmethod node:persistp ((n reading-node)) nil)

(defmethod node:livep ((n sys-node)) t)
(defmethod node:livep ((n reading-node)) t)

(defun install (root)
  (node:attach (make-instance 'sys-node :name "sys"
                                        :describes "the machine: cpu, ram, temperature, disk, uptime, load")
               root))
