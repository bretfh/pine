(defpackage #:pine/host/device
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:sh #:pine/host/shell))
  (:export #:device #:readings #:attach #:audio #:screen #:power #:net #:media
           #:clip #:clock #:tick #:sys #:env))
(in-package #:pine/host/device)

(defvar *now* (d:box 0))
(defvar *sampled* (d:box nil))
(defvar *busy* (d:box nil))

(defparameter +power-verbs+
  '(("lock" . "loginctl lock-session")
    ("suspend" . "systemctl suspend")
    ("reboot" . "systemctl reboot")
    ("poweroff" . "systemctl poweroff")
    ("logout" . "loginctl terminate-session $XDG_SESSION_ID")))

(defclass device (node:node)
  ((readings :initarg :readings :reader readings)
   (announces :initarg :announces :initform nil :reader announces)
   (refreshes :initarg :refreshes :initform nil :reader refreshes)
   (livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "Something the machine has. Its readings are rows -- a name, how
to read it, and how to write it -- not a class each.

That is what DERIVED taking a write function is for: /dev/audio was four classes and
twelve methods, and it is this list now."))

(defmethod node:announces ((n device)) (announces n))
(defmethod node:refreshes ((n device)) (refreshes n))

(defmethod node:stir :after ((n device))
  "What a device is told is that the world behind it moved, and what reads the world
is the rows. Stirring one without the other leaves every reading at whatever it
answered the first time."
  (dolist (row (node:nodes n)) (node:stir row)))

(defun %reading (n row)
  (destructuring-bind (name reads &optional writes) row
    (let ((name (string name)))
      (node:child n name
                  (lambda () (node:derive name reads :over n :writes writes))))))

(defmethod node:nodes ((n device))
  (mapcar (lambda (row) (%reading n row)) (readings n)))

(defmethod node:resolve ((n device) name)
  (let ((row (find (princ-to-string name) (readings n)
                   :key (lambda (r) (string (first r)))
                   :test #'equal)))
    (when row (%reading n row))))

(defmethod node:contents ((n device))
  (mapcar (lambda (r) (string (first r))) (readings n)))

(defun device (name readings &key announces refreshes describes)
  (make-instance 'device :name name :readings readings :announces announces
                         :refreshes refreshes :describes describes))

(defun %sinks ()
  "Every sink wireplumber lists, and which one is the default."
  (loop :for line :in (sh:lines (sh:sh "wpctl status"))
        :with inp := nil
        :do (cond ((search "Sinks:" line) (setf inp t))
                  ((and inp (search "Sources:" line)) (setf inp nil)))
        :when (and inp (search "." line) (not (search "Sinks:" line)))
          :collect (let* ((at (position #\. line))
                          (name (string-trim " " (subseq line (1+ at))))
                          (defaultp (search "*" (subseq line 0 (min 8 (length line))))))
                     (list :name (if (search "[vol:" name)
                                     (string-trim " " (subseq name 0 (search "[vol:" name)))
                                     name)
                           :default (and defaultp t)
                           :id (sh:number-in line)))))

(defun audio ()
  (flet ((level () (let ((n (sh:number-in (sh:sh "wpctl get-volume @DEFAULT_AUDIO_SINK@"))))
                     (when n (round (* 100 n)))))
         (muted () (and (search "MUTED" (sh:sh "wpctl get-volume @DEFAULT_AUDIO_SINK@")) t)))
    (device "audio"
            (list (list "volume" #'level
                        (lambda (v) (sh:sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ ~d%"
                                           (max 0 (min 100 v)))))
                  (list "muted" #'muted
                        (lambda (v) (declare (ignore v))
                          (sh:sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")))
                  (list "sinks" #'%sinks
                        (lambda (v) (sh:sh "wpctl set-default ~a" v) t))
                  (list "sink" (lambda ()
                                 (let ((it (find-if (lambda (each) (getf each :default))
                                                    (%sinks))))
                                   (getf it :name)))
                        (lambda (v) (sh:sh "wpctl set-default ~a" v) t)))
            :announces '("pactl subscribe")
            :describes "the default sink: how loud, whether it is muted, and what
else there is to play through")))

(defun screen ()
  (flet ((where () (first (directory "/sys/class/backlight/*/")))
         (level ()
           (let ((at (first (directory "/sys/class/backlight/*/"))))
             (when at
               (let ((now (sh:number-in (sh:sh "cat ~abrightness 2>/dev/null"
                                            (namestring at))))
                     (most (sh:number-in (sh:sh "cat ~amax_brightness 2>/dev/null"
                                             (namestring at)))))
                 (when (and now most (plusp most)) (round (* 100 now) most)))))))
    (device "screen"
            (list (list "brightness" #'level
                        (lambda (v)
                          (when (where)
                            (sh:sh "brightnessctl --class=backlight set ~d%"
                                   (max 1 (min 100 v)))))))
            :refreshes 5
            :describes "the backlight, as a percentage")))

(defun power ()
  (flet ((supply ()
           (or (first (directory "/sys/class/power_supply/BAT*/"))
               (first (remove-if-not
                       (lambda (each) (probe-file (merge-pathnames "capacity" each)))
                       (directory "/sys/class/power_supply/*/"))))))
    (labels ((battery () (let ((at (supply)))
                           (when at (sh:number-in (sh:sh "cat ~acapacity 2>/dev/null"
                                                      (namestring at))))))
             (state () (let ((at (supply)))
                         (when at
                           (let ((said (sh:sh "cat ~astatus 2>/dev/null"
                                              (namestring at))))
                             (cond ((search "Charging" said) :charging)
                                   ((search "Discharging" said) :discharging)
                                   ((search "Full" said) :full)
                                   ((search "Not charging" said) :idle)
                                   ((plusp (length said)) :unknown)))))))
      (device "power"
              (append (list (list "battery" #'battery)
                            (list "state" #'state)
                            (list "charging" (lambda () (eq :charging (state)))))
                      (loop :for (name . line) :in +power-verbs+
                            :collect (list name (constantly name)
                                           (let ((line line))
                                             (lambda (v) (declare (ignore v))
                                               (sh:sh "~a" line) t)))))
              :refreshes 10
              :describes "the battery, and lock suspend reboot poweroff logout"))))

(defun %wifi ()
  "What is in the air, strongest first: what it is called, how strong, whether it
wants a password and whether it is the one we are on."
  (let (found)
    (dolist (line (sh:lines (sh:sh "nmcli -t -f IN-USE,SSID,SIGNAL,SECURITY device wifi"))
                  (sort (nreverse found) #'> :key (lambda (each) (getf each :signal))))
      (let ((parts (uiop:split-string line :separator '(#\:))))
        (when (and (>= (length parts) 4) (plusp (length (second parts))))
          (push (list :ssid (second parts)
                      :signal (or (parse-integer (third parts) :junk-allowed t) 0)
                      :secure (plusp (length (string-trim " " (fourth parts))))
                      :in-use (equal "*" (first parts)))
                found))))))

(defun clip ()
  "What the desktop is holding, as a place. Reading it is what anything else on
this machine copied; writing it is copying."
  (device "clip"
          (list (list "text"
                      (lambda ()
                        (let ((said (sh:sh "wl-paste --no-newline 2>/dev/null")))
                          (when (plusp (length said)) said)))
                      (lambda (v)
                        (sh:feed "wl-copy" (princ-to-string v))
                        t)))
          :announces '("wl-paste --watch echo")
          :describes "the desktop's clipboard"))

(defun net ()
  (flet ((connection ()
           (let ((line (sh:firstp (sh:sh "nmcli -t -f NAME connection show --active"))))
             (when (and line (plusp (length line))) line)))
         (online () (and (search "connected" (sh:sh "nmcli -t -f STATE general")) t)))
    (device "net"
            (list (list "connection" #'connection)
                  (list "online" #'online)
                  (list "wifi" #'%wifi
                        (lambda (v)
                          "An ssid connects to it. :rescan looks again."
                          (if (eq :rescan v)
                              (sh:sh "nmcli device wifi rescan")
                              (sh:sh "nmcli device wifi connect ~s" v))
                          t)))
            :announces '("nmcli monitor")
            :describes "what is connected, and what else is in the air")))

(defun %seconds (said)
  (let ((n (sh:number-in said)))
    (when n (round n))))

(defun media (&key player)
  (flet ((ask (what) (sh:sh "playerctl~@[ -p ~a~] ~a 2>/dev/null" player what)))
    (labels ((meta (key) (let ((said (ask (format nil "metadata ~a" key))))
                           (when (plusp (length said)) said))))
      (device "media"
              (append
               (list (list "status" (lambda ()
                                      (let ((said (ask "status")))
                                        (cond ((search "Playing" said) :playing)
                                              ((search "Paused" said) :paused)
                                              ((plusp (length said)) :stopped)))))
                     (list "title" (lambda () (meta "xesam:title")))
                     (list "artist" (lambda () (meta "xesam:artist")))
                     (list "album" (lambda () (meta "xesam:album")))
                     (list "art" (lambda () (meta "mpris:artUrl")))
                     (list "position" (lambda () (sh:number-in (ask "position"))))
                     (list "length" (lambda ()
                                      (let ((said (meta "mpris:length")))
                                        (when said
                                          (let ((n (sh:number-in said)))
                                            (when n (round n 1000000))))))))
               (loop :for verb :in '("play" "pause" "next" "previous" "stop")
                     :collect (list verb (constantly verb)
                                    (let ((verb verb))
                                      (lambda (v) (declare (ignore v))
                                        (ask verb) t)))))
              :announces (list (format nil "playerctl~@[ -p ~a~] --follow status"
                                       player))
              :describes "what is playing, through mpris"))))

(defun %part (name at)
  (multiple-value-bind (second minute hour day month year weekday)
      (decode-universal-time at)
    (cond ((equal name "second") second)
          ((equal name "minute") (format nil "~2,'0d" minute))
          ((equal name "hour") (format nil "~2,'0d" hour))
          ((equal name "day") day)
          ((equal name "month") month)
          ((equal name "year") year)
          ((equal name "weekday") weekday))))

(defun clock ()
  (d:put! *now* (get-universal-time))
  (device "clock"
          (loop :for name :in '("second" "minute" "hour" "day" "month" "year"
                                "weekday")
                :collect (list name (let ((name name))
                                      (lambda () (%part name (d:held *now*))))))
          :refreshes 1
          :describes "the time, as paths"))

(defun tick ()
  (d:put! *now* (get-universal-time)))

(defun %file (path)
  (when (probe-file path) (ignore-errors (uiop:read-file-string path))))

(defun %busy ()
  (let* ((line (first (sh:lines (%file "/proc/stat"))))
         (numbers (loop :for word :in (rest (uiop:split-string
                                             (string-trim " " (or line ""))
                                             :separator '(#\Space)))
                        :for n := (and (plusp (length word))
                                       (every #'digit-char-p word)
                                       (parse-integer word))
                        :when n :collect n))
         (total (reduce #'+ numbers :initial-value 0))
         (idle (+ (or (nth 3 numbers) 0) (or (nth 4 numbers) 0)))
         (had (d:held *sampled*)))
    (d:put! *sampled* (cons total idle))
    (if (and had (consp had) (plusp (- total (car had))))
        (let ((moved (- total (car had))) (still (- idle (cdr had))))
          (max 0 (min 100 (round (* 100 (- moved still)) moved))))
        0)))

(defun %cpu ()
  "How busy the machine is. Two readers a moment apart are asking about the same
moment: sampling per read gives the second one no ticks to divide by."
  (let ((now (get-internal-real-time))
        (had (d:held *busy*)))
    (cond ((and had (< (- now (cdr had)) internal-time-units-per-second)) (car had))
          (t (let ((said (%busy))) (d:put! *busy* (cons said now)) said)))))

(defun %ram ()
  (let ((total 0) (free 0))
    (dolist (line (sh:lines (%file "/proc/meminfo")))
      (cond ((eql 0 (search "MemTotal:" line))
             (setf total (or (sh:number-in line) 0)))
            ((eql 0 (search "MemAvailable:" line))
             (setf free (or (sh:number-in line) 0)))))
    (if (plusp total) (round (* 100 (- total free)) total) 0)))

(defun %temp ()
  (dolist (sensor (directory "/sys/class/hwmon/*/") 0)
    (let ((name (string-trim '(#\Newline)
                             (or (%file (merge-pathnames "name" sensor)) ""))))
      (when (member name '("k10temp" "coretemp" "zenpower") :test #'equal)
        (let ((raw (sh:number-in (%file (merge-pathnames "temp1_input" sensor)))))
          (when raw (return (round raw 1000))))))))

(defun %disk ()
  "How full the filesystem this is running on is, as a percentage."
  (let* ((said (sh:sh "df -P / | tail -1"))
         (pct (find-if (lambda (word) (find #\% word)) (sh:words said))))
    (or (and pct (sh:number-in pct)) 0)))

(defun sys ()
  (device "sys"
          (list (list "cpu" #'%cpu)
                (list "ram" #'%ram)
                (list "disk" #'%disk)
                (list "temp" #'%temp)
                (list "uptime" (lambda () (round (or (sh:number-in (%file "/proc/uptime")) 0))))
                (list "load" (lambda ()
                               (mapcar (lambda (w) (or (ignore-errors
                                                        (read-from-string w))
                                                       0))
                                       (subseq (uiop:split-string
                                                (string-trim '(#\Newline)
                                                             (or (%file "/proc/loadavg") ""))
                                                :separator '(#\Space))
                                               0 3))))
                (list "user" (lambda () (uiop:getenv "USER")))
                (list "host" (lambda () (uiop:hostname))))
          :refreshes 3
          :describes "the machine: cpu, ram, temperature, uptime, load"))

(defun env ()
  (device "env"
          (loop :for entry :in (sb-ext:posix-environ)
                :for name := (subseq entry 0 (position #\= entry))
                :collect (list name
                               (let ((name name)) (lambda () (uiop:getenv name)))
                               (let ((name name))
                                 (lambda (v)
                                   (if (null v)
                                       (sb-posix:unsetenv name)
                                       (sb-posix:setenv name (princ-to-string v) 1))
                                   v))))
          :describes "the environment this image was started in"))

(defun attach (root name device)
  (node:attach device (tree:ensure root name))
  device)
