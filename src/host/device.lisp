(defpackage #:pine/host/device
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:fault #:pine/run/fault)
                    (#:sh #:pine/host/shell) (#:declared #:pine/host/declared))
  (:documentation "The devices pine ships, every one of them a declaration.

Nothing here is a function anybody calls to get a device. What this file holds is
what a config or a system of your own holds: DEFDEVICE for what the machine may
have, and DEFBACKING for each way of asking this machine about it.")
  (:export #:tick))
(in-package #:pine/host/device)

(defvar *now* (get-universal-time))
(defvar *sampled* nil)
(defvar *busy* nil)

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

(defun %volume ()
  (let ((n (sh:number-in (sh:sh "wpctl get-volume @DEFAULT_AUDIO_SINK@"))))
    (when n (round (* 100 n)))))

(defun %mutedp ()
  (and (search "MUTED" (sh:sh "wpctl get-volume @DEFAULT_AUDIO_SINK@")) t))

(defun %clamped (v) (max 0 (min 100 v)))

(defun %runs (line)
  "A write that runs LINE whatever was written to it. That is what a verb under a
device is: /dev/power/suspend is not a value somebody sets, it is a thing to do."
  (lambda (said) (declare (ignore said)) (sh:did "~a" line) t))

(defun %hands (&rest words)
  "A write that hands what was written to a program, as its last argument.

WORDS and not a line: what is written to a device is a value, and a value spliced
into a line of shell is a value that can say anything the shell can. A sink is
named by whoever named it and a network by whoever is broadcasting it, and neither
of them is somebody pine gets to trust."
  (lambda (said) (apply #'sh:argv (append words (list said))) t))

(defun %default-sink ()
  (getf (find-if (lambda (each) (getf each :default)) (%sinks)) :name))

(declared:defdevice audio
  :describes "the default sink: how loud, whether it is muted, and what
else there is to play through"
  :announces '("pactl subscribe"))

(declared:defbacking audio (:needs "wpctl")
  (volume :reads  (%volume)
          :writes (lambda (said)
                    (sh:argv "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@"
                             (format nil "~d%" (%clamped said)))))
  (muted  :reads  (%mutedp)
          :writes (%runs "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))
  (sinks  :reads  (%sinks)  :writes (%hands "wpctl" "set-default"))
  (sink   :reads  (%default-sink) :writes (%hands "wpctl" "set-default")))

(declared:defbacking audio (:needs "pamixer")
  (volume :reads  (sh:number-in (sh:sh "pamixer --get-volume"))
          :writes (lambda (said)
                    (sh:argv "pamixer" "--set-volume" (%clamped said))))
  (muted  :reads  (and (search "true" (sh:sh "pamixer --get-mute")) t)
          :writes (%runs "pamixer --toggle-mute")))

(defun %backlight () (first (directory "/sys/class/backlight/*/")))

(defun %brightness ()
  (let ((at (%backlight)))
    (when at
      (let ((now (sh:number-in (sh:sh "cat ~abrightness 2>/dev/null"
                                      (namestring at))))
            (most (sh:number-in (sh:sh "cat ~amax_brightness 2>/dev/null"
                                       (namestring at)))))
        (when (and now most (plusp most)) (round (* 100 now) most))))))

(declared:defdevice screen
  :describes "the backlight, as a percentage"
  :refreshes 5)

(declared:defbacking screen (:needs "brightnessctl")
  (brightness :reads  (%brightness)
              :writes (lambda (said)
                        (when (%backlight)
                          (sh:argv "brightnessctl" "--class=backlight" "set"
                                   (format nil "~d%" (max 1 (%clamped said))))))))

(declared:defbacking screen (:needs "light")
  (brightness :reads  (%brightness)
              :writes (lambda (said)
                        (sh:argv "light" "-S" (max 1 (%clamped said))))))

(declared:defbacking screen ()
  (brightness :reads (%brightness)))

(defun %supply ()
  (or (first (directory "/sys/class/power_supply/BAT*/"))
      (first (remove-if-not
              (lambda (each) (probe-file (merge-pathnames "capacity" each)))
              (directory "/sys/class/power_supply/*/")))))

(defun %battery ()
  (let ((at (%supply)))
    (when at (sh:number-in (sh:sh "cat ~acapacity 2>/dev/null" (namestring at))))))

(defun %charge ()
  (let ((at (%supply)))
    (when at
      (let ((said (sh:sh "cat ~astatus 2>/dev/null" (namestring at))))
        (cond ((search "Charging" said) :charging)
              ((search "Discharging" said) :discharging)
              ((search "Full" said) :full)
              ((search "Not charging" said) :idle)
              ((plusp (length said)) :unknown))))))

(declared:defdevice power
  :describes "the battery, and lock suspend reboot poweroff logout"
  :refreshes 10)

(declared:defbacking power (:needs ("systemctl" "loginctl"))
  (battery  :reads (%battery))
  (state    :reads (%charge))
  (charging :reads (eq :charging (%charge)))
  (lock     :reads "lock"     :writes (%runs "loginctl lock-session"))
  (suspend  :reads "suspend"  :writes (%runs "systemctl suspend"))
  (reboot   :reads "reboot"   :writes (%runs "systemctl reboot"))
  (poweroff :reads "poweroff" :writes (%runs "systemctl poweroff"))
  (logout   :reads "logout"
            :writes (%runs "loginctl terminate-session $XDG_SESSION_ID")))

(declared:defbacking power ()
  (battery  :reads (%battery))
  (state    :reads (%charge))
  (charging :reads (eq :charging (%charge))))

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

(defun %said-or-nothing (said)
  (when (plusp (length said)) said))

(declared:defdevice clip
  :describes "the desktop's clipboard")

(defun %copies (line)
  "A write that gives what was written to LINE on its standard input."
  (lambda (said) (sh:feed line (princ-to-string said)) t))

(declared:defbacking clip (:needs "wl-paste" :announces '("wl-paste --watch echo"))
  (text :reads  (%said-or-nothing (sh:sh "wl-paste --no-newline 2>/dev/null"))
        :writes (%copies "wl-copy")))

(declared:defbacking clip (:needs "xclip" :refreshes 2)
  (text :reads  (%said-or-nothing
                 (sh:sh "xclip -o -selection clipboard 2>/dev/null"))
        :writes (%copies "xclip -i -selection clipboard")))

(defun %route-device ()
  "The interface the default route goes out of."
  (let* ((said (sh:sh "ip -o route get 1.1.1.1 2>/dev/null"))
         (at (search " dev " said)))
    (when at (first (sh:words (subseq said (+ at 5)))))))

(declared:defdevice net
  :describes "what is connected, and what else is in the air")

(declared:defbacking net (:needs "nmcli" :announces '("nmcli monitor"))
  (connection :reads (%said-or-nothing
                      (or (sh:firstp (sh:sh "nmcli -t -f NAME connection show --active"))
                          "")))
  (online :reads (and (search "connected" (sh:sh "nmcli -t -f STATE general")) t))
  (wifi :reads  (%wifi)
        :writes (lambda (said)
                  "An ssid connects to it. :rescan looks again. An ssid is a name
somebody else is broadcasting, so it goes as an argument and never as a word of a
shell line."
                  (if (eq :rescan said)
                      (sh:argv "nmcli" "device" "wifi" "rescan")
                      (sh:argv "nmcli" "device" "wifi" "connect" said))
                  t)))

(declared:defbacking net (:needs "ip" :refreshes 10)
  (connection :reads (%route-device))
  (online :reads (plusp (length (sh:sh "ip -o route show default 2>/dev/null")))))

(defun %seconds (said)
  (let ((n (sh:number-in said)))
    (when n (round n))))

(defun %player (player what)
  (sh:sh "playerctl~@[ -p ~a~] ~a 2>/dev/null" player what))

(defun %meta (player key)
  (%said-or-nothing (%player player (format nil "metadata ~a" key))))

(defun %tells (player verb)
  "A write that tells the player to do VERB, whatever was written to it. Told and
not asked: pressing pause twice is pausing twice."
  (lambda (said)
    (declare (ignore said))
    (apply #'sh:argv "playerctl"
           (append (when player (list "-p" player)) (list verb)))
    t))

(declared:defdevice media :describes "what is playing, through mpris")

(declared:defbacking media
    (:needs "playerctl" :takes (player)
     :announces (list (format nil "playerctl~@[ -p ~a~] --follow status" player)))
  (status   :reads (let ((said (%player player "status")))
                     (cond ((search "Playing" said) :playing)
                           ((search "Paused" said) :paused)
                           ((plusp (length said)) :stopped))))
  (title    :reads (%meta player "xesam:title"))
  (artist   :reads (%meta player "xesam:artist"))
  (album    :reads (%meta player "xesam:album"))
  (art      :reads (%meta player "mpris:artUrl"))
  (position :reads (sh:number-in (%player player "position")))
  (length   :reads (let ((said (%meta player "mpris:length")))
                     (when said
                       (let ((n (sh:number-in said))) (when n (round n 1000000))))))
  (play     :reads "play"     :writes (%tells player "play"))
  (pause    :reads "pause"    :writes (%tells player "pause"))
  (next     :reads "next"     :writes (%tells player "next"))
  (previous :reads "previous" :writes (%tells player "previous"))
  (stop     :reads "stop"     :writes (%tells player "stop")))

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

(declared:defdevice clock :describes "the time, as paths" :refreshes 1)

(declared:defbacking clock ()
  (second  :reads (%part "second" *now*))
  (minute  :reads (%part "minute" *now*))
  (hour    :reads (%part "hour" *now*))
  (day     :reads (%part "day" *now*))
  (month   :reads (%part "month" *now*))
  (year    :reads (%part "year" *now*))
  (weekday :reads (%part "weekday" *now*)))

(defun tick ()
  (setf *now* (get-universal-time)))

(defun %file (path)
  (when (probe-file path)
    (fault:or-nothing "a file under /proc can go between the look and the read"
      (uiop:read-file-string path))))

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
         (had *sampled*))
    (setf *sampled* (cons total idle))
    (if (and had (consp had) (plusp (- total (car had))))
        (let ((moved (- total (car had))) (still (- idle (cdr had))))
          (max 0 (min 100 (round (* 100 (- moved still)) moved))))
        0)))

(defun %cpu ()
  "How busy the machine is. Two readers a moment apart are asking about the same
moment: sampling per read gives the second one no ticks to divide by."
  (let ((now (get-internal-real-time))
        (had *busy*))
    (cond ((and had (< (- now (cdr had)) internal-time-units-per-second)) (car had))
          (t (let ((said (%busy))) (setf *busy* (cons said now)) said)))))

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

(defun %uptime ()
  (round (or (sh:number-in (%file "/proc/uptime")) 0)))

(defun %load ()
  (mapcar (lambda (w)
            (or (fault:or-nothing "a field that is not a number"
                  (read-from-string w))
                0))
          (subseq (uiop:split-string
                   (string-trim '(#\Newline) (or (%file "/proc/loadavg") ""))
                   :separator '(#\Space))
                  0 3)))

(declared:defdevice sys
  :describes "the machine: cpu, ram, temperature, uptime, load"
  :refreshes 3)

(declared:defbacking sys ()
  (cpu    :reads (%cpu))
  (ram    :reads (%ram))
  (disk   :reads (%disk))
  (temp   :reads (%temp))
  (uptime :reads (%uptime))
  (load   :reads (%load))
  (user   :reads (uiop:getenv "USER"))
  (host   :reads (uiop:hostname)))

(defun %environment-rows ()
  "One row per variable in the environment. Not known until you ask, which is what a
backing's :ROWS is for: the machine says what is there and the rows follow."
  (loop :for entry :in (sb-ext:posix-environ)
        :for name := (subseq entry 0 (position #\= entry))
        :collect (list name
                       (let ((name name)) (lambda () (uiop:getenv name)))
                       (let ((name name))
                         (lambda (said)
                           (if (null said)
                               (sb-posix:unsetenv name)
                               (sb-posix:setenv name (princ-to-string said) 1))
                           said)))))

(declared:defdevice env
  :describes "the environment this image was started in")

(declared:defbacking env (:rows (%environment-rows)))



