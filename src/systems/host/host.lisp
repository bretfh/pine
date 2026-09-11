(defpackage #:pine/host
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs)
                    (#:job #:pine/run/job) (#:module #:pine/run/module)
                    (#:actors #:pine/run/actors) (#:watch #:pine/run/watch)
                    (#:command #:pine/run/command) (#:fault #:pine/run/fault)
                    (#:sh #:pine/host/shell))
  (:import-from #:pine/host/shell #:sh)
  (:export
   #:device #:defdevice #:defdriver #:sh #:needs
   #:make-device #:driver-for #:devices #:unanswered))
(in-package #:pine/host)

(defclass device (fs:mount)
  ((arguments :initarg :arguments :reader arguments :initform nil)
   (rows      :initform nil :accessor rows-of)))

(defgeneric needs (device)
  (:method ((d device)) nil))

(defgeneric rows (device)
  (:method ((d device)) nil))

(defclass row (fs:derived)
  ((row :initarg :row :reader row)))

(defclass unanswered (fs:derived) ())

(defmethod fs:kind ((n unanswered)) :absent)
(defmethod fs:volatile-p ((n unanswered) &optional name) (declare (ignore name)) t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %said (name) (string-downcase (princ-to-string name)))

  (defun %symbol (name)
    "A device's name, as the class it names here: a config's AUDIO and pine's are
one class."
    (intern (string-upcase (princ-to-string name)) :pine/host)))

(defun device-class (name)
  (let* ((s (find-symbol (string-upcase (princ-to-string name)) :pine/host))
         (c (and s (find-class s nil))))
    (and c (subtypep c 'device) c)))

(defun devices ()
  (sort (mapcar (lambda (c) (%said (class-name c)))
                (sb-mop:class-direct-subclasses (find-class 'device)))
        #'string<))

(defun %prototype (class)
  (unless (sb-mop:class-finalized-p class) (sb-mop:finalize-inheritance class))
  (sb-mop:class-prototype class))

(defmacro defdevice (name &key describes announces refreshes)
  (let ((class (%symbol name)) (d (gensym "D")))
    `(progn
       (defclass ,class (device) ()
         (:documentation ,(or describes "")))
       ,@(when announces `((defmethod fs:notified-by ((,d ,class)) ,announces)))
       ,@(when refreshes `((defmethod fs:polls ((,d ,class)) ,refreshes)))
       ',class)))

(defmacro %taking (d takes form)
  (if takes
      `(let ,(loop :for name :in takes
                   :collect `(,name (getf (arguments ,d)
                                          ,(intern (symbol-name name) :keyword))))
         (declare (ignorable ,@takes))
         ,form)
      form))

(defmacro defdriver (name (&key needs announces refreshes takes rows) &body readings)
  (let* ((needs (if (listp needs) needs (list needs)))
         (kind (%symbol name))
         (class (if needs (%symbol (format nil "~a-~a" name (first needs))) kind))
         (d (gensym "D")) (n (gensym "NAME")) (v (gensym "VALUE")))
    `(progn
       ,@(when needs
           `((defclass ,class (,kind) ())
             (defmethod needs ((,d ,class)) ',needs)))
       ,@(when announces
           `((defmethod fs:notified-by ((,d ,class)) (%taking ,d ,takes ,announces))))
       ,@(when refreshes `((defmethod fs:polls ((,d ,class)) ,refreshes)))
       ,@(when rows `((defmethod rows ((,d ,class)) (%taking ,d ,takes ,rows))))
       ,@(when readings
           `((defmethod fs:names ((,d ,class))
               ',(loop :for row :in readings
                       :collect (cons (intern (string-upcase (symbol-name (first row)))
                                              :keyword)
                                      "")))))
       ,@(loop :for row :in readings
               :append (destructuring-bind (word &key reads writes) row
                         (let ((key (intern (string-upcase (symbol-name word)) :keyword)))
                           `((defmethod fs:read ((,d ,class) (,n (eql ,key)))
                               (declare (ignore ,n) (ignorable ,d))
                               (%taking ,d ,takes ,reads))
                             ,@(when writes
                                 `((defmethod fs:write ((,d ,class) (,n (eql ,key)) ,v)
                                     (declare (ignore ,n) (ignorable ,d))
                                     (funcall (%taking ,d ,takes ,writes) ,v))))))))
       ',class)))

(defun %kind (d)
  (find-if (lambda (c) (member (find-class 'device) (sb-mop:class-direct-superclasses c)))
           (sb-mop:class-precedence-list (class-of d))))

(defun %ways (kind)
  (reverse (sb-mop:class-direct-subclasses kind)))

(defun driver-for (class)
  (and class
       (find-if (lambda (c) (every #'sh:has (needs (%prototype c)))) (%ways class))))

(defun readings (d)
  (append (remove-duplicates
           (loop :for c :in (cons (%kind d) (%ways (%kind d)))
                 :append (mapcar (lambda (key) (%said key))
                                 (fs:served (%prototype c))))
           :test #'equal :from-end t)
          (mapcar #'first (rows-of d))))

(defmethod initialize-instance :after ((d device) &key)
  (setf (rows-of d) (rows d)))

(defmethod fs:volatile-p ((d device) &optional name)
  (if name nil t))

(defmethod fs:read :around ((d device) name)
  (declare (ignore name))
  (fs:depend-on d)
  (call-next-method))

(defmethod fs:works ((n row))
  (fs:depend-on (fs:parent n))
  (funcall (second (row n))))

(defmethod fs:takes ((n row) value)
  (let ((writes (third (row n))))
    (unless writes (error "~a only answers, and takes no writing." (fs:full-name n)))
    (funcall writes value)))

(defmethod fs:contents ((d device)) (readings d))

(defmethod fs:child ((d device) name)
  (let* ((word (princ-to-string name))
         (row (find word (rows-of d) :key #'first :test #'equal)))
    (cond (row (fs:ensure-child d word (lambda () (make-instance 'row :name word :parent d :row row))))
          ((member word (readings d) :test #'equal)
           (or (call-next-method)
               (fs:ensure-child d word (lambda () (make-instance 'unanswered :name word :parent d))))))))

(defmethod fs:children ((d device))
  (remove nil (mapcar (lambda (word) (fs:child d word)) (readings d))))

(defun make-device (name &rest arguments)
  (let ((kind (device-class name)))
    (when kind
      (make-instance (class-name (or (driver-for kind) kind))
                     :name (%said (class-name kind))
                     :arguments arguments
                     :describes (documentation kind 'type)))))

(fs:mount (lambda () (make-instance 'fs:mount :describes "the machine, as devices"))
          "/dev")

(defvar *attending* nil)

(setf watch:*streaming* #'sh:streaming)

(defclass host (module:module) ())

(defun %make (name &rest arguments)
  (apply #'make-device name arguments))

(defun device (what &rest arguments)
  (let ((n (if (fs:nodep what)
               what
               (let ((it (apply #'%make what arguments)))
                 (when it
                   (fs:mount it (fs:at "/dev")))))))
    (when (and (fs:nodep n) fs:*owner*) (setf (fs:owner n) fs:*owner*))
    (%attend n)))

(defun %attend (n)
  (when (fs:nodep n)
    (let ((held (watch:following n)))
      (sb-ext:atomic-update *attending* (lambda (all) (cons (list n held) all)))))
  n)

(defun attending () (mapcar #'first *attending*))

(defun leave ()
  (dolist (each *attending*)
    (destructuring-bind (n held) each
      (declare (ignore n))
      (watch:let-go held)))
  (setf *attending* nil)
  (sh:forget-all))

(command:defcommand "devices" () (:describes "what the machine has")
  (mapcar #'fs:name (fs:children (fs:at (fs:root) "dev"))))

(command:defcommand "device" (name &rest arguments)
    (:describes "put a device in the tree")
  (let ((it (apply #'device name arguments)))
    (and it (fs:full-name it))))

(command:defcommand "sh" (line) (:describes "run something")
  (sh:run-line (princ-to-string line)))

(defmethod job:start ((s host))
  (fs:mount (sh:sh-node) "/sh")
  (device (fs:mount (make-device "env") "/env"))
  (device (fs:mount (make-device "sys") "/sys"))
  (fs:mount #p"/" "/file")
  (device "clock")
  (job:supervise
   (job:start (make-instance 'job:tick :name "clock" :every 1
                                         :on-fault :leave
                                         :body #'tick)))
  s)

(defmethod job:stop ((s host))
  (leave)
  s)

(defvar *now* (get-universal-time))
(defvar *sampled* nil)
(defvar *busy* nil)

(defun %sinks ()
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
  (lambda (said) (declare (ignore said)) (sh:did "~a" line) t))

(defun %hands (&rest readings)
  (lambda (said) (apply #'sh:argv (append readings (list said))) t))

(defun %default-sink ()
  (getf (find-if (lambda (each) (getf each :default)) (%sinks)) :name))

(defdevice audio
  :describes "the default sink: how loud, whether it is muted, and what
else there is to play through"
  :announces '("pactl subscribe"))

(defdriver audio (:needs "wpctl")
  (volume :reads  (%volume)
          :writes (lambda (said)
                    (sh:argv "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@"
                             (format nil "~d%" (%clamped said)))))
  (muted  :reads  (%mutedp)
          :writes (%runs "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))
  (sinks  :reads  (%sinks)  :writes (%hands "wpctl" "set-default"))
  (sink   :reads  (%default-sink) :writes (%hands "wpctl" "set-default")))

(defdriver audio (:needs "pamixer")
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

(defdevice screen
  :describes "the backlight, as a percentage"
  :refreshes 5)

(defdriver screen (:needs "brightnessctl")
  (brightness :reads  (%brightness)
              :writes (lambda (said)
                        (when (%backlight)
                          (sh:argv "brightnessctl" "--class=backlight" "set"
                                   (format nil "~d%" (max 1 (%clamped said))))))))

(defdriver screen (:needs "light")
  (brightness :reads  (%brightness)
              :writes (lambda (said)
                        (sh:argv "light" "-S" (max 1 (%clamped said))))))

(defdriver screen ()
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

(defdevice power
  :describes "the battery, and lock suspend reboot poweroff logout"
  :refreshes 10)

(defdriver power (:needs ("systemctl" "loginctl"))
  (battery  :reads (%battery))
  (state    :reads (%charge))
  (charging :reads (eq :charging (%charge)))
  (lock     :reads "lock"     :writes (%runs "loginctl lock-session"))
  (suspend  :reads "suspend"  :writes (%runs "systemctl suspend"))
  (reboot   :reads "reboot"   :writes (%runs "systemctl reboot"))
  (poweroff :reads "poweroff" :writes (%runs "systemctl poweroff"))
  (logout   :reads "logout"
            :writes (%runs "loginctl terminate-session $XDG_SESSION_ID")))

(defdriver power ()
  (battery  :reads (%battery))
  (state    :reads (%charge))
  (charging :reads (eq :charging (%charge))))

(defun %wifi ()
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

(defdevice clip
  :describes "the desktop's clipboard")

(defun %copies (line)
  (lambda (said) (sh:feed line (princ-to-string said)) t))

(defdriver clip (:needs "wl-paste" :announces '("wl-paste --watch echo"))
  (text :reads  (%said-or-nothing (sh:sh "wl-paste --no-newline 2>/dev/null"))
        :writes (%copies "wl-copy")))

(defdriver clip (:needs "xclip" :refreshes 2)
  (text :reads  (%said-or-nothing
                 (sh:sh "xclip -o -selection clipboard 2>/dev/null"))
        :writes (%copies "xclip -i -selection clipboard")))

(defun %route-device ()
  (let* ((said (sh:sh "ip -o route get 1.1.1.1 2>/dev/null"))
         (at (search " dev " said)))
    (when at (first (sh:words (subseq said (+ at 5)))))))

(defdevice net
  :describes "what is connected, and what else is in the air")

(defdriver net (:needs "nmcli" :announces '("nmcli monitor"))
  (connection :reads (%said-or-nothing
                      (or (sh:first-line (sh:sh "nmcli -t -f NAME connection show --active"))
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

(defdriver net (:needs "ip" :refreshes 10)
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
  (lambda (said)
    (declare (ignore said))
    (apply #'sh:argv "playerctl"
           (append (when player (list "-p" player)) (list verb)))
    t))

(defdevice media :describes "what is playing, through mpris")

(defdriver media
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

(defdevice clock :describes "the time, as paths" :refreshes 1)

(defdriver clock ()
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

(defdevice sys
  :describes "the machine: cpu, ram, temperature, uptime, load"
  :refreshes 3)

(defdriver sys ()
  (cpu    :reads (%cpu))
  (ram    :reads (%ram))
  (disk   :reads (%disk))
  (temp   :reads (%temp))
  (uptime :reads (%uptime))
  (load   :reads (%load))
  (user   :reads (uiop:getenv "USER"))
  (host   :reads (uiop:hostname)))

(defun %environment-rows ()
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

(defdevice env
  :describes "the environment this image was started in")

(defdriver env (:rows (%environment-rows)))

