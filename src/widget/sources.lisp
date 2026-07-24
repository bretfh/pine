(defpackage #:pine.source
  (:use #:cl)
  (:export #:start-sources #:stop-sources #:workspaces
           #:defsource #:defpoll #:set! #:ref-of
           #:select! #:act! #:select-sink!))

(in-package #:pine.source)

;;;; Data sources feed reactive cells; widgets read those cells and re-render on
;;;; change. A source is either a STREAM (a subprocess line stream that triggers
;;;; a refresh, event-driven) or a POLL (a refresh on an interval). Either way
;;;; the actual work (shell reads + set-cell) runs on a source actor owned by
;;;; the actor system, off both the reader thread and the timer thread. Sources
;;;; are declared with defsource / defpoll and started together by start-sources.
;;;; This is the runtime behind the widgets' cell reads -- pine's eww broker.

(defun ref-of (name)
  (or (pine.ref:find-ref name) (pine.ref:make-ref :name name)))

(defun set! (name value)
  "Set reactive ref NAME to VALUE (deduped by the ref)."
  (pine.ref:set-ref (ref-of name) value))

;;; shell helpers

(defun sh (&rest args)
  (or (ignore-errors
        (string-trim '(#\newline #\space)
                     (uiop:run-program args :output :string :ignore-error-status t)))
      ""))

(defun %first-number (s)
  (let ((start (position-if (lambda (c) (or (digit-char-p c) (char= c #\.))) s)))
    (when start
      (let ((end (or (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\.)))
                                       s :start start)
                     (length s))))
        (ignore-errors (read-from-string (subseq s start end)))))))

(defun read-int-file (path)
  (ignore-errors (parse-integer (sh "cat" (namestring path)))))

;;; source primitives

(defstruct source name actor process super (stopped nil) (faults 0) (last-fault nil))

(defparameter *source-fault-cap* 6
  "Consecutive faults before a source is disabled instead of retried, so a broken
source backs off and stops rather than spinning or swallowing errors silently.")

(defun %source-ok (src)
  "A refresh succeeded: clear the consecutive-fault counter."
  (setf (source-faults src) 0))

(defun %source-fault (src condition)
  "Record a refresh fault on SRC. Disable the source after *source-fault-cap*
consecutive faults. Returns the seconds to back off before the next attempt
(exponential, capped)."
  (incf (source-faults src))
  (setf (source-last-fault src) (princ-to-string condition))
  (format *error-output* "~&pine source ~a fault ~d: ~a~%"
          (or (source-name src) "?") (source-faults src) condition)
  (when (>= (source-faults src) *source-fault-cap*)
    (setf (source-stopped src) t)
    (format *error-output* "~&pine source ~a disabled after ~d faults~%"
            (or (source-name src) "?") (source-faults src)))
  (min 60 (* 2 (expt 2 (min 5 (source-faults src))))))

(defvar *running* nil "Started sources, or nil.")
(defun source-status ()
  "Every started source as (name faults last-fault stopped) -- so a broken feed
is visible, not silently dead."
  (loop for s in *running*
        collect (list (source-name s) (source-faults s)
                      (source-last-fault s) (source-stopped s))))

(defvar *actor-counter* 0
  "Serial for unique source-actor names; sento rejects a duplicate name, which
would silently drop every source after the first.")

(defun %actor (system)
  "A lightweight identity actor for a source, so it is addressable in the service
registry. The source's blocking IO does NOT run here -- it runs on the source's
own dedicated thread, never on this actor's shared pool worker."
  (sento.actor-context:actor-of system
    :name (format nil "pine-source-~d" (incf *actor-counter*))
    :receive (lambda (msg) (declare (ignore msg)) nil)))

(defparameter *stream-backoff* 2
  "Seconds a supervised stream waits before relaunching a died subprocess.")

(defun start-stream (system command refresh-fn &optional (trigger (constantly t)))
  "Spawn COMMAND under supervision on the source's OWN dedicated thread; when a
stdout line satisfies TRIGGER, run REFRESH-FN (which sets cells) on that same
thread. The blocking subprocess IO never touches the shared pool, so a slow or
flooding source cannot starve the daemon. The supervisor relaunches the
subprocess if it dies (EOF/error) after a backoff. stop-sources sets the stopped
flag and terminates the process so the read hits EOF and the thread exits
cleanly. No thread is ever killed."
  (let ((src (make-source :actor (%actor system))))
    (setf (source-super src)
          (bordeaux-threads:make-thread
           (lambda ()
             (loop until (source-stopped src) do
               (let ((wait *stream-backoff*))
                 ;; a launch failure or a refresh fault is recorded and backed
                 ;; off (and disables the source after the cap); a clean EOF is
                 ;; not a fault, just a relaunch. no silent (error () nil).
                 (handler-case
                     (let ((proc (uiop:launch-program (list "sh" "-c" command) :output :stream)))
                       (setf (source-process src) proc)
                       (funcall refresh-fn) (%source-ok src)   ; initial read
                       (loop with out = (uiop:process-info-output proc)
                             for line = (read-line out nil nil)
                             while line
                             when (funcall trigger line)
                               do (funcall refresh-fn) (%source-ok src)))
                   (error (e) (setf wait (%source-fault src e))))
                 (unless (source-stopped src) (sleep wait)))))
           :name "pine-source-stream"))
    src))

(defun start-poll (system interval refresh-fn)
  "Run REFRESH-FN every INTERVAL seconds on the source's OWN dedicated thread, so
its blocking IO never touches the shared pool. Sleeps in one-second steps so a
stop takes effect promptly."
  (let ((src (make-source :actor (%actor system))))
    (setf (source-super src)
          (bordeaux-threads:make-thread
           (lambda ()
             (loop until (source-stopped src) do
               ;; wait the interval on success, the backoff (growing, then
               ;; disable) on a fault; the fault is recorded, never swallowed.
               (let ((wait (handler-case
                               (progn (funcall refresh-fn) (%source-ok src) (max 1 interval))
                             (error (e) (%source-fault src e)))))
                 (loop repeat wait until (source-stopped src) do (sleep 1)))))
           :name "pine-source-poll"))
    src))

;;; source registry + declarative definition

(defvar *source-defs* (make-hash-table :test 'eq)
  "name -> starter function of the actor system.")

(defmacro defsource (name (system) &body body)
  "Define a source NAME. BODY, with SYSTEM bound to the actor system, starts it
and returns a source (usually via start-stream / start-poll)."
  `(setf (gethash ',name *source-defs*) (lambda (,system) ,@body)))

(defmacro defpoll (name interval &body body)
  "Reactive cell NAME refreshed to (progn BODY) every INTERVAL seconds."
  `(defsource ,name (system)
     (let ((c (ref-of ',name)))
       (start-poll system ,interval
                   (lambda () (pine.ref:set-ref c (progn ,@body)))))))

(defmacro deflisten (name command (line) &body body)
  "Reactive cell NAME fed from COMMAND's stdout: each LINE sets it to (progn
BODY) when that is non-nil."
  (let ((val (gensym)))
    `(defsource ,name (system)
       (let ((c (ref-of ',name)))
         (start-stream system ,command
           (constantly nil)
           (lambda (,line)
             (let ((,val (progn ,@body)))
               (when ,val (pine.ref:set-ref c ,val)) nil)))))))

(defun start-sources (server)
  "Start every declared source once and register it as an adapter in the service
registry (pine.actor), named source:<name>, so it is addressable and listable
alongside the other agents. A source is the desktop's data adapter."
  (unless *running*
    (let ((system (pine.server:actor-system server)))
      (setf *running*
            (loop for name being the hash-keys of *source-defs* using (hash-value starter)
                  for s = (ignore-errors (funcall starter system))
                  when s
                    do (setf (source-name s) name)
                       (ignore-errors
                        (pine.actor:register-agent
                         server (format nil "source:~(~a~)" name)
                         :source (source-actor s) :meta (list :cell name)))
                    and collect s)))))

(defun stop-sources ()
  "Stop each source: flag its supervisor to exit, then terminate its subprocess
so the read hits EOF and the supervisor loop ends. Actors and timers die with
the actor system. No thread is killed."
  (dolist (s *running*)
    (setf (source-stopped s) t)
    (when (source-process s)
      (ignore-errors (uiop:terminate-process (source-process s) :urgent t))))
  (setf *running* nil))

;;;; The built-in sources, declared with the sugar above.

(defun workspaces ()
  "niri workspaces as (:idx N :focused BOOL :urgent BOOL), sorted by idx."
  (sort (map 'list
             (lambda (w) (list :idx (gethash "idx" w)
                               :focused (gethash "is_focused" w)
                               :urgent (gethash "is_urgent" w)))
             (com.inuoe.jzon:parse (sh "niri" "msg" "--json" "workspaces")))
        #'< :key (lambda (p) (getf p :idx))))

(defsource :workspaces (system)
  (start-stream system "niri msg --json event-stream"
    (lambda () (set! :workspaces (workspaces)))
    (lambda (line) (search "Workspace" line))))

(defun audio-volume ()
  (let ((n (%first-number (sh "wpctl" "get-volume" "@DEFAULT_AUDIO_SINK@"))))
    (if (numberp n) (round (* 100 n)) 0)))

(defun audio-muted ()
  (and (search "MUTED" (sh "wpctl" "get-volume" "@DEFAULT_AUDIO_SINK@")) t))

(defun audio-sinks ()
  "pactl sinks as (:name NAME :desc DESC :default BOOL), the default first."
  (let ((default (sh "pactl" "get-default-sink"))
        (rows nil) (name nil) (desc nil))
    (flet ((flush ()
             (when name
               (push (list :name name :desc (or desc name)
                           :default (equal name default))
                     rows))
             (setf name nil desc nil)))
      (dolist (line (%lines (sh "pactl" "list" "sinks")))
        (let ((trim (string-trim '(#\space #\tab) line)))
          (cond ((%starts trim "Sink #") (flush))
                ((%starts trim "Name:")
                 (setf name (string-trim '(#\space) (subseq trim 5))))
                ((%starts trim "Description:")
                 (setf desc (string-trim '(#\space) (subseq trim 12)))))))
      (flush))
    (sort (nreverse rows) (lambda (a b) (and (getf a :default) (not (getf b :default)))))))

(defun select-sink! (name)
  "Make sink NAME the default output. Called from an audio-panel row."
  (when (and name (plusp (length name)))
    (sh "pactl" "set-default-sink" name)
    (set! :sinks (audio-sinks))))

(defsource :audio (system)
  (start-stream system "pactl subscribe"
    (lambda () (set! :vol (audio-volume)) (set! :muted (audio-muted))
            (set! :sinks (audio-sinks)))
    (lambda (line) (search "sink" line))))

(defun brightness ()
  (let ((dir (first (ignore-errors (directory "/sys/class/backlight/*/")))))
    (if dir
        (let ((cur (read-int-file (merge-pathnames "brightness" dir)))
              (mx  (read-int-file (merge-pathnames "max_brightness" dir))))
          (if (and cur mx (plusp mx)) (round (* 100 cur) mx) 50))
        50)))

(defpoll :bri 15 (brightness))

;;;; Network (nmcli). The wifi list, connection status, and the currently
;;;; selected network's actions all live in cells. select! / act! are the
;;;; in-process closures the panel's rows and buttons call -- pine's answer to
;;;; the eww nREPL callbacks: they mutate cells and shell nmcli directly.

(defun %split (s ch &optional limit)
  "Split S on CH into at most LIMIT parts (the last keeps any remaining CH)."
  (let ((parts nil) (start 0) (n 0))
    (dotimes (i (length s))
      (when (and (char= (char s i) ch) (or (null limit) (< (1+ n) limit)))
        (push (subseq s start i) parts) (setf start (1+ i)) (incf n)))
    (push (subseq s start) parts)
    (nreverse parts)))

(defun %lines (s) (remove "" (%split s #\newline) :test #'string=))
(defun cell-val (name) (let ((c (pine.ref:find-ref name))) (and c (pine.ref:deref c))))

(defun net-connected ()
  (dolist (line (%lines (sh "nmcli" "-t" "-f" "TYPE,STATE,CONNECTION"
                            "device" "status")) "")
    (destructuring-bind (&optional type state conn) (%split line #\: 3)
      (when (and (member type '("wifi" "ethernet") :test #'equal)
                 (equal state "connected"))
        (return (or conn ""))))))

(defun sig-bucket (s) (cond ((>= s 66) "hi") ((>= s 40) "mid") (t "lo")))

(defun wifi-list (&optional (rescan "no"))
  (let ((saved (%lines (sh "nmcli" "-t" "-f" "NAME" "connection" "show")))
        (best (make-hash-table :test 'equal)))
    (dolist (line (%lines (sh "nmcli" "-t" "-f" "IN-USE,SSID,SECURITY,SIGNAL"
                              "device" "wifi" "list" "--rescan" rescan)))
      (destructuring-bind (&optional inuse ssid sec sig) (%split line #\: 4)
        (when (and ssid (plusp (length ssid)))
          (let ((signal (or (ignore-errors (parse-integer sig)) 0))
                (prev (gethash ssid best)))
            (when (or (null prev) (> signal (getf prev :signal)))
              (setf (gethash ssid best)
                    (list :ssid ssid :in_use (equal inuse "*")
                          :secure (not (member sec '("" "--") :test #'equal))
                          :saved (and (member ssid saved :test #'equal) t)
                          :signal signal)))))))
    (let ((rows (loop for v being the hash-values of best collect v)))
      (mapcar (lambda (r) (list :ssid (getf r :ssid) :in_use (getf r :in_use)
                               :secure (getf r :secure) :saved (getf r :saved)
                               :sig (sig-bucket (getf r :signal))))
              (sort rows (lambda (a b)
                           (cond ((not (eq (getf a :in_use) (getf b :in_use))) (getf a :in_use))
                                 ((not (eq (getf a :saved) (getf b :saved))) (getf a :saved))
                                 (t (string< (string-downcase (getf a :ssid))
                                             (string-downcase (getf b :ssid)))))))))))

(defun connected-ssid (rows)
  (loop for r in rows when (getf r :in_use) return (getf r :ssid)))

(defun actions-for (ssid rows)
  (let ((n (find ssid rows :key (lambda (r) (getf r :ssid)) :test #'equal)))
    (cond ((null n) nil)
          ((getf n :in_use) (list (list :label "Disconnect" :style "" :kind "disconnect")
                                  (list :label "Forget" :style "no" :kind "forget")))
          ((getf n :saved)  (list (list :label "Connect" :style "go" :kind "up")
                                  (list :label "Forget" :style "no" :kind "forget")))
          ((getf n :secure) (list (list :label "Connect" :style "go" :kind "connect-pw")))
          (t                (list (list :label "Connect" :style "go" :kind "connect"))))))

(defun push-actions (rows)
  (let* ((cur (cell-val :netsel))
         (sel (if (and cur (plusp (length cur))) cur (connected-ssid rows))))
    (set! :netsel (or sel ""))
    (set! :netactions (and sel (actions-for sel rows)))))

(defun refresh-net (&optional (rescan "no"))
  (set! :net (net-connected))
  (let ((rows (wifi-list rescan)))
    (when rows
      (set! :netlist rows)
      (push-actions rows))))

(defun select! (ssid)
  "Select network SSID: update its action set. Called from a panel row."
  (set! :netsel (or ssid ""))
  (set! :netactions (and ssid (actions-for ssid (cell-val :netlist)))))

(defun act! (kind)
  "Run action KIND on the selected network. Called from a panel button."
  (let* ((cur (cell-val :netsel))
         (ssid (and cur (plusp (length cur)) cur)))
    (when ssid
      (cond
        ((equal kind "disconnect") (sh "nmcli" "connection" "down" ssid))
        ((equal kind "up")         (sh "nmcli" "connection" "up" ssid))
        ((equal kind "forget")     (sh "nmcli" "connection" "delete" ssid))
        ((equal kind "connect")    (sh "nmcli" "device" "wifi" "connect" ssid))
        ((equal kind "connect-pw")
         (let ((pw (sh "fuzzel" "--dmenu" "--password" "--prompt"
                       (format nil "~a password: " ssid))))
           (when (plusp (length pw))
             (sh "nmcli" "device" "wifi" "connect" ssid "password" pw)))))
      (set! :netsel "")
      (refresh-net "yes"))))

(defsource :network (system)
  (start-stream system "nmcli monitor" (lambda () (refresh-net "no"))))

(defsource :network-scan (system)
  (start-poll system 15 (lambda () (refresh-net "yes"))))

;;;; Media (EMMS via the emacs daemon). Polled into a :media plist cell.

(defparameter +emms-elisp+
  "(let* ((trk (ignore-errors (emms-playlist-current-selected-track))) (playing (and (boundp 'emms-player-playing-p) emms-player-playing-p)) (paused (and (boundp 'emms-player-paused-p) emms-player-paused-p))) (if trk (json-encode (list :title (or (emms-track-get trk 'info-title) \"\") :artist (or (emms-track-get trk 'info-artist) \"\") :length (or (emms-track-get trk 'info-playing-time) 0) :pos (or (and (boundp 'emms-playing-time) emms-playing-time) 0) :file (or (ignore-errors (emms-track-name trk)) \"\") :status (cond (paused \"Paused\") (playing \"Playing\") (t \"Stopped\")))) \"{}\"))")

(defun cover-for (file)
  "A cover-art image path in FILE's directory, or nil."
  (when (and (stringp file) (plusp (length file)) (ignore-errors (probe-file file)))
    (let ((dir (directory-namestring file)))
      (dolist (name '("cover.jpg" "cover.jpeg" "cover.png" "folder.jpg" "front.jpg") nil)
        (let ((path (merge-pathnames name dir)))
          (when (ignore-errors (probe-file path)) (return (namestring path))))))))

(defun %unquote-elisp (s)
  "emacsclient -e prints a Lisp string literal; strip the quotes and unescape."
  (let ((s (string-trim '(#\space #\newline) s)))
    (when (and (>= (length s) 2) (char= (char s 0) #\")
               (char= (char s (1- (length s))) #\"))
      (setf s (subseq s 1 (1- (length s)))))
    (with-output-to-string (out)
      (loop with i = 0 with n = (length s)
            while (< i n)
            do (let ((c (char s i)))
                 (if (and (char= c #\\) (< (1+ i) n))
                     (progn (write-char (char s (1+ i)) out) (incf i 2))
                     (progn (write-char c out) (incf i))))))))

(defun emms-media ()
  (let ((raw (sh "emacsclient" "-e" +emms-elisp+)))
    (when (plusp (length raw))
      (ignore-errors
        (let ((h (com.inuoe.jzon:parse (%unquote-elisp raw))))
          (list :title  (gethash "title" h "")  :artist (gethash "artist" h "")
                :status (gethash "status" h "Stopped")
                :pos    (gethash "pos" h 0)      :length (gethash "length" h 0)
                :file   (gethash "file" h "")))))))

(defsource :media (system)
  (let ((mc (ref-of :media)) (ac (ref-of :art)))
    (start-poll system 1
      (lambda ()
        (let ((m (emms-media)))
          (pine.ref:set-ref mc m)
          (pine.ref:set-ref ac (or (cover-for (getf m :file)) "")))))))

;;;; System stats (cpu / ram / temp) for the control panel.

(defun %starts (s prefix)
  (and (>= (length s) (length prefix)) (string= s prefix :end1 (length prefix))))

(defun cpu-temp ()
  (let ((dirs (ignore-errors (directory "/sys/class/hwmon/*/"))))
    (dolist (d dirs 0)
      (when (member (sh "cat" (namestring (merge-pathnames "name" d)))
                    '("k10temp" "coretemp" "zenpower") :test #'equal)
        (let ((raw (read-int-file (merge-pathnames "temp1_input" d))))
          (when raw (return (round raw 1000))))))))

(defun mem-percent ()
  (let ((total 0) (avail 0))
    (dolist (line (%lines (sh "cat" "/proc/meminfo")))
      (cond ((%starts line "MemTotal:")     (setf total (or (%first-number line) 0)))
            ((%starts line "MemAvailable:") (setf avail (or (%first-number line) 0)))))
    (if (plusp total) (round (* 100 (- total avail)) total) 0)))

(defvar *cpu-prev* nil "Previous (total . idle) from /proc/stat, for the CPU% delta.")
(defun cpu-percent ()
  "Real CPU utilisation percent from the /proc/stat delta since the last poll
(like eww's EWW_CPU.avg), not the load average."
  (let* ((line (first (%lines (sh "cat" "/proc/stat"))))
         (nums (loop for s in (rest (%split (string-trim " " (or line "")) #\space))
                     for n = (ignore-errors (parse-integer s))
                     when n collect n))
         (total (reduce #'+ nums :initial-value 0))
         (idle (+ (or (nth 3 nums) 0) (or (nth 4 nums) 0))))   ; idle + iowait
    (prog1
        (if *cpu-prev*
            (let ((dt (- total (car *cpu-prev*))) (di (- idle (cdr *cpu-prev*))))
              (if (plusp dt) (max 0 (min 100 (round (* 100 (- dt di)) dt))) 0))
            0)
      (setf *cpu-prev* (cons total idle)))))

(defun disk-percent (&optional (mount "/"))
  "Used percent of the filesystem holding MOUNT, from df."
  (let ((line (second (%lines (sh "df" "--output=pcent" mount)))))
    (or (and line (%first-number line)) 0)))

(defpoll :sys 3 (list :cpu (cpu-percent) :ram (mem-percent)
                      :temp (cpu-temp) :disk (disk-percent)))

;;;; Profile: who + how long, for the control panel header (eww's ctl-profile).

(defun uptime-string ()
  (let ((secs (or (%first-number (sh "cat" "/proc/uptime")) 0)))
    (multiple-value-bind (h rem) (floor (truncate secs) 3600)
      (format nil "up ~dh ~dm" h (floor rem 60)))))

(defpoll :user 3600 (sh "id" "-un"))
(defpoll :host 3600 (sh "hostname"))
(defpoll :uptime 60 (uptime-string))
(defpoll :clock 30 (get-universal-time))

;;;; Focused window title (for the echo strip).

(defun focused-title ()
  (ignore-errors
    (let ((w (com.inuoe.jzon:parse (sh "niri" "msg" "--json" "focused-window"))))
      (and (hash-table-p w) (gethash "title" w "")))))

(defsource :wintitle (system)
  (start-stream system "niri msg --json event-stream"
    (lambda () (set! :wintitle (or (focused-title) "")))
    (lambda (line) (search "Window" line))))
