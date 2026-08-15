(require :asdf)
(asdf:load-system :pine/test)

(defpackage #:pine/bench
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:meter #:pine/run/meter)
                    (#:node #:pine/fs/node) (#:buffer #:pine/edit/buffer)
                    (#:window #:pine/edit/window) (#:key #:pine/edit/key)
                    (#:render #:pine/edit/render) (#:parser #:pine/ts/parser)
                    (#:session #:pine/edit/session) (#:attach #:pine/net/attach)
                    (#:surface #:pine/app/surface) (#:desktop #:pine/app/desktop)
                    (#:term #:pine/edit/term) (#:world #:pine/world/world))
  (:export #:run #:workloads))
(in-package #:pine/bench)

(defvar *workloads* (d:table))
(defvar *size* 2000)
(defvar *for* 5)

(defclass probe-client (attach:client)
  ((sent :initform 0 :accessor sent))
  (:default-initargs :id 1 :kind :editor))

(defmethod attach:push-to ((c probe-client) &rest message)
  (incf (sent c))
  message)

(defmacro workload (name (&rest options) drives &body body)
  "A workload is a name, a line saying what it drives, and what it does. The
line is printed above the numbers, so a number can never appear without saying
where it came from."
  (declare (ignore options))
  `(d:keep! *workloads* ,(string-downcase (string name))
            (list :drives ,drives :run (lambda () ,@body))))

(defun %lisp-text (lines)
  (format nil "~{~a~^~%~}"
          (loop :for i :below lines
                :collect (format nil "(defun f~d (x) (+ x ~d))" i i))))

(defun %shown (name lines)
  "A buffer of LINES lines, in the window, parsed once before anything is timed."
  (let ((b (buffer:make-buffer name :mode "lisp"))
        (w (window:focused)))
    (setf (node:contents b) (%lisp-text lines))
    (setf (window:width-of w) 100 (window:height-of w) 40)
    (window:show! w b)
    (setf (buffer:current) b)
    (buffer:goto! b 10 0)
    (parser:wait b :seconds 60)
    b))

(defun %attached ()
  (let* ((client (make-instance 'probe-client))
         (s (session::%attached client)))
    (session::%work s (list :resize :cols 100 :rows 42
                            :width 900 :height 882
                            :cell-w 9 :cell-h 21 :font-px 15))
    (values s client)))

(workload typing ()
    "a key at a time through the keymap into a file being shown, with a frontend
attached: the key, the parse, the frame and the push a keystroke really makes"
  (let ((b (%shown "typing" *size*)))
    (multiple-value-bind (s client) (%attached)
      (declare (ignore client))
      (dotimes (n 200)
        (key:dispatch nil (key:make-key (string (code-char (+ 97 (mod n 26))))))
        (session::%draw s))
      (parser:wait b :seconds 30))))

(workload paging ()
    "page down and back through a file being shown: the band moves, so the parse
and the highlight walk are re-driven at every screen"
  (let ((b (%shown "paging" *size*))
        (w (window:focused)))
    (dotimes (n 40)
      (setf (window:scroll-of w) (min (max 0 (- *size* 40)) (* n 40)))
      (buffer:goto! b (window:scroll-of w) 0)
      (render:frame-tree :cols 100 :rows 42)
      (parser:wait b :seconds 20))
    (dotimes (n 40)
      (setf (window:scroll-of w) (max 0 (- (window:scroll-of w) 40)))
      (buffer:goto! b (window:scroll-of w) 0)
      (render:frame-tree :cols 100 :rows 42)
      (parser:wait b :seconds 20))))

(workload huge ()
    "the same typing, in a hundred thousand lines"
  (let ((*size* 100000))
    (funcall (getf (d:at (d:all *workloads*) "typing") :run))))

(workload cold ()
    "what the first of everything costs: the first parse of a file, the first
frame after a frontend attaches, the first whole push"
  (let ((b (%shown "cold" *size*)))
    (declare (ignore b))
    (multiple-value-bind (s client) (%attached)
      (declare (ignore client))
      (session:push-frame s :whole t))))

(workload desktop ()
    "a bar and an echo strip pushed to a frontend while the world underneath
them moves: the surface built, what was pushed, and what was the same as before"
  (pine:load-config (merge-pathnames "examples/init.lisp"
                                     (asdf:system-source-directory :pine)))
  (let* ((client (make-instance 'probe-client :id 2 :kind :desktop))
         (s (desktop::%attached client))
         (clock (world:ensure world:*world* "clock"))
         (until (+ (get-universal-time) *for*)))
    (declare (ignore clock))
    (loop :while (< (get-universal-time) until)
          :do (pine/provider/clock:tick)
              (desktop:flush s)
              (sleep 1/20))))

(workload many ()
    "two hundred buffers and the frame that has to keep working with them open"
  (dotimes (n 200)
    (let ((b (buffer:make-buffer (format nil "many-~d" n) :mode "lisp")))
      (setf (node:contents b) (%lisp-text 50))))
  (let ((b (%shown "many-shown" *size*)))
    (dotimes (n 50)
      (key:dispatch nil (key:make-key "x"))
      (render:frame-tree :cols 100 :rows 42))
    (parser:wait b :seconds 30)))

(workload flood ()
    "a shell writing faster than a screen can take it: what the terminal drops
and what it costs to keep up"
  (let ((tm (term:open-terminal "flood" :command "/bin/sh")))
    (unwind-protect
         (progn
           (term:send tm (format nil "for i in $(seq 1 2000); do echo ~a; done~%"
                                 (make-string 60 :initial-element #\x)))
           (let ((until (+ (get-universal-time) *for*)))
             (loop :while (< (get-universal-time) until)
                   :do (render:frame-tree :cols 100 :rows 42)
                       (sleep 1/30))))
      (term:close-terminal "flood"))))

(workload idle ()
    "a daemon with the config loaded that nobody is touching: what ticks, what
forks, what pushes when nothing is happening"
  (pine:load-config (merge-pathnames "examples/init.lisp"
                                     (asdf:system-source-directory :pine)))
  (multiple-value-bind (s client) (%attached)
    (declare (ignore s client))
    (sleep *for*)))

(defun %where (which)
  "Where a run is kept: beside the live sample, in the cache, out of the tree
that is being measured."
  (merge-pathnames (format nil "pine/~a.lisp-data" which) (uiop:xdg-cache-home)))

(defun %read-rows (file)
  (when (probe-file file)
    (with-open-file (in file)
      (let ((*read-eval* nil)) (read in nil nil)))))

(defun %row (rows name) (find name rows :key (lambda (r) (getf r :name))))

(defun compare (synthetic live &key (to *standard-output*))
  "One table beside the other. A workload resembles use or it does not, and the
only way to say so is to put the same instruments side by side. An instrument
one side never touched says so rather than showing a zero."
  (format to "~&~a~%"
          (format nil "workload ~a (~a) against ~a"
                  (getf synthetic :workload) (getf synthetic :image)
                  (if (getf live :workload) "a running daemon" "nothing")))
  (format to "~&~26a ~10a ~10a ~10a ~10a ~8a~%"
          "instrument" "count" "count" "mean ms" "mean ms" "ratio")
  (format to "~&~26a ~10a ~10a ~10a ~10a ~8a~%"
          "" "workload" "live" "workload" "live" "")
  (let ((names (remove-duplicates
                (append (mapcar (lambda (r) (getf r :name)) (getf synthetic :rows))
                        (mapcar (lambda (r) (getf r :name)) (getf live :rows))))))
    (dolist (name (sort names #'string< :key #'princ-to-string) t)
      (let* ((a (%row (getf synthetic :rows) name))
             (b (%row (getf live :rows) name))
             (a-mean (and a (/ (or (getf a :mean) 0) 1000000.0)))
             (b-mean (and b (/ (or (getf b :mean) 0) 1000000.0))))
        (format to "~&~26a ~10a ~10a ~10a ~10a ~8a~%"
                (string-downcase (princ-to-string name))
                (if a (format nil "~:d" (getf a :count)) "-")
                (if b (format nil "~:d" (getf b :count)) "-")
                (if (and a (eq :time (getf a :kind))) (format nil "~,3f" a-mean) "-")
                (if (and b (eq :time (getf b :kind))) (format nil "~,3f" b-mean) "-")
                (if (and a b (eq :time (getf a :kind))
                         (plusp (or b-mean 0)) (plusp (or a-mean 0)))
                    (format nil "~,2fx" (/ a-mean b-mean))
                    "-"))))))

(defun workloads ()
  (sort (d:keys (d:all *workloads*)) #'string<))

(defun %image ()
  (string-trim '(#\Newline #\Space)
               (with-output-to-string (out)
                 (ignore-errors
                  (uiop:run-program '("git" "describe" "--always" "--dirty")
                                    :output out :ignore-error-status t)))))

(defun run (name)
  (let ((it (d:at (d:all *workloads*) name)))
    (unless it
      (format t "~&no workload called ~a. there is: ~{~a~^ ~}~%" name (workloads))
      (return-from run nil))
    (pine:start)
    (meter:reset)
    (let ((start (meter:now)))
      (unwind-protect (funcall (getf it :run))
        (let* ((seconds (/ (- (meter:now) start) 1000000000.0))
               (rows (meter:said)))
          (meter:report
           rows
           :about (format nil "~&workload ~a, size ~d, for ~ds, pine ~a~%~
                               it drives: ~a~%~
                               it ran for ~,2f seconds~%"
                          name *size* *for* (%image) (getf it :drives) seconds))
          (ensure-directories-exist (%where "workload"))
          (with-open-file (out (%where "workload")
                               :direction :output :if-exists :supersede)
            (write (list :workload name :size *size* :for *for*
                         :image (%image) :seconds seconds :rows rows)
                   :stream out))
          (pine:stop))))))

(defun %given (name default)
  "What the make target passed, where an unset variable arrives as an empty
string rather than as nothing."
  (let ((said (uiop:getenv name)))
    (or (and said (plusp (length said)) (parse-integer said :junk-allowed t))
        default)))

(let ((*size* (%given "SIZE" 2000))
      (*for* (%given "FOR" 5))
      (comparing (uiop:getenv "COMPARE")))
  (if (and comparing (plusp (length comparing)))
      (let ((synthetic (%read-rows (%where "workload")))
            (live (%read-rows (%where "metrics"))))
        (cond ((null synthetic)
               (format t "~&no workload has run yet: make bench WORK=typing~%"))
              ((null live)
               (format t "~&no live sample yet. on the daemon you are using:~%~
                            ./pine run metrics-reset~%~
                            ... then use it for a while ...~%~
                            ./pine run metrics-save~%"))
              (t (compare synthetic live))))
      (run (or (uiop:getenv "WORK") "typing")))
  (sb-ext:exit :code 0))
