(require :asdf)
(require :sb-introspect)
(asdf:load-system :pine/test)

(defpackage #:pine/bench
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:meter #:pine/run/meter)
                    (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)
                    (#:mode #:pine/mode) (#:doc #:pine/text/document)
                    (#:parser #:pine/text/ts/parser)
                    (#:keys #:pine/edit/keys) (#:key #:pine/ui/key)
                    (#:window #:pine/edit/window) (#:render #:pine/edit/render)
                    (#:surface #:pine/ui/surface) (#:device #:pine/host/device))
  (:export #:run #:workloads))
(in-package #:pine/bench)

(defvar *workloads* (d:table))
(defvar *size* 2000)
(defvar *for* 5)
(defparameter +cols+ 100)
(defparameter +lines+ 42)

(defmacro workload (name (&rest options) drives &body body)
  "A workload is a name, a line saying what it drives, and what it does. The line
is printed above the numbers, so a number can never appear without saying where it
came from."
  (declare (ignore options))
  `(d:keep! *workloads* ,(string-downcase (string name))
            (list :drives ,drives :run (lambda () ,@body))))

(defun %lisp-text (lines)
  (format nil "~{~a~^~%~}"
          (loop :for i :below lines
                :collect (format nil "(defun f~d (x) (+ x ~d))" i i))))

(defun %sized ()
  "Say how big the surface came out, the way the screen would."
  (setf (node:contents (tree:at nil "surface/editor/size"))
        (list :wide (* 9 +cols+) :tall (* 18 +lines+)
              :cols +cols+ :lines +lines+ :font 15)))

(defun %parsed (d &key (seconds 60))
  "Wait for the parse to catch up with the document. Setting a workload up is not
the workload: what is timed below starts from a document already walked."
  (let ((p (parser:parser-for d)))
    (when p
      (loop :repeat (round (/ seconds 0.02))
            :until (parser:currentp p)
            :do (sleep 0.02)))
    p))

(defun %shown (name lines)
  "A document of LINES lines, in the window, parsed once before anything is timed."
  (let ((d (doc:make-document name :mode (make-instance 'mode:lisp)))
        (w (window:focused)))
    (setf (node:contents d) (%lisp-text lines))
    (setf (window:cols w) +cols+ (window:lines w) +lines+)
    (window:show w d)
    (setf (doc:current) d)
    (doc:goto d 10 0)
    (%sized)
    (%parsed d :seconds 60)
    d))

(defun %wire (&optional (name "editor"))
  "What the screen would be handed. This is the whole of what a frame costs."
  (node:contents (tree:at nil (format nil "surface/~a/wire" name))))

(defun %ready ()
  (pine:use :text)
  (pine:use :edit))

(workload typing ()
    "a key at a time through the keymap into a document being shown, and the tree
the screen is handed after each: the key, the parse, the surface and the wire a
keystroke really makes"
  (%ready)
  (let ((d (%shown "typing" *size*)))
    (dotimes (n 200)
      (keys:dispatch (key:make-key (string (code-char (+ 97 (mod n 26))))))
      (%wire))
    (%parsed d :seconds 30)))

(workload paging ()
    "page down and back through a document being shown: the band moves, so the
parse and the highlight walk are re-driven at every screen"
  (%ready)
  (let ((d (%shown "paging" *size*))
        (w (window:focused)))
    (dotimes (n 40)
      (setf (window:scroll w) (min (max 0 (- *size* 40)) (* n 40)))
      (doc:goto d (window:scroll w) 0)
      (%wire)
      (%parsed d :seconds 20))
    (dotimes (n 40)
      (setf (window:scroll w) (max 0 (- (window:scroll w) 40)))
      (doc:goto d (window:scroll w) 0)
      (%wire)
      (%parsed d :seconds 20))))

(workload huge ()
    "the same typing, in a hundred thousand lines"
  (let ((*size* 100000))
    (funcall (getf (d:lookup (d:all *workloads*) "typing") :run))))

(workload cold ()
    "what the first of everything costs: the first parse of a document, and the
first tree the screen is handed"
  (%ready)
  (%shown "cold" *size*)
  (%wire))

(workload desktop ()
    "a bar and its panels worked out while the world underneath them moves: the
surface built, the tree written down, and what came out the same as before"
  (%ready)
  (pine:use :host)
  (pine:use :desk)
  (let ((until (+ (get-universal-time) *for*)))
    (loop :while (< (get-universal-time) until)
          :do (device:tick)
              (dolist (each (surface:surfaces))
                (when (surface:shown each) (%wire (node:name each))))
              (sleep 1/20))))

(workload many ()
    "two hundred documents and the frame that has to keep working with them open"
  (%ready)
  (dotimes (n 200)
    (let ((d (doc:make-document (format nil "many-~d" n)
                                :mode (make-instance 'mode:lisp))))
      (setf (node:contents d) (%lisp-text 50))))
  (let ((d (%shown "many-shown" *size*)))
    (dotimes (n 50)
      (keys:dispatch (key:make-key "x"))
      (%wire))
    (%parsed d :seconds 30)))

(workload idle ()
    "a daemon with the config loaded that nobody is touching: what ticks, what
forks, what is worked out again when nothing is happening"
  (%ready)
  (pine:load-config (merge-pathnames "examples/init.lisp"
                                     (asdf:system-source-directory :pine)))
  (%sized)
  (sleep *for*))

(defun %where (which)
  "Where a run is kept: beside the live sample, in the cache, out of the tree that
is being measured."
  (merge-pathnames (format nil "pine/~a.lisp-data" which) (uiop:xdg-cache-home)))

(defun %read-rows (file)
  (when (probe-file file)
    (with-open-file (in file)
      (let ((*read-eval* nil)) (read in nil nil)))))

(defun %row (rows name) (find name rows :key (lambda (r) (getf r :name))))

(defun compare (synthetic live &key (to *standard-output*))
  "One table beside the other. A workload resembles use or it does not, and the
only way to say so is to put the same instruments side by side. An instrument one
side never touched says so rather than showing a zero."
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
  (or (uiop:getenv "PINE_VERSION") (lisp-implementation-version)))

(defun run (name)
  (let ((it (d:lookup (d:all *workloads*) name)))
    (unless it
      (format t "~&no workload called ~a. there is: ~{~a~^ ~}~%" name (workloads))
      (return-from run nil))
    (pine:start)
    (meter:reset)
    (let ((start (meter:now)))
      (unwind-protect (funcall (getf it :run))
        (let* ((seconds (/ (- (meter:now) start) 1000000000.0))
               (rows (meter:readings)))
          (meter:report
           rows
           :about (format nil "~&workload ~a, size ~d, for ~ds, sbcl ~a~%~
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
  "What the make target passed, where an unset variable arrives as an empty string
rather than as nothing."
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
