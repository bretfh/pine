;;;; Hot-path microbenchmarks for the pine substrate. Each bench reports ns/op,
;;;; bytes consed/op, and the GC fraction of wall time, because allocation rate
;;;; is the property that matters for a daemon meant never to restart. Load :pine
;;;; and :pine/vt first (run.lisp does), then (pine.bench:run-all).

(defpackage :pine.bench
  (:use :cl)
  (:export #:run-all #:run-bench #:*target-ms*))

(in-package :pine.bench)

(defvar *target-ms* 150
  "Grow rep count until a timed batch reaches at least this many ms.")

(defstruct row label reps ns/op bytes/op gc-ms total-ms)

(declaim (inline %now))
(defun %now () (get-internal-real-time))
(defun %ms (ticks) (* 1000d0 (/ ticks internal-time-units-per-second)))

(defun run-bench (label thunk &key (target-ms *target-ms*))
  (funcall thunk)
  (let ((reps 1))
    (loop
      (let ((b0 (sb-ext:get-bytes-consed))
            (g0 sb-ext:*gc-run-time*)
            (t0 (%now)))
        (dotimes (i reps) (funcall thunk))
        (let* ((dt (- (%now) t0))
               (ms (%ms dt)))
          (when (or (>= ms target-ms) (>= reps 200000000))
            (return
              (make-row :label label :reps reps
                        :ns/op (/ (* ms 1d6) reps)
                        :bytes/op (/ (- (sb-ext:get-bytes-consed) b0) (float reps 1d0))
                        :gc-ms (%ms (- sb-ext:*gc-run-time* g0))
                        :total-ms ms)))
          (setf reps (* reps 4)))))))

(defmacro defbench (label &body body)
  `(run-bench ,label (lambda () ,@body)))

(defun print-table (title rows)
  (format t "~&~%#+CAPTION: ~a~%" title)
  (format t "| op | reps | ns/op | bytes/op | gc% |~%")
  (format t "|-~%")
  (dolist (r rows)
    (when r
      (format t "| ~a | ~:d | ~:d | ~:d | ~,1f |~%"
              (row-label r) (row-reps r)
              (round (row-ns/op r)) (round (row-bytes/op r))
              (if (plusp (row-total-ms r))
                  (* 100d0 (/ (row-gc-ms r) (row-total-ms r))) 0d0))))
  (finish-output))

(defun a-line (cols i)
  (let ((s (make-string cols)))
    (dotimes (c cols s) (setf (char s c) (code-char (+ 97 (mod (+ i c) 26)))))))

(defun lines-state (nlines &optional (cols 40))
  (let ((seq (fset:empty-seq)))
    (dotimes (i nlines) (setf seq (fset:with-last seq (a-line cols i))))
    (make-instance 'pine.buffer:buffer-state
                   :lines seq
                   :meta (fset:with (fset:empty-map) :name "bench"))))

(defun lisp-source (nforms)
  (with-output-to-string (o)
    (dotimes (i nforms)
      (format o "(defun f~d (x y)~%  (let ((z (+ x y)))~%    (* z ~d)))~%~%" i i))))

(defun sample-tree ()
  (apply #'pine.layout:row
         (append (loop repeat 8 collect (pine.layout:label "item"))
                 (list (pine.layout:meter :value 42 :min 0 :max 100)
                       (pine.layout:meter :value 70 :min 0 :max 100)))))

(defparameter *plain-chunk*
  (with-output-to-string (o)
    (dotimes (i 40)
      (format o "the quick brown fox jumps over the lazy dog line ~d~%" i))))

(defparameter *sgr-chunk*
  (with-output-to-string (o)
    (dotimes (i 40)
      (dotimes (w 9) (format o "~c[3~dmword~c[0m " #\escape (1+ (mod w 6)) #\escape))
      (terpri o))))

(defun bench-buffer ()
  (let (rows)
    (dolist (n '(100 1000 10000))
      (let* ((st (lines-state n))
             (mid (floor n 2)))
        (push (defbench (format nil "insert-char @mid, ~d lines" n)
                (pine.buffer:insert-char st mid 0 #\x)) rows)
        (push (defbench (format nil "insert-newline @mid, ~d lines" n)
                (pine.buffer:insert-newline st mid 5)) rows)
        (push (defbench (format nil "delete-region 3ch @mid, ~d lines" n)
                (pine.buffer:delete-region st mid 0 mid 3)) rows)
        (push (defbench (format nil "state->snapshot, ~d lines" n)
                (pine.buffer:state->snapshot st)) rows)
        (push (defbench (format nil "state->string, ~d lines" n)
                (pine.buffer:state->string st)) rows)))
    (print-table "buffer edits (fset immutable state)" (nreverse rows))))

(defun bench-load-content ()
  (let (rows)
    (dolist (n '(100 1000 5000))
      (let ((text (with-output-to-string (o)
                    (dotimes (i n) (write-line (a-line 40 i) o)))))
        (push (defbench (format nil "load-content, ~d lines" n)
                (pine.buffer:load-content text)) rows)))
    (print-table "load-content (file open path)" (nreverse rows))))

(defun bench-vt ()
  (let* ((term (pine.vt:make-term :width 80 :height 24)) rows)
    (push (defbench (format nil "process-output plain (~d B)" (length *plain-chunk*))
            (pine.vt:term-process-output term *plain-chunk*)) rows)
    (push (defbench (format nil "process-output sgr (~d B)" (length *sgr-chunk*))
            (pine.vt:term-process-output term *sgr-chunk*)) rows)
    (push (defbench "render-line (one row)" (pine.vt:term-render-line term 0)) rows)
    (push (defbench "dump-to-string (full 80x24)" (pine.vt:term-dump-to-string term)) rows)
    (print-table "vt terminal (mutable cell grid)" (nreverse rows))))

(defun %safe (thunk)
  (handler-case (funcall thunk)
    (error (e) (format t "~&  (bench skipped: ~a)~%" e) nil)))

(defun bench-widget ()
  (let ((pine.server:*server* (make-instance 'pine.server:server)))
    (let* ((tree (sample-tree))
           (wire (pine.layout:node->wire tree)) rows)
      (push (%safe (lambda () (defbench "node->wire (serialize bar)"
                                (pine.layout:node->wire tree)))) rows)
      (push (%safe (lambda () (defbench "wire->node (deserialize bar)"
                                (pine.layout:wire->node wire)))) rows)
      (push (%safe (lambda () (defbench "render (bar -> cell rows, w=80)"
                                (pine.layout:render tree 80)))) rows)
      (print-table "widget view build (per push / per frame)" (nreverse rows)))))

;;;; The push path: what a frame costs to put on the wire. The other groups
;;;; measure functions; this one measures a message, because the size of what
;;;; crosses is the thing that was never looked at.

(defun %row (cols runs &optional (char #\a))
  "One wire row: COLS characters of text, RUNS face runs, as frame->rows emits."
  (cons (make-string cols :initial-element char)
        (loop :for i :below runs
              :collect (list (* i (max 1 (floor cols (max 1 runs))))
                             200 200 200 30 30 40 0))))

(defun %editor-wire (cols rows runs)
  "The wire form of an editor tree of ROWS lines at COLS columns, each line
carrying RUNS face runs."
  (pine.layout:node->wire
   (pine.layout:column
    (pine.layout:window (loop :repeat rows :collect (%row cols runs)) :kind :window)
    (pine.layout:window (list (%row cols 1)) :kind :echo)
    (pine.layout:window (list (%row cols 2)) :kind :modeline))))

(defun %frame-bytes (wire)
  "(values PAYLOAD FRAME) bytes: the serialized message, and the envelope the
transport actually writes, which re-encodes those bytes as decimal text."
  (let* ((payload (flexi-streams:string-to-octets
                   (write-to-string wire :readably t) :external-format :utf-8))
         (frame (flexi-streams:string-to-octets
                 (write-to-string (list :target-path "/user/display" :sender-path nil
                                        :message (coerce payload 'list)
                                        :message-type :tell :correlation-id nil)
                                  :readably t)
                 :external-format :utf-8)))
    (values (length payload) (length frame))))

(defun print-sizes (title entries)
  (format t "~&~%#+CAPTION: ~a~%" title)
  (format t "| push | payload B | frame B | x | ceiling |~%")
  (format t "|-~%")
  (dolist (e entries)
    (destructuring-bind (label payload frame) e
      (format t "| ~a | ~:d | ~:d | ~,1f | ~,1f% |~%"
              label payload frame (/ frame (max 1 payload))
              (* 100d0 (/ frame 2097152d0)))))
  (finish-output))

(defun bench-push ()
  (let ((pine.server:*server* (make-instance 'pine.server:server)))
    (let ((shapes (list (list "bar (the tree benched above)" (pine.layout:node->wire (sample-tree)))
                        (list "editor 88x25, plain" (%editor-wire 88 25 1))
                        (list "editor 88x25, highlighted" (%editor-wire 88 25 15))
                        (list "editor 284x78, plain" (%editor-wire 284 78 1))
                        (list "editor 284x78, highlighted" (%editor-wire 284 78 40))))
          sizes rows)
      (dolist (s shapes)
        (multiple-value-bind (payload frame) (%frame-bytes (second s))
          (push (list (first s) payload frame) sizes)))
      (print-sizes "one push on the wire (2 MB frame cap)" (nreverse sizes))
      ;; the same frame, as a patch: one line of it changed
      (let* ((before (%editor-wire 88 25 15))
             (after (let ((f (%editor-wire 88 25 15)))
                      (setf (getf (second (first (pine.layout:wire-windows f))) :rows)
                            (let ((rows (copy-list
                                         (getf (second (first (pine.layout:wire-windows f)))
                                               :rows))))
                              (setf (nth 12 rows) (%row 88 15 #\b))
                              rows))
                      f))
             (patch (pine.layout:rows-patch before after))
             (whole nil))
        (declare (ignorable whole))
        (multiple-value-bind (payload frame) (%frame-bytes (list :rows-patch patch))
          (print-sizes "one changed line, as a patch"
                       (list (list "editor 88x25 highlighted, 1 line" payload frame)))))
      (let ((wire (%editor-wire 88 25 15)))
        (push (%safe (lambda () (defbench "node->wire (editor 88x25 highlighted)"
                                  (%editor-wire 88 25 15)))) rows)
        (push (%safe (lambda ()
                       (let ((payload (flexi-streams:string-to-octets
                                       (write-to-string wire :readably t)
                                       :external-format :utf-8)))
                         (defbench "envelope encode (the transport's second pass)"
                           (write-to-string
                            (list :target-path "/user/display" :sender-path nil
                                  :message (coerce payload 'list)
                                  :message-type :tell :correlation-id nil)
                            :readably t))))) rows)
        (push (%safe (lambda ()
                       (let* ((payload (flexi-streams:string-to-octets
                                        (write-to-string wire :readably t)
                                        :external-format :utf-8))
                              (frame (write-to-string
                                      (list :target-path "/user/display" :sender-path nil
                                            :message (coerce payload 'list)
                                            :message-type :tell :correlation-id nil)
                                      :readably t)))
                         (defbench "envelope decode (parse the bytes back)"
                           (read-from-string frame))))) rows)
        (push (%safe (lambda () (defbench "wire->node (editor 88x25 highlighted)"
                                  (pine.layout:wire->node wire)))) rows))
      (print-table "the push path, per frame" (nreverse rows)))))

(defun bench-ts ()
  (handler-case
      (let* ((rt (pine.ts:make-ts-runtime)) rows)
        (dolist (n '(20 200))
          (let* ((text (lisp-source n))
                 (ps (pine.ts:make-parse-state rt :commonlisp)))
            (unless ps (error "no commonlisp grammar"))
            (pine.ts:parse-full! ps text)
            (push (defbench (format nil "reparse! full, ~d forms (~d B)" n (length text))
                    (pine.ts:reparse! ps text)) rows)
            (push (defbench (format nil "highlight full walk, ~d forms" n)
                    (progn (setf (pine.ts::ps-hl-cache ps) nil)
                           (pine.ts:parse-highlights ps text))) rows)
            ;; the typing path: one small edit, reparse, re-highlight
            (let* ((mid (floor (length text) 2))
                   (alt (concatenate 'string (subseq text 0 mid) "x" (subseq text mid)))
                   (cur text))
              (pine.ts:reparse! ps cur)
              (pine.ts:parse-highlights ps cur)
              (push (defbench (format nil "edit+reparse+highlight, ~d forms" n)
                      (progn (setf cur (if (eq cur text) alt text))
                             (pine.ts:reparse! ps cur)
                             (pine.ts:parse-highlights ps cur)))
                    rows))))
        (print-table "tree-sitter reparse + highlight" (nreverse rows)))
    (error (e) (format t "~&(ts group skipped: ~a)~%" e))))

(defun bench-eval ()
  (let ((sem (sb-thread:make-semaphore)) rows)
    (push (defbench "evaluate-thunk round-trip (thread spawn)"
            (pine.eval:evaluate-thunk
             (lambda () 42)
             :on-done (lambda (ev) (declare (ignore ev)) (sb-thread:signal-semaphore sem)))
            (sb-thread:wait-on-semaphore sem)) rows)
    (print-table "eval path (one thread per evaluation)" (nreverse rows))))

#-pine-cairo
(defun bench-paint ()
  (format t "~&(paint group skipped: pine/cairo not loaded)~%"))

#+pine-cairo
(defun bench-paint ()
  "The app-side frame cost: measure/arrange/paint a bar tree into a cairo image
surface, headless -- the same path the wayland clients run per frame."
  (handler-case
      (progn
        (unless pine.server:*server*
          (setf pine.server:*server* (make-instance 'pine.server:server)))
        (let ((tree (sample-tree)) rows)
          (pine.layout:with-cairo-layout
            (let ((surface (cairo:create-image-surface :argb32 560 120)))
              (cairo:with-context ((cairo:create-context surface))
                (push (defbench "paint-tree (bar, 560x120, reused surface)"
                        (pine.layout:paint-tree tree 560 120))
                      rows))
              (cairo:destroy surface))
            (push (defbench "fresh surface + paint + destroy (560x480)"
                    (let ((surface (cairo:create-image-surface :argb32 560 480)))
                      (cairo:with-context ((cairo:create-context surface))
                        (pine.layout:paint-tree tree 560 480))
                      (cairo:destroy surface)))
                  rows))
          (print-table "cairo paint (per frame in the app)" (nreverse rows))))
    (error (e) (format t "~&(paint group skipped: ~a)~%" e))))

(defun bench-agent ()
  "The crash-boundary cost: spawn a real SBCL process agent over remoting and
measure the eval round-trip (serialize, send, eval in the other image, result
home). Spawn-to-connect is reported once; it is a fresh image loading :pine."
  (handler-case
      (let ((srv (pine.server:start-server :remoting-port 18191)))
        (setf pine.server:*server* srv)
        (pine.actor:start-agent-registry srv)
        (pine.actor:start-agent-debug srv)
        (setf pine.actor::*agent-port* 18191)
        (let ((sem (sb-thread:make-semaphore))
              (t0 (get-internal-real-time)))
          (setf pine.actor:*agent-debug-hook*
                (lambda (msg)
                  (when (eq (first msg) :agent-result)
                    (sb-thread:signal-semaphore sem))))
          (let ((info (pine.actor:spawn-agent srv "bench-agent")))
            (format t "~&agent spawn-to-connect: ~,1fs (fresh SBCL image loading :pine)~%"
                    (/ (float (- (get-internal-real-time) t0) 1d0)
                       internal-time-units-per-second))
            (unwind-protect
                 (let (rows)
                   (push (defbench "process-agent eval round-trip (remoting)"
                           (progn (pine.actor:agent-eval srv info "42")
                                  (sb-thread:wait-on-semaphore sem :timeout 30)))
                         rows)
                   (print-table "process agent (the crash boundary)" (nreverse rows)))
              (pine.actor:unsupervise-agent "bench-agent")
              (ignore-errors (sento.actor:tell (pine.actor:agent-info-actor info) '(:crash)))
              (ignore-errors (pine.actor:unregister-agent srv "bench-agent"))))))
    (error (e) (format t "~&(agent group skipped: ~a)~%" e))))

(defun machine-header ()
  (format t "~&#+BEGIN_EXAMPLE~%")
  (format t "sbcl:   ~a~%" (lisp-implementation-version))
  (ignore-errors
   (with-open-file (f "/proc/cpuinfo" :if-does-not-exist nil)
     (when f
       (loop for line = (read-line f nil) while line
             when (and (> (length line) 10) (string= "model name" line :end2 10))
               do (format t "cpu:   ~a~%"
                          (string-trim " " (subseq line (1+ (position #\: line)))))
                  (return)))))
  (format t "target-ms per bench: ~d~%" *target-ms*)
  (format t "#+END_EXAMPLE~%"))

(defun run-all ()
  (machine-header)
  (dolist (g (list #'bench-buffer #'bench-load-content #'bench-vt
                   #'bench-widget #'bench-push #'bench-ts #'bench-eval
                   #'bench-paint #'bench-agent))
    (handler-case (funcall g)
      (error (e) (format t "~&(group ~a failed: ~a)~%" g e))))
  (finish-output))
