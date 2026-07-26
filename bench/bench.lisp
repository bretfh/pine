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
    (make-instance 'pine.text.buffer:buffer-state
                   :lines seq
                   :meta (fset:with (fset:empty-map) :name "bench"))))

(defun lisp-source (nforms)
  (with-output-to-string (o)
    (dotimes (i nforms)
      (format o "(defun f~d (x y)~%  (let ((z (+ x y)))~%    (* z ~d)))~%~%" i i))))

(defun sample-tree ()
  (apply #'pine.ui.build:row
         (append (loop repeat 8 collect (pine.ui.build:label "item"))
                 (list (pine.ui.build:meter :value 42 :min 0 :max 100)
                       (pine.ui.build:meter :value 70 :min 0 :max 100)))))

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
                (pine.text.buffer:insert-char st mid 0 #\x)) rows)
        (push (defbench (format nil "insert-newline @mid, ~d lines" n)
                (pine.text.buffer:insert-newline st mid 5)) rows)
        (push (defbench (format nil "delete-region 3ch @mid, ~d lines" n)
                (pine.text.buffer:delete-region st mid 0 mid 3)) rows)
        (push (defbench (format nil "state->snapshot, ~d lines" n)
                (pine.text.buffer:state->snapshot st)) rows)
        (push (defbench (format nil "state->string, ~d lines" n)
                (pine.text.buffer:state->string st)) rows)))
    (print-table "buffer edits (fset immutable state)" (nreverse rows))))

(defun bench-load-content ()
  (let (rows)
    (dolist (n '(100 1000 5000))
      (let ((text (with-output-to-string (o)
                    (dotimes (i n) (write-line (a-line 40 i) o)))))
        (push (defbench (format nil "load-content, ~d lines" n)
                (pine.text.buffer:load-content text)) rows)))
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
  (let ((pine.core.server:*server* (make-instance 'pine.core.server:server)))
    (let* ((tree (sample-tree))
           (wire (pine.ui.wire:node->wire tree)) rows)
      (push (%safe (lambda () (defbench "node->wire (serialize bar)"
                                (pine.ui.wire:node->wire tree)))) rows)
      (push (%safe (lambda () (defbench "wire->node (deserialize bar)"
                                (pine.ui.wire:wire->node wire)))) rows)
      (push (%safe (lambda () (defbench "render (bar -> cell rows, w=80)"
                                (pine.ui.cells:render tree 80)))) rows)
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
  (pine.ui.wire:node->wire
   (pine.ui.build:column
    (pine.ui.build:window (loop :repeat rows :collect (%row cols runs)) :kind :window)
    (pine.ui.build:window (list (%row cols 1)) :kind :echo)
    (pine.ui.build:window (list (%row cols 2)) :kind :modeline))))

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
  (let ((pine.core.server:*server* (make-instance 'pine.core.server:server)))
    (let ((shapes (list (list "bar (the tree benched above)" (pine.ui.wire:node->wire (sample-tree)))
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
                      (setf (getf (second (first (pine.ui.wire:wire-windows f))) :rows)
                            (let ((rows (copy-list
                                         (getf (second (first (pine.ui.wire:wire-windows f)))
                                               :rows))))
                              (setf (nth 12 rows) (%row 88 15 #\b))
                              rows))
                      f))
             (patch (pine.ui.wire:rows-patch before after))
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
                                  (pine.ui.wire:wire->node wire)))) rows))
      (print-table "the push path, per frame" (nreverse rows)))))

(defun bench-ts ()
  (handler-case
      (let* ((rt (pine.ts.runtime:make-ts-runtime)) rows)
        (dolist (n '(20 200))
          (let* ((text (lisp-source n))
                 (lines (fset:convert 'fset:seq
                                      (uiop:split-string text :separator '(#\Newline))))
                 (ps (pine.ts.runtime:make-parse-state rt :commonlisp)))
            (unless ps (error "no commonlisp grammar"))
            (pine.ts.runtime:parse-lines! ps lines)
            (push (defbench (format nil "parse from lines, ~d forms (~d B)" n (length text))
                    (pine.ts.runtime:parse-lines! ps lines)) rows)
            (push (defbench (format nil "highlight full walk, ~d forms" n)
                    (progn (setf (pine.ts.runtime::ps-hl-cache ps) nil)
                           (pine.ts.highlight:parse-highlights ps))) rows)
            ;; the typing path: one line edited, tree shifted, window re-walked.
            ;; This is what the parser actor does per keystroke, minus the
            ;; message hop that takes it off the buffer's thread.
            (let* ((mid (floor (fset:size lines) 2))
                   (was (fset:@ lines mid))
                   (alt-lines (fset:with lines mid (concatenate 'string was "x")))
                   (cur lines))
              (push (defbench (format nil "edit+reparse+highlight, ~d forms" n)
                      (let ((next (if (eq cur lines) alt-lines lines)))
                        (pine.ts.runtime:parse-lines!
                         ps next :edit (list mid 1 1 (if (eq next alt-lines) 1 -1)))
                        (setf cur next)
                        (pine.ts.highlight:parse-highlights ps)))
                    rows)
              ;; the same keystroke against a 30-line window, which is what a
              ;; buffer with a viewport actually walks
              (let ((top (max 0 (- (floor (fset:size lines) 2) 15))))
                (push (defbench (format nil "edit+reparse+highlight window, ~d forms" n)
                        (let ((next (if (eq cur lines) alt-lines lines)))
                          (pine.ts.runtime:parse-lines!
                           ps next :edit (list mid 1 1 (if (eq next alt-lines) 1 -1)))
                          (setf cur next)
                          (pine.ts.highlight:parse-highlights
                           ps :from-line top :to-line (+ top 30))))
                      rows))
              (push (defbench (format nil "highlight window only, ~d forms" n)
                      (pine.ts.highlight:parse-highlights ps :from-line 0 :to-line 30))
                    rows))))
        (print-table "tree-sitter reparse + highlight" (nreverse rows)))
    (error (e) (format t "~&(ts group skipped: ~a)~%" e))))

(defun lisp-state (nforms viewport)
  "A lisp-mode buffer state of NFORMS forms, carrying VIEWPORT when given."
  (let ((state (pine.text.buffer:set-meta
                (pine.text.buffer:load-content (lisp-source nforms))
                :mode :lisp-mode)))
    (if viewport
        (pine.text.buffer:set-meta state :viewport viewport)
        state)))

(defun bench-refresh ()
  "The work a keystroke causes: the incremental reparse and the highlight of what
a window shows. The editor runs this on the buffer's parser actor rather than in
the buffer's receive, so these rows are the cost itself, without the message hop
that takes it off the edit path. The windowless rows are the same cycle before
the viewport existed, so the pair is the measurement.

The edit descriptor is passed exactly as a buffer passes it: without one the tree
cannot be shifted and every call would reparse the file from scratch, which is a
different measurement wearing the same name."
  (handler-case
      (let ((rt (pine.ts.runtime:make-ts-runtime))
            rows)
        (unless pine.core.server:*server*
          (setf pine.core.server:*server* (make-instance 'pine.core.server:server)))
        (setf (pine.core.server:ts-runtime pine.core.server:*server*) rt)
        (dolist (nforms '(40 400))
          (dolist (viewport (list nil (cons 0 30)))
            (let* ((quiet (lisp-state nforms viewport))
                   (typed (pine.text.buffer:insert-char quiet 3 0 #\x))
                   (ps (nth-value 1 (pine.text.buffer:refresh-highlights nil quiet)))
                   (flip nil))
              (when ps
                (push (defbench (format nil "refresh-highlights, ~d forms~a"
                                        nforms (if viewport ", window" ""))
                        (progn (setf flip (not flip))
                               (setf ps (nth-value
                                         1 (pine.text.buffer:refresh-highlights
                                            ps (if flip typed quiet)
                                            ;; line 3 gained or lost one character
                                            :edit (list 3 1 1 (if flip 1 -1)))))))
                      rows)))))
        (print-table "the per-keystroke cycle (buffer text + reparse + highlight)"
                     (nreverse rows)))
    (error (e) (format t "~&(refresh group skipped: ~a)~%" e))))

(defun bench-eval ()
  (let ((sem (sb-thread:make-semaphore)) rows)
    (push (defbench "evaluate-thunk round-trip (thread spawn)"
            (pine.core.eval:evaluate-thunk
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
        (unless pine.core.server:*server*
          (setf pine.core.server:*server* (make-instance 'pine.core.server:server)))
        (let ((tree (sample-tree)) rows)
          (pine.cairo.paint:with-cairo-layout
            (let ((surface (cairo:create-image-surface :argb32 560 120)))
              (cairo:with-context ((cairo:create-context surface))
                (push (defbench "paint-tree (bar, 560x120, reused surface)"
                        (pine.cairo.paint:paint-tree tree 560 120))
                      rows))
              (cairo:destroy surface))
            (push (defbench "fresh surface + paint + destroy (560x480)"
                    (let ((surface (cairo:create-image-surface :argb32 560 480)))
                      (cairo:with-context ((cairo:create-context surface))
                        (pine.cairo.paint:paint-tree tree 560 480))
                      (cairo:destroy surface)))
                  rows))
          (print-table "cairo paint (per frame in the app)" (nreverse rows))))
    (error (e) (format t "~&(paint group skipped: ~a)~%" e))))

(defun bench-agent ()
  "The crash-boundary cost: spawn a real SBCL process agent over remoting and
measure the eval round-trip (serialize, send, eval in the other image, result
home). Spawn-to-connect is reported once; it is a fresh image loading :pine."
  (handler-case
      (let ((srv (pine.core.server:start-server :remoting-port 18191)))
        (setf pine.core.server:*server* srv)
        (pine.core.actor:start-agent-registry srv)
        (pine.core.actor:start-agent-debug srv)
        (setf pine.core.actor::*agent-port* 18191)
        (let ((sem (sb-thread:make-semaphore))
              (t0 (get-internal-real-time)))
          (setf pine.core.actor:*agent-debug-hook*
                (lambda (msg)
                  (when (eq (first msg) :agent-result)
                    (sb-thread:signal-semaphore sem))))
          (let ((info (pine.core.actor:spawn-agent srv "bench-agent")))
            (format t "~&agent spawn-to-connect: ~,1fs (fresh SBCL image loading :pine)~%"
                    (/ (float (- (get-internal-real-time) t0) 1d0)
                       internal-time-units-per-second))
            (unwind-protect
                 (let (rows)
                   (push (defbench "process-agent eval round-trip (remoting)"
                           (progn (pine.core.actor:agent-eval srv info "42")
                                  (sb-thread:wait-on-semaphore sem :timeout 30)))
                         rows)
                   (print-table "process agent (the crash boundary)" (nreverse rows)))
              (pine.core.actor:unsupervise-agent "bench-agent")
              (ignore-errors (sento.actor:tell (pine.core.actor:agent-info-actor info) '(:crash)))
              (ignore-errors (pine.core.actor:unregister-agent srv "bench-agent"))))))
    (error (e) (format t "~&(agent group skipped: ~a)~%" e))))

(defun bench-mailbox ()
  "What a buffer's own thread costs. A buffer actor is pinned: its own thread and
mailbox, so a receive that parks in the debugger cannot take a worker the
renderer or the input path needs. These rows are the price of that isolation,
measured against the same receive on the shared dispatcher."
  (handler-case
      (let* ((srv (pine.core.server:start-server))
             (sys (pine.core.server:actor-system srv))
             (echo (lambda (msg)
                     (when (eq (first msg) :ping) (sento.actor:reply :pong)))))
        (setf pine.core.server:*server* srv)
        (unwind-protect
             (let ((pinned (sento.actor-context:actor-of sys :name "bench-pinned"
                                                             :dispatcher :pinned
                                                             :state nil :receive echo))
                   (shared (sento.actor-context:actor-of sys :name "bench-shared"
                                                             :state nil :receive echo))
                   (buffer (pine.text.buffer:make-buffer-actor sys "bench-mailbox"))
                   rows)
               (push (defbench "ask round-trip, pinned mailbox"
                       (sento.actor:ask-s pinned '(:ping) :time-out 5))
                     rows)
               (push (defbench "ask round-trip, shared dispatcher"
                       (sento.actor:ask-s shared '(:ping) :time-out 5))
                     rows)
               (push (defbench "buffer insert then read back (a keystroke, end to end)"
                       (progn (sento.actor:tell buffer '(:insert :char #\a))
                              (sento.actor:ask-s buffer '(:get-snapshot) :time-out 5)))
                     rows)
               (print-table "the mailbox (what a buffer's own thread costs)"
                            (nreverse rows)))
          (pine.core.server:stop-server srv)))
    (error (e) (format t "~&(mailbox group skipped: ~a)~%" e))))

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
                   #'bench-widget #'bench-push #'bench-ts #'bench-refresh
                   #'bench-eval #'bench-mailbox #'bench-paint #'bench-agent))
    (handler-case (funcall g)
      (error (e) (format t "~&(group ~a failed: ~a)~%" g e))))
  (finish-output))
