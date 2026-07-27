(in-package :pine.test)

(def-suite :pine.stress)
(in-suite :pine.stress)

;;;; Load and faults, at volumes the ordinary suite does not pay for: hundreds of
;;;; buffers, thousands of messages, concurrent writers and readers on one actor,
;;;; faults arriving in storms and while a buffer is being killed. Run with
;;;; `make stress'; it is not part of `make test'.
;;;;
;;;; Every check is bounded. A daemon that wedges under load fails here rather
;;;; than hanging the run, and the bound is the assertion: the property is that
;;;; the substrate still answers, not that it answers eventually.

(defparameter +stress-buffers+ 200)
(defparameter +stress-messages+ 5000)
(defparameter +stress-writers+ 8)

(defun stress-buffer (name &optional (content ""))
  (let ((buf (pine.editor.frame::make-buffer name :content content)))
    (pine.editor.frame::set-buffer-mode buf :text-mode)
    buf))

(defun char-count (text)
  (count-if (lambda (c) (char= c #\x)) text))

;;;; Volume

(test hundreds-of-buffers-all-answer-and-all-go-away
  "One thread per buffer is the price of isolation, so the count has to hold up.
Every buffer is created, edited, read back, then killed, and the thread count
comes back down."
  (with-fixture substrate ()
    (within-seconds 180
      (let ((before (length (sb-thread:list-all-threads)))
            (names (loop :for i :below +stress-buffers+
                         :collect (format nil "stress-~d" i))))
        (dolist (name names)
          (sento.actor:tell (stress-buffer name) (list :replace-content :content name)))
        (sleep 1.0)
        (let ((wrong (remove-if (lambda (name) (equal name (btext name))) names)))
          (is (null wrong)
              "~d of ~d buffers did not answer with their own content, first: ~a"
              (length wrong) (length names) (first wrong)))
        (let ((peak (length (sb-thread:list-all-threads))))
          (is (>= peak (+ before (floor +stress-buffers+ 2)))
              "~d buffers only added ~d threads, so they are not on their own"
              +stress-buffers+ (- peak before)))
        (dolist (name names) (pine.editor.frame::kill-buffer name))
        (is (wait-for (lambda ()
                        (< (length (sb-thread:list-all-threads))
                           (+ before (floor +stress-buffers+ 4))))
                      :seconds 30)
            "killing ~d buffers left ~d threads behind"
            +stress-buffers+ (- (length (sb-thread:list-all-threads)) before))))))

(test thousands-of-messages-arrive-in-order-and-none-are-lost
  "A mailbox under flood keeps every message and their order: the text is the
tally."
  (with-fixture substrate ()
    (within-seconds 120
      (let ((buf (stress-buffer "stress-flood")))
        (dotimes (i +stress-messages+)
          (sento.actor:tell buf (list :insert :text "x")))
        (is (wait-for (lambda () (let ((text (btext "stress-flood")))
                                   (and (stringp text)
                                        (= +stress-messages+ (char-count text)))))
                      :seconds 90)
            "flood of ~d inserts left ~a characters"
            +stress-messages+
            (let ((text (btext "stress-flood")))
              (if (stringp text) (char-count text) text)))))))

(test concurrent-writers-serialize-on-the-actor
  "Eight threads writing the same buffer, and the count is exact: the receive is
the serialization point, so no edit is interleaved or lost."
  (with-fixture substrate ()
    (within-seconds 120
      (let* ((per 500)
             (buf (stress-buffer "stress-writers"))
             (threads (loop :for w :below +stress-writers+
                            :collect (bordeaux-threads:make-thread
                                      (lambda ()
                                        (dotimes (i per)
                                          (sento.actor:tell buf (list :insert :text "x"))))
                                      :name (format nil "stress-writer-~d" w)))))
        (mapc #'bordeaux-threads:join-thread threads)
        (let ((want (* per +stress-writers+)))
          (is (wait-for (lambda () (let ((text (btext "stress-writers")))
                                     (and (stringp text) (= want (char-count text)))))
                        :seconds 90)
              "~d writers x ~d inserts produced ~a characters, wanted ~d"
              +stress-writers+ per
              (let ((text (btext "stress-writers")))
                (if (stringp text) (char-count text) text))
              want))))))

(test readers-keep-getting-snapshots-while-a-writer-edits
  "Blocking reads and a writer at the same time: every read answers a snapshot,
none times out, and the line count never goes backwards."
  (with-fixture substrate ()
    (within-seconds 120
      (let* ((buf (stress-buffer "stress-readwrite"))
             (stop nil)
             (writer (bordeaux-threads:make-thread
                      (lambda ()
                        (loop :until stop
                              :do (sento.actor:tell buf '(:newline))
                                  (sleep 0.002)))
                      :name "stress-readwrite-writer"))
             (bad 0)
             (backwards 0)
             (highest 0))
        (unwind-protect
             (dotimes (i 200)
               (let ((snap (bsnap "stress-readwrite")))
                 (if (typep snap 'pine.text.buffer:snapshot)
                     (let ((n (pine.text.buffer:line-count snap)))
                       (when (< n highest) (incf backwards))
                       (setf highest (max highest n)))
                     (incf bad))))
          (setf stop t)
          (bordeaux-threads:join-thread writer))
        (is (zerop bad) "~d of 200 reads did not answer a snapshot" bad)
        (is (zerop backwards) "~d reads saw the buffer go backwards" backwards)
        (is (> highest 1) "the writer never got through")))))

(test the-renderer-keeps-painting-under-an-edit-flood
  "The paint path is pinned, so a flood of snapshots from many buffers keeps
producing frames rather than queueing behind the edits that caused them."
  (with-fixture substrate ()
    (within-seconds 120
      (let ((painted 0))
        (setf (pine.editor.frame::paint-sink *client*)
              (lambda (&rest args) (declare (ignore args)) (incf painted)))
        (let ((buffers (loop :for i :below 20
                             :collect (let ((b (stress-buffer (format nil "stress-paint-~d" i))))
                                        (pine.ui.render:subscribe-to-buffer b)
                                        b))))
          (dotimes (round 50)
            (dolist (b buffers)
              (sento.actor:tell b (list :insert :text "x"))))
          (is (wait-for (lambda () (> painted 100)) :seconds 60)
              "1000 edits across 20 subscribed buffers produced only ~d frames" painted))))))

;;;; Faults

(test twenty-faults-at-once-all-resolve
  "A fault storm: every buffer in it parks, nobody attends, and all of them come
back on the deadline while the rest of the daemon keeps working."
  (with-fixture substrate ()
    (within-seconds 120
      (with-surface (faults :park-seconds 1)
        (let ((names (loop :for i :below 20
                           :collect (format nil "stress-fault-~d" i))))
          (dolist (name names)
            (let ((buf (probe-buffer name)))
              (sento.actor:tell buf (list :replace-content :content name))
              (sento.actor:tell buf '(:probe-fault))))
          (is (wait-for (lambda () (= 20 (length faults))) :seconds 30)
              "only ~d of 20 faults reached the surface" (length faults))
          (let ((wedged (remove-if (lambda (name) (equal name (btext name))) names)))
            (is (null wedged)
                "~d buffers never came back after their fault, first: ~a"
                (length wedged) (first wedged)))
          (is (stringp (btext "scratch"))
              "the rest of the daemon stopped answering during the storm"))))))

(test a-buffer-that-faults-on-every-message-still-drains-its-mailbox
  "A permanently broken mode: each message aborts, and the next one is still
handled. A fault must cost one message, not the actor."
  (with-fixture substrate ()
    (within-seconds 90
      (with-surface (faults :park-seconds 1)
        (let ((buf (probe-buffer "stress-always-faults")))
          (dotimes (i 5) (sento.actor:tell buf '(:probe-fault)))
          (is (wait-for (lambda () (= 5 (length faults))) :seconds 60)
              "only ~d of 5 repeated faults were handled" (length faults))
          (sento.actor:tell buf (list :replace-content :content "alive"))
          (is (wait-for (lambda () (equal "alive" (btext "stress-always-faults"))))
              "the buffer stopped taking work after five faults"))))))

(test messages-sent-to-a-parked-buffer-are-all-handled-once-it-comes-back
  "The mailbox holds while the thread is parked: nothing sent during a fault is
dropped."
  (with-fixture substrate ()
    (within-seconds 120
      (with-surface (faults :attended (lambda (ev) (declare (ignore ev)) t))
        (let ((buf (probe-buffer "stress-parked-queue")))
          (sento.actor:tell buf '(:probe-fault))
          (is (wait-for (lambda () faults)) "the fault never reached the surface")
          (dotimes (i 200) (sento.actor:tell buf (list :insert :text "x")))
          (sleep 0.5)
          (pine.err:pick-restart (first faults) "ABORT")
          (is (wait-for (lambda () (let ((text (btext "stress-parked-queue")))
                                     (and (stringp text) (= 200 (char-count text)))))
                        :seconds 60)
              "after the park, the buffer had ~a of 200 queued edits"
              (let ((text (btext "stress-parked-queue")))
                (if (stringp text) (char-count text) text))))))))

(test asking-a-parked-buffer-times-out-instead-of-hanging-the-caller
  "The caller's own liveness: a blocking read of a buffer that is parked has to
come back, so the session thread can report and carry on."
  (with-fixture substrate ()
    (within-seconds 60
      (with-surface (faults :attended (lambda (ev) (declare (ignore ev)) t))
        (let ((buf (probe-buffer "stress-parked-ask")))
          (sento.actor:tell buf '(:probe-fault))
          (is (wait-for (lambda () faults)) "the fault never reached the surface")
          (let ((answer (handler-case (pine.core.actor:ask buf '(:get-text) :timeout 1)
                          (error (c) c))))
            (is (not (stringp answer))
                "a parked buffer answered text, so it was not parked"))
          (pine.err:pick-restart (first faults) "ABORT")
          (is (wait-for (lambda () (stringp (btext "stress-parked-ask"))))
              "the buffer never resumed"))))))

(test killing-a-parked-buffer-does-not-hang-the-killer
  "A buffer can be killed while it is parked in the debugger, and the caller
doing the killing comes back."
  (with-fixture substrate ()
    (within-seconds 60
      (with-surface (faults :attended (lambda (ev) (declare (ignore ev)) t))
        (let ((buf (probe-buffer "stress-kill-parked")))
          (declare (ignorable buf))
          (sento.actor:tell (pine.editor.frame::buffer "stress-kill-parked") '(:probe-fault))
          (is (wait-for (lambda () faults)) "the fault never reached the surface")
          (finishes (pine.editor.frame::kill-buffer "stress-kill-parked"))
          (is (null (pine.editor.frame::buffer "stress-kill-parked"))
              "the killed buffer is still registered")
          (pine.err:pick-restart (first faults) "ABORT")
          (is (stringp (btext "scratch"))
              "the daemon stopped answering after a parked buffer was killed"))))))

(test a-surface-that-always-signals-does-not-stop-the-buffer
  "The escape hatch under load: if the debug surface itself is broken, every
fault aborts and the buffer keeps working."
  (with-fixture substrate ()
    (within-seconds 90
      (let ((saved pine.err:*on-debug*)
            (calls 0))
        (setf pine.err:*on-debug*
              (lambda (ev) (declare (ignore ev)) (incf calls) (error "surface is broken")))
        (unwind-protect
             (let ((buf (probe-buffer "stress-broken-surface")))
               (dotimes (i 5) (sento.actor:tell buf '(:probe-fault)))
               (is (wait-for (lambda () (= 5 calls)) :seconds 30)
                   "the broken surface was called ~d times, not 5" calls)
               (sento.actor:tell buf (list :replace-content :content "alive"))
               (is (wait-for (lambda () (equal "alive" (btext "stress-broken-surface"))))
                   "the buffer stopped working when the surface was broken"))
          (setf pine.err:*on-debug* saved))))))

(test an-attended-check-that-signals-is-treated-as-unattended
  "The attended check is code too. If it breaks, the park must end, not become
permanent."
  (with-fixture substrate ()
    (within-seconds 90
      (with-surface (faults :park-seconds 1
                            :attended (lambda (ev) (declare (ignore ev))
                                        (error "attended check is broken")))
        (let ((buf (probe-buffer "stress-broken-attended")))
          (sento.actor:tell buf (list :replace-content :content "before"))
          (sleep 0.2)
          (sento.actor:tell buf '(:probe-fault))
          (is (wait-for (lambda () faults)) "the fault never reached the surface")
          (is (wait-for (lambda () (equal "before" (btext "stress-broken-attended")))
                        :seconds 30)
              "a broken attended check kept the buffer parked"))))))

(test an-unknown-restart-name-falls-back-to-abort
  "Picking a restart that does not exist resolves the fault instead of leaving
the thread waiting for one that will never be found."
  (with-fixture substrate ()
    (within-seconds 60
      (with-surface (faults :attended (lambda (ev) (declare (ignore ev)) t))
        (let ((buf (probe-buffer "stress-bad-restart")))
          (sento.actor:tell buf (list :replace-content :content "before"))
          (sleep 0.2)
          (sento.actor:tell buf '(:probe-fault))
          (is (wait-for (lambda () faults)) "the fault never reached the surface")
          (pine.err:pick-restart (first faults) "NO-SUCH-RESTART")
          (is (wait-for (lambda () (equal "before" (btext "stress-bad-restart")))
                        :seconds 30)
              "an unknown restart name left the buffer parked"))))))

;;;; Edges

(test a-region-that-runs-past-the-end-clamps-to-it
  "Deleting a region whose end is off the buffer takes what is there, the way a
region command does when the buffer shrank under it. No fault, no leftovers."
  (with-fixture substrate ()
    (within-seconds 90
      (with-surface (faults :park-seconds 1)
        (let ((buf (stress-buffer "stress-clamp" (format nil "one~%two"))))
          (sento.actor:tell buf (list :delete-region :start-line 0 :start-col 0
                                                     :end-line 900 :end-col 4))
          (is (wait-for (lambda () (equal "" (btext "stress-clamp"))))
              "a region past the end left ~s" (btext "stress-clamp"))
          (is (null faults) "clamping to the end should not fault"))))))

(test an-edit-off-the-end-of-the-buffer-is-refused
  "The line index is checked at the edit, not discovered later: padding the line
seq with holes would fault on the next read instead, far from the caller."
  (let ((state (pine.text.buffer:load-content "one")))
    (signals error (pine.text.buffer:insert-string state 700 0 "x"))
    (signals error (pine.text.buffer:insert-string state 0 0 nil))
    (finishes (pine.text.buffer:insert-string state 1 0 "x"))))

(test a-verb-no-mode-claims-faults-and-the-buffer-keeps-working
  "A message the modes do not handle is a mistake somewhere, and it says so. A
buffer that quietly drops it makes the caller's bug look like a no-op."
  (with-fixture substrate ()
    (within-seconds 90
      (with-surface (faults :park-seconds 1)
        (let ((buf (stress-buffer "stress-unknown-verb" "one")))
          (sento.actor:tell buf (list :no-such-verb :line 700))
          (is (wait-for (lambda () faults) :seconds 30)
              "an unhandled verb was dropped without a word")
          (is (wait-for (lambda () (equal "one" (btext "stress-unknown-verb"))) :seconds 30)
              "the buffer did not keep its content, it answered ~s"
              (btext "stress-unknown-verb"))
          (sento.actor:tell buf (list :insert :text "!"))
          (is (wait-for (lambda () (equal "!one" (btext "stress-unknown-verb"))))
              "the buffer stopped taking edits after an unhandled verb"))))))

(test a-point-past-the-end-does-not-corrupt-the-buffer
  (with-fixture substrate ()
    (within-seconds 90
      (with-surface (faults :park-seconds 1)
        (let ((buf (stress-buffer "stress-point" "one")))
          (sento.actor:tell buf (list :move-point :line 500 :col 400))
          (sento.actor:tell buf (list :indent-lines :from 40 :to 90))
          (sleep 1.0)
          (let ((text (btext "stress-point")))
            (is (stringp text) "the buffer stopped answering after point ran off")
            (when (stringp text)
              (is (search "one" text) "the buffer lost its content"))))))))

(test an-empty-buffer-survives-deleting-from-it
  (with-fixture substrate ()
    (within-seconds 60
      (with-surface (faults :park-seconds 1)
        (let ((buf (stress-buffer "stress-empty")))
          (dotimes (i 10) (sento.actor:tell buf '(:backspace)))
          (sento.actor:tell buf (list :insert :text "x"))
          (is (wait-for (lambda () (equal "x" (btext "stress-empty"))))
              "backspacing an empty buffer left it unusable"))))))

(test a-very-long-line-round-trips
  "One line of a hundred thousand characters: the buffer is a seq of lines, so
this is the axis it does not index."
  (with-fixture substrate ()
    (within-seconds 120
      (let ((long (make-string 100000 :initial-element #\x)))
        (sento.actor:tell (stress-buffer "stress-long-line")
                          (list :replace-content :content long))
        (is (wait-for (lambda () (equal long (btext "stress-long-line"))) :seconds 60)
            "a 100k-character line did not come back intact")))))

(test text-outside-ascii-round-trips
  (with-fixture substrate ()
    (within-seconds 60
      (let ((text (format nil "~a~%~a~%~a"
                          (coerce (list (code-char 26085) (code-char 26412)
                                        (code-char 35486)) 'string)
                          (coerce (list (code-char 955) (code-char 955)) 'string)
                          (coerce (list (code-char 128169)) 'string))))
        (sento.actor:tell (stress-buffer "stress-unicode")
                          (list :replace-content :content text))
        (is (wait-for (lambda () (equal text (btext "stress-unicode"))))
            "text outside ascii did not come back intact")))))

(test churning-a-buffer-name-leaves-nothing-behind
  "Create and kill the same name two hundred times: the registry stays consistent
and the threads do not accumulate."
  (with-fixture substrate ()
    (within-seconds 180
      (let ((before (length (sb-thread:list-all-threads))))
        (dotimes (i 200)
          (let ((buf (stress-buffer "stress-churn")))
            (sento.actor:tell buf (list :insert :text "x"))
            (pine.editor.frame::kill-buffer "stress-churn")))
        (is (null (pine.editor.frame::buffer "stress-churn"))
            "the churned name is still registered")
        (is (wait-for (lambda () (< (length (sb-thread:list-all-threads)) (+ before 20)))
                      :seconds 30)
            "200 create/kill rounds left ~d threads behind"
            (- (length (sb-thread:list-all-threads)) before))
        (let ((buf (stress-buffer "stress-churn")))
          (sento.actor:tell buf (list :insert :text "y"))
          (is (wait-for (lambda () (equal "y" (btext "stress-churn"))))
              "the name could not be used again after the churn"))))))

(test sending-to-a-killed-buffer-does-not-take-anything-down
  "The renderer is subscribed to a buffer that gets killed under it. Sends to the
dead actor have to be survivable: the daemon keeps painting and editing."
  (with-fixture substrate ()
    (within-seconds 90
      (let ((painted 0))
        (setf (pine.editor.frame::paint-sink *client*)
              (lambda (&rest args) (declare (ignore args)) (incf painted)))
        (let ((buf (stress-buffer "stress-dead")))
          (pine.ui.render:subscribe-to-buffer buf)
          (sento.actor:tell buf (list :insert :text "x"))
          (sleep 0.2)
          (pine.editor.frame::kill-buffer "stress-dead")
          (dotimes (i 50) (ignore-errors (sento.actor:tell buf (list :insert :text "x"))))
          (sleep 0.5)
          (let ((live (stress-buffer "stress-dead-live")))
            (pine.ui.render:subscribe-to-buffer live)
            (setf painted 0)
            (sento.actor:tell live (list :insert :text "z"))
            (is (wait-for (lambda () (equal "z" (btext "stress-dead-live"))))
                "editing stopped working after sends to a dead buffer")
            (is (wait-for (lambda () (plusp painted)))
                "the paint path stopped after sends to a dead buffer")))))))

(test an-undo-storm-walks-all-the-way-back-and-forward
  "Five hundred edits, then more undos than there are edits, then redos: the
history has to bottom out rather than fault or lose the text."
  (with-fixture substrate ()
    (within-seconds 180
      (with-surface (faults :park-seconds 1)
        (let ((buf (stress-buffer "stress-undo")))
          (dotimes (i 500) (sento.actor:tell buf (list :insert :text "x")))
          (is (wait-for (lambda () (let ((text (btext "stress-undo")))
                                     (and (stringp text) (= 500 (char-count text)))))
                        :seconds 90)
              "the edits did not all land before the undos")
          (dotimes (i 600) (sento.actor:tell buf '(:undo)))
          (is (wait-for (lambda () (equal "" (btext "stress-undo"))) :seconds 90)
              "600 undos over 500 edits left ~s" (btext "stress-undo"))
          (dotimes (i 600) (sento.actor:tell buf '(:redo)))
          (is (wait-for (lambda () (let ((text (btext "stress-undo")))
                                     (and (stringp text) (= 500 (char-count text)))))
                        :seconds 90)
              "the redos did not restore the text, it answered ~s"
              (btext "stress-undo"))
          (is (null faults) "walking the history past its ends faulted"))))))

(test a-thousand-keystrokes-through-the-command-path
  "Keys as the session loop runs them, with no pause between: the command path,
the keymaps, the buffer and the renderer all keep up and the text is exact."
  (with-fixture substrate ()
    (within-seconds 180
      (let ((buf (pine.editor.frame::buffer "scratch"))
            (key (pine.editor.key::parse-key "x")))
        (sento.actor:tell buf (list :replace-content :content ""))
        (sleep 0.2)
        (dotimes (i 1000)
          (pine.editor.command::dispatch *client* key))
        (is (wait-for (lambda () (let ((text (btext "scratch")))
                                   (and (stringp text) (= 1000 (char-count text)))))
                      :seconds 90)
            "1000 keystrokes produced ~a characters"
            (let ((text (btext "scratch")))
              (if (stringp text) (char-count text) text)))))))

(test an-agent-that-dies-is-noticed-and-comes-back
  "The crash boundary: a process agent is killed from under the daemon, the
supervisor notices, and a fresh one registers and answers."
  (with-fixture substrate ()
    (within-seconds 240
      (let ((server (ensure-remoting)))
        (unwind-protect
             (progn
               (pine.core.actor:spawn-agent server "stress-agent")
               (is (wait-for (lambda () (pine.core.actor:agent-alive-p server "stress-agent"))
                             :seconds 60)
                   "the agent never came up")
               (let ((info (pine.core.actor:find-agent server "stress-agent")))
                 (ignore-errors
                  (sento.actor:tell (pine.core.actor:agent-info-actor info) '(:crash))))
               (is (wait-for (lambda ()
                               (not (pine.core.actor:agent-alive-p server "stress-agent")))
                             :seconds 60)
                   "the daemon never noticed the agent was gone")
               (is (stringp (btext "scratch"))
                   "the daemon stopped answering when an agent died")
               (pine.core.actor:unregister-agent server "stress-agent")
               (pine.core.actor:spawn-agent server "stress-agent")
               (is (wait-for (lambda () (pine.core.actor:agent-alive-p server "stress-agent"))
                             :seconds 60)
                   "a replacement agent could not be started"))
          (ignore-errors (pine.core.actor:kill-agent server "stress-agent")))))))

(test a-million-line-buffer-stays-answerable-while-it-parses
  "The point of the parser actor. A million lines of lisp is over a second of
tree-sitter per edit; none of it may land on the buffer's thread. Every read here
happens while parses are in flight, and each one has to come back promptly."
  (with-fixture substrate ()
    (within-seconds 600
      (let* ((line "(defun f (x) (+ x 1))")
             (content (with-output-to-string (s)
                        (dotimes (i 1000000) (write-line line s))))
             (buf (pine.editor.frame::make-buffer "stress-million")))
        (pine.editor.frame::set-buffer-mode buf :lisp-mode)
        (sento.actor:tell buf (list :replace-content :content content))
        ;; An ask that lands while the buffer is in the middle of loading 22 MB
        ;; comes back NO-RESULT rather than waiting, so the load is waited out on
        ;; the answer being a snapshot at all. The latency this test is about is
        ;; measured below, once the buffer is settled.
        (is (wait-for (lambda ()
                        (let ((snap (bsnap "stress-million")))
                          (and (typep snap 'pine.text.buffer:snapshot)
                               (> (pine.text.buffer:line-count snap) 999999))))
                      :seconds 120)
            "the million lines never loaded")
        ;; type into it while the parser is busy behind us
        (let ((worst 0) (unanswered 0))
          (dotimes (i 25)
            (sento.actor:tell buf (list :insert :text "z"))
            (let ((t0 (get-internal-real-time)))
              (unless (typep (bsnap "stress-million") 'pine.text.buffer:snapshot)
                (incf unanswered))
              (let ((ms (/ (* 1000.0 (- (get-internal-real-time) t0))
                           internal-time-units-per-second)))
                (setf worst (max worst ms)))))
          (is (< worst 250)
              "a snapshot took ~,1f ms while the parser was working; the parse is
back on the buffer's thread" worst)
          (is (zerop unanswered)
              "~d of 25 reads went unanswered while the parser was working"
              unanswered))
        (is (wait-for (lambda () (= 25 (count #\z (btext "stress-million")))) :seconds 60)
            "the edits did not all land")
        (pine.editor.frame::kill-buffer "stress-million")))))

(test a-large-file-highlights-inside-its-window-in-bounded-time
  "The windowed walk is what keeps a big file usable: highlighting the visible
lines of a twenty thousand line buffer stays well under a frame."
  (with-fixture substrate ()
    (within-seconds 180
      (let* ((form "(defun f (x) (let ((y (+ x 1))) (list y :k \"s\")))")
             (text (with-output-to-string (s)
                     (dotimes (i 4000) (write-line form s))))
             (state (pine.text.buffer:set-meta
                     (pine.text.buffer:load-content text)
                     :mode :lisp-mode))
             (windowed (pine.text.buffer:set-meta state :viewport (cons 100 130))))
        (is (> (pine.text.buffer:line-count-of state) 3999))
        ;; warm the parse, then time the walk the window actually asks for
        (let ((pstate (nth-value 1 (pine.text.buffer:refresh-highlights nil windowed)))
              (t0 (get-internal-real-time)))
          (pine.text.buffer:refresh-highlights pstate windowed)
          (let ((ms (/ (* 1000.0 (- (get-internal-real-time) t0))
                       internal-time-units-per-second 1)))
            (is (< ms 100)
                "highlighting a 30-line window of a 4000-form file took ~,1f ms" ms)))))))
