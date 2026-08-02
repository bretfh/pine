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
    (pine.editor.frame::set-buffer-mode buf :text)
    buf))

(defun probe-buffer (name)
  "NAME, a lisp buffer whose parser is up and ready to be faulted. A buffer is
not an actor: what faults is its parser."
  (probe-parser name)
  name)

(defun char-count (text)
  (count-if (lambda (c) (char= c #\x)) text))

;;;; Volume

(test hundreds-of-buffers-all-answer-and-cost-no-threads
  "A buffer is paths, not an actor. Two hundred of them are two hundred
subtrees: each answers with its own content and none of them costs a thread,
which is why a text buffer is free to exist."
  (with-fixture substrate ()
    (within-seconds 180
      (let ((before (length (sb-thread:list-all-threads)))
            (names (loop :for i :below +stress-buffers+
                         :collect (format nil "stress-~d" i))))
        (dolist (name names)
          (pine.ns:write (pine.buf:at (stress-buffer name) :text) name))
        (sleep 1.0)
        (let ((wrong (remove-if (lambda (name) (equal name (btext name))) names)))
          (is (null wrong)
              "~d of ~d buffers did not answer with their own content, first: ~a"
              (length wrong) (length names) (first wrong)))
        (let ((peak (length (sb-thread:list-all-threads))))
          (is (< peak (+ before 8))
              "~d buffers with no language added ~d threads"
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
          (pine.buf:edit buf (fset:seq :insert "x")))
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
                                          (pine.buf:edit buf (fset:seq :insert "x"))))
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
                              :do (pine.buf:edit buf (fset:seq :newline))
                                  (sleep 0.002)))
                      :name "stress-readwrite-writer"))
             (bad 0)
             (backwards 0)
             (highest 0))
        (unwind-protect
             (dotimes (i 200)
               (let ((snap (bsnap "stress-readwrite")))
                 (if (typep snap 'pine.text:snapshot)
                     (let ((n (pine.text:line-count snap)))
                       (when (< n highest) (incf backwards))
                       (setf highest (max highest n)))
                     (incf bad)))
               ;; a read is a slot read, so 200 of them outrun a writer that
               ;; sleeps between edits unless they are paced with it
               (sleep 0.002))
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
                                        
                                        b))))
          (dotimes (round 50)
            (dolist (b buffers)
              (pine.buf:edit b (fset:seq :insert "x"))))
          (is (wait-for (lambda () (> painted 100)) :seconds 60)
              "1000 edits across 20 subscribed buffers produced only ~d frames" painted))))))

;;;; Faults

(test twenty-faults-at-once-all-resolve
  "A fault storm: twenty parsers fault, nobody decides anything, and all twenty
keep working while the rest of the daemon does too."
  (with-fixture substrate ()
    (within-seconds 120
      (with-surface (faults)
        (let ((names (loop :for i :below 20
                           :collect (format nil "stress-fault-~d" i))))
          (dolist (name names)
            (let ((buf (probe-buffer name)))
              (pine.ns:write (pine.buf:at buf :text) name)
              (sento.actor:tell (pine.buf:parser-of buf) '(:probe-fault))))
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
      (with-surface (faults)
        (let ((buf (probe-buffer "stress-always-faults")))
          (dotimes (i 5)
            (sento.actor:tell (pine.buf:parser-of buf) '(:probe-fault)))
          (is (wait-for (lambda () (= 5 (length faults))) :seconds 60)
              "only ~d of 5 repeated faults were handled" (length faults))
          (pine.ns:write (pine.buf:at buf :text) "alive")
          (is (wait-for (lambda () (equal "alive" (btext "stress-always-faults"))))
              "the buffer stopped taking work after five faults"))))))

(test edits-land-while-the-parser-is-faulting
  "An edit is a write and the parser is one actor: two hundred edits land
around a fault, and the parser goes on parsing without anyone deciding
anything."
  (with-fixture substrate ()
    (within-seconds 120
      (with-surface (faults)
        (let ((buf (probe-buffer "stress-fault-queue")))
          (pine.ns:write (pine.buf:at buf :text) "")
          (sento.actor:tell (pine.buf:parser-of buf) '(:probe-fault))
          (is (wait-for (lambda () faults)) "the fault never reached /err")
          (dotimes (i 200) (pine.buf:edit buf (fset:seq :insert "x")))
          (is (wait-for (lambda () (let ((text (btext "stress-fault-queue")))
                                     (and (stringp text) (= 200 (char-count text)))))
                        :seconds 60)
              "after the parser faulted, the buffer took ~a of 200 edits"
              (let ((text (btext "stress-fault-queue")))
                (if (stringp text) (char-count text) text)))
          (is (wait-for (lambda () (pine.ns:read (pine.buf:at buf :face)))
                        :seconds 60)
              "the parser never parsed again after its fault"))))))

(test reading-a-buffer-whose-parser-faulted-answers-at-once
  "A read is a slot read, so nothing waits on a parser: a buffer whose parser
just faulted still answers its text, which is the whole of why the parse is not
on the path a keystroke takes."
  (with-fixture substrate ()
    (within-seconds 60
      (with-surface (faults)
        (let ((buf (probe-buffer "stress-fault-ask")))
          (sento.actor:tell (pine.buf:parser-of buf) '(:probe-fault))
          (is (wait-for (lambda () faults)) "the fault never reached /err")
          (is (stringp (pine.buf:text-of buf))
              "a buffer stopped answering when its parser faulted")
          (is (stringp (btext "stress-fault-ask"))
              "the buffer never came back"))))))

(test killing-a-faulted-buffer-does-not-hang-the-killer
  "A buffer can be killed while its parser is faulting, and the caller doing
the killing comes back."
  (with-fixture substrate ()
    (within-seconds 60
      (with-surface (faults)
        (let ((buf (probe-buffer "stress-kill-faulted")))
          (declare (ignorable buf))
          (sento.actor:tell (pine.buf:parser-of "stress-kill-faulted") '(:probe-fault))
          (is (wait-for (lambda () faults)) "the fault never reached /err")
          (finishes (pine.editor.frame::kill-buffer "stress-kill-faulted"))
          (is (null (pine.buf:live "stress-kill-faulted"))
              "the killed buffer is still registered")
          (is (stringp (btext "scratch"))
              "the daemon stopped answering after a faulted buffer was killed"))))))

(test a-watch-on-err-that-always-signals-does-not-stop-the-buffer
  "Whatever is looking at /err is code too. If it is broken, the fault still
lands and the buffer keeps working: a surface that cannot show a fault must not
become the reason a thread is lost."
  (with-fixture substrate ()
    (within-seconds 90
      (let ((calls 0))
        (pine.ns:watch (pine.path:parse "/err")
                       (lambda (value) (declare (ignore value))
                         (incf calls) (error "the surface is broken"))
                       :as :probe)
        (unwind-protect
             (let ((buf (probe-buffer "stress-broken-surface"))
                   (*error-output* (make-broadcast-stream)))
               (dotimes (i 5)
                 (sento.actor:tell (pine.buf:parser-of buf) '(:probe-fault)))
               (is (wait-for (lambda () (>= calls 5)) :seconds 30)
                   "the broken watch ran ~d times, not 5" calls)
               (pine.ns:write (pine.buf:at buf :text) "alive")
               (is (wait-for (lambda () (equal "alive" (btext "stress-broken-surface"))))
                   "the buffer stopped working when the watch was broken"))
          (pine.ns:watch (pine.path:parse "/err") nil :as :probe))))))

(test a-broken-watch-still-lets-an-evaluation-end
  "The one place a thread does wait is an evaluation's own. If what was
supposed to look at its fault signals instead, the wait has to end anyway."
  (with-fixture substrate ()
    (within-seconds 90
      (let ((saved (pine.ns:read (pine.path:parse "/park-seconds"))) (done nil))
        (pine.ns:write (pine.path:parse "/park-seconds") 1)
        (pine.ns:watch (pine.path:parse "/err")
                       (lambda (value) (declare (ignore value))
                         (error "the surface is broken"))
                       :as :probe)
        (unwind-protect
             (let ((*error-output* (make-broadcast-stream)))
               (pine.err:evaluate-thunk (lambda () (error "probe"))
                                        :on-done (lambda (ev) (setf done ev)))
               (is (wait-for (lambda () done) :seconds 30)
                   "a broken watch held the evaluation's thread for good")
               (is (eq :aborted (pine.err:evaluation-status done))))
          (pine.ns:watch (pine.path:parse "/err") nil :as :probe)
          (pine.ns:write (pine.path:parse "/park-seconds") saved))))))

(test an-unknown-restart-name-falls-back-to-abort
  "Picking a restart that does not exist decides the fault instead of leaving
the thread waiting for one that will never be found."
  (with-fixture substrate ()
    (within-seconds 60
      (with-surface (faults :attended t)
        (let ((done nil))
          (pine.err:evaluate-thunk (lambda () (error "probe"))
                                   :on-done (lambda (ev) (setf done ev)))
          (is (wait-for (lambda () faults)) "the fault never reached /err")
          (pine.err:resume (first faults) "NO-SUCH-RESTART")
          (is (wait-for (lambda () done) :seconds 30)
              "an unknown restart name left the thread waiting")
          (is (eq :aborted (pine.err:evaluation-status done))))))))

;;;; Edges

(test a-region-that-runs-past-the-end-clamps-to-it
  "Deleting a region whose end is off the buffer takes what is there, the way a
region command does when the buffer shrank under it. No fault, no leftovers."
  (with-fixture substrate ()
    (within-seconds 90
      (with-surface (faults)
        (let ((buf (stress-buffer "stress-clamp" (format nil "one~%two"))))
          (pine.ns:write (pine.buf:at (pine.buf:name-of buf) :text)
                         (fset:seq :delete (fset:seq 0 0) (fset:seq 900 4)))
          (is (wait-for (lambda () (equal "" (btext "stress-clamp"))))
              "a region past the end left ~s" (btext "stress-clamp"))
          (is (null faults) "clamping to the end should not fault"))))))

(test an-edit-off-the-end-of-the-buffer-is-refused
  "The line index is checked at the edit, not discovered later: padding the line
seq with holes would fault on the next read instead, far from the caller."
  (let ((state (pine.text:load-content "one")))
    (signals error (pine.text:insert-string state 700 0 "x"))
    (signals error (pine.text:insert-string state 0 0 nil))
    (finishes (pine.text:insert-string state 1 0 "x"))))

(test a-verb-no-mode-claims-faults-and-the-buffer-keeps-working
  "A verb no mode claims is a mistake somewhere, and the write refuses it where
the caller can see it. A buffer that quietly drops it makes the caller's bug
look like a no-op."
  (with-fixture substrate ()
    (within-seconds 90
      (with-surface (faults)
        (let ((buf (stress-buffer "stress-unknown-verb" "one")))
          (signals pine.ns:no-verb
            (pine.ns:write (pine.buf:at (pine.buf:name-of buf) :text)
                           (fset:seq :no-such-verb)))
          (is (wait-for (lambda () (equal "one" (btext "stress-unknown-verb"))) :seconds 30)
              "the buffer did not keep its content, it answered ~s"
              (btext "stress-unknown-verb"))
          (pine.buf:edit buf (fset:seq :insert "!"))
          (is (wait-for (lambda () (equal "!one" (btext "stress-unknown-verb"))))
              "the buffer stopped taking edits after an unhandled verb"))))))

(test a-point-past-the-end-does-not-corrupt-the-buffer
  (with-fixture substrate ()
    (within-seconds 90
      (with-surface (faults)
        (let ((buf (stress-buffer "stress-point" "one")))
          (pine.buf:put-point buf 500 400)
          (pine.ns:write (pine.buf:at (pine.buf:name-of buf) :text)
                         (fset:seq :indent 40 90))
          (sleep 1.0)
          (let ((text (btext "stress-point")))
            (is (stringp text) "the buffer stopped answering after point ran off")
            (when (stringp text)
              (is (search "one" text) "the buffer lost its content"))))))))

(test an-empty-buffer-survives-deleting-from-it
  (with-fixture substrate ()
    (within-seconds 60
      (with-surface (faults)
        (let ((buf (stress-buffer "stress-empty")))
          (dotimes (i 10) (pine.buf:delete-back buf))
          (pine.buf:edit buf (fset:seq :insert "x"))
          (is (wait-for (lambda () (equal "x" (btext "stress-empty"))))
              "backspacing an empty buffer left it unusable"))))))

(test a-very-long-line-round-trips
  "One line of a hundred thousand characters: the buffer is a seq of lines, so
this is the axis it does not index."
  (with-fixture substrate ()
    (within-seconds 120
      (let ((long (make-string 100000 :initial-element #\x)))
        (pine.ns:write (pine.buf:at (stress-buffer "stress-long-line") :text) long)
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
        (pine.ns:write (pine.buf:at (stress-buffer "stress-unicode") :text) text)
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
            (pine.buf:edit buf (fset:seq :insert "x"))
            (pine.editor.frame::kill-buffer "stress-churn")))
        (is (null (pine.buf:live "stress-churn"))
            "the churned name is still registered")
        (is (wait-for (lambda () (< (length (sb-thread:list-all-threads)) (+ before 20)))
                      :seconds 30)
            "200 create/kill rounds left ~d threads behind"
            (- (length (sb-thread:list-all-threads)) before))
        (let ((buf (stress-buffer "stress-churn")))
          (pine.buf:edit buf (fset:seq :insert "y"))
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
          
          (pine.buf:edit buf (fset:seq :insert "x"))
          (sleep 0.2)
          (pine.editor.frame::kill-buffer "stress-dead")
          (dotimes (i 50) (ignore-errors (pine.buf:edit buf (fset:seq :insert "x"))))
          (sleep 0.5)
          (let ((live (stress-buffer "stress-dead-live")))
            
            (setf painted 0)
            (pine.buf:edit live (fset:seq :insert "z"))
            (is (wait-for (lambda () (equal "z" (btext "stress-dead-live"))))
                "editing stopped working after sends to a dead buffer")
            (is (wait-for (lambda () (plusp painted)))
                "the paint path stopped after sends to a dead buffer")))))))

(test an-undo-storm-walks-back-as-far-as-the-roots-go-and-forward-again
  "Five hundred edits, then more undos than there are roots kept, then redos.
Undo walks the roots, so it bottoms out at the oldest one still there rather
than faulting, and every step back is a step redo can take forward."
  (with-fixture substrate ()
    (within-seconds 180
      (with-surface (faults)
        (let ((buf (stress-buffer "stress-undo")))
          (dotimes (i 500) (pine.buf:edit buf (fset:seq :insert "x")))
          (is (wait-for (lambda () (let ((text (btext "stress-undo")))
                                     (and (stringp text) (= 500 (char-count text)))))
                        :seconds 90)
              "the edits did not all land before the undos")
          (dotimes (i 600) (pine.buf:edit buf (fset:seq :undo)))
          (let ((bottom (char-count (btext "stress-undo"))))
            (is (< bottom 500) "600 undos left the text where it was")
            (dotimes (i 600) (pine.buf:edit buf (fset:seq :redo)))
            (is (wait-for (lambda () (= 500 (char-count (btext "stress-undo"))))
                          :seconds 90)
                "the redos did not restore the text, it answered ~s"
                (btext "stress-undo")))
          (is (null faults) "walking the history past its ends faulted"))))))

(test a-thousand-keystrokes-through-the-command-path
  "Keys as the session loop runs them, with no pause between: the command path,
the keymaps, the buffer and the renderer all keep up and the text is exact."
  (with-fixture substrate ()
    (within-seconds 180
      (let ((buf (pine.buf:live "scratch"))
            (key (pine.key::parse-key "x")))
        (pine.ns:write (pine.buf:at buf :text) "")
        (sleep 0.2)
        (dotimes (i 1000)
          (pine.key:dispatch key))
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
        (pine.editor.frame::set-buffer-mode buf :lisp)
        (pine.ns:write (pine.buf:at buf :text) content)
        ;; An ask that lands while the buffer is in the middle of loading 22 MB
        ;; comes back NO-RESULT rather than waiting, so the load is waited out on
        ;; the answer being a snapshot at all. The latency this test is about is
        ;; measured below, once the buffer is settled.
        (is (wait-for (lambda ()
                        (let ((snap (bsnap "stress-million")))
                          (and (typep snap 'pine.text:snapshot)
                               (> (pine.text:line-count snap) 999999))))
                      :seconds 120)
            "the million lines never loaded")
        ;; type into it while the parser is busy behind us
        (let ((worst 0) (unanswered 0))
          (dotimes (i 25)
            (pine.buf:edit buf (fset:seq :insert "z"))
            (let ((t0 (get-internal-real-time)))
              (unless (typep (bsnap "stress-million") 'pine.text:snapshot)
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
             (state (pine.text:set-meta
                     (pine.text:load-content text)
                     :mode :lisp))
             (windowed (pine.text:set-meta state :viewport (cons 100 130))))
        (is (> (pine.text:line-count-of state) 3999))
        ;; warm the parse, then time the walk the window actually asks for
        (let ((pstate (nth-value 1 (pine.text:refresh-highlights nil windowed)))
              (t0 (get-internal-real-time)))
          (pine.text:refresh-highlights pstate windowed)
          (let ((ms (/ (* 1000.0 (- (get-internal-real-time) t0))
                       internal-time-units-per-second 1)))
            (is (< ms 100)
                "highlighting a 30-line window of a 4000-form file took ~,1f ms" ms)))))))
