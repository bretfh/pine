(in-package :pine.test)

(def-suite* :pine.async :in :pine)

;;;; Parsing happens on the buffer's parser actor, not in its receive. What that
;;;; has to buy is here: an edit answers immediately whatever the parse costs,
;;;; the highlights that eventually arrive are the ones a full walk would give,
;;;; and a parser that is slow, wedged or gone leaves the buffer editable.

(defparameter +test-viewport+ '(0 . 200)
  "The window these buffers pretend to be shown in. Highlights are painted, so a
buffer no window has claimed is not walked at all; the renderer sends this in a
live session.")

(defun lisp-buffer (name content)
  (let ((buf (pine.editor.frame::make-buffer name :content content)))
    (pine.editor.frame::set-buffer-mode buf :lisp)
    (pine.buf:showing name (fset:seq (car +test-viewport+) (cdr +test-viewport+)))
    (sleep 0.2)
    buf))

(defun highlights-of (name)
  "NAME's highlights as the buffer currently holds them. Asked for directly: a
snapshot only carries highlights when one is built for a subscriber."
  (pine.ns:read (pine.buf:at name :face)))

(defun why-no-highlights (name)
  "Everything that has to be true for colours to land, so a failure says which
of them was not."
  (let* ((buf (pine.editor.frame::buffer name))
         (parser (pine.buf:parser-of name))
         (thread (when parser
                   (let ((box (sento.actor-cell:msgbox parser)))
                     (when (typep box 'sento.messageb:message-box/bt)
                       (slot-value box 'sento.messageb::queue-thread)))))
         (faults (pine.ns:read (pine.path:parse "/err/*") (fset:empty-map))))
    (format nil "buffer=~a parser=~a alive=~a running=~a mode=~s viewport=~s ~
                 watched=~a text=~a grammar=~a parked=~d told=~s space=~a did=~s ~
                 log=~s~@[ last-fault=~s~]"
            (if buf "yes" "no") (if parser "yes" "no")
            (if (and thread (bordeaux-threads-2:thread-alive-p thread)) "yes" "no")
            (if (and parser (sento.actor-cell:running-p parser)) "yes" "no")
            (pine.ns:read (pine.buf:at name :mode))
            (pine.buf:asked name :viewport)
            (if (pine.ns:watched (pine.buf:at name :text)) "yes" "no")
            (fset:size (or (pine.ns:held (pine.buf:at name :text)) (fset:empty-seq)))
            (if (pine.ts.runtime:ensure-language
                 (pine.core.server:ts-runtime *server*) :commonlisp)
                "loaded" "MISSING")
            (fset:size faults)
            ;; the space it was told about is compared, not printed: a whole
            ;; namespace in a failure message buries the failure
            (let ((told (pine.buf:asked name :told)))
              (and told (subseq told 0 (min 3 (length told)))))
            (if (eq (fourth (pine.buf:asked name :told)) pine.ns:*space*)
                "same" "DIFFERENT")
            (pine.ts.parser:did name)
            ;; a parse that produced nothing says so at /log, so the reason is
            ;; already written down by the time this is asked
            (pine.ns:read (pine.path:parse "/log"))
            (let ((last nil))
              (fset:do-map (p v faults) (declare (ignore p))
                (setf last (and (fset:map? v) (fset:lookup v :condition))))
              last))))

(defun full-walk-of (content &key (viewport +test-viewport+))
  "The highlights a synchronous walk gives for CONTENT over the same window."
  (let ((state (pine.text.buffer:set-meta
                (pine.text.buffer:set-meta
                 (pine.text.buffer:load-content content) :mode :lisp)
                :viewport viewport)))
    (pine.text.buffer:refresh-highlights nil state)))

(test an-edit-is-answered-before-its-parse-is
  "The buffer commits and repaints on its own thread; the parse happens
elsewhere. So the snapshot carries the edit whether or not the colours have
caught up."
  (with-fixture substrate ()
    (within-seconds 30
      (let ((buf (lisp-buffer "async-edit" "(defun f (x) x)")))
        (pine.text.buffer:edit buf (fset:seq :insert "y"))
        (is (wait-for (lambda () (search "y" (btext "async-edit"))) :seconds 5)
            "the edit did not land promptly")))))

(test highlights-arrive-and-match-a-full-walk
  (with-fixture substrate ()
    (within-seconds 60
      (let ((content (format nil "(defun f (x)~%  \"doc\"~%  (+ x 1))")))
        (lisp-buffer "async-hl" content)
        (is (wait-for (lambda () (highlights-of "async-hl")) :seconds 30)
            "no highlights ever arrived: ~a" (why-no-highlights "async-hl"))
        (is (equal (sort (copy-list (full-walk-of content)) #'< :key #'first)
                   (sort (copy-list (highlights-of "async-hl")) #'< :key #'first))
            "the highlights that landed differ from a full walk")))))

(test highlights-catch-up-after-a-burst-of-edits
  "Backpressure keeps one parse in flight, so a burst produces one parse per
completed parse. What matters is where it settles: the final colours describe the
final text."
  (with-fixture substrate ()
    (within-seconds 90
      (let ((buf (lisp-buffer "async-burst" "(defun f (x) x)")))
        (dotimes (i 20) (pine.text.buffer:edit buf (fset:seq :insert "z")))
        (is (wait-for (lambda () (= 20 (count #\z (btext "async-burst")))) :seconds 30)
            "the edits did not all land")
        (let ((settled (btext "async-burst")))
          (is (wait-for (lambda ()
                          (equal (sort (copy-list (full-walk-of settled)) #'< :key #'first)
                                 (sort (copy-list (highlights-of "async-burst"))
                                       #'< :key #'first)))
                        :seconds 45)
              "the highlights never caught up with the settled text"))))))

(test a-newline-lands-before-its-indent
  "Electric indent is an answer from the parser, so the line exists immediately
and takes its column when the parse says what it is."
  (with-fixture substrate ()
    (within-seconds 60
      ;; balanced source, so the body column is not a matter of opinion
      (let ((buf (lisp-buffer "async-indent" (format nil "(defun f ()~%(bar))"))))
        (pine.text.buffer:put-point buf 0 11)
        (sleep 0.2)
        (pine.text.buffer:edit buf (fset:seq :newline))
        (is (wait-for (lambda () (= 3 (length (pine.text.buffer:split-lines
                                               (btext "async-indent")))))
                      :seconds 10)
            "the newline did not land")
        (is (wait-for (lambda ()
                        (let ((lines (pine.text.buffer:split-lines (btext "async-indent"))))
                          (and (= 3 (length lines))
                               (= 2 (pine.text.buffer:line-indent-width (second lines))))))
                      :seconds 30)
            "the indent never reached the defun body, the line reads ~s"
            (second (pine.text.buffer:split-lines (btext "async-indent"))))))))

(test the-highlights-that-land-match-a-fresh-parse-after-any-verb-sequence
  "The descriptors are what let the parser shift its tree instead of rebuilding
it, and a wrong one produces a plausible wrong tree rather than an error. So every
verb is driven through a real buffer and the colours that settle are compared
against a parse of the same lines from scratch.

This is the check that would have caught a newline being described as a line
insertion: whole-line shifts cannot express a split."
  (with-fixture substrate ()
    (let ((*num-trials* 4))
      (for-all ((seed (gen-integer :min 0 :max 100000)))
        (within-seconds 90
          (let* ((name (format nil "async-prop-~d" seed))
                 (buf (lisp-buffer name (format nil "(defun f (x)~%  (+ x 1))~%(f 2)")))
                 (state seed))
            (flet ((next (n)
                     (setf state (mod (+ (* state 1103515245) 12345) 2147483648))
                     (mod state n)))
              (dotimes (i 12)
                (let ((snap (bsnap name)))
                  (case (next 5)
                    (0 (pine.text.buffer:edit buf (fset:seq :insert "z")))
                    (1 (pine.text.buffer:edit buf (fset:seq :newline)))
                    (2 (pine.text.buffer:delete-back buf))
                    (3 (pine.text.buffer:put-point
                        buf (next (max 1 (pine.text.buffer:line-count snap)))
                        (next 4)))
                    (t (pine.text.buffer:edit buf (fset:seq :insert "(g 1)")))))
                (sleep 0.05)))
            (sleep 1.5)
            (let ((settled (btext name)))
              (is (wait-for (lambda ()
                              (equal (sort (copy-list (full-walk-of settled)) #'< :key #'first)
                                     (sort (copy-list (highlights-of name)) #'< :key #'first)))
                            :seconds 30)
                  "seed ~d: the colours that settled disagree with a fresh parse of~% ~s"
                  seed settled))
            (pine.editor.frame::kill-buffer name)))))))

(test every-parser-started-at-once-produces-a-tree
  "Starting a parser is grammar loading, a TSParser and a language claim, and
each of those happens on whichever thread asked. A parser whose language did not
take answers every parse with no tree at all, quietly, so a buffer opened at the
same moment as ten others is the case that has to hold."
  (with-fixture substrate ()
    (within-seconds 90
      (let ((names (loop :for i :from 0 :below 10
                         :collect (format nil "async-many-~d" i))))
        (dolist (name names)
          (pine.editor.frame::make-buffer name :content "(defun f (x) x)")
          (pine.editor.frame::set-buffer-mode name :lisp))
        ;; every window claims its buffer at once, so ten parsers start together
        (dolist (name names)
          (pine.buf:showing name (fset:seq (car +test-viewport+)
                                           (cdr +test-viewport+))))
        (dolist (name names)
          (is (wait-for (lambda () (highlights-of name)) :seconds 30)
              "~a never got colours: ~a" name (why-no-highlights name)))
        (dolist (name names) (pine.editor.frame::kill-buffer name))))))

(test a-buffer-with-no-language-needs-no-parser
  (with-fixture substrate ()
    (within-seconds 20
      (let ((buf (pine.editor.frame::make-buffer "async-plain")))
        (pine.editor.frame::set-buffer-mode buf :text)
        (pine.text.buffer:edit buf (fset:seq :insert "plain"))
        (is (wait-for (lambda () (equal "plain" (btext "async-plain"))))
            "a text buffer stopped editing")
        (is (null (pine.buf:parser-of (pine.text.buffer:name-of buf)))
            "a buffer with no language should have no parser")))))

(test a-wedged-parser-leaves-the-buffer-editable
  "The isolation claim, tested: the parser is held in a fault and the buffer goes
on taking edits, keeping the colours it already had."
  (with-fixture substrate ()
    (within-seconds 90
      (with-surface (faults :attended (lambda (ev) (declare (ignore ev)) t))
        (let* ((buf (lisp-buffer "async-wedge" "(defun f (x) x)"))
               (parser (pine.buf:parser-of (pine.text.buffer:name-of buf))))
          (is (not (null parser)) "a lisp buffer should have a parser")
          ;; a verb its receive does not know faults it, and the fault is held
          (sento.actor:tell parser '(:no-such-verb))
          (is (wait-for (lambda () faults) :seconds 30)
              "the parser never faulted")
          (dotimes (i 5) (pine.text.buffer:edit buf (fset:seq :insert "q")))
          (is (wait-for (lambda () (= 5 (count #\q (btext "async-wedge")))) :seconds 20)
              "the buffer stopped editing while its parser was wedged")
          (is (stringp (btext "scratch"))
              "the rest of the daemon stopped answering"))))))

;;;; The attach handshake. The verbs crossing between images are a contract, and
;;;; an image that does not speak this one should hear about it at attach rather
;;;; than one unknown verb at a time.

(test the-protocol-version-comes-from-the-asd
  (is (equal (asdf:component-version (asdf:find-system :pine))
             (pine.core.attach:protocol-version))
      "the version of record is the .asd, so there is no second place to bump")
  (is-true (pine.core.attach:version-accepted-p (pine.core.attach:protocol-version)))
  (is-false (pine.core.attach:version-accepted-p "0.0.0-not-this"))
  (is-false (pine.core.attach:version-accepted-p nil)
            "an app that reports no version at all is not accepted either"))

(defun attach-probe (name version)
  "Send the daemon an attach announcing VERSION, and answer what it replies.

The daemon answers a display actor by remote ref, so the probe stands one up in
this image and hands over its uri: the same path a frontend takes, without a
second process."
  (ensure-remoting)
  (let* ((sys (pine.core.server:actor-system *server*))
         (heard nil)
         (display (sento.actor-context:actor-of
                   sys :name name :dispatcher :pinned :state nil
                   :receive (lambda (m) (push m heard) nil)))
         (listener (pine.core.attach:start-attach-listener *server*)))
    (unwind-protect
         (progn
           (sento.actor:tell listener
             (list :attach
                   :display-uri (pine.core.server:local-uri name +agent-port+)
                   :kind :probe :version version))
           (wait-for (lambda () heard) :seconds 15)
           (first heard))
      (sento.actor-context:stop sys display)
      (sento.actor-context:stop sys listener))))

(test an-attach-of-the-wrong-version-is-refused-with-a-reason
  (with-fixture substrate ()
    (within-seconds 60
      (let ((reply (attach-probe (format nil "vprobe-bad-~a" (gensym)) "0.0.0-not-this")))
        (is (eq :refused (first reply))
            "a wrong-version attach should be refused, the daemon said ~s" reply)
        (when (eq :refused (first reply))
          (is (search (pine.core.attach:protocol-version) (getf (rest reply) :reason))
              "the refusal should name the version this daemon speaks: ~s"
              (getf (rest reply) :reason)))))))

(test an-attach-of-the-right-version-is-accepted
  (with-fixture substrate ()
    (within-seconds 60
      (let ((reply (attach-probe (format nil "vprobe-good-~a" (gensym))
                                 (pine.core.attach:protocol-version))))
        (is (eq :attached (first reply))
            "a matching attach should be accepted, the daemon said ~s" reply)
        (when (eq :attached (first reply))
          (is (equal (pine.core.attach:protocol-version) (getf (rest reply) :version))
              "the acceptance carries the daemon's version too"))))))

(test killing-a-buffer-takes-its-parser-with-it
  (with-fixture substrate ()
    (within-seconds 60
      (let* ((buf (lisp-buffer "async-kill" "(defun f (x) x)"))
             (parser (pine.buf:parser-of (pine.text.buffer:name-of buf)))
             (before (length (sb-thread:list-all-threads))))
        (is (not (null parser)))
        (pine.editor.frame::kill-buffer "async-kill")
        (is (wait-for (lambda () (< (length (sb-thread:list-all-threads)) before))
                      :seconds 20)
            "killing the buffer left its parser running")))))
