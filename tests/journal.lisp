(in-package :pine.test)

(def-suite* :pine.journal :in :pine)

;;;; Working state that survives the daemon. Restarting it is the repair tool,
;;;; so it must not be the thing that loses your work. Each check here writes,
;;;; drops the live buffer entirely, and asks the store to bring it back.

(def-fixture journalled ()
  "A private store with the journal writer running against it."
  (pine.state.store:open-store ":memory:")
  (setf pine.state.journal:*snapshot-source* #'pine.editor.frame:snapshot-source)
  (unwind-protect (&body)
    (pine.state.journal:stop-journal-writer)
    (open-fresh-store)))

(defun settle-journal ()
  "Commit whatever is pending, the way the writer would."
  (pine.state.journal:flush-journal))

(test an-edit-becomes-a-row
  (with-fixture substrate ()
    (with-fixture journalled ()
      (within-seconds 30
        (let* ((buf (pine.editor.frame::make-buffer "j-rows"))
               (id (pine.text.buffer:buffer-local
                    (pine.core.actor:ask buf '(:get-state) :timeout 5) :id)))
          (pine.editor.frame::set-buffer-mode buf :text-mode)
          (is (not (null id)) "a buffer carries a durable id")
          (dotimes (i 3) (sento.actor:tell buf (list :insert :text "x")))
          (sleep 0.3)
          (settle-journal)
          (is (= 3 (pine.state.journal:journal-count id))
              "three edits should be three rows, found ~d"
              (pine.state.journal:journal-count id)))))))

(test a-buffer-comes-back-after-its-actor-is-gone
  "The point of the whole facility: the live buffer is killed outright and the
store rebuilds it from a snapshot plus the edits after it."
  (with-fixture substrate ()
    (with-fixture journalled ()
      (within-seconds 60
        (let* ((buf (pine.editor.frame::make-buffer "j-restore"))
               (id (pine.text.buffer:buffer-local
                    (pine.core.actor:ask buf '(:get-state) :timeout 5) :id)))
          (pine.editor.frame::set-buffer-mode buf :text-mode)
          (sento.actor:tell buf (list :replace-content :content "before"))
          (sleep 0.2)
          ;; a snapshot, then edits after it, which is the shape recovery has to
          ;; handle rather than either half alone
          (multiple-value-bind (name meta content tick)
              (pine.editor.frame:snapshot-source id)
            (pine.state.journal:snapshot! id name meta content tick))
          (sento.actor:tell buf (list :insert :text "!!"))
          (sleep 0.2)
          (settle-journal)
          ;; what the live buffer held is the answer the store has to reproduce,
          ;; rather than a string written down here twice
          (let ((was (btext "j-restore")))
            (pine.editor.frame::kill-buffer "j-restore")
            (is (null (pine.editor.frame::buffer "j-restore"))
                "the live buffer should be gone before restoring")
            (let ((restored (pine.editor.frame:restore-buffer id)))
              (is (not (null restored)) "the store did not know that buffer")
              (when restored
                (is (wait-for (lambda () (equal was (btext "j-restore"))) :seconds 20)
                    "restored buffer reads ~s, the live one held ~s"
                    (btext "j-restore") was)))))))))

(test a-snapshot-drops-the-rows-it-covers
  (with-fixture substrate ()
    (with-fixture journalled ()
      (within-seconds 60
        (let* ((buf (pine.editor.frame::make-buffer "j-compact"))
               (id (pine.text.buffer:buffer-local
                    (pine.core.actor:ask buf '(:get-state) :timeout 5) :id)))
          (pine.editor.frame::set-buffer-mode buf :text-mode)
          (dotimes (i 5) (sento.actor:tell buf (list :insert :text "y")))
          (sleep 0.3)
          (settle-journal)
          (is (plusp (pine.state.journal:journal-count id)))
          (multiple-value-bind (name meta content tick)
              (pine.editor.frame:snapshot-source id)
            (pine.state.journal:snapshot! id name meta content tick))
          (is (zerop (pine.state.journal:journal-count id))
              "the snapshot should have dropped the rows it covers, ~d left"
              (pine.state.journal:journal-count id))
          (is (equal "yyyyy" (nth-value 2 (pine.state.journal:buffer-snapshot id)))
              "the snapshot holds the content"))))))

(test the-store-lists-what-it-holds
  (with-fixture substrate ()
    (with-fixture journalled ()
      (within-seconds 30
        (let* ((buf (pine.editor.frame::make-buffer "j-listed"))
               (id (pine.text.buffer:buffer-local
                    (pine.core.actor:ask buf '(:get-state) :timeout 5) :id)))
          (pine.editor.frame::set-buffer-mode buf :text-mode)
          (multiple-value-bind (name meta content tick)
              (pine.editor.frame:snapshot-source id)
            (pine.state.journal:snapshot! id name meta content tick))
          (is (member "j-listed" (mapcar #'cdr (pine.state.journal:journaled-buffers))
                      :test #'equal)
              "the buffer should be listed, saw ~s"
              (pine.state.journal:journaled-buffers))
          (pine.state.journal:forget-buffer id)
          (is (not (member "j-listed"
                           (mapcar #'cdr (pine.state.journal:journaled-buffers))
                           :test #'equal))
              "forgetting should remove it"))))))

(test recording-costs-the-buffer-nothing
  "A row is a push onto a queue. If writing ever moved onto the buffer's thread
this is what would notice."
  (with-fixture substrate ()
    (with-fixture journalled ()
      (within-seconds 60
        (let ((buf (pine.editor.frame::make-buffer "j-latency")))
          (pine.editor.frame::set-buffer-mode buf :text-mode)
          (let ((worst 0))
            (dotimes (i 50)
              (sento.actor:tell buf (list :insert :text "z"))
              (let ((t0 (get-internal-real-time)))
                (bsnap "j-latency")
                (setf worst (max worst (/ (* 1000.0 (- (get-internal-real-time) t0))
                                          internal-time-units-per-second)))))
            (is (< worst 100)
                "a read took ~,1f ms while edits were being journalled" worst)))))))
