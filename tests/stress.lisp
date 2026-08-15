(in-package :pine.test)

(def-suite* :pine.stress :in :pine)

(test hundreds-of-buffers-answer-and-cost-no-threads
  "A buffer is an object, not a thread. Opening two hundred of them costs the
threads pine already had."
  (unwind-protect
       (progn
         (pine:start)
         (let ((threads (length (pine/run/task:tasks))))
           (dotimes (i 200)
             (let ((b (pine.edit.buffer:make-buffer (format nil "probe-~d" i))))
               (setf (pine.fs.node:contents b) (format nil "line ~d" i))))
           (is (<= 200 (length (pine.edit.buffer:buffers))))
           (is (equal "line 199"
                      (pine.fs.node:contents
                       (pine.edit.buffer:buffer-named "probe-199"))))
           (is (eql threads (length (pine/run/task:tasks)))
               "and not one of them took a thread")))
    (pine:stop)))

(test a-thousand-messages-arrive-in-order-and-none-are-lost
  (unwind-protect
       (progn
         (pine:start)
         (let* ((seen (pine/data:box nil))
                (a (pine/run/agent:agent
                    "probe-order"
                    (lambda (m) (pine/data:swap! seen (lambda (all) (cons m all)))))))
           (unwind-protect
                (progn
                  (dotimes (i 1000) (pine/run/agent:tell a i))
                  (is-true (wait-until (lambda () (= 1000 (length (pine/data:held seen))))
                                       :seconds 30))
                  (is (equal (loop :for i :below 1000 :collect i)
                             (reverse (pine/data:held seen)))
                      "in the order they were sent"))
             (pine/run/agent:stop a))))
    (pine:stop)))

(test readers-keep-reading-while-a-writer-edits
  "A read is a slot read of a value nobody is mutating, so a reader never sees
half an edit and never waits for one."
  (unwind-protect
       (progn
         (pine:start)
         (let* ((b (pine.edit.buffer:make-buffer "probe-race"))
                (torn (pine/data:box 0))
                (stop (pine/data:box nil))
                (readers
                  (loop :for i :below 4
                        :collect (pine/run/task:spawn
                                  (format nil "probe-reader-~d" i)
                                  (lambda ()
                                    (loop :until (pine/data:held stop)
                                          :for text := (pine.fs.node:contents b)
                                          :do (unless (or (equal text "")
                                                          (char= #\x (char text 0)))
                                                (pine/data:swap! torn #'1+))))))))
           (dotimes (n 300)
             (setf (pine.fs.node:contents b)
                   (make-string (1+ (mod n 40)) :initial-element #\x)))
           (pine/data:put! stop t)
           (mapc (lambda (tk) (pine/run/task:join tk :timeout 5)) readers)
           (is (zerop (pine/data:held torn))
               "a reader saw something that was never written")))
    (pine:stop)))

(test twenty-faults-at-once-all-come-back
  (unwind-protect
       (progn
         (pine:start)
         (pine/run/fault:forget-faults)
         (let ((tasks (loop :for i :below 20
                            :collect (pine/run/task:spawn
                                      (format nil "probe-fault-~d" i)
                                      (lambda ()
                                        (pine/run/fault:attempt
                                         (lambda () (error "a probe"))
                                         "probing"))))))
           (mapc (lambda (tk) (pine/run/task:join tk :timeout 10)) tasks)
           (is (= 20 (length (pine/run/fault:faults)))
               "every one of them was kept, and none of them took the image")))
    (pine/run/fault:forget-faults)
    (pine:stop)))

(test a-restart-nobody-offers-does-not-hang-the-thread-standing-there
  (unwind-protect
       (progn
         (pine:start)
         (let ((worker (pine/run/task:spawn
                        "probe-standing"
                        (lambda ()
                          (pine/run/fault:with-debugger
                            (pine/run/fault:attempt
                             (lambda ()
                               (with-simple-restart (probe-on "carry on")
                                 (error "a probe")))
                             "a probe"))))))
           (is-true (wait-until (lambda () (pine/run/fault:standing))))
           (let ((f (first (pine/run/fault:standing))))
             (is (null (pine/run/fault:resume f "NO-SUCH-RESTART"))
                 "a name nobody offers is refused rather than taken")
             (is-true (pine/run/fault:resume f "PROBE-ON"))
             (is-true (pine/run/task:join worker :timeout 10)
                      "and the thread went on its way"))))
    (pine/run/fault:forget-faults)
    (pine:stop)))

(test an-edit-that-runs-past-the-end-clamps-to-it
  (unwind-protect
       (progn
         (pine:start)
         (let ((b (pine.edit.buffer:make-buffer "probe-clamp")))
           (setf (pine.fs.node:contents b) "one
two")
           (pine.edit.buffer:goto! b 99 99)
           (is (= 1 (pine.edit.buffer:point-line b)) "point cannot leave the buffer")
           (is (= 3 (pine.edit.buffer:point-col b)))
           (pine.edit.buffer:delete-region! b 0 0 99 99)
           (is (equal "" (pine.fs.node:contents b)))))
    (pine:stop)))
