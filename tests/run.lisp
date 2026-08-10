(in-package :pine.test)

(def-suite* :pine.run :in :pine)

(defun wait-until (test &key (seconds 5))
  (loop :repeat (round (/ seconds 0.02))
        :when (funcall test) :do (return t)
        :do (sleep 0.02)))

(test a-box-is-replaced-by-a-pure-function-of-what-it-held
  (let ((c (pine.data:box 1)))
    (is (eql 1 (pine.data:held c)))
    (is (eql 2 (pine.data:swap! c #'1+)))
    (is (eql 2 (pine.data:held c)))
    (is (eql 5 (pine.data:swap! c #'+ 3)))))

(test a-hundred-threads-swapping-one-box-lose-nothing
  "No locks anywhere: the value is immutable and the swap retries. A count that
comes out short is the whole reason locks are usually reached for."
  (let ((c (pine.data:box 0))
        (tasks nil))
    (dotimes (i 100)
      (push (pine.run.task:spawn (format nil "probe-~d" i)
                                 (lambda ()
                                   (dotimes (n 100)
                                     (pine.data:swap! c #'1+))))
            tasks))
    (is-true (wait-until (lambda () (notany #'pine.run.task:alivep tasks))
                         :seconds 30))
    (is (eql 10000 (pine.data:held c)))
    (mapc #'pine.run.task:stop tasks)))

(test a-cas-that-loses-the-race-says-so
  (let ((c (pine.data:box :a)))
    (is-true (pine.data:cas c :a :b))
    (is-false (pine.data:cas c :a :c))
    (is (eq :b (pine.data:held c)))))

(test a-task-runs-and-answers
  (let ((tk (pine.run.task:spawn "probe-answer" (lambda () (+ 1 2)))))
    (is-true (wait-until (lambda () (not (pine.run.task:alivep tk)))))
    (is (eql 3 (pine.run.task:answered tk)))
    (is (member tk (pine.run.task:tasks)))))

(test a-task-that-faults-keeps-the-fault-and-not-the-image
  (let ((tk (pine.run.task:spawn "probe-fault" (lambda () (error "a probe")))))
    (is-true (wait-until (lambda () (not (pine.run.task:alivep tk)))))
    (is (typep (pine.run.task:fault tk) 'error))))

(test a-task-that-repeats-stops-when-it-is-told
  (let* ((n (pine.data:box 0))
         (tk (pine.run.task:each "probe-tick" 0.01
                                 (lambda () (pine.data:swap! n #'1+)))))
    (is-true (wait-until (lambda () (> (pine.data:held n) 3))))
    (pine.run.task:stop tk)
    (is-true (pine.run.task:join tk))
    (let ((stopped (pine.data:held n)))
      (sleep 0.1)
      (is (eql stopped (pine.data:held n))))))

(test a-mailbox-hands-messages-over-in-order
  (let ((m (pine.run.mailbox:mailbox)))
    (pine.run.mailbox:send m :one)
    (pine.run.mailbox:send m :two)
    (is (eql 2 (pine.run.mailbox:count m)))
    (is (eq :one (pine.run.mailbox:take m)))
    (is (eq :two (pine.run.mailbox:take m)))
    (is-true (pine.run.mailbox:emptyp m))))

(test an-actor-is-a-task-with-an-inbox
  (let* ((seen (pine.data:box nil))
         (tk (pine.run.task:actor
              "probe-actor"
              (lambda (message)
                (if (and (consp message) (eq :ask (first message)))
                    (pine.run.task:reply (second message)
                                         (list :answered (third message)))
                    (pine.data:swap! seen (lambda (all) (cons message all))))))))
    (unwind-protect
         (progn
           (pine.run.task:tell tk :hello)
           (is-true (wait-until (lambda () (pine.data:held seen))))
           (is (equal '(:hello) (pine.data:held seen)))
           (is (equal '(:answered :ping) (pine.run.task:ask tk :ping :timeout 5))))
      (pine.run.task:stop tk))))

(test two-threads-writing-one-table-both-land
  "A table is a map in a box, so a write is a swap and neither of two threads
writing different keys reads the other out of it."
  (let ((table (pine.data:table))
        (tasks nil))
    (dotimes (i 20)
      (let ((i i))
        (push (pine.run.task:spawn
               (format nil "probe-table-~d" i)
               (lambda () (dotimes (n 50)
                            (pine.data:keep! table (format nil "~d-~d" i n) n))))
              tasks)))
    (is-true (wait-until (lambda () (notany #'pine.run.task:alivep tasks))
                         :seconds 30))
    (is (eql 1000 (pine.data:size (pine.data:all table))))
    (is (eql 7 (pine.data:at (pine.data:all table) "3-7")))
    (pine.data:drop! table "3-7")
    (is (null (pine.data:at (pine.data:all table) "3-7")))
    (mapc #'pine.run.task:stop tasks)))

(test claiming-a-place-hands-the-loser-the-winners-object
  (let ((table (pine.data:table)))
    (is (eq :first (pine.data:claim table "probe" :first)))
    (is (eq :first (pine.data:claim table "probe" :second))
        "the second caller gets what the first put there, not its own")
    (pine.data:clear! table)
    (is-true (pine.data:emptyp (pine.data:all table)))))
