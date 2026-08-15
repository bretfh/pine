(in-package :pine.test)

(def-suite* :pine.run :in :pine)

(defun wait-until (test &key (seconds 5))
  (loop :repeat (round (/ seconds 0.02))
        :when (funcall test) :do (return t)
        :do (sleep 0.02)))

(test a-box-is-replaced-by-a-pure-function-of-what-it-held
  (let ((c (pine/data:box 1)))
    (is (eql 1 (pine/data:held c)))
    (is (eql 2 (pine/data:swap! c #'1+)))
    (is (eql 2 (pine/data:held c)))
    (is (eql 5 (pine/data:swap! c #'+ 3)))))

(test a-hundred-threads-swapping-one-box-lose-nothing
  "No locks anywhere: the value is immutable and the swap retries. A count that
comes out short is the whole reason locks are usually reached for."
  (let ((c (pine/data:box 0))
        (tasks nil))
    (dotimes (i 100)
      (push (pine/run/task:spawn (format nil "probe-~d" i)
                                 (lambda ()
                                   (dotimes (n 100)
                                     (pine/data:swap! c #'1+))))
            tasks))
    (is-true (wait-until (lambda () (notany #'pine/run/task:alivep tasks))
                         :seconds 30))
    (is (eql 10000 (pine/data:held c)))
    (mapc #'pine/run/task:stop tasks)))

(test a-cas-that-loses-the-race-says-so
  (let ((c (pine/data:box :a)))
    (is-true (pine/data:cas c :a :b))
    (is-false (pine/data:cas c :a :c))
    (is (eq :b (pine/data:held c)))))

(test a-task-runs-and-answers
  (let ((tk (pine/run/task:spawn "probe-answer" (lambda () (+ 1 2)))))
    (is-true (wait-until (lambda () (not (pine/run/task:alivep tk)))))
    (is (eql 3 (pine/run/task:answered tk)))
    (is (member tk (pine/run/task:tasks)))))

(test a-task-that-faults-keeps-the-fault-and-not-the-image
  (let ((tk (pine/run/task:spawn "probe-fault" (lambda () (error "a probe")))))
    (is-true (wait-until (lambda () (not (pine/run/task:alivep tk)))))
    (is (typep (pine/run/task:fault tk) 'error))))

(test what-repeats-runs-on-the-images-one-clock
  "Nothing sleeps in a loop to repeat: the wheel timer the actor system already
carries is what says when, and a tick runs on a worker rather than on it."
  (unwind-protect
       (progn
         (pine:start)
         (let ((n (pine/data:box 0))
               (threads (length (pine/run/task:tasks))))
           (pine/run/timer:every-seconds 0.05 (lambda () (pine/data:swap! n #'1+))
                                         :as :probe-tick)
           (is (member :probe-tick (pine/run/timer:names)))
           (is-true (wait-until (lambda () (> (pine/data:held n) 3))))
           (is (eql threads (length (pine/run/task:tasks)))
               "and it took no thread of its own")
           (pine/run/timer:cancel :probe-tick)
           (let ((stopped (pine/data:held n)))
             (sleep 0.3)
             (is (<= (- (pine/data:held n) stopped) 1)))
           (is (null (member :probe-tick (pine/run/timer:names))))))
    (pine:stop)))

(test an-endpoint-takes-messages-in-order-and-answers-what-it-is-asked
  "An agent is a sento actor: one at a time, in the order they were sent."
  (unwind-protect
       (progn
         (pine:start)
         (let* ((seen (pine/data:box nil))
                (a (pine/run/agent:agent
                    "probe-agent"
                    (lambda (message)
                      (if (eq :ping message)
                          (list :answered message)
                          (pine/data:swap! seen (lambda (all) (cons message all))))))))
           (unwind-protect
                (progn
                  (pine/run/agent:tell a :one)
                  (pine/run/agent:tell a :two)
                  (is-true (wait-until (lambda () (= 2 (length (pine/data:held seen))))))
                  (is (equal '(:one :two) (reverse (pine/data:held seen))))
                  (is (equal '(:answered :ping) (pine/run/agent:ask a :ping)))
                  (is (eq a (pine/run/agent:agent-named "probe-agent"))))
             (pine/run/agent:stop a))
           (is (null (pine/run/agent:agent-named "probe-agent")))))
    (pine:stop)))

(test asking-from-inside-a-receive-is-refused-rather-than-hung
  "A receive owes its mailbox an answer, so it may not wait for one."
  (unwind-protect
       (progn
         (pine:start)
         (let* ((said (pine/data:box nil))
                (a nil))
           (setf a (pine/run/agent:agent
                    "probe-blocking"
                    (lambda (message)
                      (declare (ignore message))
                      (pine/data:put!
                       said
                       (handler-case (pine/run/agent:ask a :ping :timeout 1)
                         (pine/run/agent:blocking-ask () :refused))))))
           (unwind-protect
                (progn
                  (pine/run/agent:tell a :go)
                  (is-true (wait-until (lambda () (pine/data:held said))))
                  (is (eq :refused (pine/data:held said))))
             (pine/run/agent:stop a))))
    (pine:stop)))

(test two-threads-writing-one-table-both-land
  "A table is a map in a box, so a write is a swap and neither of two threads
writing different keys reads the other out of it."
  (let ((table (pine/data:table))
        (tasks nil))
    (dotimes (i 20)
      (let ((i i))
        (push (pine/run/task:spawn
               (format nil "probe-table-~d" i)
               (lambda () (dotimes (n 50)
                            (pine/data:keep! table (format nil "~d-~d" i n) n))))
              tasks)))
    (is-true (wait-until (lambda () (notany #'pine/run/task:alivep tasks))
                         :seconds 30))
    (is (eql 1000 (pine/data:size (pine/data:all table))))
    (is (eql 7 (pine/data:at (pine/data:all table) "3-7")))
    (pine/data:drop! table "3-7")
    (is (null (pine/data:at (pine/data:all table) "3-7")))
    (mapc #'pine/run/task:stop tasks)))

(test claiming-a-place-hands-the-loser-the-winners-object
  (let ((table (pine/data:table)))
    (is (eq :first (pine/data:claim table "probe" :first)))
    (is (eq :first (pine/data:claim table "probe" :second))
        "the second caller gets what the first put there, not its own")
    (pine/data:clear! table)
    (is-true (pine/data:emptyp (pine/data:all table)))))

(test live-nodes-are-read-on-one-sweep-however-many-are-watched
  "A watcher is not a thread. Twenty live nodes cost one entry on the clock."
  (unwind-protect
       (progn
         (pine:start)
         (let ((threads (length (pine/run/task:tasks)))
               (heard (pine/data:box 0)))
           (dotimes (i 20)
             (let ((n (pine.world.world:ensure pine.world.world:*world*
                                               (format nil "probe-live-~d" i))))
               (pine/fs/watch:watch n (lambda (of value)
                                        (declare (ignore of value))
                                        (pine/data:swap! heard #'1+))
                                    :every 0.05 :poll t)))
           (is (= 20 (length (pine/fs/watch:polled))))
           (is (eql threads (length (pine/run/task:tasks)))
               "and not one of them took a thread")
           (is (member :watch (pine/run/timer:names))
               "they are read on one sweep")
           (pine/fs/watch:forget-all)
           (is (null (member :watch (pine/run/timer:names))))))
    (pine:stop)))

(test the-image-knows-where-its-c-libraries-are
  "A binary is run from a terminal that never entered the build environment.
It carries the profile it was built against, so tree-sitter is there and a
buffer highlights."
  (is-true (pine/run/libs:dirs))
  (is (find "tree-sitter" (pine/run/libs:dirs) :test #'search)
      "the grammars sit under lib/tree-sitter, not lib")
  (pine/run/libs:attend)
  (is (every (lambda (dir) (member (pathname dir) cffi:*foreign-library-directories*
                                   :test #'equal))
             (pine/run/libs:dirs))
      "and the loader is told about each of them"))
