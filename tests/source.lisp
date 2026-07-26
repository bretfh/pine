(in-package :pine.test)

(def-suite* :pine.source :in :pine)

(test split-cuts-on-the-character-and-honours-a-limit
  (is (equal '("a" "b" "c") (pine.source:split "a:b:c" #\:)))
  (is (equal '("a" "b:c") (pine.source:split "a:b:c" #\: 2)))
  (is (equal '("" "a") (pine.source:split ":a" #\:)))
  (is (equal '("a" "") (pine.source:split "a:" #\:)))
  (is (equal '("") (pine.source:split "" #\:))))

(test lines-drops-the-empty-ones
  (is (equal '("a" "b") (pine.source:lines (format nil "a~%~%b~%")))))

(test starts-with-compares-the-prefix
  (is-true (pine.source:starts-with "abcdef" "abc"))
  (is-true (pine.source:starts-with "abc" "abc"))
  (is-true (pine.source:starts-with "abc" ""))
  (is-false (pine.source:starts-with "abc" "abcd"))
  (is-false (pine.source:starts-with "abcdef" "bcd")))

(test first-number-reads-the-leading-figure
  (is (= 42 (pine.source:first-number "vol: 42%")))
  (is (= 0.35 (pine.source:first-number "Volume: 0.35 [on]")))
  (is (= 7 (pine.source:first-number "7")))
  (is (null (pine.source:first-number "no digits here"))))

(test json-parses-or-answers-nothing
  (let ((parsed (pine.source:json "{\"a\": 1}")))
    (is (= 1 (gethash "a" parsed))))
  (is (null (pine.source:json "not json"))))

;;;; Faults. A source that keeps failing backs off and then stops, so a broken
;;;; feed is visible rather than spinning or silently dead.

(defun faulting-source ()
  (pine.source::make-source :name :probe))

(test a-fault-is-counted-and-backs-off-further-each-time
  (let ((src (faulting-source))
        (*error-output* (make-broadcast-stream)))
    (let ((first (pine.source::%source-fault src (make-condition 'simple-error :format-control "probe")))
          (second (pine.source::%source-fault src (make-condition 'simple-error :format-control "probe"))))
      (is (= 2 (pine.source::source-faults src)))
      (is (< first second)))))

(test the-backoff-is-capped
  (let ((src (faulting-source))
        (*error-output* (make-broadcast-stream)))
    (dotimes (i 20) (pine.source::%source-fault src (make-condition 'simple-error :format-control "probe")))
    (is (<= (pine.source::%source-fault src (make-condition 'simple-error :format-control "probe")) 60))))

(test a-source-is-disabled-once-it-passes-the-fault-cap
  (let ((src (faulting-source))
        (*error-output* (make-broadcast-stream)))
    (dotimes (i (1- pine.source::*source-fault-cap*))
      (pine.source::%source-fault src (make-condition 'simple-error :format-control "probe")))
    (is-false (pine.source::source-stopped src))
    (pine.source::%source-fault src (make-condition 'simple-error :format-control "probe"))
    (is-true (pine.source::source-stopped src))))

(test a-success-clears-the-consecutive-count
  (let ((src (faulting-source))
        (*error-output* (make-broadcast-stream)))
    (pine.source::%source-fault src (make-condition 'simple-error :format-control "probe"))
    (pine.source::%source-ok src)
    (is (= 0 (pine.source::source-faults src)))))

(test a-fault-records-what-went-wrong
  (let ((src (faulting-source))
        (*error-output* (make-broadcast-stream)))
    (pine.source::%source-fault src (make-condition 'simple-error
                                                    :format-control "probe text"))
    (is (search "probe text" (pine.source::source-last-fault src)))))

(test a-fault-is-loud-on-the-error-stream
  (let ((src (faulting-source)))
    (let ((report (with-output-to-string (*error-output*)
                    (pine.source::%source-fault
                     src (make-condition 'simple-error
                                         :format-control "probe text")))))
      (is (search "probe text" report)))))

(test ref-of-gets-or-makes-the-named-ref
  (let ((ref (pine.source:ref-of :probe-source-ref)))
    (is (eq ref (pine.source:ref-of :probe-source-ref)))
    (is (eq ref (pine.state.ref:find-ref :probe-source-ref)))))

(test defpoll-registers-a-starter-under-its-name
  (pine.source:defpoll probe-poll 5 42)
  (is (not (null (gethash 'probe-poll pine.source::*source-defs*))))
  (remhash 'probe-poll pine.source::*source-defs*))
