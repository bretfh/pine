(in-package :pine/test)

(def-suite* :pine/serve :in :pine)

(defun %through-the-wire (value)
  "VALUE written down, sent, and read back, the way it crosses to somebody who is
not a lisp: as json, and as json again on the way out."
  (let* ((out (pine/serve/json:render value))
         (back (pine/serve/json:parse out)))
    (values back out)))

(test what-crosses-is-json-and-comes-back-what-it-was
  "The wire carried fset objects, so every client had to be a lisp image with
fset. What crosses now is spelled, and the spelling is the whole of what a client
has to know."
  (dolist (each (list 42 "text" :a-word t nil
                      (list 1 2 3)
                      (list :title "x" :app "sh")
                      (d:map :a 1)
                      (d:seq 1 2)
                      (d:set 1 2)
                      (d:map :a (d:seq 1 (d:map :b 2)))))
    (multiple-value-bind (back text) (%through-the-wire each)
      (is (d:same each back) "~s crossed as ~a and came back ~s" each text back))))

(test a-list-that-looks-like-a-collection-stays-a-list
  "The spelling names a map, a seq and a set. A list of somebody's own that begins
with one of those words has to come back a list, on both sides: read as one it
would be a collection that never was, and written as one it would be stored as
something else and only look right because the read undid it again."
  (dolist (each (list (list :map :a 1) (list :seq 1 2) (list :set 1)
                      (list :quoted 1) (list 1 (list :seq 2))))
    (multiple-value-bind (back text) (%through-the-wire each)
      (is (equal each back) "~s crossed as ~a and came back ~s" each text back)
      (is (not (d:collectionp back)) "~s came back a collection" each))))

(test what-is-written-is-what-lands-and-not-only-what-reads-back
  "Both halves of a spelling can be wrong the same way and a round trip through
the wire will not say so: the read undoes what the write did. What lands has to be
asked about where it landed."
  (with-tree
    (flet ((wrote (json)
             (pine/run/peer::received
              (multiple-value-bind (message id)
                  (pine/serve/wire:asked
                   (format nil "{\"id\":1,\"do\":\"write\",\"path\":\"/probe\",~
                                \"value\":~a}" json))
                (declare (ignore id))
                message))
             (node:contents (tree:at "/probe"))))
      (is (d:mapp (wrote "{\"map\":[[\":a\",1]]}")) "a map lands a map")
      (is (d:seqp (wrote "{\"seq\":[1,2]}")) "a seq lands a seq")
      (is (d:setp (wrote "{\"set\":[1]}")) "a set lands a set")
      (let ((it (wrote "[\":seq\",1,2]")))
        (is (not (d:collectionp it)) "a list that looks like one lands a list")
        (is (equal (list :seq 1 2) it)))
      (is (null (wrote "null")) "nothing lands nothing")
      (is (equal (list :title "x") (wrote "[\":title\",\"x\"]"))))))

(test a-line-that-is-not-a-question-is-answered
  "This is the edge of the image. On the other side of it is somebody who can do
nothing with a dropped connection and something with a sentence."
  (flet ((said (line)
           (multiple-value-bind (message id) (pine/serve/wire:asked line)
             (declare (ignore id))
             message)))
    (is (eq :no (first (said "{\"do\":\"sing\",\"path\":\"/x\"}"))))
    (is (eq :no (first (said "{\"path\":\"/x\"}"))))
    (is (eq :no (first (said "{\"do\":\"read\"}"))))
    (is (eq :no (first (said "{\"do\":\"eval\",\"path\":\"/x\"}")))
        "and evaluating is not offered unless it is asked for")))

(test asking-about-nothing-and-about-an-object-are-both-answered
  (with-tree
    (is (eq :no (first (pine/run/peer::received (list :contents "/nowhere")))))
    (tree:built (tree:root))
    (ui:make-surface "test-surface" (lambda () (ui:label "hi")) :as 'ui:panel)
    (let ((said (pine/run/peer::received (list :contents "/surface/test-surface"))))
      (is (eq :no (first said)) "a widget has no spelling")
      (is (search "test-surface" (second said)) "and the answer names the place"))
    (is (eq :ok (first (pine/run/peer::received
                        (list :contents "/surface/test-surface/wire"))))
        "while what is under it does have one")))


(test a-watch-goes-back-the-way-the-question-came
  "Somebody on a stream has no address to be told at. The way back is the
connection the question arrived on, which is what makes a watch possible for
anything that is not an actor."
  (with-tree
    (booted)
    (pine::write "/probe/watched" "before")
    (let ((heard nil)
          (held (cons :watching nil)))
      (pine/run/peer:telling ((lambda (said) (push said heard)) held)
        (is (eq :ok (first (pine/run/peer::received
                            (list :watch "/probe/watched"))))
            "it is taken up with no address at all")
        (pine::write "/probe/watched" "after")
        (is (until (lambda ()
                     (equal '(:moved "/probe/watched" t) (first heard))))
            "and the event comes back the way the question went")
        (is (= 1 (length (cdr held))) "and it is held against the connection"))
      (pine/run/peer:forget-watches held)
      (setf heard nil)
      (pine::write "/probe/watched" "later")
      (is (null heard) "and goes when the connection does"))))

(test a-watch-with-nowhere-to-go-is-refused
  (with-tree
    (booted)
    (pine::write "/probe/watched" "before")
    (is (eq :no (first (pine/run/peer::received
                        (list :watch "/probe/watched"))))
        "nobody asked in a way that can be answered")))

(test one-connection-going-takes-its-own-watches-and-no-others
  (with-tree
    (booted)
    (pine::write "/probe/watched" "before")
    (let ((a nil) (b nil)
          (for-a (cons :watching nil))
          (for-b (cons :watching nil)))
      (pine/run/peer:telling ((lambda (said) (push said a)) for-a)
        (pine/run/peer::received (list :watch "/probe/watched")))
      (pine/run/peer:telling ((lambda (said) (push said b)) for-b)
        (pine/run/peer::received (list :watch "/probe/watched")))
      (pine/run/peer:forget-watches for-a)
      (setf a nil b nil)
      (pine::write "/probe/watched" "after")
      (is (until (lambda () (= 1 (length b)))) "the one still there hears it")
      (is (null a) "and the one that left hears nothing"))))
