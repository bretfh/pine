(defpackage #:pine/kernel/call
  (:use #:cl)
  (:shadow #:read #:write #:make #:erase)
  (:local-nicknames (#:d #:pine/data) (#:name #:pine/kernel/name)
                    (#:place #:pine/kernel/place) (#:graph #:pine/kernel/graph)
                    (#:tell #:pine/kernel/tell) (#:tree #:pine/kernel/tree)
                    (#:watch #:pine/kernel/watch))
  (:export
   #:read #:write #:swap #:make #:erase #:ls #:watch #:together #:standsp))
(in-package #:pine/kernel/call)

(defvar +swappers+
  (list (cons :not #'not) (cons :1+ #'1+) (cons :1- #'1-)
        (cons :yes (constantly t)) (cons :no (constantly nil)))
  "The functions a swap can be asked for by name.

A shell has no closures to send, so what it can say is a word. Not a way of
naming any function: a small list, because what crosses a socket deciding what
code runs is how a namespace becomes a way in.")

(defun %said (p) (place:held p))

(defun branchp (p)
  "Whether it holds nothing because it is a branch rather than because somebody
wrote nothing there."
  (and (typep p 'place:value)
       (not (place:written p))
       (plusp (d:size (place:beneath p)))))

(defun read (said)
  "What stands at SAID, and whether anything does.

Two answers, because there are four ways to hold nothing and they are not the
same thing. Nothing stands there at all; a branch stands there and a branch holds
nothing by definition; or somebody wrote nothing there on purpose. Whoever only
wants the value reads the first answer and never knows the difference; whoever
has to tell them apart is told."
  (let ((p (tree:reach said)))
    (cond ((null p) (values nil :absent))
          ((branchp p) (values nil :branch))
          (t (values (%said p) :held)))))

(defun standsp (said) (tree:standsp said))

(defun write (said value)
  "Put VALUE at SAID, making the place if nothing has been put there yet.

A read finds what stands and a write makes what does not. That is the whole of
the difference between them, and it is why there is no third call for making
somewhere to write."
  (let ((p (or (tree:reach said) (tree:make-under said :value))))
    (setf (place:held p) value)
    value))

(defun %swapper (function)
  (cond ((functionp function) function)
        ((cdr (assoc function +swappers+)))
        (t (error "~s is not a way of changing what stands somewhere." function))))

(defun swap (said function)
  "Replace what stands at SAID with FUNCTION of it, and answer that.

The one call that is not a read and not a write. A read and a write with a gap
between them are two acts, and whoever wrote in the gap is lost -- which is what
a button that turns something off does when what it means is turn it round.
FUNCTION runs again if somebody got there first, so it must be pure.

A word is one of a few functions the kernel knows, so a shell can say it."
  (let ((p (or (tree:reach said) (tree:make-under said :value)))
        (fn (%swapper function)))
    (if (typep p 'place:value)
        (progn
          (place:moving p)
          (let ((next (d:swap (slot-value p 'place::holds) fn)))
            (setf (place:written p) t)
            (place:moved p)
            next))
        (let ((next (funcall fn (place:held p))))
          (setf (place:held p) next)
          next))))

(defun make (said kind &rest initargs)
  "Bring a place of KIND into being at SAID.

One call and one word for every kind there is, because they differ in what they
hold and in nothing else. A slot left empty is not a kind: a place that is worked
out says what works it out, here, or it is not one."
  (let* ((main (and initargs (not (keywordp (first initargs)))
                    (pop initargs)))
         (key (ecase kind
                (:value :holds) (:derived :works) (:world :asks)
                (:listing :names) (:mount :reached) (:job :runs))))
    (when (tree:reach said) (erase said))
    (apply #'tree:make-under said kind
           (if main (list* key main initargs) initargs))))

(defun erase (said)
  "Take the place SAID names away, and everything under it with it.

Its own call, and not a write of nothing, because nothing cannot be written:
absence is what a name has when nothing stands at it, and a value that meant
absence would be a fifth way of holding nothing."
  (tree:take-away said))

(defun ls (said)
  "The names of what stands under SAID, and whether anything stands there at all."
  (let ((p (tree:reach said)))
    (if (null p)
        (values nil :absent)
        (values (mapcar #'place:name-of (place:kids p)) :held))))

(defun watch (said tells &key (when :on-change))
  "Tell TELLS when what stands at SAID moves.

Told on whatever thread the news is dispatched on, and never on the thread that
wrote. A place that is worked out is watched the same way anything else is: it
moved when what it is worked out from moved."
  (watch:watch said tells :when when))

(defmacro together (&body body)
  "Every write in BODY, told as one piece of news.

The kernel already does this around a key press, a line off a socket, a command
and a config, so this is for grouping by hand inside one of those. Two places
that move together are read together by whatever is worked out from both, and
without this there is a moment between them when that pair stood -- and something
would be worked out from it."
  `(tell:together ,@body))
