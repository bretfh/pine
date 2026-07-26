(defpackage #:pine.ts.parser
  (:use #:cl)
  (:export
   #:start-parser
   #:parse-link
   #:make-parse-link
   #:parse-link-p
   #:link-actor
   #:link-inflight
   #:link-dirty
   #:link-tick)
  (:documentation "Parsing as an actor, off the buffer's thread.

A buffer's receive owes its mailbox an answer, so it cannot afford to wait for
tree-sitter: at a million lines one edit costs over a second inside the parser,
and that would be a second of input latency. The parse lives here instead, on a
thread of its own, and answers arrive at the buffer as ordinary messages.

The buffer keeps at most one request in flight, so a burst of typing produces one
parse per completed parse rather than one per keystroke, and this mailbox cannot
grow behind a slow file."))

(in-package #:pine.ts.parser)

(defstruct (parse-link (:constructor make-parse-link (actor)))
  "A buffer's handle on its parser: the actor, whether a parse is out, whether
an edit has landed since it went out, and the tick that was last requested."
  actor
  (inflight nil)
  (dirty nil)
  (tick 0))

(defun link-actor (link) (parse-link-actor link))
(defun link-inflight (link) (parse-link-inflight link))
(defun link-dirty (link) (parse-link-dirty link))
(defun link-tick (link) (parse-link-tick link))

(defun %ensure-tree (ps lines edit)
  "Bring PS's tree up to LINES, using EDIT when it describes the difference."
  (unless (eq lines (pine.ts.runtime:ps-lines ps))
    (pine.ts.runtime:parse-lines! ps lines :edit edit))
  ps)

(defun %highlights (ps viewport)
  "Face runs for the lines VIEWPORT covers.

No viewport means no window is showing this buffer, and highlights exist only to
be painted, so there is nothing to compute. Walking anyway is how a background
buffer of a million lines produced millions of tuples nobody would ever look at.
A window claims the buffer by sending :set-viewport, and that asks for a parse."
  (when viewport
    (pine.ts.highlight:parse-highlights ps :from-line (car viewport)
                                           :to-line (cdr viewport))))

(defun %receive (ps msg)
  "Handle one request against PS. Every answer is a TELL back to the buffer: the
parser never replies to an ask, so no caller can be waiting on it."
  (destructuring-bind (tag &key lines edit tick viewport buffer from to kind line col
                       &allow-other-keys)
      msg
    (case tag
      (:parse
       (%ensure-tree ps lines edit)
       (sento.actor:tell buffer (list :highlights :tick tick
                                                  :hl (%highlights ps viewport))))
      (:indent
       (%ensure-tree ps lines edit)
       (let ((targets (loop :for l :from from :to to
                            :for target = (pine.ts.highlight:parse-indent ps l)
                            :when target :collect (cons l target))))
         (sento.actor:tell buffer (list :indent-region :tick tick :targets targets))))
      (:motion
       (%ensure-tree ps lines edit)
       (multiple-value-bind (l c) (pine.ts.runtime:parse-motion ps kind line col)
         (when l
           (sento.actor:tell buffer (list :move-point :line l :col c)))))
      (:stop
       (pine.ts.runtime:free-parse-state ps))
      (t (error "The parser has no handler for ~s." msg)))))

(defun start-parser (system runtime language name)
  "An actor parsing NAME's buffer in LANGUAGE, or nil when the grammar is
unavailable. Pinned: it owns the parse state, and no other thread may touch a
TSParser."
  (let ((ps (pine.ts.runtime:make-parse-state runtime language)))
    (when ps
      (sento.actor-context:actor-of
       system
       :name (format nil "parser:~a-~a" name (gensym))
       :dispatcher :pinned
       :state ps
       :receive
       (lambda (msg)
         ;; A parse fault parks this actor alone: the buffer keeps taking edits
         ;; and keeps painting its last good highlights, so the file stays
         ;; editable while the fault is attended.
         (pine.core.eval:with-debugger
             (:label (format nil "parser ~a <- ~a" name (first msg)))
           (%receive sento.actor:*state* msg)))))))
