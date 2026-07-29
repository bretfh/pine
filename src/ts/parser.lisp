(defpackage #:pine.ts.parser
  (:use #:cl)
  (:export
   #:start-parser
   #:parse-link
   #:make-parse-link
   #:parse-link-p
   #:link-actor)
  (:documentation "Parsing as an actor, off whatever asked for it.

Nothing can afford to wait for tree-sitter: at a million lines one edit costs
over a second inside the parser, and that would be a second of input latency.
The parse lives here instead, on a thread of its own, because a TSParser may not
be touched from two threads at once.

It is told and never asked, and it answers by writing what it computed, so
nothing is ever waiting on it and the answer is a place rather than a message."))

(in-package #:pine.ts.parser)

(defstruct (parse-link (:constructor make-parse-link (actor)))
  "A buffer's handle on its parser."
  actor)

(defun link-actor (link) (parse-link-actor link))

(defun %ensure-tree (ps lines edit from viewport)
  "Bring PS's tree up to LINES over the band VIEWPORT needs, using EDIT when it
describes the difference from FROM, which is the state it was computed against."
  (pine.ts.runtime:parse-lines! ps lines :edit edit :from from :viewport viewport)
  ps)

(defun %highlights (ps viewport)
  "Face runs for the lines VIEWPORT covers.

No viewport means no window is showing this buffer, and highlights exist only to
be painted, so there is nothing to compute. Walking anyway is how a background
buffer of a million lines produced millions of tuples nobody would ever look at.
A window claims the buffer by saying what it is showing, and that asks for a
parse."
  (when viewport
    (pine.ts.highlight:parse-highlights ps :from-line (car viewport)
                                           :to-line (cdr viewport))))

(defun %at (name &rest leaf)
  (apply #'pine.path:path (pine.path:parse "/buf") name leaf))

(defun %receive (ps msg)
  "Handle one request against PS.

Every answer is a write. The parser never replies to an ask, so nothing can be
waiting on it, and what it computed is a place anyone can read rather than a
message one caller receives."
  (destructuring-bind (tag &key lines edit tick viewport name from to kind line col
                       answer space from-lines &allow-other-keys)
      msg
    (declare (ignorable tick))
    ;; the answer goes back into the namespace that asked, not into whichever
    ;; one is current when this thread gets around to it
    (let ((pine.ns:*space* (or space pine.ns:*space*)))
     (case tag
      (:parse
       (%ensure-tree ps lines edit from-lines viewport)
       (pine.ns:write (%at name "face") (%highlights ps viewport) :keep nil))
      (:indent
       ;; where a line should sit is not a place, so it goes back to whoever
       ;; asked rather than through a path of its own
       (%ensure-tree ps lines edit from-lines viewport)
       (let ((targets (loop :for l :from from :to to
                            :for target = (pine.ts.highlight:parse-indent ps l)
                            :when target :collect (cons l target))))
         (when answer (funcall answer targets))))
      (:motion
       (%ensure-tree ps lines edit from-lines viewport)
       (multiple-value-bind (l c) (pine.ts.runtime:parse-motion ps kind line col)
         (when l
           (pine.ns:write (%at name "point") (fset:seq l c)))))
      (:stop
       (pine.ts.runtime:free-parse-state ps))
      (t (error "The parser has no handler for ~s." msg))))))

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
         (pine.err:with-debugger
             (:label (format nil "parser ~a <- ~a" name (first msg)))
           (%receive sento.actor:*state* msg)))))))
