(defpackage #:pine.ts.parser
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:proc #:pine.proc))
  (:export #:start-parser #:parse-link #:make-parse-link #:parse-link-p
           #:link-actor #:link-state #:tree-of)
  (:documentation "Parsing as an actor, off whatever asked for it.

Nothing can afford to wait for tree-sitter: at a million lines one edit costs
over a second inside the parser, and that would be a second of input latency.
The parse lives here instead, on a thread of its own, because a TSParser may not
be touched from two threads at once.

It is told and never asked, and it answers by writing what it computed, so
nothing is ever waiting on it and the answer is a place rather than a message."))

(in-package #:pine.ts.parser)

(defstruct (parse-link (:constructor make-parse-link (actor state)))
  "A buffer's handle on its parser: the actor to tell, and the parse state it
owns, which is a foreign pointer and has to be freed."
  actor state)

(defun link-actor (link) (parse-link-actor link))
(defun link-state (link) (parse-link-state link))

(defun tree-of (name)
  "The parse tree NAME's parser last built, or NIL. Live: the parser's own
tree, not a copy, so it is never stored."
  (let ((link (ns:read (p:path (p:parse "/proc") (format nil "parse:~a" name)))))
    (when (fset:map? link)
      (let ((it (fset:lookup link :parser)))
        (and (parse-link-p it) (pine.ts.runtime:ps-tree (parse-link-state it)))))))

(defun %ensure-tree (ps lines edit from viewport)
  "Bring PS's tree up to LINES over the band VIEWPORT needs, using EDIT when it
describes the difference from FROM."
  (pine.ts.runtime:parse-lines! ps lines :edit edit :from from :viewport viewport)
  ps)

(defun %highlights (ps viewport)
  "Face runs for the lines VIEWPORT covers.

No viewport means no window is showing this buffer, and highlights exist only
to be painted. Walking anyway is how a background buffer of a million lines
produced millions of tuples nobody would look at."
  (when viewport
    (pine.ts.highlight:parse-highlights ps :from-line (car viewport)
                                           :to-line (cdr viewport))))

(defun %at (name &rest leaf)
  (apply #'p:path (p:parse "/buf") name leaf))

(defun %as-properties (runs)
  "Face runs as the buffer's own properties. A run follows the text it colours,
and the walker emits exactly one face per token, so nothing downstream has to
decide between two of them."
  (fset:convert 'fset:seq
                (mapcar (lambda (run)
                          (destructuring-bind (line start end face) run
                            (fset:map (:from (fset:seq line start))
                                      (:to (fset:seq line end))
                                      (:class face)
                                      (:policy :sticky)
                                      (:by :syntax))))
                        runs)))

(defun %receive (ps msg)
  "Handle one request against PS. Every answer is a write."
  (destructuring-bind (tag &key lines edit tick viewport name from to kind line col
                       answer space from-lines &allow-other-keys)
      msg
    (declare (ignorable tick))
    ;; the answer goes back into the namespace that asked, not whichever one is
    ;; current when this thread gets around to it
    (let ((ns:*space* (or space ns:*space*)))
      (case tag
        (:parse
         (%ensure-tree ps lines edit from-lines viewport)
         (ns:write (%at name "prop")
                   (fset:seq :replace :syntax
                             (%as-properties (%highlights ps viewport)))))
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
           (when l (ns:write (%at name "point") (fset:seq l c)))))
        (:stop (pine.ts.runtime:free-parse-state ps))
        (t (error "The parser has no handler for ~s." msg))))))

(defun start-parser (system runtime language name)
  "An actor parsing NAME's buffer in LANGUAGE, or NIL when the grammar is
unavailable.

Pinned: it owns the parse state, and no other thread may touch a TSParser."
  (let ((ps (pine.ts.runtime:make-parse-state runtime language)))
    (when ps
      (make-parse-link
       (sento.actor-context:actor-of
        system
        :name (format nil "parser:~a-~a" name (gensym))
        :dispatcher :pinned
        :state ps
        :receive
        (lambda (msg)
          ;; a parse that fails records itself at /err and unwinds: the buffer
          ;; keeps taking edits and keeps its last good colours
          (pine.err:with-debugger
              (:label (format nil "parser ~a <- ~a" name (first msg)))
            (%receive sento.actor:*state* msg))))
       ps))))

;;;; A parser is a /proc runnable: it holds a TSParser, which is a foreign
;;;; pointer, and /proc is where anything holding one lives.

(defmethod proc:start ((kind (eql :parser)) entry &key &allow-other-keys)
  (fset:map-union
   entry
   (fset:map (:took (fset:lookup (fset:lookup entry :declaration) :parser))
             (:state :running))))

(defmethod proc:halt ((kind (eql :parser)) entry)
  "Tell it to stop, so FREE-PARSE-STATE runs on the thread that owns the
TSParser. Freeing it from here would be freeing it under its own actor."
  (let ((link (proc:took entry)))
    (when (parse-link-p link)
      (ignore-errors (sento.actor:tell (parse-link-actor link) '(:stop)))
      (ignore-errors (sento.actor-cell:stop (parse-link-actor link)))))
  (fset:map-union entry (fset:map (:took nil) (:state :stopped))))

(defmethod proc:alive-p ((kind (eql :parser)) entry)
  (let ((link (proc:took entry)))
    (and (parse-link-p link) t)))

(proc:runnable :parser)
