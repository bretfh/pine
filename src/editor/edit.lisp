(defpackage #:pine.editor.edit
  (:use #:cl)
  (:documentation "The dispatch-message methods for base-mode and text-mode:
what every buffer does with a verb, and what a text buffer layers on top. Adds
methods only; it exports nothing."))

(in-package #:pine.editor.edit)

;;;; Buffer behavior: the dispatch-message verb methods for base-mode and
;;;; text-mode. base-mode has the infrastructure verbs, text-mode layers
;;;; editing. The modes that wrap another subsystem carry their own methods
;;;; where that subsystem lives: repl-mode in pine.editor.repl, terminal-mode in
;;;; pine.term.

(defun %layout-state (state builder width &optional selection)
  "A fresh buffer state whose content is BUILDER's node tree rendered at WIDTH:
lines are the row texts, meta carries :layout-builder, :layout-rows,
:layout-tree, :layout-width, and :layout-selection. Name/mode/pathname/vars
carry over like :replace-content; point is clamped into the new content."
  (let* ((sel (or selection (pine.text.buffer:buffer-local state :layout-selection 0)))
         (tree0 (funcall builder state)))
    (multiple-value-bind (rows tree) (pine.ui.cells:render tree0 width :selection sel)
      (let* ((texts (mapcar (lambda (r) (string-right-trim " " (car r))) rows))
             (old-meta (pine.text.buffer:meta state))
             (carried (reduce (lambda (st key)
                                (multiple-value-bind (val present)
                                    (fset:lookup old-meta key)
                                  (if present (pine.text.buffer:set-meta st key val) st)))
                              '(:id :name :mode :pathname :vars)
                              :initial-value
                              (pine.text.buffer:load-content
                               (format nil "~{~a~^~%~}" texts))))
             (new (reduce (lambda (st kv) (pine.text.buffer:set-meta st (car kv) (cdr kv)))
                          (list (cons :layout-builder builder)
                              (cons :layout-rows rows)
                              (cons :layout-tree tree)
                              (cons :layout-width width)
                              (cons :layout-selection sel))
                          :initial-value carried))
             (snap (pine.text.buffer:state->snapshot state))
             (pl (min (pine.text.buffer:point-line snap) (max 0 (1- (length texts))))))
        (pine.text.buffer:move-mark new :point pl 0)))))

(defun apply-indent-targets (state undo subs hl link targets)
  "Reindent each (LINE . COLUMN) of TARGETS, commit and repaint.

Point rides its character: anchored as an offset past its line's first
non-whitespace, which also lands it on the first non-whitespace when it sat
inside the old indentation. Shared by the treeless path, which computes targets
itself, and by the parser's :indent-region answer."
  (let* ((snap (pine.text.buffer:state->snapshot state))
         (pl (pine.text.buffer:point-line snap))
         (pc (pine.text.buffer:point-col snap))
         (p-firstnw (pine.text.buffer:line-indent-width
                     (fset:@ (pine.text.buffer:lines state) pl)))
         (p-tail (max 0 (- pc p-firstnw)))
         (st state)
         (changed nil)
         (bytes 0)
         (lo most-positive-fixnum)
         (hi 0))
    (loop :for (line . target) :in targets
          :for cur = (pine.text.buffer:line-indent-width
                      (fset:@ (pine.text.buffer:lines st) line))
          :do (when (/= target cur)
                (setf st (nth-value 0 (pine.text.buffer:reindent-line st line cur target pc))
                      changed t
                      lo (min lo line)
                      hi (max hi line))
                (incf bytes (- target cur))))
    (if (not changed)
        (setf sento.actor:*state* (list state undo nil subs hl link))
        (let* ((new-indent (pine.text.buffer:line-indent-width
                            (fset:@ (pine.text.buffer:lines st) pl)))
               (final (pine.text.buffer:set-meta
                       (pine.text.buffer:move-mark st :point pl (+ new-indent p-tail))
                       :overlays nil))
               (span (1+ (- hi lo))))
          (setf sento.actor:*state* (list final (cons state undo) nil subs hl link))
          (pine.text.buffer:notify-subscribers subs final hl)
          (pine.text.buffer:request-parse link final
                                          :extra (list :edit (list lo span span bytes)))))))

(defmethod pine.editor.mode:dispatch-message ((mode pine.editor.mode:base-mode) self tag plist)
  (declare (ignore self))
  (destructuring-bind (state undo redo subs hl pstate) sento.actor:*state*
    (case tag
      (:get-state (sento.actor:reply state))
      (:get-snapshot (sento.actor:reply (pine.text.buffer:state->snapshot state)))
      (:get-text (sento.actor:reply (pine.text.buffer:state->string state)))
      ;; the face runs the buffer currently holds. A snapshot carries them only
      ;; when one is built for a subscriber, so this is how anything else asks.
      (:get-highlights (sento.actor:reply hl))
      (:get-local
       (sento.actor:reply
        (pine.text.buffer:buffer-local state (getf plist :key) (getf plist :default))))
      (:subscribe
       (let ((r (getf plist :renderer)))
         (setf sento.actor:*state* (list state undo redo (adjoin r subs :test #'eq) hl pstate))
         (sento.actor:tell r
           (list :snapshot :snapshot (pine.text.buffer:state->snapshot-with-hl state hl)))))
      (:unsubscribe
       (let ((r (getf plist :renderer)))
         (setf sento.actor:*state* (list state undo redo (remove r subs :test #'eq) hl pstate))))
      ;; The parser's answers. Both carry the tick they were computed for and are
      ;; dropped when the buffer has moved on, because a late answer describes
      ;; text that no longer exists.
      (:highlights
       (let ((new-hl (getf plist :hl))
             (link pstate))
         (when link
           (setf (pine.ts.parser::parse-link-inflight link) nil))
         (when (eql (getf plist :tick) (pine.text.buffer:tick state))
           (setf sento.actor:*state* (list state undo redo subs new-hl link))
           ;; a repaint only earns itself when the colours actually moved
           (unless (equal new-hl hl)
             (pine.text.buffer:notify-subscribers subs state new-hl)))
         ;; edits landed while that parse was out: send the newest lines now
         (when (and link (pine.ts.parser:link-dirty link))
           (pine.text.buffer:request-parse link (first sento.actor:*state*)))))
      (:indent-region
       (when (eql (getf plist :tick) (pine.text.buffer:tick state))
         (apply-indent-targets state undo subs hl pstate (getf plist :targets))))
      ;; explicit highlights for tool buffers (debugger, help): the buffer's
      ;; face runs are handed in as data instead of computed from a parse tree
      (:set-highlights
       (let ((new-hl (getf plist :highlights)))
         (setf sento.actor:*state* (list state undo redo subs new-hl pstate))
         (pine.text.buffer:notify-subscribers subs state new-hl)))
      ;; point motion is navigation, not editing: it belongs to every buffer
      ;; (structured surfaces included), so it lives on base-mode.
      (:move-point
       (let ((new (pine.text.buffer:move-mark state :point
                                         (getf plist :line) (getf plist :col))))
         (setf sento.actor:*state* (list new undo redo subs hl pstate))
         (pine.text.buffer:notify-subscribers subs new hl)))
      ;; char/line motion computed from the buffer's own state, so the editor
      ;; never blocks on a round-trip just to move point.
      (:move-by
       (multiple-value-bind (l c)
           (pine.text.buffer:point-after-move (pine.text.buffer:state->snapshot state)
                                         (getf plist :unit) (getf plist :n))
         (let ((new (pine.text.buffer:move-mark state :point l c)))
           (setf sento.actor:*state* (list new undo redo subs hl pstate))
           (pine.text.buffer:notify-subscribers subs new hl))))
      ;; structural motion off the persistent tree; no reparse, no whole-buffer
      ;; string, computed from the buffer's own point.
      ;; the parser answers with :move-point, so a structural jump in a large
      ;; buffer waits on nothing here
      (:ts-motion
       (let ((link (pine.text.buffer:ensure-parser
                    state pstate (pine.text.buffer:buffer-local state :name "")))
             (snap (pine.text.buffer:state->snapshot state)))
         (when link
           (setf sento.actor:*state* (list state undo redo subs hl link))
           (pine.text.buffer:request-parse
            link state :verb :motion
            :extra (list :kind (getf plist :kind)
                         :line (pine.text.buffer:point-line snap)
                         :col (pine.text.buffer:point-col snap))))))
      ;; the buffer's parser, so KILL-BUFFER can stop it with the buffer
      (:get-parser
       (sento.actor:reply (and pstate (pine.ts.parser:link-actor pstate))))
      ;; Undo and redo swap whole states, so there is no edit to describe: the
      ;; parser rebuilds. It does that on its own thread like any other parse,
      ;; and the old highlights stay on screen until it answers.
      (:undo
       (when undo
         (let ((prev (first undo))
               (link (pine.text.buffer:ensure-parser
                      state pstate (pine.text.buffer:buffer-local state :name ""))))
           (setf sento.actor:*state* (list prev (rest undo) (cons state redo) subs hl link))
           (pine.text.buffer:notify-subscribers subs prev hl)
           (pine.text.buffer:request-parse link prev))))
      (:redo
       (when redo
         (let ((next (first redo))
               (link (pine.text.buffer:ensure-parser
                      state pstate (pine.text.buffer:buffer-local state :name ""))))
           (setf sento.actor:*state* (list next (cons state undo) (rest redo) subs hl link))
           (pine.text.buffer:notify-subscribers subs next hl)
           (pine.text.buffer:request-parse link next))))
      ((:set-local :set-meta)
       (let ((new (pine.text.buffer:set-meta state (getf plist :key) (getf plist :value))))
         ;; a mode change (re)builds the parse-state and highlights immediately,
         ;; so opening a file or setting lisp-mode colours it at once.
         (if (eq (getf plist :key) :mode)
             (let ((link (pine.text.buffer:ensure-parser
                          new nil (pine.text.buffer:buffer-local new :name ""))))
               (setf sento.actor:*state* (list new undo redo subs hl link))
               (pine.text.buffer:notify-subscribers subs new hl)
               (pine.text.buffer:request-parse link new))
             (progn
               (setf sento.actor:*state* (list new undo redo subs hl pstate))
               (pine.text.buffer:notify-subscribers subs new hl)))))
      ;; The lines some window is showing, as (FROM . TO). Highlighting walks
      ;; this range instead of the file. Sent by the renderer, which is the only
      ;; thing that knows a window's scroll position; a range equal to the one
      ;; already held is dropped, so the notify this triggers cannot come back
      ;; round as another viewport message.
      (:set-viewport
       (let ((new-range (cons (getf plist :from) (getf plist :to)))
             (old-range (pine.text.buffer:buffer-local state :viewport)))
         (unless (equal new-range old-range)
           ;; scrolling asks for the new window's colours; what is on screen
           ;; stays until they arrive
           (let ((new (pine.text.buffer:set-meta state :viewport new-range)))
             (setf sento.actor:*state* (list new undo redo subs hl pstate))
             (pine.text.buffer:request-parse pstate new)))))
      (:set-var
       (let* ((vars (or (fset:@ (pine.text.buffer:meta state) :vars) (fset:empty-map)))
              (new (pine.text.buffer:set-meta
                    state :vars (fset:with vars (getf plist :key) (getf plist :value)))))
         (setf sento.actor:*state* (list new undo redo subs hl pstate))
         (pine.text.buffer:notify-subscribers subs new hl)))
      ;; overlays: transient per-line annotations riding the meta (and so
      ;; every snapshot); the renderer draws them after the line's text.
      ;; Any text edit clears them.
      (:overlay
       (let* ((ovs (or (fset:@ (pine.text.buffer:meta state) :overlays) (fset:empty-map)))
              (new (pine.text.buffer:set-meta
                    state :overlays
                    (fset:with ovs (getf plist :line)
                               (list (getf plist :text)
                                     (or (getf plist :class) :eval-result))))))
         (setf sento.actor:*state* (list new undo redo subs hl pstate))
         (pine.text.buffer:notify-subscribers subs new hl)))
      (:clear-overlays
       (let ((new (pine.text.buffer:set-meta state :overlays nil)))
         (setf sento.actor:*state* (list new undo redo subs hl pstate))
         (pine.text.buffer:notify-subscribers subs new hl)))
      ;; the buffer as a layout buffer: BUILDER (state -> node tree) is
      ;; stored and run; lines become the row texts; the rows and the arranged
      ;; tree ride the meta for the renderer and point->node lookup. History
      ;; resets like :replace-content. :reproject re-runs the stored builder
      ;; (selection change, data change, resize) and preserves history.
      (:set-layout
       (let ((new (%layout-state state (getf plist :builder)
                                    (or (getf plist :width)
                                        (pine.text.buffer:buffer-local state :layout-width 80))
                                    (getf plist :selection))))
         (setf sento.actor:*state* (list new nil nil subs nil pstate))
         (pine.text.buffer:notify-subscribers subs new nil)))
      (:reproject
       (let ((builder (pine.text.buffer:buffer-local state :layout-builder)))
         (when builder
           (let ((new (%layout-state state builder
                                        (or (getf plist :width)
                                            (pine.text.buffer:buffer-local state :layout-width 80))
                                        (getf plist :selection))))
             (setf sento.actor:*state* (list new undo redo subs nil pstate))
             (pine.text.buffer:notify-subscribers subs new nil)))))
      (:replace-content
       ;; fresh content clears history, but the buffer keeps its identity: name,
       ;; mode, pathname, and buffer-locals carry over so highlighting and the
       ;; mode survive a content replace (loading a file, reverting).
       (let* ((old (pine.text.buffer:meta state))
              (new (reduce (lambda (st key)
                             (multiple-value-bind (val present) (fset:lookup old key)
                               (if present (pine.text.buffer:set-meta st key val) st)))
                           '(:id :name :mode :pathname :vars)
                           :initial-value (pine.text.buffer:load-content (getf plist :content)))))
         ;; new content shares nothing with the old, so the tree is rebuilt; the
         ;; buffer shows the text immediately and takes its colours when they come
         (let ((link (pine.text.buffer:ensure-parser
                      new pstate (pine.text.buffer:buffer-local new :name ""))))
           (setf sento.actor:*state* (list new nil nil subs nil link))
           (pine.text.buffer:notify-subscribers subs new nil)
           (pine.text.buffer:request-parse link new))))
      ;; The end of the mode chain. A verb nobody claimed is a caller's mistake,
      ;; and dropping it silently is how a wrong message looks exactly like a
      ;; message that did nothing. A mode that means to ignore a verb says so,
      ;; the way terminal-mode lists the edits it has no use for.
      (t (error "Buffer ~a has no handler for ~s~@[ ~s~]."
                (pine.text.buffer:buffer-local state :name "?") tag plist)))))

(defmethod pine.editor.mode:dispatch-message ((mode pine.editor.mode:text-mode) self tag plist)
  (destructuring-bind (state undo redo subs hl pstate) sento.actor:*state*
    ;; edits push the old state onto UNDO, clear REDO, and reparse the tree
    ;; incrementally so the notified snapshot already carries fresh highlights.
    ;; An edit commits and paints at once; the parse happens on the parser's own
    ;; thread and its highlights arrive later as a message. EDIT is
    ;; (line old-lines new-lines byte-delta), which the parser needs to shift its
    ;; tree instead of rebuilding it; LINE and DELTA carry the same change to the
    ;; highlights already on screen so the lines below an inserted one do not
    ;; briefly wear the colours of their old neighbours.
    (macrolet ((commit (new-state &key edit)
                 `(let* ((new (pine.text.buffer:set-meta ,new-state :overlays nil))
                         (descriptor ,edit)
                         (hl2 (if descriptor
                                  (pine.text.buffer:shift-highlights
                                   hl (first descriptor)
                                   (- (third descriptor) (second descriptor)))
                                  hl))
                         (link (pine.text.buffer:ensure-parser
                                new pstate (pine.text.buffer:buffer-local new :name ""))))
                    (setf sento.actor:*state* (list new (cons state undo) nil subs hl2 link))
                    (pine.text.buffer:notify-subscribers subs new hl2)
                    ;; the edit is durable from here: a row on a queue, committed
                    ;; by the journal writer on its own thread
                    (pine.state.journal:record!
                     (pine.text.buffer:buffer-local new :id)
                     (pine.text.buffer:tick new)
                     (cons tag plist))
                    (pine.text.buffer:request-parse
                     link new :extra (list :edit descriptor)))))
      (case tag
        (:insert
         (let* ((snap (pine.text.buffer:state->snapshot state))
                (l (pine.text.buffer:point-line snap))
                (c (pine.text.buffer:point-col snap))
                (text (getf plist :text)))
           (commit (pine.text.buffer:insert-string state l c text)
                   ;; pasted text carries its own newlines, so one line can
                   ;; become several
                   :edit (list l 1 (1+ (count #\Newline text))
                               (pine.ts.index:string-bytes text)))))
        ;; The newline lands now and the electric indent follows as a message:
        ;; the line appears the moment it is typed however long the parse takes,
        ;; and the parser answers with :indent-region when it knows the column.
        (:newline
         (let* ((snap (pine.text.buffer:state->snapshot state))
                (l (pine.text.buffer:point-line snap))
                (c (pine.text.buffer:point-col snap)))
           (commit (pine.text.buffer:move-mark
                    (pine.text.buffer:insert-newline state l c) :point (1+ l) 0)
                   :edit (list l 1 2 1))
           (destructuring-bind (new undo2 redo2 subs2 hl2 link)
               sento.actor:*state*
             (declare (ignore undo2 redo2 subs2 hl2))
             (pine.text.buffer:request-parse link new :verb :indent
                                             :extra (list :from (1+ l) :to (1+ l))))))
        ;; Reindent lines [:from .. :to] (default: the point line). Each line is
        ;; reindented from the parse tree and the tree reparsed before the next,
        ;; so column alignment sees the shifted text. Point rides its character:
        ;; anchored as offset past its line's first non-whitespace, which also
        ;; lands point on the first non-ws when it sat in the old indentation.
        ;; This one primitive serves Tab, indent-region, and format-buffer.
        ;; A treeless buffer indents from its previous line, here and now. A
        ;; buffer with a tree asks its parser for every target in the region at
        ;; once and applies them when :indent-region comes back, so Tab on a
        ;; thousand lines costs one parse instead of one per line.
        (:indent-lines
         (let* ((snap0 (pine.text.buffer:state->snapshot state))
                (nlines (pine.text.buffer:line-count snap0))
                (from (max 0 (or (getf plist :from) (pine.text.buffer:point-line snap0))))
                (to   (min (or (getf plist :to) (pine.text.buffer:point-line snap0))
                           (1- nlines)))
                (link (pine.text.buffer:ensure-parser
                       state pstate (pine.text.buffer:buffer-local state :name ""))))
           (if link
               (progn
                 (setf sento.actor:*state* (list state undo redo subs hl link))
                 (pine.text.buffer:request-parse link state :verb :indent
                                                 :extra (list :from from :to to)))
               (apply-indent-targets
                state undo subs hl link
                (loop :for l :from from :to to
                      :for target = (pine.text.buffer:previous-line-indent state l)
                      :when target :collect (cons l target))))))
        (:backspace
         (let* ((snap (pine.text.buffer:state->snapshot state))
                (l (pine.text.buffer:point-line snap))
                (c (pine.text.buffer:point-col snap)))
           (cond
             ((plusp c)
              (let* ((line (fset:@ (pine.text.buffer:lines state) l))
                     (gone (pine.ts.index:string-bytes (string (char line (1- c))))))
                (commit (pine.text.buffer:move-mark
                         (pine.text.buffer:delete-char state l (1- c)) :point l (1- c))
                        :edit (list l 1 1 (- gone)))))
             ((plusp l)
              ;; joining two lines: the pair becomes one and the newline goes
              (let ((prev-len (length (fset:@ (pine.text.buffer:lines state) (1- l)))))
                (commit (pine.text.buffer:move-mark
                         (pine.text.buffer:delete-char state (1- l) prev-len)
                         :point (1- l) prev-len)
                        :edit (list (1- l) 2 1 -1)))))))
        (:delete-region
         ;; a region command can name an end past the buffer (the text shrank
         ;; under it), and delete-region clamps. The descriptor is measured
         ;; against the same clamped bounds, so it describes what was removed
         ;; rather than what was asked for.
         (let* ((lines (pine.text.buffer:lines state))
                (last (1- (pine.text.buffer:line-count-of state)))
                (sl (max 0 (min (getf plist :start-line) last)))
                (el (max 0 (min (getf plist :end-line) last)))
                (sc (max 0 (min (getf plist :start-col) (length (fset:@ lines sl)))))
                (ec (max 0 (min (getf plist :end-col) (length (fset:@ lines el)))))
                (removed (pine.ts.index:string-bytes
                          (pine.text.buffer:region-string state sl sc el ec))))
           (commit (pine.text.buffer:delete-region state
                                                   (getf plist :start-line)
                                                   (getf plist :start-col)
                                                   (getf plist :end-line)
                                                   (getf plist :end-col))
                   :edit (list sl (1+ (- el sl)) 1 (- removed)))))
        (:append-with-prompt
         (let* ((text (getf plist :text)) (pr (getf plist :prompt))
                (snap (pine.text.buffer:state->snapshot state))
                (last-line (1- (pine.text.buffer:line-count snap)))
                (last-col (length (fset:@ (pine.text.buffer:lines state) last-line)))
                (s1 (pine.text.buffer:move-mark state :point last-line last-col))
                (s2 (pine.text.buffer:insert-newline s1 last-line last-col))
                (s3 (pine.text.buffer:insert-string s2 (1+ last-line) 0 text))
                (s4-snap (pine.text.buffer:state->snapshot s3))
                (s4-line (1- (pine.text.buffer:line-count s4-snap)))
                (s4-col (length (fset:@ (pine.text.buffer:lines s3) s4-line)))
                (s5 (pine.text.buffer:insert-newline s3 s4-line s4-col)))
           (commit (pine.text.buffer:insert-string s5 (1+ s4-line) 0 pr))))
        (t (call-next-method))))))
