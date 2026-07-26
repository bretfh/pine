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
                              '(:name :mode :pathname :vars)
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

(defmethod pine.editor.mode:dispatch-message ((mode pine.editor.mode:base-mode) self tag plist)
  (declare (ignore self))
  (destructuring-bind (state undo redo subs hl pstate) sento.actor:*state*
    (case tag
      (:get-state (sento.actor:reply state))
      (:get-snapshot (sento.actor:reply (pine.text.buffer:state->snapshot state)))
      (:get-text (sento.actor:reply (pine.text.buffer:state->string state)))
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
      (:ts-motion
       (when pstate
         (let ((snap (pine.text.buffer:state->snapshot state)))
           (multiple-value-bind (l c)
               (pine.ts:parse-motion pstate (getf plist :kind)
                                     (pine.text.buffer:point-line snap)
                                     (pine.text.buffer:point-col snap))
             (when l
               (let ((new (pine.text.buffer:move-mark state :point l c)))
                 (setf sento.actor:*state* (list new undo redo subs hl pstate))
                 (pine.text.buffer:notify-subscribers subs new hl)))))))
      (:undo
       (when undo
         (let ((prev (first undo)))
           (multiple-value-bind (hl2 ps2) (pine.text.buffer:refresh-highlights pstate prev)
             (setf sento.actor:*state* (list prev (rest undo) (cons state redo) subs hl2 ps2))
             (pine.text.buffer:notify-subscribers subs prev hl2)))))
      (:redo
       (when redo
         (let ((next (first redo)))
           (multiple-value-bind (hl2 ps2) (pine.text.buffer:refresh-highlights pstate next)
             (setf sento.actor:*state* (list next (cons state undo) (rest redo) subs hl2 ps2))
             (pine.text.buffer:notify-subscribers subs next hl2)))))
      ((:set-local :set-meta)
       (let ((new (pine.text.buffer:set-meta state (getf plist :key) (getf plist :value))))
         ;; a mode change (re)builds the parse-state and highlights immediately,
         ;; so opening a file or setting lisp-mode colours it at once.
         (if (eq (getf plist :key) :mode)
             (multiple-value-bind (hl2 ps2) (pine.text.buffer:refresh-highlights pstate new)
               (setf sento.actor:*state* (list new undo redo subs hl2 ps2))
               (pine.text.buffer:notify-subscribers subs new hl2))
             (progn
               (setf sento.actor:*state* (list new undo redo subs hl pstate))
               (pine.text.buffer:notify-subscribers subs new hl)))))
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
                           '(:name :mode :pathname :vars)
                           :initial-value (pine.text.buffer:load-content (getf plist :content)))))
         (multiple-value-bind (hl2 ps2) (pine.text.buffer:refresh-highlights pstate new)
           (setf sento.actor:*state* (list new nil nil subs hl2 ps2))
           (pine.text.buffer:notify-subscribers subs new hl2)))))))

(defmethod pine.editor.mode:dispatch-message ((mode pine.editor.mode:text-mode) self tag plist)
  (destructuring-bind (state undo redo subs hl pstate) sento.actor:*state*
    ;; edits push the old state onto UNDO, clear REDO, and reparse the tree
    ;; incrementally so the notified snapshot already carries fresh highlights.
    (macrolet ((commit (new-state)
                 `(let ((new (pine.text.buffer:set-meta ,new-state :overlays nil)))
                    (multiple-value-bind (hl2 ps2) (pine.text.buffer:refresh-highlights pstate new)
                      (setf sento.actor:*state* (list new (cons state undo) nil subs hl2 ps2))
                      (pine.text.buffer:notify-subscribers subs new hl2)))))
      (case tag
        (:insert
         (let* ((snap (pine.text.buffer:state->snapshot state))
                (l (pine.text.buffer:point-line snap))
                (c (pine.text.buffer:point-col snap)))
           (commit (pine.text.buffer:insert-string state l c (getf plist :text)))))
        (:newline
         (let* ((snap (pine.text.buffer:state->snapshot state))
                (l (pine.text.buffer:point-line snap))
                (c (pine.text.buffer:point-col snap))
                (s1 (pine.text.buffer:insert-newline state l c)))
           (if pstate
               ;; electric: reparse so the new empty line exists in the tree,
               ;; then indent it to the language's target column
               (multiple-value-bind (hl1 ps1) (pine.text.buffer:refresh-highlights pstate s1)
                 (declare (ignore hl1))
                 (let* ((target (or (and ps1 (pine.ts.highlight:parse-indent ps1 (1+ l))) 0))
                        (s2 (if (plusp target)
                                (pine.text.buffer:insert-string
                                 s1 (1+ l) 0 (make-string target :initial-element #\Space))
                                s1))
                        (final (pine.text.buffer:set-meta
                                (pine.text.buffer:move-mark s2 :point (1+ l) target)
                                :overlays nil)))
                   (multiple-value-bind (hl2 ps2) (pine.text.buffer:refresh-highlights ps1 final)
                     (setf sento.actor:*state* (list final (cons state undo) nil subs hl2 ps2))
                     (pine.text.buffer:notify-subscribers subs final hl2))))
               (commit s1))))
        ;; Reindent lines [:from .. :to] (default: the point line). Each line is
        ;; reindented from the parse tree and the tree reparsed before the next,
        ;; so column alignment sees the shifted text. Point rides its character:
        ;; anchored as offset past its line's first non-whitespace, which also
        ;; lands point on the first non-ws when it sat in the old indentation.
        ;; This one primitive serves Tab, indent-region, and format-buffer.
        (:indent-lines
         (let* ((snap0 (pine.text.buffer:state->snapshot state))
                (nlines (pine.text.buffer:line-count snap0))
                (from (max 0 (or (getf plist :from) (pine.text.buffer:point-line snap0))))
                (to   (min (or (getf plist :to) (pine.text.buffer:point-line snap0)) (1- nlines)))
                (pl   (pine.text.buffer:point-line snap0))
                (pc   (pine.text.buffer:point-col snap0))
                (p-firstnw (pine.text.buffer:line-indent-width (fset:@ (pine.text.buffer:lines state) pl)))
                (p-tail (max 0 (- pc p-firstnw)))
                (st state) (ps pstate) (changed nil))
           (loop for l from from to to do
             (let* ((cur (pine.text.buffer:line-indent-width (fset:@ (pine.text.buffer:lines st) l)))
                    ;; with a tree, parse-indent is authoritative -- nil means
                    ;; "leave this line" (inside a multiline string), not "fall
                    ;; back". Only a treeless (plain-text) buffer uses the
                    ;; previous-line fallback.
                    (target (if ps
                                (pine.ts.highlight:parse-indent ps l)
                                (pine.text.buffer:previous-line-indent st l))))
               (when (and target (/= target cur))
                 (setf st (nth-value 0 (pine.text.buffer:reindent-line st l cur target pc))
                       changed t)
                 (multiple-value-bind (hl2 ps2) (pine.text.buffer:refresh-highlights ps st)
                   (declare (ignore hl2))
                   (setf ps ps2)))))
           (let* ((new-indent (pine.text.buffer:line-indent-width (fset:@ (pine.text.buffer:lines st) pl)))
                  (final (pine.text.buffer:set-meta
                          (pine.text.buffer:move-mark st :point pl (+ new-indent p-tail))
                          :overlays nil)))
             (multiple-value-bind (hl3 ps3) (pine.text.buffer:refresh-highlights ps final)
               (setf sento.actor:*state*
                     (list final (if changed (cons state undo) undo) (if changed nil redo)
                           subs hl3 ps3))
               (pine.text.buffer:notify-subscribers subs final hl3)))))
        (:backspace
         (let* ((snap (pine.text.buffer:state->snapshot state))
                (l (pine.text.buffer:point-line snap))
                (c (pine.text.buffer:point-col snap)))
           (cond
             ((plusp c)
              (commit (pine.text.buffer:move-mark
                       (pine.text.buffer:delete-char state l (1- c)) :point l (1- c))))
             ((plusp l)
              (let ((prev-len (length (fset:@ (pine.text.buffer:lines state) (1- l)))))
                (commit (pine.text.buffer:move-mark
                         (pine.text.buffer:delete-char state (1- l) prev-len)
                         :point (1- l) prev-len)))))))
        (:delete-region
         (commit (pine.text.buffer:delete-region state
                                            (getf plist :start-line) (getf plist :start-col)
                                            (getf plist :end-line) (getf plist :end-col))))
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
