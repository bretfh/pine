(defpackage #:pine.text.buffer
  (:use :cl)
  (:export #:actor-dead-p #:at #:band #:name-of #:snapshot-of #:state-of #:text-of #:from-paths #:to-paths #:buffer-local #:buffer-state #:buffer-table #:copy-state #:delete-char #:delete-region #:ensure-parser #:highlights #:insert-char #:insert-newline #:insert-string #:line-at #:line-count #:line-count-of #:line-indent-width #:lines #:load-content #:make-buffer-actor #:make-empty-state #:marks #:meta #:move-mark #:name #:notify-subscribers #:point-after-move #:point-col #:point-line #:previous-line-indent #:refresh-highlights #:region-bounds #:region-string #:reindent-line #:request-parse #:set-meta #:shift-highlights #:snapshot #:split-lines #:start-buffer-registry #:state->snapshot #:state->snapshot-with-hl #:state->string #:tick))

(in-package #:pine.text.buffer)

;;;; buffers
(defclass buffer-state ()
  ((lines :initarg :lines :accessor lines :initform (fset:seq ""))
   (marks :initarg :marks :accessor marks :initform (fset:empty-map))
   (meta  :initarg :meta  :accessor meta  :initform (fset:empty-map))
   (tick  :initarg :tick  :accessor tick  :initform 0)))

(defclass snapshot ()
  ((name       :initarg :name       :accessor name       :initform "")
   (tick       :initarg :tick       :accessor tick       :initform 0)
   (lines      :initarg :lines      :accessor lines      :initform (fset:seq ""))
   (line-count :initarg :line-count :accessor line-count  :initform 1)
   (point-line :initarg :point-line :accessor point-line  :initform 0)
   (point-col  :initarg :point-col  :accessor point-col   :initform 0)
   (highlights :initarg :highlights :accessor highlights  :initform nil)
   (meta       :initarg :meta       :accessor meta        :initform (fset:empty-map))))

(defun make-empty-state (name)
  (let ((s (make-instance 'buffer-state)))
    (setf (meta s) (fset:with (meta s) :name name))
    s))

(defun state->snapshot (state)
  (let ((ls (lines state))
        (ms (marks state))
        (mt (meta state)))
    (make-instance 'snapshot
                   :name (or (fset:@ mt :name) "")
                   :tick (tick state)
                   :lines ls
                   :line-count (max 1 (fset:size ls))
                   :point-line (or (fset:@ ms :point-line) 0)
                   :point-col (or (fset:@ ms :point-charpos) 0)
                   :meta mt)))

(defgeneric buffer-local (state-or-snapshot key &optional default)
  (:documentation "Read a buffer-local value by key."))

(defmethod buffer-local ((bs buffer-state) key &optional default)
  (multiple-value-bind (val presentp) (fset:lookup (meta bs) key)
    (if presentp val default)))

(defmethod buffer-local ((sn snapshot) key &optional default)
  (multiple-value-bind (val presentp) (fset:lookup (meta sn) key)
    (if presentp val default)))

(defun state->snapshot-with-hl (state hl)
  "Create snapshot including highlights."
  (let ((snap (state->snapshot state)))
    (setf (highlights snap) hl)
    snap))

(defun state->string (state)
  "STATE's lines joined by newlines.

Measured once for the exact size and filled in one pass: a growing output stream
reallocates its buffer as it doubles, and this runs on every edit of a buffer
that has a tree-sitter language."
  (let ((ls (lines state))
        (total 0))
    (fset:do-seq (line ls) (incf total (1+ (length line))))
    (let ((out (make-string (max 0 (1- total))))
          (pos 0)
          (firstp t))
      (fset:do-seq (line ls)
        (if firstp
            (setf firstp nil)
            (progn (setf (char out pos) #\Newline) (incf pos)))
        (replace out line :start1 pos)
        (incf pos (length line)))
      out)))

(defun copy-state (state &key (lines nil lines-p) (marks nil marks-p)
                           (meta nil meta-p) (tick nil tick-p))
  (make-instance 'buffer-state
                 :lines (if lines-p lines (lines state))
                 :marks (if marks-p marks (marks state))
                 :meta  (if meta-p meta (meta state))
                 :tick  (if tick-p tick (tick state))))


(defun split-lines (string)
  (loop with start = 0
        for i from 0 below (length string)
        when (char= (char string i) #\Newline)
          collect (subseq string start i) into parts
          and do (setf start (1+ i))
        finally (return (append parts (list (subseq string start))))))

(defun %insert-multiline (state line-idx col string)
  "Insert multi-line STRING at LINE-IDX/COL, splitting on its newlines: the
first part joins the line before the cut, the last part takes the remainder,
the middle parts become whole lines. Point lands after the inserted text."
  (let* ((parts (split-lines string))
         (ls (lines state))
         (old (if (< line-idx (fset:size ls)) (fset:@ ls line-idx) ""))
         (c (min col (length old)))
         (last-part (car (last parts)))
         (built (fset:with ls line-idx
                           (concatenate 'string (subseq old 0 c) (first parts))))
         (at (1+ line-idx)))
    (dolist (m (butlast (rest parts)))
      (setf built (fset:insert built at m))
      (incf at))
    (setf built (fset:insert built at
                             (concatenate 'string last-part (subseq old c))))
    (copy-state state
                :lines built
                :marks (let ((m (marks state)))
                         (fset:with (fset:with m :point-line at)
                                    :point-charpos (length last-part)))
                :tick (1+ (tick state)))))

(defun insert-string (state line-idx col string)
  "Insert STRING at LINE-IDX/COL, splitting on any newlines it holds. LINE-IDX
may be the line after the last, which is where an append lands.

Both arguments are checked, because both have a quiet failure. NIL is a sequence,
so a message that arrived without its text would concatenate nothing and report an
edit that never happened; a line index off the end would pad the line seq with
holes that only fault later, on the next read."
  (check-type string string)
  (let ((n (fset:size (lines state))))
    (unless (and (integerp line-idx) (<= 0 line-idx n))
      (error "Insert at line ~s, but the buffer has ~d line~:p." line-idx n)))
  (if (find #\Newline string)
      (%insert-multiline state line-idx col string)
      (let* ((ls (lines state))
             (old (if (< line-idx (fset:size ls)) (fset:@ ls line-idx) ""))
             (c (min col (length old)))
             (new (concatenate 'string (subseq old 0 c) string (subseq old c))))
        (copy-state state
                    :lines (fset:with ls line-idx new)
                    :marks (let ((m (marks state)))
                             (fset:with (fset:with m :point-line line-idx)
                                        :point-charpos (+ c (length string))))
                    :tick (1+ (tick state))))))

(defun insert-char (state line-idx col char)
  "Insert CHAR at LINE-IDX/COL."
  (insert-string state line-idx col (string char)))

(defun insert-newline (state line-idx col)
  "Split the line at LINE-IDX/COL in two."
  (let* ((ls (lines state))
         (old (if (< line-idx (fset:size ls)) (fset:@ ls line-idx) ""))
         (c (min col (length old)))
         (before (subseq old 0 c))
         (after (subseq old c))
         (built (if (zerop (fset:size ls))
                    (fset:seq "" "")
                    (fset:insert (fset:with ls line-idx before)
                                 (1+ line-idx) after))))
    (copy-state state
                :lines built
                :marks (fset:with (fset:with (marks state)
                                             :point-line (1+ line-idx))
                                  :point-charpos 0)
                :tick (1+ (tick state)))))

(defun delete-char (state line-idx col)
  "Delete the character before LINE-IDX/COL, joining lines at a line start."
  (let* ((ls (lines state))
         (line (fset:@ ls line-idx))
         (len (length line)))
    (cond
      ((< col len)
       (let ((new (concatenate 'string (subseq line 0 col) (subseq line (1+ col)))))
         (copy-state state
                     :lines (fset:with ls line-idx new)
                     :tick (1+ (tick state)))))
      ((< (1+ line-idx) (fset:size ls))
       (let* ((next (fset:@ ls (1+ line-idx)))
              (joined (concatenate 'string line next))
              (built (fset:less (fset:with ls line-idx joined) (1+ line-idx))))
         (copy-state state :lines built :tick (1+ (tick state)))))
      (t state))))

(defun delete-region (state start-line start-col end-line end-col)
  "Delete from START-LINE/START-COL up to END-LINE/END-COL."
  (let* ((ls (lines state))
         (first-line (fset:@ ls start-line))
         (last-line (fset:@ ls end-line))
         (new-line (concatenate 'string
                                (subseq first-line 0 (min start-col (length first-line)))
                                (subseq last-line (min end-col (length last-line)))))
         (built (fset:concat (fset:subseq ls 0 start-line)
                             (fset:seq new-line)
                             (fset:subseq ls (min (1+ end-line) (fset:size ls))))))
    (copy-state state
                :lines built
                :marks (fset:with (fset:with (marks state) :point-line start-line)
                                  :point-charpos start-col)
                :tick (1+ (tick state)))))

(defun move-mark (state mark-name line-idx col)
  "Put MARK-NAME at LINE-IDX/COL."
  (let ((key-line (intern (format nil "~a-LINE" mark-name) :keyword))
        (key-col (intern (format nil "~a-CHARPOS" mark-name) :keyword)))
    (copy-state state
                :marks (fset:with (fset:with (marks state) key-line line-idx) key-col col))))

(defun set-meta (state key value)
  "Set buffer-local KEY to VALUE."
  (copy-state state :meta (fset:with (meta state) key value)))

(defun region-bounds (state)
  "The region between mark and point as (values start-line start-col end-line
end-col), normalized so start precedes end. Nil when there is no mark."
  (let* ((mark (fset:@ (meta state) :mark))
         (ml (and (fset:seq? mark) (fset:lookup mark 0)))
         (mc (and (fset:seq? mark) (fset:lookup mark 1)))
         (snap (state->snapshot state))
         (pl (point-line snap))
         (pc (point-col snap)))
    (when (and ml mc)
      (if (or (< ml pl) (and (= ml pl) (<= mc pc)))
          (values ml mc pl pc)
          (values pl pc ml mc)))))

(defun line-count-of (state)
  (fset:size (lines state)))

(defun line-at (state line-idx)
  (fset:@ (lines state) line-idx))

(defun region-string (state start-line start-col end-line end-col)
  (if (= start-line end-line)
      (let ((line (fset:@ (lines state) start-line)))
        (subseq line (min start-col (length line)) (min end-col (length line))))
      (with-output-to-string (s)
        (let ((first (fset:@ (lines state) start-line)))
          (write-string (subseq first (min start-col (length first))) s))
        (loop for i from (1+ start-line) below end-line
              do (write-char #\Newline s)
                 (write-string (fset:@ (lines state) i) s))
        (write-char #\Newline s)
        (let ((last (fset:@ (lines state) end-line)))
          (write-string (subseq last 0 (min end-col (length last))) s)))))


;;;; Buffer Actor

(defun load-content (content)
  (let ((state (make-empty-state "tmp")))
    (if (string= content "")
        state
        (copy-state state
                    :lines (fset:convert 'fset:seq (split-lines content))))))

(defun %state-language (state)
  "The tree-sitter language keyword for STATE's mode, or nil."
  (let* ((mode-name (buffer-local state :mode :base-mode))
         (mode (pine.editor.mode:find-mode mode-name)))
    (and mode (typep mode 'pine.editor.mode:major-mode) (pine.editor.mode:ts-language mode))))

(defun %ts-runtime ()
  (when pine.core.server:*server* (pine.core.server:ts-runtime pine.core.server:*server*)))

(defun shift-highlights (hl line delta)
  "HL with every tuple at or after LINE moved by DELTA lines.

What a buffer paints between an edit and the parse that describes it. The colours
on the edited line may be a moment out of date; the lines below it are not, which
is what keeps a big file from flickering while its parse catches up."
  (if (or (null hl) (zerop delta))
      hl
      (loop :for tuple :in hl
            :collect (if (>= (first tuple) line)
                         (list (+ (first tuple) delta) (second tuple)
                               (third tuple) (fourth tuple))
                         tuple))))

(defun %hear-the-parser (name self)
  "Bring what the parser writes back to the buffer actor SELF.

The parser answers by writing /buf/NAME/face and /buf/NAME/indent rather than
replying, so the buffer reads those places like anything else would. This is
the whole of the buffer's side of the parse, and it goes when the actor does."
  (pine.ns:watch (pine.path:path (pine.path:parse "/buf") name "face")
                 (lambda (hl) (sento.actor:tell self (list :highlights :hl hl))
                   (fset:empty-map))
                 :as (list :buffer-face name))
  (pine.ns:watch (pine.path:path (pine.path:parse "/buf") name "indent")
                 (lambda (targets)
                   (sento.actor:tell self (list :indent-region :targets targets))
                   (fset:empty-map))
                 :as (list :buffer-indent name)))

(defun ensure-parser (state link name)
  "LINK, starting the buffer's parser actor the first time its mode names a
language. NIL when the buffer has no language or the grammar is unavailable."
  (or link
      (let ((lang (%state-language state))
            (rt (%ts-runtime))
            (srv pine.core.server:*server*))
        (when (and lang rt srv)
          (let ((actor (pine.ts.parser:start-parser
                        (pine.core.server:actor-system srv) rt lang name)))
            (when actor
              (%hear-the-parser name sento.actor:*self*)
              (pine.ts.parser:make-parse-link actor)))))))

(defun request-parse (link state &key (verb :parse) extra)
  "Tell LINK's parser about STATE.

Its mailbox is the queue and its one thread drains it in order, so a burst of
typing arrives as a burst and the newest lines are the last thing parsed. The
answer is a write, not a reply, so nothing here waits and nothing has to
remember that a request is out."
  (when link
    (sento.actor:tell (pine.ts.parser:link-actor link)
                      (list* verb
                             :lines (lines state)
                             :tick (tick state)
                             :viewport (buffer-local state :viewport)
                             :name (buffer-local state :name "")
                             extra)))
  link)

(defun refresh-highlights (pstate new-state &key edit)
  "Parse and walk NEW-STATE's lines here and now, answering (values highlights
pstate). No language available -> (values nil pstate), leaving highlights off.

This is the synchronous path, for benchmarks and tools that want the cost on
their own thread. A buffer does not use it: an edit sends its parser a message
and keeps going, because at a million lines this call is over a second.

EDIT is the (line old-lines new-lines byte-delta) descriptor for the change, when
the caller has one: with it the tree is shifted and reused, without it both tree
and byte index are rebuilt. Nothing here ever builds a string, so the cost does
not grow with the file.

Highlights cover NEW-STATE's :viewport when it has one: the windows showing this
buffer only ever paint the lines they show, so walking the whole tree to throw
most of it away is work nobody reads. A buffer no window has claimed -- a tool
buffer, a buffer being restored -- has no viewport and gets the whole file."
  (let ((lang (%state-language new-state))
        (rt (%ts-runtime)))
    (if (or (null lang) (null rt))
        (values nil pstate)
        (let ((ps (or pstate (pine.ts.runtime:make-parse-state rt lang))))
          (if (null ps)
              (values nil nil)
              (let ((viewport (band (buffer-local new-state :viewport))))
                (pine.ts.runtime:parse-lines! ps (lines new-state)
                                              :edit edit :viewport viewport)
                (values (if viewport
                            (pine.ts.highlight:parse-highlights
                             ps
                             :from-line (car viewport) :to-line (cdr viewport))
                            (pine.ts.highlight:parse-highlights ps))
                        ps)))))))

(defun point-after-move (snap unit n)
  "Target (values line col) after moving point UNIT (:char, :word, or :line) by
signed N, clamped to the buffer. Computed from SNAP's lines in one shot so a
prefix count repeats correctly."
  (let ((lines (lines snap)) (nlines (line-count snap))
        (l (point-line snap)) (c (point-col snap)))
    (flet ((word-char-p (line col)
             (let ((s (fset:@ lines line)))
               (and (>= col 0) (< col (length s)) (alphanumericp (char s col)))))
           (step-fwd ()
             (let ((len (length (fset:@ lines l))))
               (cond ((< c len) (incf c) t)
                     ((< (1+ l) nlines) (setf l (1+ l) c 0) t))))
           (step-back ()
             (cond ((plusp c) (decf c) t)
                   ((plusp l) (setf l (1- l) c (length (fset:@ lines l))) t))))
      (ecase unit
        (:char
         (dotimes (i (abs n))
           (if (plusp n) (step-fwd) (step-back))))
        (:word
         (dotimes (i (abs n))
           (if (plusp n)
               (progn
                 (loop while (and (not (word-char-p l c)) (step-fwd)))
                 (loop while (and (word-char-p l c) (step-fwd))))
               (progn
                 (loop while (and (not (word-char-p l (1- c))) (step-back)))
                 (loop while (and (word-char-p l (1- c)) (step-back)))))))
        (:line
         (let ((tl (max 0 (min (1- nlines) (+ l n)))))
           (setf l tl c (min c (length (fset:@ lines tl))))))))
    (values l c)))

;;;; Indentation support. LINE-INDENT-WIDTH and PREVIOUS-LINE-INDENT read the
;;;; buffer; REINDENT-LINE rewrites one line's leading whitespace and reports
;;;; where point lands. The target column is computed elsewhere (the parse tree
;;;; for a lisp buffer, PREVIOUS-LINE-INDENT for a plain one).

(defun line-indent-width (line)
  "Count of leading space/tab characters in LINE."
  (or (position-if-not (lambda (ch) (or (char= ch #\Space) (char= ch #\Tab))) line)
      (length line)))

(defun previous-line-indent (state line)
  "Indent width of the nearest non-blank line above LINE, or 0."
  (let ((lines (lines state)))
    (loop for i from (1- line) downto 0
          for txt = (fset:@ lines i)
          when (plusp (length (string-trim '(#\Space #\Tab) txt)))
            return (line-indent-width txt)
          finally (return 0))))

(defun reindent-line (state line cur-indent target point-col)
  "Replace LINE's leading whitespace (CUR-INDENT chars) with TARGET spaces.
Returns (values new-state new-point-col). Point moves to TARGET when it sat
inside the old indentation, otherwise shifts by (TARGET - CUR-INDENT)."
  (let* ((s1 (if (plusp cur-indent) (delete-region state line 0 line cur-indent) state))
         (s2 (if (plusp target)
                 (insert-string s1 line 0 (make-string target :initial-element #\Space))
                 s1))
         (new-col (if (<= point-col cur-indent) target (+ point-col (- target cur-indent)))))
    (values s2 (max 0 new-col))))

;;;; The bridge between a buffer's leaves and the value the pure functions
;;;; above take. The tree is what a buffer is; this is only how one message's
;;;; worth of work reads it and lands it again.

(defun at (name &rest leaf)
  (apply #'pine.path:path (pine.path:parse "/buf") name leaf))

(defun band (viewport)
  "VIEWPORT as the (FROM . TO) the parser and the highlight walk take.

A window's range is [from to] at its path, because that is what a seq is for;
the walks below take a cons. This is the one place the two meet."
  (cond ((null viewport) nil)
        ((fset:seq? viewport) (cons (fset:lookup viewport 0) (fset:lookup viewport 1)))
        (t viewport)))

(defparameter +structural+ (quote (:lines :point :tick :face :indent))
  "The leaves this bridge carries itself, or that the parser owns. Everything
else under a buffer is a local and rides the meta.")

(defun %locals (name)
  "NAME's buffer-locals, which is every leaf that is not part of its shape."
  (let ((out (fset:empty-map)))
    (fset:do-map (path value (pine.ns:read (pine.path:path (at name) (pine.path:any))
                                           (fset:empty-map)))
      (let ((key (pine.path:key (pine.path:leaf path))))
        (unless (member key +structural+)
          (setf out (fset:with out key value)))))
    out))

(defparameter +actor-prefix+ "buffer:"
  "What a buffer actor's own name begins with, so the buffer it serves can be
read off it without a table.")

(defun name-of (x)
  "X's buffer name: a string is one, an actor carries its own.

Off the actor rather than out of the table, because a buffer that no table
holds -- the minibuffer, a detached view -- is still a buffer."
  (cond ((stringp x) x)
        ((null x) nil)
        (t (let ((actor-name (ignore-errors (sento.actor-cell:name x))))
             (when (and actor-name
                        (uiop:string-prefix-p +actor-prefix+ actor-name))
               (subseq actor-name (length +actor-prefix+)))))))

(defun snapshot-of (x)
  "X's buffer as a snapshot, read from its leaves. No actor is asked."
  (let ((name (name-of x)))
    (when name (state->snapshot (from-paths name)))))

(defun state-of (x)
  "X's buffer as a state, read from its leaves."
  (let ((name (name-of x)))
    (when name (from-paths name))))

(defun text-of (x)
  "X's buffer as one string, read from its leaves."
  (let ((name (name-of x)))
    (when name (state->string (from-paths name)))))

(defun from-paths (name)
  "NAME's leaves as one buffer-state."
  (let ((point (pine.ns:read (at name :point))))
    (flet ((part (seq n) (and (fset:seq? seq) (fset:lookup seq n))))
      (copy-state
       (make-empty-state name)
       :lines (or (pine.ns:read (at name :lines)) (fset:seq ""))
       :marks (fset:map (:point-line (or (part point 0) 0))
                        (:point-charpos (or (part point 1) 0)))
       :meta (fset:with (%locals name) :name name)
       :tick (or (pine.ns:read (at name :tick)) 0)))))

(defun to-paths (name state)
  "The write-map taking NAME's leaves to what STATE says. One transaction, so
nothing sees the buffer half edited."
  (let ((marks (marks state))
        (meta (meta state))
        (out (fset:empty-map)))
    (setf out (fset:with out (at name :lines) (lines state)))
    (setf out (fset:with out (at name :point)
                         (fset:seq (or (fset:lookup marks :point-line) 0)
                                   (or (fset:lookup marks :point-charpos) 0))))
    (setf out (fset:with out (at name :tick) (tick state)))
    (fset:do-map (key value meta)
      (unless (eq key :name)
        (setf out (fset:with out (at name key) value))))
    out))

(defun actor-dead-p (actor)
  "True when ACTOR will never take another message: stopped, or its own thread
gone.

A buffer parked in the debugger is not dead. Its thread is alive and waiting for
a restart, and mistaking that for death would throw away the fault someone is
looking at, so this asks about the thread rather than about whether it answers."
  (or (not (sento.actor-cell:running-p actor))
      (let ((box (sento.actor-cell:msgbox actor)))
        (and (typep box 'sento.messageb:message-box/bt)
             (let ((thread (slot-value box 'sento.messageb::queue-thread)))
               ;; sento's mailbox threads are bordeaux-threads-2 objects, which
               ;; the v1 predicates this codebase otherwise uses will not take
               (and thread (not (bordeaux-threads-2:thread-alive-p thread))))))))

(defun make-buffer-actor (system name &key (content "") id)
  "A buffer as an actor on SYSTEM: NAME holding CONTENT, dispatching each message
through its mode on a thread of its own.

ID names the buffer across images: another pine, or an agent, addresses this
buffer by it. The layer that creates buffers mints it; this one is below
persistence and does not."
  (let ((initial (move-mark (set-meta (set-meta (load-content content) :name name)
                                      :id id)
                            :point 0 0)))
    ;; the buffer is its leaves, so it exists once they do
    (pine.ns:write (to-paths name initial))
    (sento.actor-context:actor-of system
                                  :name (concatenate (quote string) +actor-prefix+ name)
                                  ;; A buffer runs on its own thread, not a worker
                                  ;; of the shared dispatcher. A receive that
                                  ;; parks in the debugger then costs this buffer
                                  ;; and nothing else: the renderer, the attach
                                  ;; actors carrying input, the registries and
                                  ;; every other buffer keep running. KILL-BUFFER
                                  ;; stops the actor, which ends the thread.
                                  :dispatcher :pinned
                                  ;; state tuple: (STATE UNDO REDO SUBSCRIBERS HIGHLIGHTS PARSE-STATE)
                                  :state (list initial nil nil nil nil nil)
                                  :receive
                                  (lambda (msg)
                                    ;; the tree is what the buffer is: this
                                    ;; message's work reads it here and lands it
                                    ;; again below, so nothing else has to ask
                                    ;; the actor what the buffer says
                                    (setf sento.actor:*state*
                                          (list* (from-paths name)
                                                 (second sento.actor:*state*)
                                                 (third sento.actor:*state*)
                                                 (fourth sento.actor:*state*)
                                                 (pine.ns:read (at name :face))
                                                 (list (sixth sento.actor:*state*))))
                                    (let* ((state (first sento.actor:*state*))
                                           (before state)
                                           (mode-name (buffer-local state :mode :base-mode))
                                           (mode (or (pine.editor.mode:find-mode mode-name)
                                                     (pine.editor.mode:find-mode :base-mode))))
                                      ;; The buffer runs off the session thread, so
                                      ;; an edit error opens the *debugger* restart
                                      ;; menu and parks THIS thread while the fault
                                      ;; is attended, aborting if it is not.
                                      ;; *state* is committed as each handler's last
                                      ;; step, so an error before the commit leaves
                                      ;; the prior buffer intact; `abort' drops the
                                      ;; edit and the actor keeps receiving.
                                      (let ((answer
                                              (pine.err:with-debugger
                                                  (:label (format nil "buffer ~a <- ~a"
                                                                  name (first msg)))
                                                ;; RETRY re-runs the edit after a
                                                ;; live fix (fix the failing defun
                                                ;; in this image, then pick retry);
                                                ;; ABORT drops it and keeps the
                                                ;; prior buffer.
                                                (loop
                                                  (with-simple-restart
                                                      (retry "Retry this edit")
                                                    (return
                                                      (pine.editor.mode:dispatch-message
                                                       mode sento.actor:*self*
                                                       (first msg) (rest msg))))))))
                                        ;; landed only on a normal return, so an
                                        ;; edit that aborted in the debugger
                                        ;; leaves the buffer as it was. The
                                        ;; receive's own value is the answer to
                                        ;; an ask, so it is what comes back.
                                        (let ((after (first sento.actor:*state*)))
                                          (unless (eq before after)
                                            (pine.ns:write (to-paths name after))))
                                        answer))))))

(defun notify-subscribers (subscribers state &optional hl)
  "Send SNAPSHOT to every subscriber. One that cannot be reached does not stop
the others, and does not pass unnoticed either: a renderer that stopped taking
snapshots is a window that has quietly stopped painting."
  (let ((snap (state->snapshot-with-hl state hl)))
    (dolist (ref subscribers)
      (handler-case (sento.actor:tell ref (list :snapshot :snapshot snap))
        (error (c)
          (pine.err:report-failure
           c (format nil "notifying a subscriber of ~a"
                     (buffer-local state :name "a buffer"))))))))


;;;; Buffer Registry

(defun buffer-table (srv)
  (or (pine.core.server:buffer-table srv)
      (setf (pine.core.server:buffer-table srv) (make-hash-table :test 'equal))))

(defun start-buffer-registry (server)
  (let ((sys (pine.core.server:actor-system server)))
    (setf (pine.core.server:buffer-registry server)
          (sento.actor-context:actor-of sys
                                        :name "buffer-registry"
                                        :dispatcher :pinned
                                        :state (fset:empty-map)
                                        :receive
                                        (lambda (msg)
                                          (case (first msg)
                                            (:register
                                             (destructuring-bind (&key name actor) (rest msg)
                                               (setf sento.actor:*state* (fset:with sento.actor:*state* name actor))
                                               (sento.actor:reply actor)))
                                            (:unregister
                                             (destructuring-bind (&key name) (rest msg)
                                               (setf sento.actor:*state* (fset:less sento.actor:*state* name))
                                               (sento.actor:reply t)))
                                            (:lookup
                                             (destructuring-bind (&key name) (rest msg)
                                               (sento.actor:reply (fset:@ sento.actor:*state* name))))
                                            (:list
                                             (let ((names nil))
                                               (fset:do-map (k v sento.actor:*state*)
                                                 (declare (ignore v))
                                                 (push k names))
                                               (sento.actor:reply (nreverse names))))
                                            (:count
                                             (sento.actor:reply (fset:size sento.actor:*state*)))))))))

