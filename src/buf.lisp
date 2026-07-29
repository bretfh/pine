(defpackage #:pine.buf
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:b #:pine.text.buffer))
  (:export #:mount #:unmount #:state #:at #:names #:parser-of #:drop #:motion
           #:indent #:showing #:asked #:forget #:band-of #:*verbs*))

(in-package #:pine.buf)
(named-readtables:in-readtable pine.path:syntax)

;;;; Buffers, as paths. A buffer's content, its point, its locals and its marks
;;;; are places, so nothing has to be exported for something else to read them
;;;; and another image reaches them without an accessor written for the
;;;; occasion.
;;;;
;;;;   /buf/?name/text    the whole string, held as its lines
;;;;   /buf/?name/point   [line col]
;;;;   /buf/?name/mark    [line col], or nothing
;;;;   /buf/?name/mode    a mode keyword
;;;;   /buf/?name/minor   a set
;;;;   /buf/?name/file    the file it visits
;;;;   /buf/?name/tick    moves on an edit, not on a motion
;;;;   /buf/?name/face    what the parser last said about it
;;;;
;;;; and computed from those:
;;;;
;;;;   /buf/?name/line/?n         one line, or the range FROM..TO
;;;;   /buf/?name/face/${a}..${b} the runs a window's range needs
;;;;
;;;; Text is written as a string and held as a seq of lines, so an edit is a
;;;; new seq sharing every line it did not touch rather than a new copy of the
;;;; file, and a line is a lookup rather than a scan.
;;;;
;;;; The band that made a million lines work is not a policy in the parser: it
;;;; is what the window read.

(defun at (name &rest leaf)
  "NAME's path, or the leaf under it."
  (apply #'p:path /buf name leaf))

;;;; What a buffer and its parser say to each other is not a place. The edit
;;;; descriptor, the indent a newline asked for and the range a window is
;;;; showing are one side of a conversation, so they live here rather than
;;;; under the buffer, and (read /buf/?name) is the buffer the doc describes.

(defvar *asked* (sento.atomic:make-atomic-reference :value (fset:empty-map))
  "Name to what has been asked about it.")

(defun asked (name key)
  (let ((m (fset:lookup (sento.atomic:atomic-get *asked*) name)))
    (and m (fset:lookup m key))))

(defun (setf asked) (value name key)
  (sento.atomic:atomic-swap
   *asked*
   (lambda (m)
     (let ((mine (or (fset:lookup m name) (fset:empty-map))))
       (fset:with m name (if (null value)
                             (fset:less mine key)
                             (fset:with mine key value))))))
  value)

(defun forget (name)
  "Forget what was asked about NAME. What a buffer going away does."
  (sento.atomic:atomic-swap *asked* (lambda (m) (fset:less m name))))

(defun names ()
  "Every buffer there is. /buf/current is the one that is current and not a
buffer of its own, so it is not one of them."
  (sort (remove "current"
                (mapcar (lambda (path) (p:leaf (p:parent path)))
                        (pine.data:keys (ns:read /buf/*/text {})))
                :test #'string=)
        #'string<))

;;;; The leaves as one value, so the pure functions in pine.text.buffer apply
;;;; unchanged. Nothing here holds state: this is a read of the tree, a call,
;;;; and the write-map taking the tree to what the call answered.

(defun %point (name)
  (let ((point (ns:read (at name :point))))
    (if (fset:seq? point)
        (values (or (fset:lookup point 0) 0) (or (fset:lookup point 1) 0))
        (values 0 0))))

(defun state (name)
  "NAME's leaves as a buffer-state."
  (multiple-value-bind (line col) (%point name)
    (b:copy-state
     (b:make-empty-state name)
     :lines (or (ns:held (at name :text)) (fset:seq ""))
     :marks (fset:map (:point-line line) (:point-charpos col))
     :meta (fset:map (:name name)
                     (:mode (ns:read (at name :mode)))
                     (:mark (ns:read (at name :mark))))
     :tick (or (ns:read (at name :tick)) 0))))

(defun %landing (name state)
  "The write-map taking NAME's leaves to what STATE says. A value that did not
change does not move, so this is the whole of applying an edit."
  (let ((marks (b:marks state)))
    (fset:map ((at name :text) (b:lines state))
              ((at name :point) (fset:seq (or (fset:lookup marks :point-line) 0)
                                          (or (fset:lookup marks :point-charpos) 0)))
              ((at name :modified) t)
              ((at name :tick) (b:tick state)))))

(defun %edit (name fn &optional descriptor)
  "Apply FN, a state-to-state function, to NAME.

DESCRIPTOR says what the edit did -- at which line, how many lines became how
many, and by how many bytes -- so the parser can shift its tree instead of
rebuilding it. It carries the lines it produced, so it can only be used for the
edit that produced them. Overlays describe the text as it was, so they go."
  (let* ((next (funcall fn (state name)))
         (landing (%landing name next)))
    ;; said before the lines move, because the lines moving is what wakes the
    ;; parse, and it happens on this thread before the write answers
    (setf (asked name :edit)
          (when descriptor
            (fset:with (fset:with descriptor :lines (b:lines next))
                       :from (b:lines (state name)))))
    (ns:write (fset:with landing (at name :overlays) nil))))

;;;; The verbs

(defun %insert (name text)
  (multiple-value-bind (line col) (%point name)
    (let* ((s (state name))
           (at-line (b:line-at s line))
           ;; point past the end of its line inserts at the end, and a shift
           ;; that says the wrong column moves the tree to the wrong place, so
           ;; a clamped insert rebuilds instead
           (exact (and at-line (<= col (length at-line)))))
      (%edit name
             (lambda (s) (b:insert-string s line col text))
             ;; pasted text carries its own newlines, so one line can become
             ;; several
             (when exact
               {:at line :old 1 :new (1+ (count #\Newline text))
                :bytes (pine.ts.index:string-bytes text)})))))

(defun %newline (name)
  "Split the line at point.

Nothing here indents: a mode that wants an electric indent says so, and
lisp-mode does, so the line lands now and takes its column when the parse says
what it is."
  (multiple-value-bind (line col) (%point name)
    (let* ((next (b:insert-newline (state name) line col))
           (lines (b:lines next)))
      (setf (asked name :edit)
            {:at line :old 1 :new 2 :bytes 1 :lines lines
             :from (b:lines (state name))})
      (ns:write (fset:with (%landing name next) (at name :overlays) nil)))))

(defun %delete (name from to)
  "Delete the region FROM..TO, which is what a backspace is one character back.

The lines it spans become one, and that is a whole-line shift, so the parser
moves its tree by the bytes that went rather than rebuilding it."
  (let* ((s (state name))
         (from-line (fset:lookup from 0))
         (from-col (fset:lookup from 1))
         (to-line (fset:lookup to 0))
         (to-col (fset:lookup to 1))
         (gone (b:region-string s from-line from-col to-line to-col)))
    (%edit name
           (lambda (s) (b:delete-region s from-line from-col to-line to-col))
           ;; a shift describes lines, so it can say a backspace and a line
           ;; join and no more: a region spanning three lines leaves the middle
           ;; one nowhere in the description, and a tree shifted by a
           ;; description that does not fit is worse than one rebuilt
           (when (<= (- to-line from-line) 1)
             {:at from-line :old (1+ (- to-line from-line)) :new 1
              :bytes (- (pine.ts.index:string-bytes gone))}))))

(defun %kill (name)
  "Cut the region to /kill, the ring a yank reads."
  (let ((s (state name)))
    (multiple-value-bind (from-line from-col to-line to-col) (b:region-bounds s)
      (when from-line
        (ns:write /kill (b:region-string s from-line from-col to-line to-col)
                  :max 60)
        (%delete name (fset:seq from-line from-col) (fset:seq to-line to-col))))))

(defun %move (name unit n)
  (let ((s (state name)))
    (multiple-value-bind (line col)
        (b:point-after-move (b:state->snapshot s) unit n)
      (ns:write (at name :point) (fset:seq line col)))))

;;;; Indenting
;;;;
;;;; A buffer with a tree asks its parser for every target in the region at
;;;; once and applies them when the answer lands, so Tab on a thousand lines
;;;; costs one parse instead of one per line. A buffer without one indents from
;;;; its previous line, here and now.

(defun %indented (name targets)
  "The write-map that reindents each (LINE . COLUMN) of TARGETS, or an empty
one when every line already sits where it should.

Point rides its character: anchored as an offset past its line's first
non-whitespace, which also lands it on the first non-whitespace when it sat
inside the old indentation.

No edit descriptor: reindenting rewrites a line's leading whitespace through a
delete and an insert, and one whole-line shift cannot describe that pair, so the
parse rebuilds."
  (let* ((before (state name))
         (snap (b:state->snapshot before))
         (line (b:point-line snap))
         (col (b:point-col snap))
         (first-nw (b:line-indent-width (b:line-at before line)))
         (tail (max 0 (- col first-nw)))
         (next before)
         (changed nil))
    (dolist (target targets)
      (let ((where (car target))
            (want (cdr target)))
        (let ((have (b:line-indent-width (b:line-at next where))))
          (unless (eql want have)
            (setf next (nth-value 0 (b:reindent-line next where have want col))
                  changed t)))))
    (if (not changed)
        (fset:empty-map)
        (let* ((indent (b:line-indent-width (b:line-at next line)))
               (final (b:move-mark next :point line (+ indent tail))))
          (setf (asked name :edit) nil)
          (fset:with (%landing name final) (at name :overlays) nil)))))

;;;; Undo has no stacks. The doc: undo of a paste and undo of a window split are
;;;; the same operation, because they are the same kind of thing. So an undo is
;;;; the newest change the file remembers under this buffer, written back to
;;;; what it was.

(defun %changes-under (name)
  "Every change the file remembers under NAME, newest first."
  (let ((log (ns:read /history))
        (under (at name)))
    (when (fset:seq? log)
      (remove-if-not (lambda (change)
                       (p:prefixp under (fset:lookup change :path)))
                     (fset:convert 'list log)))))

(defun %undo (name)
  "Put back what the newest edit to NAME changed.

An edit is one transaction, and the file remembers which rows one transaction
moved, so the lines, the point and the tick go back together. There is no undo
stack anywhere."
  (let* ((rows (%changes-under name))
         (edit (find (at name :tick) rows
                     :key (lambda (change) (fset:lookup change :path))
                     :test #'fset:equal?)))
    (when edit
      (let ((commit (fset:lookup edit :commit)))
        (dolist (change rows)
          (when (eql commit (fset:lookup change :commit))
            (ns:write (fset:lookup change :path)
                      (fset:lookup change :old))))
        ;; what was put back, so a redo knows which edit to apply again
        (setf (asked name :undone) commit)))))

(defun %redo (name)
  "Apply again the edit the last undo put back."
  (let ((commit (asked name :undone)))
    (when commit
      (dolist (change (%changes-under name))
        (when (eql commit (fset:lookup change :commit))
          (ns:write (fset:lookup change :path) (fset:lookup change :new))))
      (setf (asked name :undone) nil))))

;;;; What text is, on the way in and on the way out

(defun %lines (value)
  "VALUE as the lines the tree holds. Text is written as a string; what lands
is its lines, so the next edit shares every line it did not touch."
  (if (stringp value)
      (fset:convert 'fset:seq (b:split-lines value))
      value))

(defun %joined (lines)
  (if (fset:seq? lines)
      (format nil "~{~a~^~%~}" (fset:convert 'list lines))
      ""))

;;;; The file it visits

(defun %file-path (file)
  "The /file path FILE names. A file is a place, so saving and visiting are a
write and a read of one."
  (p:path /file (p:spliced (if (stringp file) file (p:text file)))))

(defun %text (name)
  (%joined (ns:held (at name :text))))

(defun %save (name)
  (let ((file (ns:read (at name :file))))
    (when file
      (ns:write (%file-path file) (%text name))
      (ns:write (at name :modified) nil)
      t)))

(defun %visit (name file)
  "Read FILE into NAME. Its text is derived from the file, so it is not stored
and comes back by being read again."
  (ns:write (fset:map ((at name :file) file)
                      ((at name :text) (or (ns:read (%file-path file)) ""))
                      ((at name :point) (fset:seq 0 0))
                      ((at name :modified) nil))))

(defun %revert (name)
  "Read the file again, throwing away what was typed. There is no revert
function and nothing to invalidate: the file is a path, so this is a read."
  (let ((file (ns:read (at name :file))))
    (when file (%visit name file))))

;;;; Reading a range, which is the only reason the whole file is ever walked

(defun %span (segment)
  "FROM..TO, when a segment names a range rather than one line."
  (let ((dots (search ".." segment)))
    (when dots
      (let ((from (parse-integer segment :end dots :junk-allowed t))
            (to (parse-integer segment :start (+ dots 2) :junk-allowed t)))
        (when (and from to) (cons from to))))))

(defun %put-line (name n text)
  "Replace line N. One line for one line, so the parser shifts by the bytes
that changed."
  (let* ((s (state name))
         (was (b:line-at s n)))
    (unless (equal was text)
      (%edit name
             (lambda (s) (b:copy-state s :lines (fset:with (b:lines s) n text)
                                         :tick (1+ (b:tick s))))
             {:at n :old 1 :new 1
              :bytes (- (pine.ts.index:string-bytes text)
                        (pine.ts.index:string-bytes (or was "")))}))))

(defun %range (name from to)
  (let* ((lines (ns:held (at name :text)))
         (size (and (fset:seq? lines) (fset:size lines))))
    (when size
      (fset:subseq lines (max 0 (min from size)) (max 0 (min (1+ to) size))))))

(defun %runs (name from to)
  "The highlight runs covering lines FROM through TO."
  (let ((face (ns:read (at name :face))))
    (when face
      (fset:convert 'fset:seq
                    (remove-if-not (lambda (run)
                                     (<= from (first run) to))
                                   (fset:convert 'list face))))))

;;;; The verbs, through whatever mode claims them
;;;;
;;;; A minor mode's :on entry answers first, then the major mode's and its
;;;; parents', then the built-in below. A handler answers a map of writes, which
;;;; is applied as one transaction; nothing is asked, so this is safe from
;;;; wherever the verb was written.

(defvar *dispatching* nil
  "The (buffer . verb) pairs a mode handler is answering right now.")

(defun %verb (name verb args fallback)
  "Answer VERB for NAME through whatever mode claims it, or the built-in.

A handler writes the verb it claimed to reach the built-in one, which is what
lisp-mode's newline does: {text [:newline] buf [:indent-line]}. So while a
handler for a verb is running, that verb on that buffer is the built-in."
  (let* ((claim (cons name verb))
         (handler (unless (member claim *dispatching* :test #'equal)
                    (pine.mode:handler name verb))))
    (if handler
        (let ((*dispatching* (cons claim *dispatching*)))
          (let ((answer (apply handler name args)))
            (when (fset:map? answer) (ns:write answer))
            nil))
        (funcall fallback))))

;;;; A tool buffer is a mode with a :view: a function of the buffer answering a
;;;; widget tree. The tree is written here as an expression, so what it read is
;;;; recorded and it is computed again when any of it moves. That is the whole
;;;; of a tool buffer: nothing polls, nothing subscribes, and the buffer holds
;;;; no copy of what it is showing.

(defvar *verbs* (fset:empty-map)
  "The built-in verbs of a buffer that need what is layered above /buf: what a
row activates, and moving between rows. The layer that has the widgets
installs them, the way the buffer actor's messages are installed.")

(defun %builtin (name verb args)
  (let ((fn (fset:lookup *verbs* verb)))
    (when fn (apply fn name args))))

(defun %view (name)
  "Write NAME's view, if its mode has one. Written as an expression, so what
the view read is what wakes it.

A buffer showing a view takes the minor mode that moves between its rows, so
Return and the arrows mean what they mean in a tool buffer whatever its major
mode is."
  (let* ((mode (ns:read (at name :mode)))
         (fn (and mode (pine.mode:setting mode :view))))
    (cond (fn
           (ns:write (at name :view) (funcall fn name) :keep nil)
           (ns:write (at name :minor) (fset:seq :conj :view)))
          (t
           (ns:write (at name :view) nil)
           (ns:write (at name :minor) (fset:seq :disj :view))))))

;;;; The buffer that is current, which is a place like any other. Nothing holds
;;;; a pointer to it: /buf/current names a buffer, and a leaf under it is that
;;;; buffer's leaf, so a command that acts on "the current buffer" writes a
;;;; path and needs no client.

(defun %current ()
  "The buffer /buf/current names, as a path, or NIL."
  (let ((value (ns:held /buf/current)))
    (cond ((p:pathp value) value)
          ((stringp value) (at value)))))

(defun %there (&rest leaf)
  "LEAF under the current buffer, or NIL when there is none."
  (let ((current (%current)))
    (when current (apply #'p:path current leaf))))

;;;; The paths

(defun provider ()
  (ns:provider
   {:scope (p:root)}
   (/buf/current/?leaf/?which
    {:read (pine.data:fn [] (let ((there (%there leaf which)))
                              (and there (ns:read there))))
     :write (pine.data:fn [v] (let ((there (%there leaf which)))
                                (when there (ns:write there v))))
     :doc "the current buffer's leaf"})
   (/buf/current/?leaf
    {:read (pine.data:fn [] (let ((there (%there leaf)))
                              (and there (ns:read there))))
     :write (pine.data:fn [v] (let ((there (%there leaf)))
                                (when there (ns:write there v))))
     :verbs {t (pine.data:fn [verb] (let ((there (%there leaf)))
                                      (when there (ns:write there verb))))}
     :doc "the current buffer's leaf"})
   (/buf/current
    {:verbs {t (pine.data:fn [verb] (let ((there (%current)))
                                      (when there (ns:write there verb))))}
     :doc "the buffer that is current; write a buffer path to switch"})
   (/buf/?name/line/?which
    {:read (pine.data:fn []
             (let ((span (%span which)))
               (if span
                   (%range name (car span) (cdr span))
                   (let ((n (parse-integer which :junk-allowed t))
                         (lines (ns:held (at name :text))))
                     (and n (fset:seq? lines) (fset:lookup lines n))))))
     :write (pine.data:fn [v]
              (let ((n (parse-integer which :junk-allowed t)))
                (when n (%put-line name n v))))
     :doc "one line, or the range FROM..TO a window is showing"})
   (/buf/?name/face/?which
    {:read (pine.data:fn []
             (let ((span (%span which)))
               (when span (%runs name (car span) (cdr span)))))
     :doc "the highlight runs a window's range needs"})
   (/buf/?name/text
    {:in (pine.data:fn [v] (%lines v))
     :out (pine.data:fn [lines] (%joined lines))
     :verbs {:insert (pine.data:fn [text]
                       (%verb name :insert (list text)
                              (lambda () (%insert name text))))
             :newline (pine.data:fn []
                        (%verb name :newline nil (lambda () (%newline name))))
             :delete (pine.data:fn [from to]
                       (%verb name :delete (list from to)
                              (lambda () (%delete name from to))))
             :indent (pine.data:fn (&optional from to) (indent name from to))
             :kill (pine.data:fn [] (%kill name))
             :undo (pine.data:fn [] (%undo name))
             :redo (pine.data:fn [] (%redo name))
             :save (pine.data:fn [] (%save name))
             :revert (pine.data:fn [] (%revert name))}
     :doc "the whole string; [:insert TEXT] [:delete FROM TO] [:kill] [:undo]"})
   (/buf/?name/point
    {:verbs {:move (pine.data:fn [unit n] (%move name unit n))}
     :doc "[line col]; [:move :word 1] to step by something"})
   (/buf/?name
    {:verbs {:visit (pine.data:fn [file] (%visit name file))
             :indent-line (pine.data:fn [] (indent name))
             :activate (pine.data:fn []
                         (%verb name :activate nil
                                (lambda () (%builtin name :activate nil))))
             :select (pine.data:fn [delta]
                       (%verb name :select (list delta)
                              (lambda () (%builtin name :select (list delta)))))}
     :doc "the buffer; [:visit FILE], [:activate] what is selected"})
   (/buf
    {:ls (pine.data:fn [] (names))
     :doc "every buffer"})))

;;;; The parse, off a watch
;;;;
;;;; A buffer's parser owns a TSParser, which may not be touched from two
;;;; threads, so it stays an actor of its own. It is told and never asked, and
;;;; it answers by writing /buf/?name/face, so nothing waits on a parse and
;;;; what it computed is a place rather than a message.
;;;;
;;;; What starts one is the lines moving, and what bounds it is the range some
;;;; window said it was showing.

(defvar *parsers* (sento.atomic:make-atomic-reference :value (fset:empty-map))
  "Name to parse-link, for the buffers that have a language.")

(defvar *where* nil
  "The actor system and tree-sitter runtime a parser is started on, as
(SYSTEM . RUNTIME). What MOUNT was given, so that anything which needs a parse
can have one started rather than only the two watches that drive it: a buffer
whose text and mode both landed before /buf was watching still parses the
moment a window asks to see it.")

(defun %link (name system runtime)
  (or (fset:lookup (sento.atomic:atomic-get *parsers*) name)
      (let* ((mode (ns:read (at name :mode)))
             (grammar (and mode (pine.mode:setting mode :grammar))))
        (when (and grammar system runtime)
          (let ((actor (pine.ts.parser:start-parser system runtime grammar name)))
            (when actor
              (let ((link (pine.ts.parser:make-parse-link actor)))
                (sento.atomic:atomic-swap
                 *parsers* (lambda (m) (fset:with m name link)))
                link)))))))

(defun %for (name key lines)
  "What was asked about NAME under KEY, or NIL when it describes some other
lines.

An edit descriptor and an indent request are said just before the lines they
belong to move, and they carry those lines, so identity is the whole of the
check: the same object means it is that very edit."
  (let ((about (asked name key)))
    (when (and about (eq (fset:lookup about :lines) lines))
      about)))

(defun parser-of (name)
  "The actor parsing NAME, or NIL. /buf owns the parsers, so this is where
anything that has business with one asks."
  (let ((link (fset:lookup (sento.atomic:atomic-get *parsers*) name)))
    (and link (pine.ts.parser:link-actor link))))

(defun drop (name)
  "Stop parsing NAME and forget it. What a buffer going away does."
  (let ((actor (parser-of name)))
    (when actor (sento.actor:tell actor '(:stop)))
    (sento.atomic:atomic-swap *parsers* (lambda (m) (fset:less m name)))
    (forget name)
    actor))

(defun band-of (name)
  "The range a window said it is showing NAME over, as the parser wants it."
  (b:band (asked name :viewport)))

(defun motion (name kind)
  "Ask NAME's parser for a structural target of KIND from where point is.

It answers by writing /buf/?name/point, so nothing waits here and the jump
lands the way any other move does."
  (%parse name)
  (let ((actor (parser-of name)))
    (when actor
      (multiple-value-bind (line col) (%point name)
        (sento.actor:tell actor
                          (list :motion :name name :kind kind :line line :col col
                                :lines (or (ns:held (at name :text)) (fset:seq ""))
                                :viewport (band-of name)
                                :space ns:*space*))))))

(defun %reindent (name)
  "How the parser answers an indent: with the lines it says should move, which
this puts in. Not a place, so it goes back to whoever asked."
  (lambda (targets) (ns:write (%indented name targets))))

(defun indent (name &optional from to)
  "Indent the lines FROM through TO, defaulting to the line point is on.

With a parser, it is asked for every target at once and applies what it says
when it answers; without one, each line takes its previous line's indent."
  (let* ((current (state name))
         (snap (b:state->snapshot current))
         (count (b:line-count snap))
         (first-line (max 0 (or from (b:point-line snap))))
         (last-line (min (or to (b:point-line snap)) (1- count)))
         (actor (progn (%parse name) (parser-of name))))
    (if actor
        (sento.actor:tell actor
                          (list :indent :name name :from first-line :to last-line
                                :lines (b:lines current)
                                :viewport (band-of name)
                                :space ns:*space*
                                :answer (%reindent name)))
        (ns:write
         (%indented name
                    (loop :for line :from first-line :to last-line
                          :for target = (b:previous-line-indent current line)
                          :when target :collect (cons line target)))))))

(defun %tell-parse (name actor)
  "Tell ACTOR the lines as they stand, over the range a window said it is
showing.

Whoever edited said what it did just before the lines moved, so the parser can
shift its tree instead of rebuilding it, and an indent someone asked for goes
into the same mailbox behind the parse it needs."
  (let* ((lines (or (ns:held (at name :text)) (fset:seq "")))
         (band (band-of name))
         (edit (%for name :edit lines))
         (indent (%for name :indent-request lines)))
    (sento.actor:tell
     actor
     (list :parse :name name :lines lines :viewport band :space ns:*space*
           :tick (or (ns:read (at name :tick)) 0)
           :edit (when edit
                   (list (fset:lookup edit :at) (fset:lookup edit :old)
                         (fset:lookup edit :new) (fset:lookup edit :bytes)))
           :from-lines (and edit (fset:lookup edit :from))))
    (when indent
      (sento.actor:tell
       actor
       (list :indent :name name :lines lines :viewport band :space ns:*space*
             :from (fset:lookup indent :from)
             :to (fset:lookup indent :to)
             :answer (%reindent name))))))

(defun %parse (name &optional (system (car *where*)) (runtime (cdr *where*)))
  "Parse NAME, starting its parser when it names a grammar and has none.

Everything that wants a parse comes through here, so a parser is made when one
is needed rather than only when a watch happened to fire. A buffer whose text
and mode landed together -- or landed again unchanged, which moves nothing --
still parses the moment anything asks."
  (let ((link (%link name system runtime)))
    (when link (%tell-parse name (pine.ts.parser:link-actor link)))))

(defun showing (name band)
  "Say which lines a window is showing NAME over, as [from to].

Highlights exist to be painted, so what is painted is what bounds the parse.
That is why a background buffer of a million lines costs nothing: nobody read
its faces, so nobody computed them."
  (unless (fset:equal? band (asked name :viewport))
    (setf (asked name :viewport) band)
    (%parse name))
  band)

(defun mount (&key system runtime)
  "Serve /buf. With SYSTEM and RUNTIME a buffer that names a grammar is parsed
whenever its text or its mode moves."
  (ns:write /buf (provider))
  (when (and system runtime)
    (setf *where* (cons system runtime))
    (ns:watch /buf/*/text
              (pine.data:fn [v]
                (declare (ignore v))
                (%parse (p:leaf (p:parent (ns:here))) system runtime)
                {})
              :as :buf-parse)
    (ns:watch /buf/*/mode
              (pine.data:fn [v]
                (declare (ignore v))
                (%parse (p:leaf (p:parent (ns:here))) system runtime)
                {})
              :as :buf-mode))
  ;; a mode with a :view makes the buffer a tool buffer
  (ns:watch /buf/*/mode
            (pine.data:fn [v]
              (declare (ignore v))
              (%view (p:leaf (p:parent (ns:here))))
              {})
            :as :buf-view)
  nil)

(defun unmount ()
  (ns:watch /buf/*/text nil :as :buf-parse)
  (ns:watch /buf/*/mode nil :as :buf-mode)
  (ns:watch /buf/*/mode nil :as :buf-view)
  (fset:do-map (name link (sento.atomic:atomic-get *parsers*))
    (declare (ignore name))
    (sento.actor:tell (pine.ts.parser:link-actor link) '(:stop)))
  (sento.atomic:atomic-swap *parsers* (lambda (m) (declare (ignore m))
                                        (fset:empty-map)))
  (sento.atomic:atomic-swap *asked* (lambda (m) (declare (ignore m))
                                      (fset:empty-map)))
  (ns:write /buf nil))
