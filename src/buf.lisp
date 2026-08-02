(defpackage #:pine.buf
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:b #:pine.text))
  (:shadow #:make)
  (:export #:buf #:bufp
           #:make #:server #:at #:names #:drop #:kill #:forget
           #:state #:state-of #:snapshot-of #:text-of #:name-of #:live
           #:local #:put #:edit #:put-point #:point #:landing
           #:overlay #:clear-overlays
           #:motion #:indent #:showing #:asked #:band-of #:parser-of #:tree-of
           #:*verbs* #:move #:delete-back
           #:find-file #:read-file #:write-file #:record-places #:tool-p #:show
           #:current #:current-name #:focused-snap #:save-current
           #:move-chars #:move-lines #:move-words #:move-sexp))

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
;;;; Text is written as a string and held as a seq of lines, so an edit is a new
;;;; seq sharing every line it did not touch rather than a new copy of the file,
;;;; and a line is a lookup rather than a scan. The band that makes a million
;;;; lines work is not a policy in the parser: it is what the window read.

(defvar *dispatching* nil
  "The (buffer . verb) pairs a mode handler is answering right now.")

(defvar *verbs* (fset:empty-map)
  "The built-in verbs of a buffer that need what is layered above /buf: what a
row activates, and moving between rows. The layer that has the widgets installs
them, the way the buffer actor's messages are installed.")

(defun at (name &rest leaf)
  "NAME's path, or the leaf under it."
  (apply #'p:path /buf name leaf))

(defun bufp (x)
  "Whether X is a buffer: a path under /buf, or the name of one."
  (or (and (stringp x) (plusp (length x)))
      (and (p:pathp x) (p:prefixp /buf x))))

(deftype buf ()
  "A buffer. One of the core types the layer offers: text, a point, a mark, a
mode, the file it visits, what the parser said about it -- all leaves under
/buf/?name. There is no buffer object, because a buffer is its paths, so the
type is what names them."
  '(satisfies bufp))

(defun names ()
  "Every buffer there is. /buf/current names one and is not a buffer of its own,
so it is not among them. Read off the held map rather than through a pattern:
/buf answers its own children with this."
  (let ((held (ns:held /buf))
        (acc nil))
    (when (fset:map? held)
      (fset:do-map (key value held)
        (let ((name (p:name key)))
          (when (and (not (string= name "current"))
                     (fset:map? value)
                     (fset:lookup value (p:key "text")))
            (push name acc)))))
    (sort acc #'string<)))

;;;; What a buffer and its parser say to each other: the edit descriptor, the
;;;; indent a newline asked for, the range a window is showing. Under the buffer
;;;; at /buf/?name/asked/?key, so it is the buffer's and not this image's.

(defun asked (name key)
  (ns:read (at name :asked (p:name key))))

(defun %said (value)
  "VALUE as the write that replaces what is there. A map written to a path
merges its keys, which is right for a buffer's leaves and wrong here: what was
asked is one answer, not an accumulation of every answer so far."
  (if (fset:map? value) (fset:seq :set value) value))

(defun (setf asked) (value name key)
  (ns:write (at name :asked (p:name key)) (%said value))
  value)

(defun forget (name)
  "Forget what was asked about NAME."
  (ns:write (at name :asked) nil))

;;;; What text is, on the way in and on the way out.

(defun %lines (value)
  "VALUE as the lines the tree holds. Text is written as a string; what lands is
its lines, so the next edit shares every line it did not touch."
  (if (stringp value)
      (fset:convert 'fset:seq (b:split-lines value))
      value))

(defun %joined (lines)
  (if (fset:seq? lines)
      (format nil "~{~a~^~%~}" (fset:convert 'list lines))
      ""))

(defun %text (name)
  (%joined (ns:held (at name :text))))

;;;; The leaves as one value, so the pure functions in pine.text apply
;;;; unchanged.

(defun point (name)
  "Where point is in NAME, as (values LINE COL). A read of the leaf, so nothing
waits and a buffer with nothing there is at the top."
  (let ((point (ns:read (at name :point))))
    (if (fset:seq? point)
        (values (or (fset:lookup point 0) 0) (or (fset:lookup point 1) 0))
        (values 0 0))))

(defun %point (name) (point name))

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

(defun landing (name state)
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
edit that produced them.

The edit is computed from the text it read and guarded on that text, so two
threads editing one buffer at once do not lose each other's work: the one that
loses the swap computes again against what landed."
  (loop :for was = (ns:held (at name :text))
        :for from = (b:lines (state name))
        :for next = (funcall fn (state name))
        :for landing = (landing name next)
        :do
           ;; the descriptor lands with the lines, in one transaction: it is
           ;; what the parse reads when the lines wake it, and a write of its
           ;; own would be a second root for undo to walk onto
           (let ((moved (ns:write (fset:with
                                   (fset:with landing (at name :overlays) nil)
                                   (at name :asked "edit")
                                   (%said
                                    (when descriptor
                                      (fset:with (fset:with descriptor :lines
                                                            (b:lines next))
                                                 :from from))))
                                  :when (fset:map ((at name :text) was)))))
             ;; nothing moved either because the guard refused it, or because
             ;; the edit had nothing to do; only the first is worth trying again
             (when (or (not (fset:empty? moved))
                       (eq was (ns:held (at name :text))))
               (return moved))
             (bordeaux-threads:thread-yield))))

;;;; The buffer that is current, which is a place like any other. Nothing holds
;;;; a pointer to it: /buf/current names a buffer, and a leaf under it is that
;;;; buffer's leaf, so a command that acts on "the current buffer" writes a path
;;;; and needs no client.

(defun make (name &key (content ""))
  "The buffer NAME, holding CONTENT. It exists once its leaves do."
  (unless (ns:held (at name :text))
    (ns:write (fset:map ((at name :text) content)
                        ((at name :point) (fset:seq 0 0)))))
  name)

(defun %current ()
  "The buffer /buf/current names, as a path, or NIL."
  (let ((value (ns:held /buf/current)))
    (cond ((p:pathp value) value)
          ((stringp value) (at value)))))

(defun show (name)
  "Make NAME the current buffer, in the focused window when there is one."
  (let ((w (pine.win:focused)))
    (when w
      (ns:write (fset:map ((p:child w "buf") (at name))
                          ((p:child w "scroll") 0)))))
  (ns:write /buf/current (at name))
  name)

(defun %there (&rest leaf)
  "LEAF under the current buffer, or NIL when there is none."
  (let ((current (%current)))
    (when current (apply #'p:path current leaf))))

;;;; Overlays: a transient annotation on a line, cleared by the next edit. Not
;;;; stored, because it describes the text as it stands.

(defun overlay (name line text &optional (class :eval-result))
  "Put TEXT beside LINE in NAME, drawn after that line's own text."
  (let ((was (or (ns:read (at name :overlays)) (fset:empty-map))))
    (ns:write (at name :overlays) (fset:with was line (list text class))
              :keep nil)))

(defun clear-overlays (name)
  (ns:write (at name :overlays) nil))

;;;; The file it visits.

(defun read-file (path)
  (with-open-file (s path :direction :input :if-does-not-exist nil)
    (when s
      (with-output-to-string (out)
        (loop :for line = (read-line s nil nil)
              :while line
              :do (write-string line out)
                  (write-char #\Newline out))))))

(defun write-file (path text)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string text s))
  path)

(defun %file-path (file)
  "The /file path FILE names. A file is a place, so saving and visiting are a
write and a read of one."
  (p:path /file (p:spliced (if (stringp file) file (p:text file)))))

(defun %save (name)
  (let ((file (ns:read (at name :file))))
    (when file
      (ns:write (%file-path file) (%text name))
      (ns:write (at name :modified) nil)
      t)))

(defun %read-in (name file)
  "The write-map putting FILE's contents in NAME. Its text is derived from the
file, so it is not stored and comes back by being read again, which is also what
brings a buffer back at boot."
  (fset:map ((at name :text) (or (ns:read (%file-path file)) ""))
            ((at name :modified) nil)))

(defun %visit (name file)
  "Read FILE into NAME, and put point at the top."
  (ns:write (fset:with (fset:with (%read-in name file) (at name :file) file)
                       (at name :point) (fset:seq 0 0))))

(defun %revert (name)
  "Read the file again, throwing away what was typed. There is no revert
function and nothing to invalidate: the file is a path, so this is a read."
  (let ((file (ns:read (at name :file))))
    (when file (%visit name file))))

(defun %clamped (content line col)
  (let* ((lines (or (uiop:split-string content :separator '(#\Newline)) (list "")))
         (l (max 0 (min line (1- (length lines)))))
         (c (max 0 (min col (length (nth l lines))))))
    (values l c)))

(defun tool-p (name)
  (and (stringp name) (plusp (length name)) (char= #\* (char name 0))))

(defun find-file (path)
  "Open PATH in a buffer named for it, at the place it was left."
  (let* ((expanded (merge-pathnames path))
         (exists (probe-file expanded))
         (namestring (namestring expanded))
         (name (let ((n (file-namestring expanded)))
                 (if (string= n "") namestring n)))
         (content (if exists (or (read-file expanded) "") ""))
         (place (and exists (ns:read (at name :point)))))
    (make name :content content)
    (ns:write (at name :file) namestring)
    (ns:write /recent namestring :max 100 :keep t)
    (if (fset:seq? place)
        (multiple-value-bind (l c)
            (%clamped content (or (fset:lookup place 0) 0)
                      (or (fset:lookup place 1) 0))
          (ns:write (at name :point) (fset:seq l c)))
        (ns:write (at name :point) (fset:seq 0 0)))
    (ns:write (at name :mode) (or (pine.mode:for-file namestring) :text))
    (show name)
    name))

(defun record-place (name)
  "Keep where point is, so the next visit resumes there."
  (when (ns:read (at name :file))
    (multiple-value-bind (line col) (point name)
      (ns:write (at name :point) (fset:seq line col) :keep t))))

(defun record-places ()
  (dolist (name (names)) (ignore-errors (record-place name))))

;;;; The verbs, through whatever mode claims them. A minor mode's :on entry
;;;; answers first, then the major mode's and its parents', then the built-in.
;;;; A handler answers a map of writes, which is applied as one transaction;
;;;; nothing is asked, so this is safe from wherever the verb was written.

(defun %builtin (name verb args)
  (let ((fn (fset:lookup *verbs* verb)))
    (when fn (apply fn name args))))

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
  "Split the line at point. Nothing here indents: a mode that wants an electric
indent says so, so the line lands now and takes its column when the parse says
what it is."
  (multiple-value-bind (line col) (%point name)
    (let* ((next (b:insert-newline (state name) line col))
           (lines (b:lines next)))
      (setf (asked name :edit)
            {:at line :old 1 :new 2 :bytes 1 :lines lines
             :from (b:lines (state name))})
      (ns:write (fset:with (landing name next) (at name :overlays) nil)))))

(defun %delete (name from to)
  "Delete the region FROM..TO, which is what a backspace is one character back."
  (let* ((s (state name))
         (from-line (fset:lookup from 0))
         (from-col (fset:lookup from 1))
         (to-line (fset:lookup to 0))
         (to-col (fset:lookup to 1))
         (gone (b:region-string s from-line from-col to-line to-col)))
    (%edit name
           (lambda (s) (b:delete-region s from-line from-col to-line to-col))
           ;; a shift describes lines, so it can say a backspace and a line join
           ;; and no more: a region spanning three lines leaves the middle one
           ;; nowhere in the description
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

;;;; Undo has no stacks: a write supersedes rather than destroys, so an undo is
;;;; the newest change the tree remembers under this buffer, written back.

(defun %was (name n)
  "NAME's leaves as of N supersessions back, or NIL."
  (let ((root (ns:root-at n)))
    (when root (ns:at-root root (at name)))))

(defun %walk (name)
  "The undo walk NAME is in, or a fresh one. Anything but an undo or a redo
moving the text starts the walk again, which is what makes an edit after an undo
the new end of the history."
  (let ((state (asked name :walk)))
    (if (and state (eq (fset:lookup state :text) (ns:held (at name :text))))
        state
        (fset:map (:seen nil) (:redo nil) (:text nil)))))

(defun %undo (name)
  "Put NAME back to the newest root this walk has not been to. Where the walk
has already been is remembered, so a second undo goes further rather than back
to where it started, and it bottoms out at the oldest root kept."
  (let* ((now (ns:held (at name :text)))
         (state (%walk name))
         (seen (cons now (fset:lookup state :seen))))
    (loop :for n :from 0 :below pine.ns:*roots-kept*
          :for was = (%was name n)
          :while was
          :for text = (fset:lookup was :text)
          :when (and text (notany (lambda (s) (eq s text)) seen))
            ;; the walk goes in with the buffer it is putting back: :SET
            ;; replaces everything under the buffer, so a walk written first
            ;; would be the first thing the undo threw away
            :do (return
                  (ns:write (at name)
                            (fset:seq
                             :set (fset:with was (p:key "asked")
                                             (fset:map ((p:key "walk")
                                                        (fset:map
                                                         (:seen seen)
                                                         (:redo (cons now (fset:lookup state :redo)))
                                                         (:text text)))))))))))

(defun %redo (name)
  "Put back what the last undo took away."
  (let* ((state (%walk name))
         (stack (fset:lookup state :redo)))
    (when stack
      (setf (asked name :walk)
            (fset:map (:seen (rest (fset:lookup state :seen)))
                      (:redo (rest stack))
                      (:text (first stack))))
      (ns:write (at name :text) (first stack)))))

;;;; Indenting. A buffer with a tree asks its parser for every target in the
;;;; region at once and applies them when the answer lands, so Tab on a thousand
;;;; lines costs one parse instead of one per line. A buffer without one indents
;;;; from its previous line, here and now.

(defun %indented (name targets)
  "The write-map that reindents each (LINE . COLUMN) of TARGETS, or an empty one
when every line already sits where it should.

Point rides its character: anchored as an offset past its line's first
non-whitespace, which also lands it on the first non-whitespace when it sat
inside the old indentation. No edit descriptor, because reindenting is a delete
and an insert and one whole-line shift cannot describe that pair."
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
          (fset:with (landing name final) (at name :overlays) nil)))))

(defun %reindent (name)
  "How the parser answers an indent: with the lines it says should move, which
this puts in. Not a place, so it goes back to whoever asked."
  (lambda (targets) (ns:write (%indented name targets))))

;;;; The parse, off a watch. A buffer's parser owns a TSParser, which may not be
;;;; touched from two threads, so it stays an actor of its own. It is told and
;;;; never asked, and it answers by writing /buf/?name/face. What starts one is
;;;; the lines moving, and what bounds it is the range some window said it was
;;;; showing.

(defun %parser-at (name)
  "Where NAME's parser is declared. A parser holds a TSParser, so /proc is where
it lives and stopping it is a write."
  (p:child (p:parse "/proc") (format nil "parse:~a" name)))

(defun %link-of (name)
  (let ((entry (ns:read (%parser-at name))))
    (when (fset:map? entry)
      (let ((it (fset:lookup entry :parser)))
        (and (pine.ts.parser:parse-link-p it) it)))))

(defun %where ()
  "The actor system and tree-sitter runtime a parser starts on, as
(SYSTEM . RUNTIME). Asked of this image's server every time: a grammar is loaded
into the runtime that asked for it, and a parser started against a runtime that
is no longer the server's would look for a grammar in a table nothing writes."
  (let ((server pine.core.server:*server*))
    (when server
      (cons (pine.core.server:actor-system server)
            (pine.core.server:ts-runtime server)))))

(defun %link (name system runtime)
  "NAME's parser, started if it names a grammar and has none. Declared at /proc,
so two threads asking at once settle the way every other declaration does and
the loser's actor is stopped rather than left holding a thread nobody sends to."
  (or (%link-of name)
      (let* ((mode (ns:read (at name :mode)))
             (grammar (and mode (pine.mode:setting mode :grammar))))
        (when (and grammar system runtime)
          (let ((mine (pine.ts.parser:start-parser system runtime grammar name)))
            (when mine
              (ns:write (%parser-at name) (fset:map (:parser mine)))
              (let ((landed (%link-of name)))
                (unless (eq landed mine)
                  (sento.actor:tell (pine.ts.parser:link-actor mine) '(:stop)))
                landed)))))))

(defun %for (name key lines)
  "What was asked about NAME under KEY, or NIL when it describes some other
lines. An edit descriptor and an indent request are said just before the lines
they belong to move, and they carry those lines, so identity is the whole check."
  (let ((about (asked name key)))
    (when (and about (eq (fset:lookup about :lines) lines))
      about)))

(defun tree-of (name)
  "NAME's parse tree, or NIL when it has no parser."
  (pine.ts.parser:tree-of name))

(defun parser-of (name)
  "The actor parsing NAME, or NIL."
  (let ((link (%link-of name)))
    (and link (pine.ts.parser:link-actor link))))

(defun band-of (name)
  "The range a window said it is showing NAME over, as the parser wants it."
  (b:band (asked name :viewport)))

(defun %tell-parse (name actor)
  "Tell ACTOR the lines as they stand, over the range a window said it is
showing. Whoever edited said what it did just before the lines moved, so the
parser can shift its tree instead of rebuilding it, and an indent someone asked
for goes into the same mailbox behind the parse it needs."
  (let* ((lines (or (ns:held (at name :text)) (fset:seq "")))
         (band (band-of name))
         (edit (%for name :edit lines))
         (indent (%for name :indent-request lines)))
    (setf (asked name :told)
          (list (fset:size lines) band (and edit t) ns:*space*))
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

(defun %parse (name &optional system runtime)
  "Parse NAME, starting its parser when it names a grammar and has none.
Everything that wants a parse comes through here, so a parser is made when one
is needed rather than only when a watch happened to fire."
  (let ((where (%where)))
    (let ((link (%link name (or system (car where)) (or runtime (cdr where)))))
      (when link (%tell-parse name (pine.ts.parser:link-actor link))))))

(defun showing (name band)
  "Say which lines a window is showing NAME over, as [from to]. Highlights exist
to be painted, so what is painted is what bounds the parse: a background buffer
of a million lines costs nothing.

A window saying what it shows always asks for a parse, whether or not the range
moved, because the range is remembered here rather than under the buffer."
  (setf (asked name :viewport) band)
  (%parse name)
  band)

(defun motion (name kind)
  "Ask NAME's parser for a structural target of KIND from where point is. It
answers by writing /buf/?name/point, so nothing waits here."
  (%parse name)
  (let ((actor (parser-of name)))
    (when actor
      (multiple-value-bind (line col) (%point name)
        (sento.actor:tell actor
                          (list :motion :name name :kind kind :line line :col col
                                :lines (or (ns:held (at name :text)) (fset:seq ""))
                                :viewport (band-of name)
                                :space ns:*space*))))))

(defun indent (name &optional from to)
  "Indent the lines FROM through TO, defaulting to the line point is on. With a
parser, it is asked for every target at once and applies what it says when it
answers; without one, each line takes its previous line's indent."
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

;;;; Reading a range, which is the only reason the whole file is ever walked.

(defun %span (segment)
  "FROM..TO, when a segment names a range rather than one line."
  (let ((dots (search ".." segment)))
    (when dots
      (let ((from (parse-integer segment :end dots :junk-allowed t))
            (to (parse-integer segment :start (+ dots 2) :junk-allowed t)))
        (when (and from to) (cons from to))))))

(defun %put-line (name n text)
  "Replace line N. One line for one line, so the parser shifts by the bytes that
changed."
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

;;;; Going away.

(defun %forget-leaves (name)
  "Everything under NAME that nobody asked to keep. What was kept stays: where
point was left and what file the buffer visited outlive the buffer, which is
what makes reopening a file land where it was."
  (let ((held (ns:held (at name))))
    (when (fset:map? held)
      (let ((gone (fset:empty-map)))
        (fset:do-map (key value held)
          (declare (ignore value))
          (unless (ns:setting (at name (p:name key)) :keep)
            (setf gone (fset:with gone key nil))))
        (ns:write (at name) gone)))))

(defun drop (name)
  "Stop parsing NAME and forget what was asked about it."
  (let ((actor (parser-of name)))
    (ns:write (%parser-at name) nil)
    (forget name)
    actor))

(defun kill (name)
  "NAME goes away: its parser stops and every leaf nobody kept goes with it."
  (prog1 (drop name)
    (%forget-leaves name)))

;;;; A tool buffer is a mode with a :view: a function of the buffer answering a
;;;; widget tree. The tree is written here as an expression, so what it read is
;;;; recorded and it is computed again when any of it moves.

(defun %view (name)
  "Write NAME's view, if its mode has one. A buffer showing a view takes the
minor mode that moves between its rows, so Return and the arrows mean what they
mean in a tool buffer whatever its major mode is.

A mode with no view of its own says nothing about the buffer's, so what clears a
view is writing one."
  (let* ((mode (ns:read (at name :mode)))
         (fn (and mode (pine.mode:setting mode :view))))
    (when fn
      (ns:write (at name :view) (funcall fn name) :keep nil)
      (ns:write (at name :minor) (fset:seq :conj :view)))))

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
   (/buf/?name/file
    {:doc "the file it visits; write another to visit it"})
   (/buf/?name/mark
    {:doc "[line col] where the region starts, or nothing"})
   (/buf/?name/mode
    {:doc "the mode it is in; write another to change it"})
   (/buf/?name/minor
    {:doc "the minor modes on it, as a set; [:conj M] [:disj M]"})
   (/buf/?name/modified
    {:doc "whether it has changed since it was read or saved"})
   (/buf/?name/view
    {:doc "a widget tree, when it is a tool buffer rather than text"})
   (/buf/?name/tree
    {:read (pine.data:fn [] (tree-of name))
     :doc "the parse tree"})
   (/buf/?name/selection
    {:doc "the path the selected row names, in a tool buffer"})
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

;;;; What a caller holding a name does with it.

(defgeneric name-of (x)
  (:documentation "The buffer X names: a name, a path to one, or a value read
out of one. :CURRENT is what /buf/current says and :FOCUSED is what the focused
window shows, so a caller holding neither still has a way to say which buffer it
means.")
  (:method ((x null)) nil)
  (:method ((x string)) x)
  (:method ((x p:path)) (p:leaf x))
  (:method ((x b:snapshot)) (b:name x))
  (:method ((x b:buffer-state)) (fset:lookup (b:meta x) :name))
  (:method ((x symbol))
    (case x
      (:current (name-of (ns:held /buf/current)))
      (:focused (name-of (pine.win:buf-of (pine.win:focused))))
      (t (error "No buffer is named ~s; use a name, :current or :focused." x)))))

(defun live (x)
  "X's name when that buffer is here, else NIL."
  (let ((name (name-of x)))
    (when (and name (ns:held (at name :text))) name)))

(defun state-of (x)
  (let ((name (name-of x))) (when name (state name))))

(defun snapshot-of (x)
  (let ((name (name-of x))) (when name (b:state->snapshot (state name)))))

(defun text-of (x)
  (let ((name (name-of x))) (when name (b:state->string (state name)))))

(defun local (x key &optional default)
  (let ((name (name-of x)))
    (or (and name (ns:read (at name key))) default)))

(defun put (x key value)
  (let ((name (name-of x))) (when name (ns:write (at name key) value))))

(defun edit (x verb)
  (let ((name (name-of x))) (when name (ns:write (at name :text) verb))))

(defun put-point (x line col)
  (put x :point (fset:seq line col)))

(defun move (x unit n)
  (let ((name (name-of x)))
    (when name (ns:write (at name :point) (fset:seq :move unit n)))))

(defun delete-back (x)
  (let ((name (name-of x)))
    (when name
      (multiple-value-bind (line col) (point name)
        (cond ((plusp col)
               (edit name (fset:seq :delete (fset:seq line (1- col))
                                    (fset:seq line col))))
              ((plusp line)
               (let ((lines (ns:held (at name :text))))
                 (edit name (fset:seq :delete
                                      (fset:seq (1- line)
                                                (length (fset:@ lines (1- line))))
                                      (fset:seq line 0))))))))))

(defun current () (%current))

(defun current-name () (name-of :current))

(defun move-chars (n)
  (let ((name (current-name))) (when name (move name :char n))))

(defun move-lines (n)
  (let ((name (current-name))) (when name (move name :line n))))

(defun move-words (n)
  (let ((name (current-name))) (when name (move name :word n))))

(defun move-sexp (kind)
  (let ((name (current-name))) (when name (motion name kind))))

(defun focused-snap ()
  (let ((name (current-name))) (when name (snapshot-of name))))

(defun save-current ()
  (let ((name (current-name)))
    (if (and name (ns:read (at name :file)))
        (progn (%save name)
               (record-place name)
               (ns:write /recent (ns:read (at name :file)) :max 100 :keep t)
               (ns:write /echo/hint (format nil "wrote ~a" (ns:read (at name :file)))))
        (ns:write /echo/hint "no file path for this buffer"))))

;;;; The server

(defclass server (ns:server) ()
  (:default-initargs :name :buf :serves (list /buf) :after (list :mode :view))
  (:documentation "Buffers. After :MODE and :VIEW because a buffer whose mode
declares a view gets one on every mode change, and that asks the mode what it
declared and needs the minor mode that moves between its rows."))

(defmethod ns:raise ((s server) &key system runtime &allow-other-keys)
  "Serve /buf. With SYSTEM and RUNTIME a buffer that names a grammar is parsed
whenever its text or its mode moves. The watches take neither: where a parser
starts is this image's server, asked when the parse is wanted."
  (ns:write /buf (provider))
  (when (and system runtime)
    (ns:watch /buf/*/text
              (pine.data:fn [v]
                (declare (ignore v))
                (%parse (p:leaf (p:parent (ns:here))))
                {})
              :as :buf-parse)
    (ns:watch /buf/*/mode
              (pine.data:fn [v]
                (declare (ignore v))
                (%parse (p:leaf (p:parent (ns:here))))
                {})
              :as :buf-mode))
  ;; a buffer's file is stored and its text is not, so the text follows the
  ;; file: this is what a buffer coming back at boot is
  (ns:watch /buf/*/file
            (pine.data:fn [file]
              (let ((name (p:leaf (p:parent (ns:here)))))
                (if (and file (null (ns:held (at name :text))))
                    (%read-in name file)
                    {})))
            :as :buf-visit)
  ;; a mode with a :view makes the buffer a tool buffer
  (ns:watch /buf/*/mode
            (pine.data:fn [v]
              (declare (ignore v))
              (%view (p:leaf (p:parent (ns:here))))
              {})
            :as :buf-view)
  nil)

(defmethod ns:lower ((s server))
  (ns:watch /buf/*/text nil :as :buf-parse)
  (ns:watch /buf/*/mode nil :as :buf-mode)
  (ns:watch /buf/*/mode nil :as :buf-view)
  (ns:watch /buf/*/file nil :as :buf-visit)
  ;; the parsers come down with /proc, which is what declared them
  (call-next-method))

(ns:register (make-instance 'server))
