(defpackage #:pine.buf
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:b #:pine.text.buffer))
  (:export #:mount #:unmount #:state #:at #:names))

(in-package #:pine.buf)
(named-readtables:in-readtable pine.path:syntax)

;;;; Buffers, as paths. A buffer's content, its point, its locals and its marks
;;;; are places, so nothing has to be exported for something else to read them
;;;; and another image reaches them without an accessor written for the
;;;; occasion.
;;;;
;;;;   /buf/?name/lines   the seq of strings, held
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
;;;;   /buf/?name/text            the lines joined
;;;;   /buf/?name/line/?n         one line, or the range FROM..TO
;;;;   /buf/?name/face/${a}..${b} the runs a window's range needs
;;;;
;;;; The band that made a million lines work is not a policy in the parser: it
;;;; is what the window read.

(defun at (name &rest leaf)
  "NAME's path, or the leaf under it."
  (apply #'p:path /buf name leaf))

(defun names ()
  "Every buffer there is."
  (sort (mapcar (lambda (path) (p:leaf (p:parent path)))
                (pine.data:keys (ns:read /buf/*/lines {})))
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
    (let ((mark (ns:read (at name :mark))))
      (b:copy-state
       (b:make-empty-state name)
       :lines (or (ns:read (at name :lines)) (fset:seq ""))
       :marks (fset:map (:point-line line) (:point-charpos col))
       :meta (fset:map (:name name)
                       (:mode (ns:read (at name :mode)))
                       (:mark-line (and (fset:seq? mark) (fset:lookup mark 0)))
                       (:mark-col (and (fset:seq? mark) (fset:lookup mark 1))))
       :tick (or (ns:read (at name :tick)) 0)))))

(defun %landing (name state)
  "The write-map taking NAME's leaves to what STATE says. A value that did not
change does not move, so this is the whole of applying an edit."
  (let ((marks (b:marks state)))
    (fset:map ((at name :lines) (b:lines state))
              ((at name :point) (fset:seq (or (fset:lookup marks :point-line) 0)
                                          (or (fset:lookup marks :point-charpos) 0)))
              ((at name :tick) (b:tick state)))))

(defun %edit (name fn)
  "Apply FN, a state-to-state function, to NAME."
  (ns:write (%landing name (funcall fn (state name)))))

;;;; The verbs

(defun %insert (name text)
  (%edit name (lambda (s)
                (multiple-value-bind (line col) (%point name)
                  (b:insert-string s line col text)))))

(defun %newline (name)
  (%edit name (lambda (s)
                (multiple-value-bind (line col) (%point name)
                  (b:insert-newline s line col)))))

(defun %backspace (name)
  (multiple-value-bind (line col) (%point name)
    (cond
      ((plusp col)
       (%edit name (lambda (s)
                     (let ((deleted (b:delete-char s line (1- col))))
                       (b:move-mark deleted :point line (1- col))))))
      ((plusp line)
       (let* ((s (state name))
              (previous (length (b:line-at s (1- line)))))
         (ns:write (%landing name (b:move-mark (b:delete-char s (1- line) previous)
                                               :point (1- line) previous))))))))

(defun %delete (name from to)
  (%edit name (lambda (s)
                (b:delete-region s (fset:lookup from 0) (fset:lookup from 1)
                                 (fset:lookup to 0) (fset:lookup to 1)))))

(defun %move (name unit n)
  (let ((s (state name)))
    (multiple-value-bind (line col)
        (b:point-after-move (b:state->snapshot s) unit n)
      (ns:write (at name :point) (fset:seq line col)))))

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
                      (fset:lookup change :old))))))))

;;;; The file it visits

(defun %text (name)
  (let ((lines (ns:read (at name :lines))))
    (if (fset:seq? lines)
        (format nil "~{~a~^~%~}" (fset:convert 'list lines))
        "")))

(defun %save (name)
  (let ((file (ns:read (at name :file))))
    (when file
      (ns:write (p:path /file (p:spliced (p:text file))) (%text name))
      (ns:write (at name :modified) nil)
      t)))

(defun %visit (name file)
  "Read FILE into NAME. Its text is derived from the file, so it is not stored
and comes back by being read again."
  (ns:write (fset:map ((at name :file) file)
                      ((at name :lines)
                       (fset:convert 'fset:seq
                                     (b:split-lines
                                      (or (ns:read (p:path /file (p:spliced (p:text file))))
                                          ""))))
                      ((at name :point) (fset:seq 0 0)))))

;;;; Reading a range, which is the only reason the whole file is ever walked

(defun %span (segment)
  "FROM..TO, when a segment names a range rather than one line."
  (let ((dots (search ".." segment)))
    (when dots
      (let ((from (parse-integer segment :end dots :junk-allowed t))
            (to (parse-integer segment :start (+ dots 2) :junk-allowed t)))
        (when (and from to) (cons from to))))))

(defun %range (name from to)
  (let* ((lines (ns:read (at name :lines)))
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

;;;; The paths

(defun provider ()
  (ns:provider
   (/buf/?name/line/?which
    {:read (pine.data:fn []
             (let ((span (%span which)))
               (if span
                   (%range name (car span) (cdr span))
                   (let ((n (parse-integer which :junk-allowed t))
                         (lines (ns:read (at name :lines))))
                     (and n (fset:seq? lines) (fset:lookup lines n))))))
     :doc "one line, or the range FROM..TO a window is showing"})
   (/buf/?name/face/?which
    {:read (pine.data:fn []
             (let ((span (%span which)))
               (when span (%runs name (car span) (cdr span)))))
     :doc "the highlight runs a window's range needs"})
   (/buf/?name/text
    {:read (pine.data:fn [] (%text name))
     :write (pine.data:fn [v]
              (ns:write (fset:map ((at name :lines)
                                   (fset:convert 'fset:seq (b:split-lines v)))
                                  ((at name :point) (fset:seq 0 0)))))
     :verbs {:insert (pine.data:fn [text] (%insert name text))
             :newline (pine.data:fn [] (%newline name))
             :backspace (pine.data:fn [] (%backspace name))
             :delete (pine.data:fn [from to] (%delete name from to))
             :undo (pine.data:fn [] (%undo name))
             :save (pine.data:fn [] (%save name))
             :visit (pine.data:fn [file] (%visit name file))}
     :doc "the whole string; [:insert TEXT] [:newline] [:backspace] [:undo]"})
   (/buf/?name/point
    {:verbs {:move (pine.data:fn [unit n] (%move name unit n))}
     :doc "[line col]; [:move :word 1] to step by something"})
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
  "What NAME's KEY says about LINES, or NIL when it describes some other lines.

An edit descriptor and an indent request are written in the same transaction as
the lines they belong to, and they carry those lines, so identity is the whole
of the check: the same object means it is that very edit."
  (let ((asked (ns:read (at name key))))
    (when (and asked (eq (fset:lookup asked :lines) lines))
      asked)))

(defun %parse (name system runtime)
  "Tell NAME's parser the lines as they stand, over the range a window said it
is showing.

This is the only thing that drives a parse. Whoever edited wrote what it did at
/buf/?name/edit in the same transaction as the lines, so the parser can shift
its tree instead of rebuilding it, and an indent someone asked for goes into the
same mailbox behind the parse it needs."
  (let ((link (%link name system runtime)))
    (when link
      (let* ((actor (pine.ts.parser:link-actor link))
             (lines (or (ns:read (at name :lines)) (fset:seq "")))
             (band (pine.text.buffer:band (ns:read (at name :viewport))))
             (edit (%for name :edit lines))
             (indent (%for name :indent-request lines)))
        (sento.actor:tell
         actor
         (list :parse :name name :lines lines :viewport band
               :tick (or (ns:read (at name :tick)) 0)
               :edit (when edit
                       (list (fset:lookup edit :at) (fset:lookup edit :old)
                             (fset:lookup edit :new) (fset:lookup edit :bytes)))))
        (when indent
          (sento.actor:tell
           actor
           (list :indent :name name :lines lines :viewport band
                 :from (fset:lookup indent :from)
                 :to (fset:lookup indent :to))))))))

(defun mount (&key system runtime)
  "Serve /buf. With SYSTEM and RUNTIME a buffer that names a grammar is parsed
whenever its lines or its window's range move."
  (ns:write /buf (provider))
  (when (and system runtime)
    (ns:watch /buf/*/lines
              (pine.data:fn [v]
                (declare (ignore v))
                (%parse (p:leaf (p:parent (ns:here))) system runtime)
                {})
              :as :buf-parse)
    (ns:watch /buf/*/viewport
              (pine.data:fn [v]
                (declare (ignore v))
                (%parse (p:leaf (p:parent (ns:here))) system runtime)
                {})
              :as :buf-band)
    (ns:watch /buf/*/mode
              (pine.data:fn [v]
                (declare (ignore v))
                (%parse (p:leaf (p:parent (ns:here))) system runtime)
                {})
              :as :buf-mode))
  nil)

(defun unmount ()
  (ns:watch /buf/*/lines nil :as :buf-parse)
  (ns:watch /buf/*/viewport nil :as :buf-band)
  (ns:watch /buf/*/mode nil :as :buf-mode)
  (fset:do-map (name link (sento.atomic:atomic-get *parsers*))
    (declare (ignore name))
    (sento.actor:tell (pine.ts.parser:link-actor link) '(:stop)))
  (sento.atomic:atomic-swap *parsers* (lambda (m) (declare (ignore m))
                                        (fset:empty-map)))
  (ns:write /buf nil))
