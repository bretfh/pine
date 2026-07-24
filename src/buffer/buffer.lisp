(in-package :pine.buffer)

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
  (let ((ls (lines state)))
    (with-output-to-string (s)
      (loop for i from 0 below (fset:size ls)
            do (when (plusp i) (write-char #\Newline s))
               (write-string (fset:@ ls i) s)))))

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

(defgeneric insert-string (state line-idx col string))

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

(defmethod insert-string ((state buffer-state) line-idx col string)
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

(defgeneric insert-char (state line-idx col char))

(defmethod insert-char ((state buffer-state) line-idx col char)
  (insert-string state line-idx col (string char)))

(defgeneric insert-newline (state line-idx col))

(defmethod insert-newline ((state buffer-state) line-idx col)
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

(defgeneric delete-char (state line-idx col))

(defmethod delete-char ((state buffer-state) line-idx col)
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

(defgeneric delete-region (state start-line start-col end-line end-col))

(defmethod delete-region ((state buffer-state) start-line start-col end-line end-col)
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

(defgeneric move-mark (state mark-name line-idx col))

(defmethod move-mark ((state buffer-state) mark-name line-idx col)
  (let ((key-line (intern (format nil "~a-LINE" mark-name) :keyword))
        (key-col (intern (format nil "~a-CHARPOS" mark-name) :keyword)))
    (copy-state state
                :marks (fset:with (fset:with (marks state) key-line line-idx) key-col col))))

(defgeneric set-meta (state key value))

(defmethod set-meta ((state buffer-state) key value)
  (copy-state state :meta (fset:with (meta state) key value)))

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
         (mode (pine.mode:find-mode mode-name)))
    (and mode (typep mode 'pine.mode:major-mode) (pine.mode:ts-language mode))))

(defun %ts-runtime ()
  (when pine.server:*server* (pine.server:ts-runtime pine.server:*server*)))

(defun refresh-highlights (pstate new-state)
  "Reparse PSTATE to NEW-STATE's text (creating the parse-state if the buffer has
a tree-sitter language and none exists yet) and return (values highlights
pstate). No language available -> (values nil pstate), leaving highlights off."
  (let ((lang (%state-language new-state))
        (rt (%ts-runtime)))
    (if (or (null lang) (null rt))
        (values nil pstate)
        (let ((ps (or pstate (pine.ts:make-parse-state rt lang))))
          (if (null ps)
              (values nil nil)
              (let ((new-text (state->string new-state)))
                (pine.ts:reparse! ps new-text)
                (values (pine.ts:parse-highlights ps new-text) ps)))))))

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

(defun make-buffer-actor (system name &key (content ""))
  (let ((initial (move-mark (set-meta (load-content content) :name name) :point 0 0)))
    (sento.actor-context:actor-of system
                                  :name (format nil "buffer:~a" name)
                                  ;; state tuple: (STATE UNDO REDO SUBSCRIBERS HIGHLIGHTS PARSE-STATE)
                                  :state (list initial nil nil nil nil nil)
                                  :receive
                                  (lambda (msg)
                                    (let* ((state (first sento.actor:*state*))
                                           (mode-name (buffer-local state :mode :base-mode))
                                           (mode (or (pine.mode:find-mode mode-name)
                                                     (pine.mode:find-mode :base-mode))))
                                      ;; The buffer runs off the session thread, so
                                      ;; an edit error opens the *debugger* restart
                                      ;; menu and parks THIS thread until resolved.
                                      ;; *state* is committed as each handler's last
                                      ;; step, so an error before the commit leaves
                                      ;; the prior buffer intact; `abort' drops the
                                      ;; edit and the actor keeps receiving.
                                      (pine.eval:with-debugger
                                          (:label (format nil "buffer ~a <- ~a" name (first msg)))
                                        ;; RETRY re-runs the edit after a live fix
                                        ;; (fix the failing defun in this image,
                                        ;; then pick retry); ABORT (from
                                        ;; with-debugger) drops it and keeps the
                                        ;; prior buffer.
                                        (loop
                                          (with-simple-restart (retry "Retry this edit")
                                            (return
                                              (pine.mode:dispatch-message
                                               mode sento.actor:*self*
                                               (first msg) (rest msg)))))))))))

(defun notify-subscribers (subscribers state &optional hl)
  (let ((snap (state->snapshot-with-hl state hl)))
    (dolist (ref subscribers)
      (handler-case (sento.actor:tell ref (list :snapshot :snapshot snap))
        (error () nil)))))


;;;; Buffer Registry

(defun buffer-table (srv)
  (or (pine.server:buffer-table srv)
      (setf (pine.server:buffer-table srv) (make-hash-table :test 'equal))))

(defun start-buffer-registry (server)
  (let ((sys (pine.server:actor-system server)))
    (setf (pine.server:buffer-registry server)
          (sento.actor-context:actor-of sys
                                        :name "buffer-registry"
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

(defun make-buffer (name &key (content ""))
  (let* ((cli (pine.client:current-client))
         (srv (pine.client:server-of cli))
         (table (buffer-table srv))
         (existing (gethash name table)))
    (when existing (return-from make-buffer existing))
    (let ((actor (make-buffer-actor (pine.server:actor-system srv) name :content content)))
      (setf (gethash name table) actor)
      (sento.actor:tell (pine.server:buffer-registry srv)
                        (list :register :name name :actor actor))
      (when (null (pine.client:current-buffer cli))
        (setf (pine.client:current-buffer cli) actor))
      actor)))

(defun kill-buffer (name)
  (let* ((cli (pine.client:current-client))
         (srv (pine.client:server-of cli))
         (table (buffer-table srv))
         (actor (gethash name table)))
    (when actor
      (when (eq actor (pine.client:current-buffer cli))
        (setf (pine.client:current-buffer cli) nil))
      (remhash name table)
      (sento.actor-context:stop (pine.server:actor-system srv) actor)
      (sento.actor:tell (pine.server:buffer-registry srv)
                        (list :unregister :name name)))))

(defun switch-buffer (name)
  (let* ((cli (pine.client:current-client))
         (srv (pine.client:server-of cli))
         (actor (gethash name (buffer-table srv))))
    (when actor
      (setf (pine.client:current-buffer cli) actor)
      actor)))


(defun list-buffers ()
  (let ((srv (pine.client:server-of (pine.client:current-client))))
    (loop for k being the hash-keys of (buffer-table srv) collect k)))

(defun buffer-count ()
  (let ((srv (pine.client:server-of (pine.client:current-client))))
    (hash-table-count (buffer-table srv))))

(defun current-buffer-text ()
  (let* ((cli (pine.client:current-client))
         (buf (pine.client:current-buffer cli)))
    (when buf
      (sento.actor:ask-s buf '(:get-text) :time-out 5))))

(defun current-buffer-snapshot ()
  (let* ((cli (pine.client:current-client))
         (buf (pine.client:current-buffer cli)))
    (when buf
      (sento.actor:ask-s buf '(:get-snapshot) :time-out 5))))


(defun buffer (x)
  "Coerce X to a buffer actor.
- nil            -> nil
- string         -> lookup by name in current server's buffer-table
- :current       -> current client's current-buffer
- :focused       -> focused window's buffer-ref
- actor ref      -> passthrough
Unknown keywords error; nothing silently falls through."
  (cond
    ((null x) nil)
    ((stringp x)
     (let ((srv (pine.client:server-of (pine.client:current-client))))
       (gethash x (buffer-table srv))))
    ((eq x :current)
     (pine.client:current-buffer (pine.client:current-client)))
    ((eq x :focused)
     (let ((w (pine.client:focused-window (pine.client:current-client))))
       (when w (buffer-ref w))))
    ((keywordp x)
     (error "unknown buffer target ~s; use :current, :focused, or a string name"
            x))
    (t x)))

(defun tell (target tag &rest plist)
  "Send (tag . plist) to TARGET (coerced via BUFFER). Returns TARGET.
Silently no-ops on nil target."
  (let ((buf (buffer target)))
    (when buf
      (sento.actor:tell buf (list* tag plist)))
    buf))

(defparameter +server-verbs+
  '(:buffers :clients :modes :commands :faces :actor-system :describe))

(defparameter +client-verbs+
  '(:current-buffer :focused-window :windows :kill-ring :last-command
    :pending-keys :describe))

(defparameter +buffer-verbs+
  '(:state :snapshot :text :meta :name :mode :pathname :point :line :local
    :describe))

(defun %ask-server (spec)
  (let ((srv (pine.client:server-of (pine.client:current-client)))
        (query (first spec)))
    (case query
      (:buffers     (loop for k being the hash-keys of (buffer-table srv)
                          collect k))
      (:clients     (pine.server:clients srv))
      (:modes       (loop for k being the hash-keys of (pine.server:modes srv)
                          collect k))
      (:commands    (sort (loop for k being the hash-keys
                                  of (pine.server:commands srv)
                                collect k)
                          #'string<))
      (:faces       (pine.server:faces srv))
      (:actor-system (pine.server:actor-system srv))
      (:describe    +server-verbs+)
      (t (error "unknown :server query ~s; known: ~s" query +server-verbs+)))))

(defun %ask-client (spec)
  (let ((cli (pine.client:current-client))
        (query (first spec)))
    (case query
      (:current-buffer (pine.client:current-buffer cli))
      (:focused-window (pine.client:focused-window cli))
      (:windows        (pine.client:windows cli))
      (:kill-ring      (pine.client:kill-ring cli))
      (:last-command   (pine.client:last-command cli))
      (:pending-keys   (pine.client:pending-keys cli))
      (:describe       +client-verbs+)
      (t (error "unknown :client query ~s; known: ~s" query +client-verbs+)))))

(defun %ask-buffer (buf spec)
  (when buf
    (let ((query (first spec))
          (args  (rest spec))
          (timeout 5))
      (case query
        (:state    (sento.actor:ask-s buf '(:get-state) :time-out timeout))
        (:snapshot (sento.actor:ask-s buf '(:get-snapshot) :time-out timeout))
        (:text     (sento.actor:ask-s buf '(:get-text) :time-out timeout))
        (:meta     (meta (sento.actor:ask-s buf '(:get-state)
                                            :time-out timeout)))
        (:name     (name (sento.actor:ask-s buf '(:get-snapshot)
                                            :time-out timeout)))
        (:mode     (buffer-local
                    (sento.actor:ask-s buf '(:get-state) :time-out timeout)
                    :mode))
        (:pathname (buffer-local
                    (sento.actor:ask-s buf '(:get-state) :time-out timeout)
                    :pathname))
        (:point    (let ((s (sento.actor:ask-s buf '(:get-snapshot)
                                               :time-out timeout)))
                     (values (point-line s) (point-col s))))
        (:line     (let* ((n (first args))
                          (s (sento.actor:ask-s buf '(:get-snapshot)
                                                :time-out timeout)))
                     (fset:@ (lines s) n)))
        (:local    (let ((key (first args))
                         (default (getf (rest args) :default)))
                     (buffer-local
                      (sento.actor:ask-s buf '(:get-state) :time-out timeout)
                      key default)))
        (:describe +buffer-verbs+)
        (t (error "unknown buffer query ~s; known: ~s" query +buffer-verbs+))))))

(defun ask (target &rest spec)
  "Synchronous query. TARGET is :server, :client, or anything coercible
via BUFFER. SPEC is (query &rest args). Use (ask TARGET :describe) for
the verb list."
  (case target
    (:server (%ask-server spec))
    (:client (%ask-client spec))
    (t (%ask-buffer (buffer target) spec))))
