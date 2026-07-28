(defpackage #:pine.editor.minibuffer
  (:use #:cl)
  (:local-nicknames (#:world #:pine.state.world) (#:ns #:pine.ns))
  (:export #:cancel-prompt #:completing-read #:completing-read-active-p
           #:completion-next #:completion-prev #:completion-update-input
           #:ensure-minibuffer #:file-completion-active-p #:file-name-accept
           #:file-name-complete #:minibuffer-abort #:minibuffer-accept
           #:minibuffer-active-p #:minibuffer-changed #:minibuffer-complete
           #:minibuffer-history-next #:minibuffer-history-prev
           #:minibuffer-set-text #:minibuffer-text #:prompt #:read-file-name
           #:mount #:said #:popup-rows #:input-snap))

(in-package #:pine.editor.minibuffer)
(named-readtables:in-readtable pine.path:syntax)

;;;; The prompt is /echo: a map saying what to prompt with, what completes it,
;;;; which history it feeds, and what to do when it is accepted. A command that
;;;; prompts holds no callback and is re-entrant, because it wrote a place
;;;; rather than parking a closure on a client.
;;;;
;;;; What the prompt and its completion UI say to each other -- the filtered
;;;; candidates, which one is selected, the rendered popup, where the history
;;;; cycle is -- is one side of a conversation, so it lives here.

;;;; What the prompt says about itself

(defun %prompt () (pine.editor.echo:prompt))

(defun %complete ()
  "What completes this prompt: a candidate list, :FILE, or nothing."
  (let ((p (%prompt)))
    (and p (fset:lookup p :complete))))

(defun completing-read-active-p ()
  (and (%complete) t))

(defun file-completion-active-p ()
  (eq :file (%complete)))

(defun popup-rows () (pine.editor.echo:popup-rows))
(defun input-snap () (pine.editor.echo:input-snap))

(defun filter-candidates (input candidates)
  "Match and rank CANDIDATES against INPUT through the completion engine
(orderless: space-separated components, any order), tightest first. Returns
candidate OBJECTS; consumers read candidate-string / candidate-value."
  (pine.editor.completion:complete input candidates))

(defun said (key &optional default) (pine.editor.echo:said key default))
(defun (setf said) (value key) (setf (pine.editor.echo:said key) value))

(defun %filtered () (said :filtered))
(defun %index () (said :index -1))

(defun completing-read (prompt-text candidates then &key history)
  "Prompt with PROMPT-TEXT over CANDIDATES. THEN is what happens when it is
accepted: a write-map, a command path, or a function of what was chosen."
  (ns:write /echo (fset:map (:prompt prompt-text)
                            (:complete candidates)
                            (:history history)
                            (:then then))))

(defun completion-next ()
  (when (%filtered)
    (setf (said :index) (min (1+ (%index)) (1- (length (%filtered)))))
    (show-completions)))

(defun completion-prev ()
  (when (%filtered)
    (setf (said :index) (max 0 (1- (%index))))
    (show-completions)))

(defun completion-update-input (text)
  (let ((cands (if (file-completion-active-p)
                   (mapcar #'pine.editor.completion:to-candidate
                           (file-name-completions text))
                   (filter-candidates text (%complete)))))
    (setf (said :input) text
          (said :filtered) cands
          (said :index) (if cands 0 -1))
    (show-completions)))

(defun show-completions ()
  "Render the candidate popup: a visible window of the ranked set around the
selection, as styled rows + the arranged tree, for render-chrome to blit above
the echo row."
  (let* ((max-visible 12)
         (filtered (%filtered))
         (n (length filtered))
         (idx (max 0 (%index)))
         (off (pine.ui.wire:scroll-to-selection idx 0 max-visible))
         (visible (subseq filtered (min off n) (min (+ off max-visible) n)))
         (cols (pine.text.window:frame-cols
                (pine.editor.frame:frame (pine.editor.frame:current-client)))))
    (multiple-value-bind (rows tree)
        (pine.ui.cells:render (pine.editor.completion:completion-popup visible)
                              (max 10 cols)
                              :selection (and visible (- idx off)))
      (setf (said :popup-rows) rows
            (said :popup-tree) tree))))

;;;; Filesystem path completion (find-file)

(defun expand-tilde (text)
  (cond
    ((string= text "~") (namestring (user-homedir-pathname)))
    ((and (>= (length text) 2) (string= (subseq text 0 2) "~/"))
     (concatenate 'string (namestring (user-homedir-pathname)) (subseq text 2)))
    (t text)))

(defun split-path (path)
  "Directory part (through the last slash) and the trailing basename."
  (let ((slash (position #\/ path :from-end t)))
    (if slash
        (values (subseq path 0 (1+ slash)) (subseq path (1+ slash)))
        (values "" path))))

(defun directory-entries (dir)
  "Names in DIR: subdirectories with a trailing slash, then files. nil if DIR
cannot be read."
  (handler-case
      (append
       (sort (mapcar (lambda (p)
                       (concatenate 'string (car (last (pathname-directory p))) "/"))
                     (uiop:subdirectories dir))
             #'string<)
       (sort (mapcar #'file-namestring (uiop:directory-files dir)) #'string<))
    (error () nil)))

(defun file-name-completions (text)
  "Directory entries matching the basename the user is typing."
  (multiple-value-bind (dir base) (split-path (expand-tilde text))
    (let ((entries (directory-entries (if (string= dir "") "./" dir))))
      (if (string= base "")
          entries
          (remove-if-not
           (lambda (n) (and (>= (length n) (length base))
                            (string= base (subseq n 0 (length base)))))
           entries)))))

(defun longest-common-prefix (strings)
  (if (null strings)
      ""
      (let ((p (first strings)))
        (dolist (s (rest strings) p)
          (let ((n (min (length p) (length s))))
            (setf p (subseq p 0 (or (mismatch p s :end1 n :end2 n) n))))))))

(defun default-directory ()
  (let* ((buf (pine.editor.motion:cur-buffer))
         (path (and buf (ignore-errors
                          (pine.text.buffer:buffer-local
                           (pine.editor.ask:ask buf :state) :pathname nil)))))
    (if path (directory-namestring path) (namestring (uiop:getcwd)))))


(defun read-file-name (prompt-text then &key history)
  (ns:write /echo (fset:map (:prompt prompt-text)
                            (:complete :file)
                            (:history history)
                            (:initial (default-directory))
                            (:then then))))

(defun file-name-complete ()
  "Tab: extend the path by the entries' common prefix; a lone match completes
fully (directories keep their trailing slash so the next Tab descends)."
  (let ((cands (mapcar #'pine.editor.completion:candidate-string (%filtered))))
    (multiple-value-bind (dir base) (split-path (expand-tilde (said :input "")))
      (declare (ignore base))
      (when cands
        (let ((add (if (= 1 (length cands)) (first cands) (longest-common-prefix cands))))
          (minibuffer-set-text (concatenate 'string dir add)))))))

(defun %selected ()
  "The candidate string the selection names, or NIL."
  (let ((i (%index)) (f (%filtered)))
    (when (and (>= i 0) (< i (length f)))
      (pine.editor.completion:candidate-string (nth i f)))))

(defun file-name-accept ()
  "Return: a highlighted entry (when the typed path is not itself a file) is
taken; a directory is descended into, a file is opened."
  (let* ((typed (expand-tilde (said :input "")))
         (sel (%selected))
         (target (multiple-value-bind (dir base) (split-path typed)
                   (declare (ignore base))
                   (if (and sel (not (uiop:file-exists-p typed))
                            (not (uiop:directory-exists-p typed)))
                       (concatenate 'string dir sel)
                       typed))))
    (if (uiop:directory-exists-p target)
        (minibuffer-set-text (if (and (plusp (length target))
                                      (char= (char target (1- (length target))) #\/))
                                 target
                                 (concatenate 'string target "/")))
        (%finish target))))

;;;; The minibuffer as a real buffer. While a prompt is active, *minibuffer* is
;;;; the current buffer, so every editing command -- motion, kill, yank, word
;;;; ops, isearch -- operates on the input through the ordinary dispatch, exactly
;;;; like any buffer. minibuffer-mode (a minor mode) adds only the completion and
;;;; exit keys. A controller subscribed to the buffer re-filters the candidate
;;;; list and repaints on every edit, so the completion UI stays live no matter
;;;; which command changed the input.

(defun ensure-minibuffer (client)
  "The client's *minibuffer* buffer + its controller, created once per client.
The actor name is unique per session -- a re-attached editor makes a fresh one
beside the old session's."
  (or (pine.editor.frame:minibuffer-buffer client)
      (let* ((srv (pine.editor.frame:server-of client))
             (sys (pine.core.server:actor-system srv))
             (buf (pine.text.buffer:make-buffer-actor
                   sys (format nil "*minibuffer*-~a" (gensym "MB"))))
             (ctrl (sento.actor-context:actor-of sys
                     :name (format nil "mb-ctrl-~a" (gensym))
                     :dispatcher :pinned
                     :receive (lambda (msg)
                                (let ((pine.editor.frame:*client* client))
                                  (when (eq (first msg) :snapshot)
                                    (ignore-errors
                                     (minibuffer-changed
                                      client (getf (rest msg) :snapshot))))
                                  nil)))))
        ;; the controller re-filters and repaints on each edit, so it watches
        ;; the buffer it is the controller of rather than being sent it
        (pine.ns:watch (pine.text.buffer:at (pine.text.buffer:name-of buf))
                       (lambda (value)
                         (declare (ignore value))
                         (sento.actor:tell
                          ctrl (list :snapshot
                                     :snapshot (pine.text.buffer:snapshot-of buf)))
                         (fset:empty-map))
                       :as (list :minibuffer client))
        (setf (pine.editor.frame:minibuffer-buffer client) buf
              (pine.editor.frame:minibuffer-controller client) ctrl)
        buf)))

(defun minibuffer-text ()
  "The current input, read synchronously from the buffer (accept path)."
  (let* ((c (pine.editor.frame:current-client))
         (mb (pine.editor.frame:minibuffer-buffer c)))
    (if mb (or (ignore-errors (pine.text.buffer:text-of mb)) "") "")))

(defun minibuffer-set-text (text)
  "Replace the input with TEXT and put point at its end. Used by Tab completion
and file-name descent."
  (let* ((c (pine.editor.frame:current-client))
         (mb (pine.editor.frame:minibuffer-buffer c)))
    (when mb
      (pine.ns:write (pine.text.buffer:at (pine.text.buffer:name-of mb) :text) text)
      (pine.text.buffer:put-point mb 0 (length text)))))


(defun minibuffer-changed (client snap)
  "The input moved: cache the snapshot, re-filter, repaint."
  (setf (said :snap) snap)
  (when (pine.editor.echo:prompt-active-p)
    (when (completing-read-active-p)
      (completion-update-input (%snap-line0 snap)))
    (let ((r (pine.editor.frame:renderer client)))
      (when r (sento.actor:tell r '(:force-render))))))

;;;; Opening and closing, off the path. Writing a prompt map to /echo opens it
;;;; and writing nothing closes it, so a config, a command and another image
;;;; all prompt the same way.

(defun %open (client)
  (let* ((p (%prompt))
         (initial (or (fset:lookup p :initial) ""))
         (mb (ensure-minibuffer client)))
    ;; current-buffer must be the minibuffer BEFORE the minor mode goes on:
    ;; which minor modes are on is keyed on the buffer that is current. A
    ;; prompt opened over a prompt keeps the first one's return buffer, or the
    ;; next accept would "restore" to the hidden minibuffer.
    (unless (said :back)
      (setf (said :back) (pine.editor.frame:current-buffer client)))
    (setf (pine.editor.frame:current-buffer client) mb)
    (pine.editor.frame:set-buffer-mode mb :text)
    (ignore-errors (pine.editor.frame:enable-minor-mode client :minibuffer))
    (pine.editor.echo:message "")
    (ns:write (pine.text.buffer:at (pine.text.buffer:name-of mb) :text) initial)
    (pine.text.buffer:put-point mb 0 (length initial))
    (when (completing-read-active-p) (completion-update-input initial))
    mb))

(defun %close (client)
  "Leave the minibuffer: back to the buffer that was current. Never back to the
minibuffer itself -- fall back to the focused window's buffer."
  (ignore-errors (pine.editor.frame:disable-minor-mode client :minibuffer))
  (let* ((mb (pine.editor.frame:minibuffer-buffer client))
         (back (said :back)))
    (when (or (null back) (eq back mb))
      (let ((w (pine.editor.frame:focused-window client)))
        (setf back (and w (pine.text.window:buffer-ref w)))))
    (pine.editor.echo:forget)
    (setf (pine.editor.frame:current-buffer client) back))
  (let ((r (pine.editor.frame:renderer client)))
    (when r (sento.actor:tell r '(:force-render)))))

(defun mount ()
  "Open and close the minibuffer as /echo says. The prompt is the place; this
is what watches it."
  (ns:watch /echo
            (pine.data:fn [v]
              (declare (ignore v))
              (when (and (fset:equal? (ns:here) /echo) pine.editor.frame:*client*)
                (if (pine.editor.echo:prompt-active-p)
                    (%open pine.editor.frame:*client*)
                    (%close pine.editor.frame:*client*)))
              {})
            :as :echo))

(defun minibuffer-active-p ()
  (pine.editor.echo:prompt-active-p))

(defun %snap-line0 (snap)
  (if (and snap (plusp (pine.text.buffer:line-count snap)))
      (fset:@ (pine.text.buffer:lines snap) 0)
      ""))

;;;; Prompt history. A prompt opened with :history NAME reads and feeds the
;;;; store list NAME: accept pushes the input, M-p / M-n cycle it.

(defun %history ()
  (let ((p (%prompt)))
    (and p (fset:lookup p :history))))

(defun %push-history (input)
  (let ((h (%history)))
    (when (and h (stringp input) (plusp (length input)))
      (world:push h input :max 200))))

(defun minibuffer-history-prev ()
  "M-p: replace the input with the previous (older) history entry."
  (let ((h (%history)))
    (when h
      (unless (said :history-items)
        (setf (said :history-items) (world:items h :limit 200)))
      (let* ((items (said :history-items))
             (pos (said :history-pos))
             (next (if pos (1+ pos) 0)))
        (when (< next (length items))
          (setf (said :history-pos) next)
          (minibuffer-set-text (nth next items)))))))

(defun minibuffer-history-next ()
  "M-n: replace the input with the next (newer) entry; past the newest, an
empty input leaves cycling."
  (let ((items (said :history-items))
        (pos (said :history-pos)))
    (when (and items pos)
      (if (plusp pos)
          (progn (setf (said :history-pos) (1- pos))
                 (minibuffer-set-text (nth (1- pos) items)))
          (progn (setf (said :history-pos) nil)
                 (minibuffer-set-text ""))))))

;;;; Accept, abort, complete

(defun %finish (result)
  "Take RESULT as the answer: it lands at /echo/result, the prompt's :then is
done, and the prompt goes. The result is written first, so a :then that reads
${(read /echo/result)} sees it."
  (let ((then (let ((p (%prompt))) (and p (fset:lookup p :then)))))
    (%push-history result)
    (ns:write /echo/result result)
    (ns:write /echo nil)
    (handler-case (cond ((null then) nil)
                        ((functionp then) (funcall then result))
                        (t (pine.cmd:run then)))
      (error (e) (pine.editor.echo:message (format nil "error: ~a" e))))))

(defun minibuffer-accept ()
  (cond
    ((file-completion-active-p) (file-name-accept))
    ((completing-read-active-p) (%finish (or (%selected) (minibuffer-text))))
    ((%prompt) (%finish (minibuffer-text)))
    (t (ns:write /echo nil))))

(defun minibuffer-abort ()
  (when (%prompt)
    (ns:write /echo nil)
    (pine.editor.echo:message "quit")))

(defun minibuffer-complete ()
  (cond ((file-completion-active-p) (file-name-complete))
        ((completing-read-active-p)
         (let ((sel (%selected)))
           (when sel (minibuffer-set-text sel))))))

;;;; A prompt with no completion: eval-expression, new-buffer.

(defun prompt (prompt-text then &key history)
  (ns:write /echo (fset:map (:prompt prompt-text)
                            (:history history)
                            (:then then))))

(defun cancel-prompt ()
  (minibuffer-abort))
