(in-package :pine.editor)

(defun completion ()
  (pine.client:completion-state (pine.client:current-client)))

(defun filter-candidates (input candidates)
  "Match and rank CANDIDATES against INPUT through the completion engine
(orderless: space-separated components, any order), tightest first. Returns
candidate OBJECTS; consumers read candidate-string / candidate-value."
  (complete input candidates))

(defun completing-read (prompt-text candidates cb &key history)
  (let ((c (completion)))
    (setf (pine.client:active-p c) t
          (pine.client:candidates c) candidates
          (pine.client:input c) ""
          (pine.client:index c) (if candidates 0 -1)
          (pine.client:callback c) cb
          (pine.client:filtered c) (filter-candidates "" candidates)
          (pine.client:prompt c) prompt-text)
    (show-completions)
    (setf (pine.client:prompt-history (pine.client:current-client)) history)
    (activate-minibuffer (pine.client:current-client) prompt-text)))

(defun completion-cleanup ()
  "Reset the completion state. The minibuffer buffer + focus are restored
separately by DEACTIVATE-MINIBUFFER."
  (let ((c (completion)))
    (setf (pine.client:active-p c) nil
          (pine.client:candidates c) nil
          (pine.client:filtered c) nil
          (pine.client:index c) -1
          (pine.client:input c) ""
          (pine.client:callback c) nil
          (pine.client:dynamic-fn c) nil
          (pine.client:popup-rows c) nil
          (pine.client:popup-tree c) nil)))

(defun completion-next ()
  (let ((c (completion)))
    (when (pine.client:filtered c)
      (setf (pine.client:index c)
            (min (1+ (pine.client:index c)) (1- (length (pine.client:filtered c)))))
      (show-completions))))

(defun completion-prev ()
  (let ((c (completion)))
    (when (pine.client:filtered c)
      (setf (pine.client:index c) (max 0 (1- (pine.client:index c))))
      (show-completions))))

(defun completion-update-input (text)
  (let* ((c (completion))
         (cands (if (pine.client:dynamic-fn c)
                    (mapcar #'to-candidate (funcall (pine.client:dynamic-fn c) text))
                    (filter-candidates text (pine.client:candidates c)))))
    (setf (pine.client:input c) text
          (pine.client:filtered c) cands
          (pine.client:index c) (if cands 0 -1))
    (show-completions)))

(defun show-completions ()
  "Render the candidate popup: a visible window of the ranked set around the
selection, as styled rows + the arranged tree, stashed on the completion state
for render-chrome to blit above the echo row."
  (let* ((c (completion))
         (max-visible 12)
         (filtered (pine.client:filtered c))
         (n (length filtered))
         (idx (max 0 (pine.client:index c)))
         (off (pine.layout:scroll-to-selection idx 0 max-visible))
         (visible (subseq filtered (min off n) (min (+ off max-visible) n)))
         (cols (pine.buffer:frame-cols
                (pine.client:frame (pine.client:current-client)))))
    (multiple-value-bind (rows tree)
        (pine.layout:render (completion-popup visible) (max 10 cols)
                            :selection (and visible (- idx off)))
      (setf (pine.client:popup-rows c) rows
            (pine.client:popup-tree c) tree))))

(defun completing-read-active-p ()
  (let ((c pine.client:*client*))
    (and c (pine.client:active-p (pine.client:completion-state c)))))

(defun file-completion-active-p ()
  (and (completing-read-active-p)
       (pine.client:dynamic-fn (completion))))


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
  (let* ((buf (cur-buffer))
         (path (and buf (ignore-errors
                          (pine.buffer:buffer-local
                           (pine.ask:ask buf :state) :pathname nil)))))
    (if path (directory-namestring path) (namestring (uiop:getcwd)))))

(defun read-file-name (prompt-text cb &key history)
  (let ((c (completion)) (initial (default-directory)))
    (setf (pine.client:active-p c) t
          (pine.client:candidates c) nil
          (pine.client:dynamic-fn c) #'file-name-completions
          (pine.client:callback c) cb
          (pine.client:prompt c) prompt-text)
    (completion-update-input initial)
    (setf (pine.client:prompt-history (pine.client:current-client)) history)
    (activate-minibuffer (pine.client:current-client) prompt-text :initial initial)))

(defun file-name-complete ()
  "Tab: extend the path by the entries' common prefix; a lone match completes
fully (directories keep their trailing slash so the next Tab descends)."
  (let ((cands (mapcar #'candidate-string (pine.client:filtered (completion)))))
    (multiple-value-bind (dir base) (split-path (expand-tilde (pine.client:input (completion))))
      (declare (ignore base))
      (when cands
        (let ((add (if (= 1 (length cands)) (first cands) (longest-common-prefix cands))))
          (minibuffer-set-text (concatenate 'string dir add)))))))

(defun file-name-accept ()
  "Return: a highlighted entry (when the typed path is not itself a file) is
taken; a directory is descended into, a file is opened."
  (let* ((c (completion))
         (typed (expand-tilde (pine.client:input c)))
         (sel (let ((i (pine.client:index c)) (f (pine.client:filtered c)))
                (when (and (>= i 0) (< i (length f)))
                  (candidate-string (nth i f)))))
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
        (let ((cb (pine.client:callback c)))
          (%push-prompt-history (pine.client:current-client) target)
          (completion-cleanup)
          (deactivate-minibuffer (pine.client:current-client))
          (when cb (%safe-call cb target))))))


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
  (or (pine.client:minibuffer-buffer client)
      (let* ((srv (pine.client:server-of client))
             (sys (pine.core.server:actor-system srv))
             (buf (pine.buffer:make-buffer-actor
                   sys (format nil "*minibuffer*-~a" (gensym "MB"))))
             (ctrl (sento.actor-context:actor-of sys
                     :name (format nil "mb-ctrl-~a" (gensym))
                     :receive (lambda (msg)
                                (let ((pine.client:*client* client))
                                  (when (eq (first msg) :snapshot)
                                    (ignore-errors
                                     (minibuffer-changed
                                      client (getf (rest msg) :snapshot))))
                                  nil)))))
        (sento.actor:tell buf (list :subscribe :renderer ctrl))
        (setf (pine.client:minibuffer-buffer client) buf
              (pine.client:minibuffer-controller client) ctrl)
        buf)))

(defun minibuffer-active-p ()
  (let ((c (pine.client:current-client)))
    (and c (pine.client:prompt-active c))))

(defun %snap-line0 (snap)
  (if (and snap (plusp (pine.buffer:line-count snap)))
      (fset:@ (pine.buffer:lines snap) 0)
      ""))

(defun minibuffer-text ()
  "The current input, read synchronously from the buffer (accept path)."
  (let* ((c (pine.client:current-client))
         (mb (pine.client:minibuffer-buffer c)))
    (if mb (or (ignore-errors (sento.actor:ask-s mb '(:get-text) :time-out 5)) "") "")))

(defun minibuffer-set-text (text)
  "Replace the input with TEXT and put point at its end. Used by Tab completion
and file-name descent."
  (let* ((c (pine.client:current-client))
         (mb (pine.client:minibuffer-buffer c)))
    (when mb
      (sento.actor:tell mb (list :replace-content :content text))
      (sento.actor:tell mb (list :move-point :line 0 :col (length text))))))

(defun minibuffer-changed (client snap)
  "Controller callback: on each input edit, cache the snapshot, re-filter the
candidate list, and repaint."
  (setf (pine.client:minibuffer-snap client) snap)
  (when (pine.client:prompt-active client)
    (when (completing-read-active-p)
      (completion-update-input (%snap-line0 snap)))
    (let ((r (pine.client:renderer client)))
      (when r (sento.actor:tell r '(:force-render))))))

(defun activate-minibuffer (client prompt-text &key (initial ""))
  "Enter the minibuffer: make it the current buffer, enable minibuffer-mode, set
the initial input. The previous buffer is saved for restore. A prompt fired
while one is already active REPLACES it: the original saved-buffer is kept --
saving the minibuffer as its own return target would make the next accept/abort
\"restore\" current-buffer to the hidden minibuffer, where every keystroke then
vanishes (the frozen-buffer wedge)."
  (let ((mb (ensure-minibuffer client)))
    ;; current-buffer must be the minibuffer BEFORE enabling minibuffer-mode:
    ;; minor-mode enablement is keyed on the current buffer.
    (unless (pine.client:prompt-active client)
      (setf (pine.client:saved-buffer client) (pine.client:current-buffer client)))
    (setf (pine.client:current-buffer client) mb
          (pine.client:prompt-active client) t)
    (pine.client:set-buffer-mode mb :text-mode)
    (ignore-errors (pine.client:enable-minor-mode client :minibuffer-mode))
    (pine.echo:show-prompt prompt-text)
    (sento.actor:tell mb (list :replace-content :content initial))
    (sento.actor:tell mb (list :move-point :line 0 :col (length initial)))
    mb))

(defun deactivate-minibuffer (client)
  "Leave the minibuffer: restore the previous buffer and clear the prompt. Never
restore to the minibuffer itself -- fall back to the focused window's buffer."
  (ignore-errors (pine.client:disable-minor-mode client :minibuffer-mode))
  (let* ((mb (pine.client:minibuffer-buffer client))
         (back (pine.client:saved-buffer client)))
    (when (or (null back) (eq back mb))
      (let ((w (pine.client:focused-window client)))
        (setf back (and w (pine.buffer:buffer-ref w)))))
    (setf (pine.client:current-buffer client) back
          (pine.client:saved-buffer client) nil
          (pine.client:prompt-active client) nil
          (pine.client:minibuffer-snap client) nil
          (pine.client:prompt-history client) nil
          (pine.client:prompt-history-pos client) nil
          (pine.client:prompt-history-items client) nil))
  (pine.echo:hide-prompt)
  (let ((r (pine.client:renderer client)))
    (when r (sento.actor:tell r '(:force-render)))))

(defun %safe-call (fn arg)
  (when fn
    (handler-case (funcall fn arg)
      (error (e) (pine.echo:message (format nil "error: ~a" e))))))

;;;; Prompt history. A prompt opened with :history NAME reads and feeds the
;;;; store list NAME: accept pushes the input, M-p / M-n cycle it (the cycle
;;;; position and fetched items live on the client for the prompt's duration).

(defun %push-prompt-history (client input)
  (let ((h (pine.client:prompt-history client)))
    (when (and h (stringp input) (plusp (length input)))
      (pine.state.store:store-push h input :max 200))))

(defun minibuffer-history-prev ()
  "M-p: replace the input with the previous (older) history entry."
  (let* ((client (pine.client:current-client))
         (h (pine.client:prompt-history client)))
    (when h
      (unless (pine.client:prompt-history-items client)
        (setf (pine.client:prompt-history-items client)
              (pine.state.store:store-items h :limit 200)))
      (let* ((items (pine.client:prompt-history-items client))
             (pos (pine.client:prompt-history-pos client))
             (next (if pos (1+ pos) 0)))
        (when (< next (length items))
          (setf (pine.client:prompt-history-pos client) next)
          (minibuffer-set-text (nth next items)))))))

(defun minibuffer-history-next ()
  "M-n: replace the input with the next (newer) entry; past the newest, an
empty input leaves cycling."
  (let* ((client (pine.client:current-client))
         (items (pine.client:prompt-history-items client))
         (pos (pine.client:prompt-history-pos client)))
    (when (and items pos)
      (if (plusp pos)
          (progn
            (setf (pine.client:prompt-history-pos client) (1- pos))
            (minibuffer-set-text (nth (1- pos) items)))
          (progn
            (setf (pine.client:prompt-history-pos client) nil)
            (minibuffer-set-text ""))))))

;;;; Accept / abort / complete / candidate motion -- the minibuffer-mode command
;;;; bodies (the defcmd wrappers live in editor.lisp).

(defun minibuffer-accept ()
  (let* ((client (pine.client:current-client))
         (text (minibuffer-text)))
    (cond
      ((file-completion-active-p) (file-name-accept))
      ((completing-read-active-p)
       (let* ((c (completion))
              (result (if (and (>= (pine.client:index c) 0)
                               (< (pine.client:index c) (length (pine.client:filtered c))))
                          (candidate-string (nth (pine.client:index c)
                                                 (pine.client:filtered c)))
                          text))
              (cb (pine.client:callback c)))
         (%push-prompt-history client result)
         (completion-cleanup)
         (deactivate-minibuffer client)
         (%safe-call cb result)))
      ((pine.client:prompt-callback client)
       (let ((cb (pine.client:prompt-callback client)))
         (setf (pine.client:prompt-callback client) nil)
         (%push-prompt-history client text)
         (deactivate-minibuffer client)
         (%safe-call cb text)))
      (t (deactivate-minibuffer client)))))

(defun minibuffer-abort ()
  (let ((client (pine.client:current-client)))
    (when (completing-read-active-p) (completion-cleanup))
    (setf (pine.client:prompt-callback client) nil)
    (deactivate-minibuffer client)
    (pine.echo:message "quit")))

(defun minibuffer-complete ()
  (cond ((file-completion-active-p) (file-name-complete))
        ((completing-read-active-p)
         ;; insert the selected candidate as the input
         (let* ((c (completion))
                (i (pine.client:index c))
                (f (pine.client:filtered c)))
           (when (and (>= i 0) (< i (length f)))
             (minibuffer-set-text (candidate-string (nth i f))))))))


;;;; Raw-text prompt (no completion): eval-expression, new-buffer. It activates
;;;; the minibuffer with a callback; Return -> minibuffer-accept fires it.

(defun prompt (prompt-text cb &key history)
  (let ((client (pine.client:current-client)))
    (setf (pine.client:prompt-callback client) cb
          (pine.client:prompt-history client) history)
    (activate-minibuffer client prompt-text)))

(defun cancel-prompt ()
  (minibuffer-abort))
