(defpackage #:pine.editor.minibuffer
  (:use #:cl)
  (:export #:cancel-prompt #:completing-read #:completing-read-active-p #:completion #:completion-next #:completion-prev #:completion-update-input #:ensure-minibuffer #:file-completion-active-p #:file-name-accept #:file-name-complete #:minibuffer-abort #:minibuffer-accept #:minibuffer-active-p #:minibuffer-changed #:minibuffer-complete #:minibuffer-history-next #:minibuffer-history-prev #:minibuffer-set-text #:minibuffer-text #:prompt #:read-file-name))

(in-package #:pine.editor.minibuffer)

(defun completion ()
  (pine.editor.frame:completion-state (pine.editor.frame:current-client)))

(defun filter-candidates (input candidates)
  "Match and rank CANDIDATES against INPUT through the completion engine
(orderless: space-separated components, any order), tightest first. Returns
candidate OBJECTS; consumers read candidate-string / candidate-value."
  (pine.editor.completion:complete input candidates))

(defun completing-read (prompt-text candidates cb &key history)
  (let ((c (completion)))
    (setf (pine.editor.frame:active-p c) t
          (pine.editor.frame:candidates c) candidates
          (pine.editor.frame:input c) ""
          (pine.editor.frame:index c) (if candidates 0 -1)
          (pine.editor.frame:callback c) cb
          (pine.editor.frame:filtered c) (filter-candidates "" candidates)
          (pine.editor.frame:prompt c) prompt-text)
    (show-completions)
    (setf (pine.editor.frame:prompt-history (pine.editor.frame:current-client)) history)
    (activate-minibuffer (pine.editor.frame:current-client) prompt-text)))

(defun completion-cleanup ()
  "Reset the completion state. The minibuffer buffer + focus are restored
separately by DEACTIVATE-MINIBUFFER."
  (let ((c (completion)))
    (setf (pine.editor.frame:active-p c) nil
          (pine.editor.frame:candidates c) nil
          (pine.editor.frame:filtered c) nil
          (pine.editor.frame:index c) -1
          (pine.editor.frame:input c) ""
          (pine.editor.frame:callback c) nil
          (pine.editor.frame:dynamic-fn c) nil
          (pine.editor.frame:popup-rows c) nil
          (pine.editor.frame:popup-tree c) nil)))

(defun completion-next ()
  (let ((c (completion)))
    (when (pine.editor.frame:filtered c)
      (setf (pine.editor.frame:index c)
            (min (1+ (pine.editor.frame:index c)) (1- (length (pine.editor.frame:filtered c)))))
      (show-completions))))

(defun completion-prev ()
  (let ((c (completion)))
    (when (pine.editor.frame:filtered c)
      (setf (pine.editor.frame:index c) (max 0 (1- (pine.editor.frame:index c))))
      (show-completions))))

(defun completion-update-input (text)
  (let* ((c (completion))
         (cands (if (pine.editor.frame:dynamic-fn c)
                    (mapcar #'pine.editor.completion:to-candidate (funcall (pine.editor.frame:dynamic-fn c) text))
                    (filter-candidates text (pine.editor.frame:candidates c)))))
    (setf (pine.editor.frame:input c) text
          (pine.editor.frame:filtered c) cands
          (pine.editor.frame:index c) (if cands 0 -1))
    (show-completions)))

(defun show-completions ()
  "Render the candidate popup: a visible window of the ranked set around the
selection, as styled rows + the arranged tree, stashed on the completion state
for render-chrome to blit above the echo row."
  (let* ((c (completion))
         (max-visible 12)
         (filtered (pine.editor.frame:filtered c))
         (n (length filtered))
         (idx (max 0 (pine.editor.frame:index c)))
         (off (pine.ui.wire:scroll-to-selection idx 0 max-visible))
         (visible (subseq filtered (min off n) (min (+ off max-visible) n)))
         (cols (pine.text.window:frame-cols
                (pine.editor.frame:frame (pine.editor.frame:current-client)))))
    (multiple-value-bind (rows tree)
        (pine.ui.cells:render (pine.editor.completion:completion-popup visible) (max 10 cols)
                            :selection (and visible (- idx off)))
      (setf (pine.editor.frame:popup-rows c) rows
            (pine.editor.frame:popup-tree c) tree))))

(defun completing-read-active-p ()
  (let ((c pine.editor.frame:*client*))
    (and c (pine.editor.frame:active-p (pine.editor.frame:completion-state c)))))

(defun file-completion-active-p ()
  (and (completing-read-active-p)
       (pine.editor.frame:dynamic-fn (completion))))


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

(defun read-file-name (prompt-text cb &key history)
  (let ((c (completion)) (initial (default-directory)))
    (setf (pine.editor.frame:active-p c) t
          (pine.editor.frame:candidates c) nil
          (pine.editor.frame:dynamic-fn c) #'file-name-completions
          (pine.editor.frame:callback c) cb
          (pine.editor.frame:prompt c) prompt-text)
    (completion-update-input initial)
    (setf (pine.editor.frame:prompt-history (pine.editor.frame:current-client)) history)
    (activate-minibuffer (pine.editor.frame:current-client) prompt-text :initial initial)))

(defun file-name-complete ()
  "Tab: extend the path by the entries' common prefix; a lone match completes
fully (directories keep their trailing slash so the next Tab descends)."
  (let ((cands (mapcar #'pine.editor.completion:candidate-string (pine.editor.frame:filtered (completion)))))
    (multiple-value-bind (dir base) (split-path (expand-tilde (pine.editor.frame:input (completion))))
      (declare (ignore base))
      (when cands
        (let ((add (if (= 1 (length cands)) (first cands) (longest-common-prefix cands))))
          (minibuffer-set-text (concatenate 'string dir add)))))))

(defun file-name-accept ()
  "Return: a highlighted entry (when the typed path is not itself a file) is
taken; a directory is descended into, a file is opened."
  (let* ((c (completion))
         (typed (expand-tilde (pine.editor.frame:input c)))
         (sel (let ((i (pine.editor.frame:index c)) (f (pine.editor.frame:filtered c)))
                (when (and (>= i 0) (< i (length f)))
                  (pine.editor.completion:candidate-string (nth i f)))))
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
        (let ((cb (pine.editor.frame:callback c)))
          (%push-prompt-history (pine.editor.frame:current-client) target)
          (completion-cleanup)
          (deactivate-minibuffer (pine.editor.frame:current-client))
          (when cb (%safe-call cb target))))))


;;;; The minibuffer as a real buffer. While a prompt is active, *minibuffer* is
;;;; the current buffer, so every editing command -- motion, kill, yank, word
;;;; ops, isearch -- operates on the input through the ordinary dispatch, exactly
;;;; like any buffer. minibuffer-mode (a minor mode) adds only the completion and
;;;; exit keys. A controller subscribed to the buffer re-filters the pine.editor.completion:candidate
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
                     :receive (lambda (msg)
                                (let ((pine.editor.frame:*client* client))
                                  (when (eq (first msg) :snapshot)
                                    (ignore-errors
                                     (minibuffer-changed
                                      client (getf (rest msg) :snapshot))))
                                  nil)))))
        (sento.actor:tell buf (list :subscribe :renderer ctrl))
        (setf (pine.editor.frame:minibuffer-buffer client) buf
              (pine.editor.frame:minibuffer-controller client) ctrl)
        buf)))

(defun minibuffer-active-p ()
  (let ((c (pine.editor.frame:current-client)))
    (and c (pine.editor.frame:prompt-active c))))

(defun %snap-line0 (snap)
  (if (and snap (plusp (pine.text.buffer:line-count snap)))
      (fset:@ (pine.text.buffer:lines snap) 0)
      ""))

(defun minibuffer-text ()
  "The current input, read synchronously from the buffer (accept path)."
  (let* ((c (pine.editor.frame:current-client))
         (mb (pine.editor.frame:minibuffer-buffer c)))
    (if mb (or (ignore-errors (sento.actor:ask-s mb '(:get-text) :time-out 5)) "") "")))

(defun minibuffer-set-text (text)
  "Replace the input with TEXT and put point at its end. Used by Tab completion
and file-name descent."
  (let* ((c (pine.editor.frame:current-client))
         (mb (pine.editor.frame:minibuffer-buffer c)))
    (when mb
      (sento.actor:tell mb (list :replace-content :content text))
      (sento.actor:tell mb (list :move-point :line 0 :col (length text))))))

(defun minibuffer-changed (client snap)
  "Controller callback: on each input edit, cache the snapshot, re-filter the
candidate list, and repaint."
  (setf (pine.editor.frame:minibuffer-snap client) snap)
  (when (pine.editor.frame:prompt-active client)
    (when (completing-read-active-p)
      (completion-update-input (%snap-line0 snap)))
    (let ((r (pine.editor.frame:renderer client)))
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
    (unless (pine.editor.frame:prompt-active client)
      (setf (pine.editor.frame:saved-buffer client) (pine.editor.frame:current-buffer client)))
    (setf (pine.editor.frame:current-buffer client) mb
          (pine.editor.frame:prompt-active client) t)
    (pine.editor.frame:set-buffer-mode mb :text-mode)
    (ignore-errors (pine.editor.frame:enable-minor-mode client :minibuffer-mode))
    (pine.editor.echo:show-prompt prompt-text)
    (sento.actor:tell mb (list :replace-content :content initial))
    (sento.actor:tell mb (list :move-point :line 0 :col (length initial)))
    mb))

(defun deactivate-minibuffer (client)
  "Leave the minibuffer: restore the previous buffer and clear the prompt. Never
restore to the minibuffer itself -- fall back to the focused window's buffer."
  (ignore-errors (pine.editor.frame:disable-minor-mode client :minibuffer-mode))
  (let* ((mb (pine.editor.frame:minibuffer-buffer client))
         (back (pine.editor.frame:saved-buffer client)))
    (when (or (null back) (eq back mb))
      (let ((w (pine.editor.frame:focused-window client)))
        (setf back (and w (pine.text.window:buffer-ref w)))))
    (setf (pine.editor.frame:current-buffer client) back
          (pine.editor.frame:saved-buffer client) nil
          (pine.editor.frame:prompt-active client) nil
          (pine.editor.frame:minibuffer-snap client) nil
          (pine.editor.frame:prompt-history client) nil
          (pine.editor.frame:prompt-history-pos client) nil
          (pine.editor.frame:prompt-history-items client) nil))
  (pine.editor.echo:hide-prompt)
  (let ((r (pine.editor.frame:renderer client)))
    (when r (sento.actor:tell r '(:force-render)))))

(defun %safe-call (fn arg)
  (when fn
    (handler-case (funcall fn arg)
      (error (e) (pine.editor.echo:message (format nil "error: ~a" e))))))

;;;; Prompt history. A prompt opened with :history NAME reads and feeds the
;;;; store list NAME: accept pushes the input, M-p / M-n cycle it (the cycle
;;;; position and fetched items live on the client for the prompt's duration).

(defun %push-prompt-history (client input)
  (let ((h (pine.editor.frame:prompt-history client)))
    (when (and h (stringp input) (plusp (length input)))
      (pine.state.store:store-push h input :max 200))))

(defun minibuffer-history-prev ()
  "M-p: replace the input with the previous (older) history entry."
  (let* ((client (pine.editor.frame:current-client))
         (h (pine.editor.frame:prompt-history client)))
    (when h
      (unless (pine.editor.frame:prompt-history-items client)
        (setf (pine.editor.frame:prompt-history-items client)
              (pine.state.store:store-items h :limit 200)))
      (let* ((items (pine.editor.frame:prompt-history-items client))
             (pos (pine.editor.frame:prompt-history-pos client))
             (next (if pos (1+ pos) 0)))
        (when (< next (length items))
          (setf (pine.editor.frame:prompt-history-pos client) next)
          (minibuffer-set-text (nth next items)))))))

(defun minibuffer-history-next ()
  "M-n: replace the input with the next (newer) entry; past the newest, an
empty input leaves cycling."
  (let* ((client (pine.editor.frame:current-client))
         (items (pine.editor.frame:prompt-history-items client))
         (pos (pine.editor.frame:prompt-history-pos client)))
    (when (and items pos)
      (if (plusp pos)
          (progn
            (setf (pine.editor.frame:prompt-history-pos client) (1- pos))
            (minibuffer-set-text (nth (1- pos) items)))
          (progn
            (setf (pine.editor.frame:prompt-history-pos client) nil)
            (minibuffer-set-text ""))))))

;;;; Accept / abort / pine.editor.completion:complete / pine.editor.completion:candidate motion -- the minibuffer-mode command
;;;; bodies (the defcmd wrappers live in editor.lisp).

(defun minibuffer-accept ()
  (let* ((client (pine.editor.frame:current-client))
         (text (minibuffer-text)))
    (cond
      ((file-completion-active-p) (file-name-accept))
      ((completing-read-active-p)
       (let* ((c (completion))
              (result (if (and (>= (pine.editor.frame:index c) 0)
                               (< (pine.editor.frame:index c) (length (pine.editor.frame:filtered c))))
                          (pine.editor.completion:candidate-string (nth (pine.editor.frame:index c)
                                                 (pine.editor.frame:filtered c)))
                          text))
              (cb (pine.editor.frame:callback c)))
         (%push-prompt-history client result)
         (completion-cleanup)
         (deactivate-minibuffer client)
         (%safe-call cb result)))
      ((pine.editor.frame:prompt-callback client)
       (let ((cb (pine.editor.frame:prompt-callback client)))
         (setf (pine.editor.frame:prompt-callback client) nil)
         (%push-prompt-history client text)
         (deactivate-minibuffer client)
         (%safe-call cb text)))
      (t (deactivate-minibuffer client)))))

(defun minibuffer-abort ()
  (let ((client (pine.editor.frame:current-client)))
    (when (completing-read-active-p) (completion-cleanup))
    (setf (pine.editor.frame:prompt-callback client) nil)
    (deactivate-minibuffer client)
    (pine.editor.echo:message "quit")))

(defun minibuffer-complete ()
  (cond ((file-completion-active-p) (file-name-complete))
        ((completing-read-active-p)
         ;; insert the selected pine.editor.completion:candidate as the input
         (let* ((c (completion))
                (i (pine.editor.frame:index c))
                (f (pine.editor.frame:filtered c)))
           (when (and (>= i 0) (< i (length f)))
             (minibuffer-set-text (pine.editor.completion:candidate-string (nth i f))))))))


;;;; Raw-text prompt (no completion): eval-expression, new-buffer. It activates
;;;; the minibuffer with a callback; Return -> minibuffer-accept fires it.

(defun prompt (prompt-text cb &key history)
  (let ((client (pine.editor.frame:current-client)))
    (setf (pine.editor.frame:prompt-callback client) cb
          (pine.editor.frame:prompt-history client) history)
    (activate-minibuffer client prompt-text)))

(defun cancel-prompt ()
  (minibuffer-abort))
