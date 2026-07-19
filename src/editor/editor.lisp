(in-package #:pine.editor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(defun start-editor ()
  (let* ((client (pine.client:current-client))
         (server (pine.client:server-of client)))
    (pine.buffer:start-buffer-registry server)
    (handler-case (pine.ts:ensure-ts (pine.server:ts-runtime server))
      (error () nil))
    (pine.render:start-renderer client)
    (pine.mode:install-default-modes)
    (install-variables)
    (install-commands)
    (install-bindings)
    (setf pine.command:*minibuffer-handler* #'minibuffer-dispatch)
    (setf pine.command:*terminal-handler* #'pine.term:terminal-dispatch)
    (setf pine.eval:*on-debug* #'%eval-error)
    (let ((buf (pine.buffer:make-buffer "scratch")))
      (pine.buffer:make-window buf "scratch"
                               :row 0 :col 0 :width 80 :height 29 :focused t)
      (pine.render:subscribe-to-buffer buf)
      (pine.mode:set-buffer-mode buf :text-mode)
      (pine.buffer:tell buf :set-local :key :package :value :pine-user))
    (pine.render:relayout)))

;;;; Motion / eval helpers (command implementations)

(defun focused-snap ()
  (let ((w (pine.client:focused-window (pine.client:current-client))))
    (when w (pine.buffer:snap w))))

(defun cur-buffer () (pine.client:current-buffer (pine.client:current-client)))

(defun %fresh-snap ()
  (let ((buf (cur-buffer))) (when buf (pine.buffer:ask buf :snapshot))))

(defun %buffer-ts-lang ()
  (let ((mode (pine.mode:current-buffer-mode)))
    (and (typep mode 'pine.mode:major-mode) (pine.mode:ts-language mode))))

(defun %ts-runtime ()
  (pine.server:ts-runtime (pine.client:server-of (pine.client:current-client))))

(defun %sexp-move (kind)
  "Move point structurally via the buffer's persistent tree (no reparse). The
buffer walks its own tree from its own point and moves; nothing blocks here."
  (let ((buf (cur-buffer)))
    (when buf (sento.actor:tell buf (list :ts-motion :kind kind)))))

(defun move-chars (n)
  "Move point N characters (negative = left) across line boundaries. The buffer
computes the target from its own state, so this never blocks on a round-trip."
  (let ((buf (cur-buffer)))
    (when buf (sento.actor:tell buf (list :move-by :unit :char :n n)))))

(defun move-lines (n)
  "Move point N lines (negative = up), keeping the column where possible."
  (let ((buf (cur-buffer)))
    (when buf (sento.actor:tell buf (list :move-by :unit :line :n n)))))

(defun %point->offset (snap)
  (let ((pl (pine.buffer:point-line snap))
        (pc (pine.buffer:point-col snap))
        (lines (pine.buffer:lines snap)))
    (+ (loop for i from 0 below pl sum (1+ (length (fset:@ lines i)))) pc)))

(defun %whitespace-p (c)
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline) (char= c #\Return)))

(defun %sexp-delim-p (c)
  (or (%whitespace-p c) (char= c #\() (char= c #\)) (char= c #\")))

(defun %match-paren-backward (text close-pos)
  (let ((depth 1) (i (1- close-pos)) (in-string nil))
    (loop while (>= i 0) do
      (let ((c (char text i)))
        (cond
          (in-string
           (when (and (char= c #\") (or (zerop i) (not (char= (char text (1- i)) #\\))))
             (setf in-string nil))
           (decf i))
          ((char= c #\") (setf in-string t) (decf i))
          ((char= c #\)) (incf depth) (decf i))
          ((char= c #\()
           (decf depth)
           (when (zerop depth) (return-from %match-paren-backward i))
           (decf i))
          (t (decf i)))))
    nil))

(defun %match-string-backward (text close-pos)
  (let ((i (1- close-pos)))
    (loop while (>= i 0) do
      (let ((c (char text i)))
        (if (and (char= c #\") (or (zerop i) (not (char= (char text (1- i)) #\\))))
            (return-from %match-string-backward i)
            (decf i))))
    nil))

(defun %atom-start-backward (text end-inclusive)
  (let ((i end-inclusive))
    (loop while (and (>= i 0) (not (%sexp-delim-p (char text i)))) do (decf i))
    (1+ i)))

(defun %preceding-sexp-bounds (text end)
  (let ((i (1- end)))
    (loop while (and (>= i 0) (%whitespace-p (char text i))) do (decf i))
    (when (minusp i) (return-from %preceding-sexp-bounds nil))
    (let ((c (char text i)))
      (cond
        ((char= c #\)) (let ((s (%match-paren-backward text i))) (when s (values s (1+ i)))))
        ((char= c #\") (let ((s (%match-string-backward text i))) (when s (values s (1+ i)))))
        (t (values (%atom-start-backward text i) (1+ i)))))))

(defun %infer-package (text)
  "The package named by the last (in-package ...) in TEXT, like SLIME infers it
from the buffer, or nil."
  (let ((pos 0) (result nil))
    (loop for idx = (search "(in-package" text :start2 pos)
          while idx do
            (multiple-value-bind (form new)
                (ignore-errors (read-from-string text nil nil :start idx))
              (when (and (consp form) (symbolp (first form))
                         (string-equal (symbol-name (first form)) "IN-PACKAGE")
                         (second form))
                (let ((p (find-package (second form)))) (when p (setf result p))))
              (setf pos (if (and new (> new idx)) new (1+ idx)))))
    result))

(defun %buffer-package (state)
  (let ((inferred (%infer-package (pine.buffer:state->string state)))
        (name (pine.buffer:buffer-local state :package nil)))
    (or inferred (and name (find-package name)) (find-package :cl-user))))

(defun %lc->offset (text line col)
  "Character offset of LINE/COL in TEXT."
  (let ((i 0) (l 0) (n (length text)))
    (loop while (and (< i n) (< l line))
          do (when (char= (char text i) #\Newline) (incf l))
             (incf i))
    (min (+ i col) n)))

(defun %show-eval-result (form thunk)
  (handler-case
      (let ((values (multiple-value-list (funcall thunk))))
        (pine.echo:message (format nil "=> ~{~s~^, ~}" values))
        (values-list values))
    (error (c) (pine.echo:message (format nil "error in ~s: ~a" form c)) nil)))

;;;; Evaluation runs through pine.eval on its own thread, never on the UI
;;;; thread, so a slow/looping/erroring form can't hang or crash the editor.

(defvar *pending-debugger* nil "The evaluation currently waiting in the debugger.")

(defun %eval-notify (text)
  "Show TEXT in the echo area and repaint, safely from the eval thread."
  (pine.echo:message text)
  (let ((r (ignore-errors (pine.client:renderer (pine.client:current-client)))))
    (when r (sento.actor:tell r '(:force-render)))))

(defun %eval-done (ev)
  (case (pine.eval:evaluation-status ev)
    (:ok (%eval-notify (format nil "=> ~{~s~^, ~}" (pine.eval:evaluation-values ev))))
    (:aborted (%eval-notify "aborted"))))

(defun %debugger-text (ev)
  (with-output-to-string (s)
    (format s "Evaluation error~%~%~a:~%  ~a~%~%Restarts:~%"
            (pine.eval:evaluation-condition-type ev)
            (pine.eval:evaluation-condition ev))
    (loop for (name report) in (pine.eval:evaluation-restarts ev) for i from 0
          do (format s "  ~d  [~a] ~a~%" i (or name "") report))
    (format s "~%Backtrace:~%~a" (pine.eval:evaluation-backtrace ev))))

(defun %eval-error (ev)
  (setf *pending-debugger* ev
        *pending-agent-debug* nil)
  (%show-help "*debugger*" (%debugger-text ev))
  (%eval-notify (format nil "eval error: ~a  (M-x choose-restart / C-g abort)"
                        (pine.eval:evaluation-condition-type ev))))

(defvar *pending-agent-debug* nil
  "The last error reported home by a :process agent: (:agent :eval-id :restarts).")

(defun %agent-debug-surface (msg)
  "A process agent's error, surfaced in the editor: show its restarts and let
choose-restart drive the resume back to that agent. Move the decision, not the
handler."
  (when (eq (first msg) :agent-debug)
    (destructuring-bind (&key agent eval-id condition restarts &allow-other-keys)
        (rest msg)
      (setf *pending-agent-debug* (list :agent agent :eval-id eval-id :restarts restarts)
            *pending-debugger* nil)
      (%show-help "*debugger*"
                  (format nil "Error in agent ~a~%~%~a~%~%Restarts:~%~{  ~a~%~}"
                          agent condition restarts))
      (%eval-notify (format nil "agent ~a error (M-x choose-restart)" agent)))))

(defvar *eval-target* :local
  "Where C-x C-e / eval-defun run: :local (this image), or a registered agent
name (a :process agent's own image). The one eval path, target swappable.")

(defun %eval-form-string (str package)
  ;; one eval path: run STR in the chosen agent, off the caller thread, sharing
  ;; the same pine.eval engine. Errors reach *on-debug* (local) or come home
  ;; from a process agent via agent-debug.
  (if (or (null *eval-target*) (eq *eval-target* :local))
      (if pine.actor:*local-agent*
          (pine.actor:agent-eval nil pine.actor:*local-agent* str
                                 :package package
                                 :bindings (list (cons 'pine.client:*client*
                                                       (pine.client:current-client)))
                                 :on-done #'%eval-done)
          (pine.eval:evaluate-string
           str :package package
           :bindings (list (cons 'pine.client:*client* (pine.client:current-client)))
           :on-done #'%eval-done))
      ;; a remote agent's image: no local bindings cross the wire
      (pine.actor:agent-eval (pine.client:server-of (pine.client:current-client))
                             *eval-target* str :package package :on-done #'%eval-done)))

(defun eval-defun ()
  "Evaluate the top-level form point is inside (C-M-x), via the buffer's tree."
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (snap (pine.buffer:state->snapshot state))
             (lang (%buffer-ts-lang)))
        (if (null lang)
            (eval-last-sexp)
            (multiple-value-bind (sl sc el ec)
                (pine.ts:defun-bounds-pos (%ts-runtime) lang text
                                          (pine.buffer:point-line snap)
                                          (pine.buffer:point-col snap))
              (if sl
                  (%eval-form-string
                   (subseq text (%lc->offset text sl sc) (%lc->offset text el ec))
                   (%buffer-package state))
                  (eval-last-sexp))))))))

(defun %offset->lc (text offset)
  (let ((line 0) (col 0))
    (dotimes (i (min offset (length text)) (values line col))
      (if (char= (char text i) #\Newline) (setf line (1+ line) col 0) (incf col)))))

(defun %token-at (text offset)
  "The symbol token surrounding OFFSET, bounded by sexp delimiters."
  (let ((s (min offset (length text))) (e (min offset (length text))) (n (length text)))
    (loop while (and (> s 0) (not (%sexp-delim-p (char text (1- s))))) do (decf s))
    (loop while (and (< e n) (not (%sexp-delim-p (char text e)))) do (incf e))
    (when (< s e) (subseq text s e))))

(defun %open-definition (label srcs)
  (let* ((src (first srcs))
         (path (and src (sb-introspect:definition-source-pathname src)))
         (coff (and src (sb-introspect:definition-source-character-offset src))))
    (cond
      ((and path (probe-file path))
       (pine.file:find-file (namestring path))
       (when coff
         (let ((nbuf (cur-buffer)))
           (when nbuf
             (let ((ntext (pine.buffer:state->string
                           (sento.actor:ask-s nbuf '(:get-state) :time-out 5))))
               (multiple-value-bind (l c) (%offset->lc ntext coff)
                 (sento.actor:tell nbuf (list :move-point :line l :col c)))))))
       (pine.echo:message (format nil "~a" (file-namestring path))))
      (t (pine.echo:message (format nil "no source for ~a" label))))))

(defun find-definition ()
  "Jump to the source of the symbol at point (M-.), via sb-introspect."
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (snap (pine.buffer:state->snapshot state))
             (off (min (%point->offset snap) (length text)))
             (tok (%token-at text off))
             (pkg (%buffer-package state)))
        (if (null tok)
            (pine.echo:message "no symbol at point")
            (let* ((sym (let ((*package* pkg)) (ignore-errors (read-from-string tok))))
                   (srcs (and (symbolp sym)
                              (loop for kind in '(:function :macro :generic-function
                                                  :variable :class)
                                    thereis (ignore-errors
                                             (sb-introspect:find-definition-sources-by-name
                                              sym kind))))))
              (%open-definition tok srcs)))))))

(defun %token-start (text offset)
  (let ((s (min offset (length text))))
    (loop while (and (> s 0) (not (%sexp-delim-p (char text (1- s))))) do (decf s))
    s))

(defun %symbol-candidates (prefix pkg)
  "Downcased names of symbols accessible in PKG that start with PREFIX."
  (let ((up (string-upcase prefix)) (out nil))
    (do-symbols (s pkg)
      (let ((name (symbol-name s)))
        (when (and (>= (length name) (length up))
                   (string= up name :end2 (length up)))
          (pushnew (string-downcase name) out :test #'string=))))
    (sort out #'string<)))

(defun complete-symbol ()
  "Complete the symbol at point against the buffer's package; Tab when there is
no symbol to complete."
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (snap (pine.buffer:state->snapshot state))
             (off (min (%point->offset snap) (length text)))
             (start (%token-start text off))
             (prefix (subseq text start off))
             (pkg (%buffer-package state)))
        (if (zerop (length prefix))
            (pine.command:call-command "insert-tab")
            (let ((cands (%symbol-candidates prefix pkg)))
              (cond
                ((null cands) (pine.echo:message "no completions"))
                ((null (rest cands)) (%replace-prefix buf prefix (first cands)))
                (t (completing-read "Complete: " cands
                     (lambda (choice) (%replace-prefix buf prefix choice)))))))))))

(defun %replace-prefix (buf prefix choice)
  (dotimes (i (length prefix)) (sento.actor:tell buf '(:backspace)))
  (pine.buffer:tell buf :insert :text choice))

(defun symbol-arglist ()
  "Echo the lambda list of the function named at point (M-x arglist)."
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (snap (pine.buffer:state->snapshot state))
             (off (min (%point->offset snap) (length text)))
             (tok (%token-at text off))
             (pkg (%buffer-package state)))
        (when tok
          (let ((sym (let ((*package* pkg)) (ignore-errors (read-from-string tok)))))
            (if (and (symbolp sym) (fboundp sym))
                (pine.echo:message
                 (format nil "~a ~(~a~)" tok (sb-introspect:function-lambda-list sym)))
                (pine.echo:message (format nil "~a: not a function" tok)))))))))

(defun load-file ()
  "Compile and load the current buffer's file into the eval target's image."
  (let* ((buf (cur-buffer))
         (state (and buf (sento.actor:ask-s buf '(:get-state) :time-out 5)))
         (path (and state (pine.buffer:buffer-local state :pathname nil))))
    (if path
        (%eval-form-string (format nil "(load (compile-file ~s))" (namestring path))
                           (find-package :cl-user))
        (pine.echo:message "buffer has no file"))))

(defun eval-last-sexp ()
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (snap (pine.buffer:state->snapshot state))
             (offset (min (%point->offset snap) (length text))))
        (multiple-value-bind (start end) (%preceding-sexp-bounds text offset)
          (if start
              (%eval-form-string (subseq text start end) (%buffer-package state))
              (pine.echo:message "no form before point")))))))

(defun eval-buffer ()
  ;; one evaluation on the eval thread reads and runs every form in order, so
  ;; *package* changes (in-package) carry across forms, a looping form can't
  ;; hang the editor, and a reader/eval error reaches the shared debugger
  ;; surface like every other eval path.
  (let ((buf (cur-buffer)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (package (%buffer-package state))
             (cli (pine.client:current-client))
             (thunk (lambda ()
                      (let ((pine.client:*client* cli) (*package* package)
                            (pos 0) (count 0))
                        (loop
                          (multiple-value-bind (form new-pos)
                              (read-from-string text nil :eof :start pos)
                            (when (eq form :eof) (return count))
                            (eval form) (incf count) (setf pos new-pos))))))
             (done (lambda (ev)
                     (%eval-notify
                      (case (pine.eval:evaluation-status ev)
                        (:ok (format nil "eval-buffer: ~a forms"
                                     (first (pine.eval:evaluation-values ev))))
                        (:aborted "eval-buffer aborted")
                        (t "eval-buffer: error"))))))
        ;; one eval path: route the whole-buffer eval through the local agent.
        (if pine.actor:*local-agent*
            (pine.actor:agent-run nil pine.actor:*local-agent* thunk
                                  :package package :on-done done)
            (pine.eval:evaluate-thunk thunk :package package :on-done done))))))

(defun scroll-window (delta)
  (let* ((client (pine.client:current-client))
         (w (pine.client:focused-window client))
         (buf (pine.client:current-buffer client)))
    (when (and w buf)
      (let ((snap (pine.buffer:snap w)))
        (when (and snap (typep snap 'pine.buffer:snapshot))
          (let* ((max-scroll (max 0 (- (pine.buffer:line-count snap)
                                       (pine.buffer:win-height w))))
                 (new-scroll (max 0 (min max-scroll
                                         (+ (pine.buffer:scroll-top w) delta))))
                 (h (pine.buffer:win-height w))
                 (pl (pine.buffer:point-line snap))
                 (pc (pine.buffer:point-col snap)))
            (setf (pine.buffer:scroll-top w) new-scroll)
            (cond
              ((< pl new-scroll)
               (sento.actor:tell buf (list :move-point :line new-scroll
                 :col (min pc (length (fset:@ (pine.buffer:lines snap) new-scroll))))))
              ((>= pl (+ new-scroll h))
               (let ((target (+ new-scroll h -1)))
                 (sento.actor:tell buf (list :move-point :line target
                   :col (min pc (length (fset:@ (pine.buffer:lines snap) target))))))))
            (sento.actor:tell (pine.client:renderer client) '(:force-render))))))))

;;;; Help / self-documentation

(defun %show-help (name text)
  "Put TEXT into a buffer named NAME and switch to it."
  (let* ((client (pine.client:current-client))
         (buf (pine.buffer:make-buffer name)))
    (pine.buffer:tell buf :replace-content :content text)
    (pine.render:subscribe-to-buffer buf)
    (pine.buffer:switch-buffer name)
    (sento.actor:tell (pine.client:renderer client)
                      (list :switch-buffer :buffer buf :name name))
    buf))

(defun %describe-key-text (key)
  (let ((entry (pine.command:key-binding (pine.client:current-client) key))
        (s (pine.key:key->string key)))
    (cond ((pine.keymap:prefix-p entry) (format nil "~a is a prefix key" s))
          ((stringp entry) (format nil "~a runs the command ~a" s entry))
          ((pine.command:self-insert-key-p key)
           (format nil "~a runs self-insert-command" s))
          (t (format nil "~a is undefined" s)))))

(defun %bindings-text ()
  (let* ((client (pine.client:current-client))
         (rows (loop for km in (pine.mode:active-keymaps client)
                     append (pine.keymap:keymap-bindings km t))))
    (with-output-to-string (out)
      (format out "Active bindings~%~%")
      (loop for (keys . cmd) in (sort (remove-duplicates rows :test #'equal
                                                         :key #'car :from-end t)
                                      #'string< :key #'car)
            do (format out "~16a  ~a~%" keys cmd)))))

(defun install-variables ()
  (pine.var:define-variable :tab-width :default 8
    :documentation "Number of spaces the Tab key inserts."))

(defun %variables-text ()
  (with-output-to-string (out)
    (format out "Editor variables~%~%")
    (dolist (name (pine.var:all-variable-names))
      (let ((v (pine.var:find-variable name)))
        (format out "~a = ~s [~(~a~)]~%    default ~s~a~%"
                name (pine.var:variable-value name) (pine.var:variable-scope name)
                (pine.var:evar-default v)
                (let ((d (pine.var:evar-documentation v)))
                  (if (plusp (length d)) (format nil "~%    ~a" d) "")))))))

(defun %mode-text ()
  (let* ((client (pine.client:current-client))
         (major (pine.mode:current-buffer-mode))
         (minors (pine.mode:buffer-minor-modes client)))
    (with-output-to-string (out)
      (format out "Major mode: ~a (~a)~%"
              (pine.mode:mode-name major) (pine.mode:mode-indicator major))
      (loop for m = major then (pine.mode:parent-mode m) while m
            do (format out "  ~a~%" (pine.mode:mode-name m)))
      (let ((lang (and (typep major 'pine.mode:major-mode)
                       (pine.mode:ts-language major))))
        (when lang (format out "  tree-sitter language: ~a~%" lang)))
      (format out "~%Minor modes:~%")
      (if minors
          (dolist (m minors)
            (format out "  ~a (~a) precedence ~a~a~%"
                    (pine.mode:mode-name m) (pine.mode:mode-indicator m)
                    (pine.mode:precedence m)
                    (if (pine.mode:transparent m) " transparent" "")))
          (format out "  none~%")))))

;;;; Commands

(defmacro defcmd (name (&rest args) &body body)
  `(pine.command:define-command ,name ,args ,@body))

(defun install-commands ()
  (defcmd "keyboard-quit" ()
    (setf (pine.client:pending-keys (pine.client:current-client)) nil)
    (when *pending-debugger*
      (pine.eval:abort-evaluation *pending-debugger*)
      (setf *pending-debugger* nil))
    (let ((buf (cur-buffer)))
      (when buf
        (sento.actor:tell buf (list :set-meta :key :mark-line :value nil))
        (sento.actor:tell buf (list :set-meta :key :mark-col :value nil))))
    (cancel-prompt))
  (defcmd "backspace" ()
    (let ((buf (cur-buffer))) (when buf (sento.actor:tell buf '(:backspace)))))
  (defcmd "delete-char" ()
    (let ((buf (cur-buffer)) (snap (focused-snap)))
      (when (and buf snap)
        (sento.actor:tell buf
          (list :delete-region
                :start-line (pine.buffer:point-line snap) :start-col (pine.buffer:point-col snap)
                :end-line (pine.buffer:point-line snap) :end-col (1+ (pine.buffer:point-col snap)))))))
  (defcmd "newline" ()
    (let ((buf (cur-buffer))) (when buf (pine.buffer:tell buf :newline))))
  (defcmd "undo" ()
    (let ((buf (cur-buffer))) (when buf (sento.actor:tell buf '(:undo)))))
  (defcmd "redo" ()
    (let ((buf (cur-buffer))) (when buf (sento.actor:tell buf '(:redo)))))

  (defcmd "forward-char" (n)  (:interactive :number) (move-chars n))
  (defcmd "backward-char" (n) (:interactive :number) (move-chars (- n)))
  (defcmd "next-line" (n)     (:interactive :number) (move-lines n))
  (defcmd "previous-line" (n) (:interactive :number) (move-lines (- n)))
  (defcmd "universal-argument" () (:prefix)
    (let ((cli (pine.client:current-client)))
      (setf (pine.client:prefix-arg cli)
            (list (* 4 (pine.command:prefix-numeric-value
                        (pine.client:prefix-arg cli)))))))
  (defcmd "digit-argument" () (:prefix)
    (let* ((cli (pine.client:current-client))
           (key (pine.client:this-command-key cli))
           (d (and key (digit-char-p (char (pine.key:key-sym key) 0))))
           (cur (pine.client:prefix-arg cli)))
      (when d
        (setf (pine.client:prefix-arg cli)
              (cond ((eq cur '-) (- d))
                    ((and (integerp cur) (minusp cur)) (- (+ (* 10 (- cur)) d)))
                    ((integerp cur) (+ (* 10 cur) d))
                    (t d))))))
  (defcmd "negative-argument" () (:prefix)
    (let* ((cli (pine.client:current-client))
           (cur (pine.client:prefix-arg cli)))
      (setf (pine.client:prefix-arg cli)
            (cond ((eq cur '-) nil)
                  ((integerp cur) (- cur))
                  (t '-)))))

  (defcmd "beginning-of-line" ()
    (let ((buf (cur-buffer)) (snap (focused-snap)))
      (when (and buf snap)
        (sento.actor:tell buf (list :move-point :line (pine.buffer:point-line snap) :col 0)))))
  (defcmd "end-of-line" ()
    (let ((buf (cur-buffer)) (snap (focused-snap)))
      (when (and buf snap)
        (let ((len (length (fset:@ (pine.buffer:lines snap) (pine.buffer:point-line snap)))))
          (sento.actor:tell buf (list :move-point :line (pine.buffer:point-line snap) :col len))))))
  (defcmd "beginning-of-buffer" ()
    (let ((buf (cur-buffer))) (when buf (sento.actor:tell buf (list :move-point :line 0 :col 0)))))
  (defcmd "end-of-buffer" ()
    (let ((buf (cur-buffer)) (snap (focused-snap)))
      (when (and buf snap)
        (let* ((ll (1- (pine.buffer:line-count snap)))
               (lc (length (fset:@ (pine.buffer:lines snap) ll))))
          (sento.actor:tell buf (list :move-point :line ll :col lc))))))

  (defcmd "scroll-down" ()
    (let ((w (pine.client:focused-window (pine.client:current-client))))
      (when w (scroll-window (- (pine.buffer:win-height w) 2)))))
  (defcmd "scroll-up" ()
    (let ((w (pine.client:focused-window (pine.client:current-client))))
      (when w (scroll-window (- 2 (pine.buffer:win-height w))))))

  (defcmd "forward-sexp" ()      (%sexp-move :forward-sexp))
  (defcmd "backward-sexp" ()     (%sexp-move :backward-sexp))
  (defcmd "beginning-of-defun" () (%sexp-move :beginning-of-defun))
  (defcmd "end-of-defun" ()      (%sexp-move :end-of-defun))
  (defcmd "mark-sexp" ()
    (set-mark)
    (%sexp-move :forward-sexp))

  (defcmd "set-mark" ()     (set-mark))
  (defcmd "kill-line" ()    (kill-line-cmd))
  (defcmd "kill-region" ()  (kill-region-cmd))
  (defcmd "copy-region" ()  (copy-region-cmd))
  (defcmd "yank" ()         (yank-cmd))
  (defcmd "yank-pop" ()     (yank-pop-cmd))

  (defcmd "find-file" ()
    (read-file-name "Find file: "
      (lambda (path)
        (handler-case (pine.file:find-file path)
          (error (c) (pine.echo:message (format nil "error: ~a" c)))))))
  (defcmd "save-file" ()
    (handler-case (pine.file:save-current-buffer)
      (error (c) (pine.echo:message (format nil "error: ~a" c)))))
  (defcmd "kill-pine-editor" ()
    ;; the explicit teardown: unsupervise this editor so the daemon will not
    ;; respawn it, then terminate its process. Same path a WM window-close takes.
    (pine:kill-frontend "editor"))
  (defcmd "switch-buffer" ()
    (completing-read "Switch to: " (pine.buffer:list-buffers)
      (lambda (name)
        (let* ((client (pine.client:current-client))
               (buf (pine.buffer:switch-buffer name)))
          (when buf
            (sento.actor:tell (pine.client:renderer client)
                              (list :switch-buffer :buffer buf :name name))
            (pine.render:subscribe-to-buffer buf))))))
  (defcmd "list-buffers" ()
    (pine.echo:message (format nil "buffers: ~{~a~^, ~}" (pine.buffer:list-buffers))))
  (defcmd "execute-command" ()
    (completing-read "M-x " (pine.command:all-command-names)
      (lambda (name) (pine.command:call-command name))))
  (defcmd "eval-expression" ()
    (prompt "Eval: "
      (lambda (text)
        (let ((pkg (let ((buf (cur-buffer)))
                     (if buf
                         (%buffer-package (sento.actor:ask-s buf '(:get-state) :time-out 5))
                         (find-package :cl-user)))))
          (%eval-form-string text pkg)))))
  (defcmd "choose-restart" ()
    (cond
      (*pending-agent-debug*
       (destructuring-bind (&key agent eval-id restarts) *pending-agent-debug*
         (completing-read "Restart: " (remove nil restarts)
           (lambda (name)
             (let ((info (pine.actor:find-agent
                          (pine.client:server-of (pine.client:current-client)) agent)))
               (when info
                 (sento.actor:tell (pine.actor:agent-info-actor info)
                                   (list :resume :eval-id eval-id :restart name))))
             (setf *pending-agent-debug* nil)
             (pine.echo:message (format nil "resumed ~a in agent ~a" name agent))))))
      ((and *pending-debugger*
            (eq (pine.eval:evaluation-status *pending-debugger*) :error))
       (let ((ev *pending-debugger*))
         (completing-read "Restart: "
           (remove nil (mapcar #'first (pine.eval:evaluation-restarts ev)))
           (lambda (name)
             (pine.eval:pick-restart ev name)
             (setf *pending-debugger* nil)
             (pine.echo:message (format nil "invoked ~a" name))))))
      (t (pine.echo:message "no evaluation in the debugger"))))
  (defcmd "jobs" ()
    (%show-help "*jobs*"
      (with-output-to-string (s)
        (format s "Jobs  (M-x choose-restart on an errored one)~%~%~4@a  ~-10a ~-9a ~a~%"
                "id" "agent" "status" "form / condition")
        (dolist (j (pine.jobs:list-jobs))
          (format s "~4@a  ~-10a ~-9a ~a~%"
                  (getf j :id) (getf j :agent) (getf j :status)
                  (or (getf j :form) (getf j :condition) ""))))))
  (defcmd "eval-last-sexp" () (eval-last-sexp))
  (defcmd "eval-defun" ()     (eval-defun))
  (defcmd "eval-buffer" ()    (eval-buffer))
  (defcmd "find-definition" () (find-definition))
  (defcmd "arglist" ()        (symbol-arglist))
  (defcmd "complete-symbol" () (complete-symbol))
  (defcmd "load-file" ()      (load-file))
  (defcmd "set-eval-target" ()
    (completing-read
     "Eval in: "
     (cons "local"
           (mapcar #'pine.actor:agent-info-name
                   (pine.actor:list-agents
                    (pine.client:server-of (pine.client:current-client)))))
     (lambda (name)
       (setf *eval-target* (if (string= name "local") :local name))
       (pine.echo:message (format nil "eval target: ~a" name)))))
  (defcmd "new-buffer" ()
    (prompt "New buffer: "
      (lambda (name)
        (let ((buf (pine.buffer:make-buffer name))) (pine.render:subscribe-to-buffer buf)))))
  (defcmd "open-repl" ()
    (handler-case
        (let* ((client (pine.client:current-client))
               (buf (or (pine.client:repl-buffer client) (pine.repl:start-repl))))
          (pine.buffer:switch-buffer "*repl*")
          (pine.render:subscribe-to-buffer buf)
          (sento.actor:tell (pine.client:renderer client)
                            (list :switch-buffer :buffer buf :name "*repl*")))
      (error (c) (pine.echo:message (format nil "error: ~a" c)))))
  (defcmd "terminal" ()
    (handler-case
        (let* ((client (pine.client:current-client))
               (f (pine.client:frame client))
               (cols (pine.buffer:frame-cols f))
               (rows (max 1 (- (pine.buffer:frame-rows f) 2)))
               (buf (pine.buffer:make-buffer "*terminal*")))
          (pine.term:open-terminal client buf :rows rows :cols cols)
          (pine.mode:set-buffer-mode buf :terminal-mode)
          (pine.buffer:switch-buffer "*terminal*")
          (sento.actor:tell (pine.client:renderer client)
                            (list :switch-buffer :buffer buf :name "*terminal*")))
      (error (c) (pine.echo:message (format nil "error: ~a" c)))))
  (defcmd "overwrite-mode" ()
    (let ((on (pine.mode:toggle-minor-mode (pine.client:current-client) :overwrite-mode)))
      (pine.echo:message (if on "Overwrite mode enabled" "Overwrite mode disabled"))))
  (defcmd "describe-key" ()
    (pine.echo:message "Describe key: ")
    (pine.command:read-next-key
     (pine.client:current-client)
     (lambda (key) (pine.echo:message (%describe-key-text key)))))
  (defcmd "describe-bindings" ()
    (%show-help "*bindings*" (%bindings-text)))
  (defcmd "describe-mode" ()
    (%show-help "*mode*" (%mode-text)))
  (defcmd "describe-variables" ()
    (%show-help "*variables*" (%variables-text)))
  (defcmd "insert-tab" ()
    (let* ((cli (pine.client:current-client))
           (buf (pine.client:current-buffer cli))
           (n (max 0 (pine.var:variable-value :tab-width buf))))
      (when buf
        (pine.buffer:tell buf :insert :text (make-string n :initial-element #\Space))))))

;;;; Bindings

(defun k (spec) (pine.key:parse-key spec))

(defun install-bindings ()
  (let ((g (pine.mode:global-keymap))
        (tm (pine.mode:mode-keymap (pine.mode:find-mode :text-mode))))
    ;; global: quit, chords, M-x
    (pine.keymap:define-key g (k "C-g") "keyboard-quit")
    (pine.keymap:define-key g (k "Escape") "keyboard-quit")
    (pine.keymap:define-key g (list (k "C-x") (k "C-f")) "find-file")
    (pine.keymap:define-key g (list (k "C-x") (k "C-s")) "save-file")
    (pine.keymap:define-key g (list (k "C-x") (k "b")) "switch-buffer")
    (pine.keymap:define-key g (list (k "C-x") (k "n")) "new-buffer")
    (pine.keymap:define-key g (list (k "C-x") (k "r")) "open-repl")
    (pine.keymap:define-key g (list (k "C-x") (k "t")) "terminal")
    (pine.keymap:define-key g (list (k "C-x") (k "C-e")) "eval-last-sexp")
    (pine.keymap:define-key g (list (k "C-x") (k "C-c")) "kill-pine-editor")
    (pine.keymap:define-key g (k "C-M-x") "eval-defun")
    (pine.keymap:define-key g (k "M-.") "find-definition")
    (pine.keymap:define-key g (k "M-x") "execute-command")
    (pine.keymap:define-key g (k "Insert") "overwrite-mode")
    ;; prefix argument
    (pine.keymap:define-key g (k "C-u") "universal-argument")
    (pine.keymap:define-key g (k "M--") "negative-argument")
    (dotimes (d 10)
      (pine.keymap:define-key g (k (format nil "M-~d" d)) "digit-argument"))
    ;; help
    (pine.keymap:define-key g (list (k "C-h") (k "k")) "describe-key")
    (pine.keymap:define-key g (list (k "C-h") (k "b")) "describe-bindings")
    (pine.keymap:define-key g (list (k "C-h") (k "m")) "describe-mode")
    (pine.keymap:define-key g (list (k "C-h") (k "v")) "describe-variables")
    ;; text-mode editing
    (pine.keymap:define-key tm (k "BackSpace") "backspace")
    (pine.keymap:define-key tm (k "Return") "newline")
    (pine.keymap:define-key tm (k "Tab") "insert-tab")
    (pine.keymap:define-key tm (k "Up") "previous-line")
    (pine.keymap:define-key tm (k "Down") "next-line")
    (pine.keymap:define-key tm (k "Left") "backward-char")
    (pine.keymap:define-key tm (k "Right") "forward-char")
    (pine.keymap:define-key tm (k "C-f") "forward-char")
    (pine.keymap:define-key tm (k "C-b") "backward-char")
    (pine.keymap:define-key tm (k "C-n") "next-line")
    (pine.keymap:define-key tm (k "C-p") "previous-line")
    (pine.keymap:define-key tm (k "C-a") "beginning-of-line")
    (pine.keymap:define-key tm (k "C-e") "end-of-line")
    (pine.keymap:define-key tm (k "C-d") "delete-char")
    (pine.keymap:define-key tm (k "M-<") "beginning-of-buffer")
    (pine.keymap:define-key tm (k "M->") "end-of-buffer")
    (pine.keymap:define-key tm (k "C-space") "set-mark")
    (pine.keymap:define-key tm (k "C-k") "kill-line")
    (pine.keymap:define-key tm (k "C-w") "kill-region")
    (pine.keymap:define-key tm (k "M-w") "copy-region")
    (pine.keymap:define-key tm (k "C-y") "yank")
    (pine.keymap:define-key tm (k "M-y") "yank-pop")
    (pine.keymap:define-key tm (k "C-v") "scroll-down")
    (pine.keymap:define-key tm (k "M-v") "scroll-up")
    (pine.keymap:define-key tm (k "Prior") "scroll-up")
    (pine.keymap:define-key tm (k "Next") "scroll-down")
    (pine.keymap:define-key tm (k "C-z") "undo")
    (pine.keymap:define-key tm (k "C-/") "undo")
    (pine.keymap:define-key tm (k "C-?") "redo")
    ;; structural navigation (tree-sitter)
    (pine.keymap:define-key tm (k "C-M-f") "forward-sexp")
    (pine.keymap:define-key tm (k "C-M-b") "backward-sexp")
    (pine.keymap:define-key tm (k "C-M-a") "beginning-of-defun")
    (pine.keymap:define-key tm (k "C-M-e") "end-of-defun")
    (pine.keymap:define-key tm (k "C-M-space") "mark-sexp")
    ;; lisp-mode: the SLIME chord set (mode-keymap chords resolve fine)
    (let ((lm (pine.mode:mode-keymap (pine.mode:find-mode :lisp-mode))))
      (pine.keymap:define-key lm (list (k "C-c") (k "C-c")) "eval-defun")
      (pine.keymap:define-key lm (list (k "C-c") (k "C-k")) "eval-buffer")
      (pine.keymap:define-key lm (list (k "C-c") (k "C-l")) "load-file")
      (pine.keymap:define-key lm (list (k "C-c") (k "C-d")) "arglist")
      (pine.keymap:define-key lm (k "Tab") "complete-symbol"))))
