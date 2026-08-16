(defpackage #:pine/edit
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:job #:pine/run/job)
                    (#:system #:pine/run/system) (#:command #:pine/run/command)
                    (#:fault #:pine/run/fault) (#:log #:pine/run/log)
                    (#:key #:pine/ui/key) (#:surface #:pine/ui/surface)
                    (#:build #:pine/ui/build)
                    (#:mode #:pine/text/mode) (#:doc #:pine/text/document)
                    (#:parser #:pine/text/ts/parser)
                    (#:emode #:pine/edit/mode) (#:window #:pine/edit/window)
                    (#:file #:pine/edit/file)
                    (#:keys #:pine/edit/keys) (#:render #:pine/edit/render)
                    (#:prompt #:pine/edit/prompt) (#:listing #:pine/edit/listing)
                    (#:isearch #:pine/edit/isearch) (#:commands #:pine/edit/commands)
                    (#:help #:pine/edit/help) (#:evaluate #:pine/edit/eval)
                    (#:debugger #:pine/edit/debugger))
  (:export #:edit #:type-text))
(in-package #:pine/edit)

(defparameter +text-keys+
  '(("C-f" . "forward-char") ("C-b" . "backward-char")
    ("Right" . "forward-char") ("Left" . "backward-char")
    ("M-f" . "forward-word") ("M-b" . "backward-word")
    ("C-n" . "next-line") ("C-p" . "previous-line")
    ("Down" . "next-line") ("Up" . "previous-line")
    ("C-a" . "beginning-of-line") ("C-e" . "end-of-line")
    ("Home" . "beginning-of-line") ("End" . "end-of-line")
    ("M-<" . "beginning-of-document") ("M->" . "end-of-document")
    ("PageUp" . "scroll-down") ("PageDown" . "scroll-up")
    ("C-v" . "scroll-up") ("M-v" . "scroll-down") ("C-l" . "recenter")
    ("RET" . "newline") ("DEL" . "delete-backward-char")
    ("Delete" . "delete-char") ("C-d" . "delete-char")
    ("TAB" . "indent-line")
    ("C-SPC" . "set-mark") ("C-w" . "kill-region") ("M-w" . "copy-region")
    ("C-y" . "yank") ("M-y" . "yank-pop")
    ("C-/" . "undo") ("C-?" . "redo")
    ("C-k" . "kill-line") ("M-d" . "kill-word") ("M-DEL" . "backward-kill-word")
    ("C-o" . "open-line") ("C-t" . "transpose-chars")
    ("C-g" . "keyboard-quit") ("Escape" . "keyboard-quit")
    ("C-s" . "isearch-forward") ("C-r" . "isearch-backward")
    ("M-%" . "query-replace")
    ("M-;" . "comment-line") ("C-M-\\" . "indent-region")
    ("M-u" . "upcase-word") ("M-l" . "downcase-word") ("M-c" . "capitalize-word")
    ("M-x" . "run-command") ("M-:" . "eval-expression")
    ("M-g g" . "goto-line") ("M-o" . "overwrite")
    ("C-u" . "universal-argument") ("M--" . "negative-argument")
    ("C-x C-f" . "find-file") ("C-x C-r" . "find-recent")
    ("C-x C-s" . "save-document") ("C-x C-w" . "write-file")
    ("C-x b" . "switch-to-document") ("C-x k" . "kill-document")
    ("C-x n" . "new-document") ("C-x C-b" . "list-documents")
    ("C-x h" . "mark-whole-document") ("C-x C-x" . "exchange-point-and-mark")
    ("C-x 2" . "split-window-below") ("C-x 3" . "split-window-right")
    ("C-x 0" . "delete-window") ("C-x 1" . "delete-other-windows")
    ("C-x o" . "other-window")
    ("C-x ^" . "enlarge-window") ("C-x -" . "shrink-window")
    ("C-x +" . "balance-windows")
    ("C-x j" . "list-jobs") ("C-x e" . "debugger")
    ("C-h k" . "describe-key") ("C-h b" . "describe-bindings")
    ("C-h m" . "describe-mode") ("C-h c" . "describe-command")
    ("C-h f" . "describe-command") ("C-h v" . "describe-settings")))

(defparameter +code-keys+
  '(("C-M-f" . "forward-sexp") ("C-M-b" . "backward-sexp")
    ("C-c C-l" . "load-file") ("C-c TAB" . "format-document")
    ("C-c C-t" . "set-eval-target")
    ("M-." . "find-definition") ("M-," . "go-back") ("M-?" . "find-references")
    ("M-TAB" . "complete-symbol") ("C-M-i" . "complete-symbol")
    ("C-c C-a" . "arglist") ("C-c C-d" . "describe-symbol")
    ("C-x C-e" . "eval-last-expression") ("C-M-x" . "eval-defun")
    ("C-c C-k" . "eval-document")))

(defparameter +prompt-keys+
  '(("RET" . "answer") ("C-g" . "cancel") ("Escape" . "cancel")
    ("TAB" . "complete")
    ("C-n" . "next-candidate") ("C-p" . "previous-candidate")
    ("Down" . "next-candidate") ("Up" . "previous-candidate")
    ("M-p" . "history-previous") ("M-n" . "history-next")))

(defparameter +listing-keys+
  '(("RET" . "list-activate") ("n" . "list-next") ("p" . "list-previous")
    ("C-n" . "list-next") ("C-p" . "list-previous")
    ("Down" . "list-next") ("Up" . "list-previous")
    ("." . "list-place")))

(defparameter +debugger-keys+
  '(("a" . "debugger-abort") ("q" . "debugger-quit") ("TAB" . "debugger-next")))

(defclass edit (system:system) ()
  (:documentation "Windows onto documents, the chords that act on them, and the
surface a painter shows. A system like any other: nothing in the substrate names
it."))

(system:offers 'edit)

(defclass key-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp))
  (:documentation "Where a key arrives. Writing a chord here is typing it, so a
painter, a test and another pine all press keys the same way."))

(defmethod node:contents ((n key-node)) (key:text (key:pending)))

(defmethod (setf node:contents) (value (n key-node))
  (let ((said nil))
    (dolist (k (key:chord (princ-to-string value)))
      (setf said (keys:dispatch k)))
    (log:note "~a: ~a" value said))
  value)

(defun %keys ()
  (loop :for (chord . name) :in +text-keys+ :do (mode:bind 'mode:text chord name))
  (loop :for (chord . name) :in +code-keys+ :do (mode:bind 'mode:code chord name))
  (loop :for (chord . name) :in +prompt-keys+
        :do (mode:bind 'emode:prompt chord name))
  (loop :for (chord . name) :in +listing-keys+
        :do (mode:bind 'emode:listing chord name))
  (loop :for (chord . name) :in +debugger-keys+
        :do (mode:bind 'emode:debugger chord name))
  (dolist (n '(0 1 2 3 4 5 6 7 8 9))
    (mode:bind 'mode:text (format nil "M-~d" n)
               (format nil "digit-argument-~d" n))
    (mode:bind 'emode:debugger (princ-to-string n)
               (format nil "debugger-restart-~d" n))))

(defun %commands ()
  (commands:commands)
  (file:commands)
  (window:commands)
  (prompt:commands)
  (listing:commands)
  (isearch:commands)
  (help:commands)
  (evaluate:commands)
  (debugger:commands))

(defun %sources ()
  (prompt:source :command
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (c) (cons (command:name c)
                                             (command:describes c)))
                           (command:commands))))
  (prompt:source :document
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (d) (cons (node:name d)
                                             (or (doc:file-of d) "")))
                           (doc:documents))))
  (prompt:source :mode
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (c) (string-downcase (symbol-name (class-name c))))
                           (mode:modes))))
  (prompt:source :setting
                 (lambda (typed)
                   (declare (ignore typed))
                   (mapcar (lambda (each)
                             (cons (string-downcase (string (car each))) (cdr each)))
                           help:+settings+)))
  (prompt:source :file #'prompt:files))

(defun %asking (c)
  (let* ((spec (first (command:asks c)))
         (initial (getf spec :initial)))
    (prompt:ask (or (getf spec :prompt) (format nil "~a: " (command:name c)))
                :category (getf spec :category)
                :history (getf spec :history)
                :must-match (getf spec :must-match)
                :initial (if (functionp initial) (funcall initial) initial)
                :then (lambda (answer) (command:run c (list answer))))
    :asking))

(defun %confirming (question thunk)
  (prompt:ask (format nil "~a " question)
              :candidates (list "yes" "no") :must-match t
              :then (lambda (said) (when (equal "yes" said) (funcall thunk))))
  :asking)

(defun %editing-value (question had write-back)
  (prompt:ask question :initial (princ-to-string (or had ""))
                       :then (lambda (said) (funcall write-back said)))
  :asking)

(defun %frame ()
  "The editor, laid out for whatever is showing it. A painter says how big it is by
writing /surface/editor/size, and this follows that the way it follows anything
else it read."
  (let* ((s (surface:named "editor"))
         (size (and s (surface:size s)))
         (render:*font* (getf size :font)))
    (render:frame :cols (or (getf size :cols) render:*cols*)
                  :lines (or (getf size :lines) render:*lines*))))

(defun type-text (text)
  "Type TEXT a character at a time, the way a keyboard does."
  (loop :for ch :across text
        :do (keys:dispatch (key:make-key (string ch))))
  (doc:point (doc:current)))

(defmethod job:start ((s edit))
  (%commands)
  (%keys)
  (%sources)
  (setf command:*asking* #'%asking
        build:*asking* #'%confirming
        build:*editing* #'%editing-value
        doc:*on-current* #'window:follow
        parser:*showing* #'render:showing
        fault:*on-fault*
        (lambda (f)
          (if (fault:standingp f)
              (ignore-errors (debugger:show (fault:condition-of f) :fault f))
              (log:note "~a" (fault:condition-of f)))))
  (node:attach (make-instance 'key-node :name "key"
                              :describes "write a chord here to type it")
               (tree:root))
  (let ((scratch (or (doc:named "scratch")
                     (doc:make-document "scratch"
                                        :mode (make-instance 'mode:lisp)))))
    (setf (doc:current) scratch)
    (window:seed scratch))
  (surface:builds "editor" #'%frame :as 'surface:window :shown t)
  s)

(defmethod job:stop ((s edit))
  (setf command:*asking* nil
        build:*asking* nil
        build:*editing* nil
        doc:*on-current* nil
        parser:*showing* nil
        fault:*on-fault* nil)
  (key:take-next nil)
  (isearch:took-all)
  (parser:forget-all)
  (dolist (win (window:windows)) (node:detach (node:over win) (node:name win)))
  (tree:erase nil "surface/editor")
  s)
