(defpackage #:pine/edit/commands
  (:use #:cl)
  (:local-nicknames (#:text #:pine/text)
                    (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:fault #:pine/run/fault)
                    (#:command #:pine/run/command) (#:log #:pine/fs/log)
                    (#:mode #:pine/mode)
                    (#:window #:pine/edit/window) (#:render #:pine/edit/render)
                    (#:prompt #:pine/edit/prompt))
  (:export #:kill #:yank #:times #:counting #:*kill-ring*))
(in-package #:pine/edit/commands)

(defvar *kill-ring* nil)
(defvar *kill-kept* 60)
(defvar *count* nil)

(defun %clip ()
  (tree:at nil "dev" "clip" "text"))

(defun kill (string)
  (let ((n (%clip)))
    (when n (fault:attempt (lambda () (setf (node:contents n) string)) "copying")))
  (setf *kill-ring* (d:capped *kill-ring* string *kill-kept*))
  string)

(defun yank ()
  "What to put back: what the desktop is holding when that is not what pine killed
last, so what was copied in another program pastes here."
  (let* ((n (%clip))
         (theirs (and n (fault:attempt (lambda () (node:contents n)) "pasting"))))
    (if (and theirs (plusp (length theirs)) (not (equal theirs (first *kill-ring*))))
        theirs
        (first *kill-ring*))))

(defun counting () *count*)

(defun times (&optional (default 1))
  "How many times the next command runs, and forget it. A prefix argument is spent
by whoever asks for it."
  (let ((held (counting)))
    (setf *count* nil)
    (cond ((null held) default)
          ((eq held :more) 4)
          ((eq held :minus) -1)
          (t held))))

(defun %of () (text:current))

(defun %lines (document) (text:lines document))

(defun %case-word (by)
  (let* ((document (%of)) (line (text:at-line document)) (col (text:at-col document)))
    (multiple-value-bind (to-line to-col)
        (text:move-by :word (%lines document) line col 1)
      (when (= line to-line)
        (let ((word (text:delete-region document line col to-line to-col)))
          (text:insert document (funcall by word)))))))

(defun %go (document kind)
  (or (text:motion document kind
                     (lambda (line col) (text:goto document line col)))
      (log:note "the parse says nothing there")))

(defun %laying-out (document)
  (lambda (targets)
    (loop :for (line . at) :in targets
          :do (text:indent-line document line at))))

(defun %indent (document from to)
  (render:indenting document from to (%laying-out document)))

(command:defcommand "forward-sexp" ()
    (:describes "over the form after point" :on '(code "C-M-f"))
  (%go (%of) :forward-sexp))

(command:defcommand "backward-sexp" ()
    (:describes "back over the form before point" :on '(code "C-M-b"))
  (%go (%of) :backward-sexp))

(command:defcommand "beginning-of-defun" ()
    (:describes "to the top of this definition")
  (%go (%of) :beginning-of-defun))

(command:defcommand "end-of-defun" () (:describes "to the end of this definition")
  (%go (%of) :end-of-defun))

(command:defcommand "mark-sexp" ()
    (:describes "the region is the form after point")
  (let ((document (%of)))
    (text:motion document :forward-sexp
                   (lambda (line col)
                     (setf (text:mark document) (text:point document))
                     (text:goto document line col)))))

(command:defcommand "forward-char" ()
    (:describes "point one character on" :on '(text "C-f" "Right"))
  (text:move (%of) :char (times)))

(command:defcommand "backward-char" ()
    (:describes "point one character back" :on '(text "C-b" "Left"))
  (text:move (%of) :char (- (times))))

(command:defcommand "forward-word" ()
    (:describes "point one word on" :on '(text "M-f"))
  (text:move (%of) :word (times)))

(command:defcommand "backward-word" ()
    (:describes "point one word back" :on '(text "M-b"))
  (text:move (%of) :word (- (times))))

(command:defcommand "next-line" ()
    (:describes "point one line down" :on '(text "C-n" "Down"))
  (text:move (%of) :line (times)))

(command:defcommand "previous-line" ()
    (:describes "point one line up" :on '(text "C-p" "Up"))
  (text:move (%of) :line (- (times))))

(command:defcommand "beginning-of-line" ()
    (:describes "point to column zero" :on '(text "C-a" "Home"))
  (text:goto (%of) (text:at-line (%of)) 0))

(command:defcommand "end-of-line" ()
    (:describes "point to the end of the line" :on '(text "C-e" "End"))
  (text:goto (%of) (text:at-line (%of))
            (length (text:line (%of) (text:at-line (%of))))))

(command:defcommand "beginning-of-document" ()
    (:describes "point to the first line" :on '(text "M-<"))
  (text:goto (%of) 0 0))

(command:defcommand "end-of-document" ()
    (:describes "point to the last line" :on '(text "M->"))
  (text:move (%of) :text 1))

(command:defcommand "goto-line" (line)
    (:describes "point to a line by number"
     :asks '((:prompt "Line: " :as :integer))
     :on '(text "M-g g"))
  (let ((n (if (integerp line)
               line
               (parse-integer (princ-to-string line) :junk-allowed t))))
    (when n (text:goto (%of) (max 0 (1- n)) 0))))

(command:defcommand "universal-argument" ()
    (:describes "the next command, four times" :on '(text "C-u"))
  (d:swap *count* (lambda (had)
                     (cond ((null had) :more)
                           ((eq had :more) 16)
                           ((integerp had) (* had 4))
                           (t :more))))
  (log:note "C-u~@[ ~a~]" (counting)))

(command:defcommand "negative-argument" ()
    (:describes "the next command, backwards" :on '(text "M--"))
  (d:swap *count* (lambda (had) (if (integerp had) (- had) :minus)))
  (log:note "C--"))

(macrolet ((digits ()
             `(progn
                ,@(loop :for n :from 0 :to 9
                        :collect
                        `(command:defcommand ,(format nil "digit-argument-~d" n) ()
                             (:describes "a digit of the count the next command takes"
                              :on '(text ,(format nil "M-~d" n)))
                           (d:swap *count*
                                    (lambda (had)
                                      (cond ((integerp had) (+ (* 10 had) ,n))
                                            ((eq had :minus) (- ,n))
                                            (t ,n))))
                           (log:note "C-u ~a" (counting)))))))
  (digits))

(command:defcommand "newline" ()
    (:describes "break the line at point" :on '(text "RET"))
  (let ((document (%of)))
    (text:newline document)
    (%indent document (text:at-line document) (text:at-line document))
    (text:point document)))

(command:defcommand "indent-line" ()
    (:describes "indent this line as the parse says" :on '(text "TAB"))
  (%indent (%of) (text:at-line (%of)) (text:at-line (%of))))

(command:defcommand "undo" ()
    (:describes "put back what the last edit changed" :on '(text "C-/"))
  (text:undo (%of)))

(command:defcommand "redo" ()
    (:describes "do again what undo put back" :on '(text "C-?"))
  (text:redo (%of)))

(command:defcommand "delete-backward-char" ()
    (:describes "take the character before point" :on '(text "DEL"))
  (text:delete-back (%of)))

(command:defcommand "delete-char" ()
    (:describes "take the character at point" :on '(text "Delete" "C-d"))
  (let* ((document (%of))
         (line (text:at-line document))
         (col (text:at-col document)))
    (if (< col (length (text:line document line)))
        (text:delete-region document line col line (1+ col))
        (when (< (1+ line) (text:line-count document))
          (text:delete-region document line col (1+ line) 0)))))

(command:defcommand "set-mark" ()
    (:describes "put the mark at point" :on '(text "C-SPC"))
  (setf (text:mark (%of)) (text:point (%of))))

(command:defcommand "kill-region" ()
    (:describes "take the region and keep it" :on '(text "C-w"))
  (let* ((document (%of)) (taken (text:region-of document)))
    (when taken
      (kill taken)
      (destructuring-bind (line col) (text:mark document)
        (text:delete-region document line col
                           (text:at-line document) (text:at-col document))
        (setf (text:mark document) nil)))
    taken))

(command:defcommand "copy-region" ()
    (:describes "keep the region without taking it" :on '(text "M-w"))
  (let* ((document (%of)) (taken (text:region-of document)))
    (when taken (kill taken) (setf (text:mark document) nil))
    taken))

(command:defcommand "yank" ()
    (:describes "put back what was killed" :on '(text "C-y"))
  (let ((held (yank)))
    (when held (text:insert (%of) held))
    held))

(command:defcommand "yank-pop" ()
    (:describes "the kill before the one just put back" :on '(text "M-y"))
  (let ((held (second *kill-ring*)))
    (when held
      (setf *kill-ring* (append (rest *kill-ring*) (list (first *kill-ring*))))
      (text:insert (%of) (first *kill-ring*)))
    held))

(command:defcommand "kill-line" ()
    (:describes "take the rest of the line and keep it" :on '(text "C-k"))
  (let* ((document (%of))
         (line (text:at-line document))
         (col (text:at-col document))
         (text (text:line document line)))
    (kill (if (< col (length text))
              (text:delete-region document line col line (length text))
              (when (< (1+ line) (text:line-count document))
                (text:delete-region document line col (1+ line) 0))))))

(command:defcommand "kill-word" ()
    (:describes "take the word after point and keep it" :on '(text "M-d"))
  (let* ((document (%of))
         (line (text:at-line document))
         (col (text:at-col document)))
    (multiple-value-bind (to-line to-col)
        (text:move-by :word (%lines document) line col 1)
      (kill (text:delete-region document line col to-line to-col)))))

(command:defcommand "backward-kill-word" ()
    (:describes "take the word before point and keep it" :on '(text "M-DEL"))
  (let* ((document (%of))
         (line (text:at-line document))
         (col (text:at-col document)))
    (multiple-value-bind (from-line from-col)
        (text:move-by :word (%lines document) line col -1)
      (kill (text:delete-region document from-line from-col line col)))))

(command:defcommand "open-line" ()
    (:describes "a fresh line below, point where it is" :on '(text "C-o"))
  (let ((document (%of)))
    (text:newline document)
    (text:move document :char -1)))

(command:defcommand "transpose-chars" ()
    (:describes "swap the two characters around point" :on '(text "C-t"))
  (let* ((document (%of))
         (line (text:at-line document))
         (col (text:at-col document))
         (text (text:line document line)))
    (when (and (plusp col) (<= col (length text)))
      (let ((taken (text:delete-region document line (1- col) line col)))
        (text:goto document line (min (length (text:line document line)) col))
        (text:insert document taken)))))

(command:defcommand "mark-whole-document" ()
    (:describes "the region is everything" :on '(text "C-x h"))
  (let ((document (%of)))
    (setf (text:mark document) (list 0 0))
    (text:move document :text 1)))

(command:defcommand "exchange-point-and-mark" ()
    (:describes "point and mark swap places" :on '(text "C-x C-x"))
  (let* ((document (%of)) (mark (text:mark document)) (at (text:point document)))
    (when mark
      (setf (text:mark document) at)
      (text:goto document (first mark) (second mark)))))

(command:defcommand "indent-region" ()
    (:describes "indent every line of the region" :on '(text "C-M-\\"))
  (let* ((document (%of)) (span (text:mark document)))
    (when span
      (%indent document
               (min (first span) (text:at-line document))
               (max (first span) (text:at-line document))))))

(command:defcommand "format-document" ()
    (:describes "indent every line of it" :on '(code "C-c TAB"))
  (let ((document (%of)))
    (%indent document 0 (max 0 (1- (text:line-count document))))
    (text:line-count document)))

(command:defcommand "comment-line" ()
    (:describes "comment this line, or uncomment it" :on '(text "M-;"))
  (let* ((document (%of))
         (line (text:at-line document))
         (text (text:line document line))
         (mark (or (mode:setting (text:mode-of document) :comment) ";;"))
         (from (text:leading text))
         (body (subseq text from)))
    (text:goto document line 0)
    (text:delete-region document line 0 line (length text))
    (text:insert document
                (concatenate 'string
                             (make-string from :initial-element #\Space)
                             (if (and (>= (length body) (length mark))
                                      (string= mark body :end2 (length mark)))
                                 (string-left-trim " " (subseq body (length mark)))
                                 (concatenate 'string mark " " body))))))

(command:defcommand "upcase-word" ()
    (:describes "the word after point, in capitals" :on '(text "M-u"))
  (%case-word #'string-upcase))

(command:defcommand "downcase-word" ()
    (:describes "the word after point, in small letters" :on '(text "M-l"))
  (%case-word #'string-downcase))

(command:defcommand "capitalize-word" ()
    (:describes "the word after point, capitalised" :on '(text "M-c"))
  (%case-word #'string-capitalize))

(command:defcommand "insert-tab" () (:describes "a tab's worth of spaces")
  (let* ((document (%of))
         (width (max 1 (or (text:setting document :tab-width) 8))))
    (text:insert document (make-string width :initial-element #\Space))
    width))

(command:defcommand "overwrite" ()
    (:describes "type over what is there" :on '(text "M-o"))
  (let* ((document (%of)) (on (not (text:setting document :overwrite))))
    (setf (text:setting document :overwrite) on)
    (log:note "overwrite is ~:[off~;on~]" on)
    on))

(command:defcommand "refresh" () (:describes "draw everything again")
  (let ((n 0))
    (dolist (each (ui:surfaces) n)
      (fault:attempt (lambda () (node:stir each)) (node:name each))
      (incf n))))

(command:defcommand "keyboard-quit" ()
    (:describes "drop the mark, the prompt and the pending chord"
     :on '(text "C-g" "Escape"))
  (setf (text:mark (%of)) nil)
  (text:forget-spans (%of))
  (if (prompt:askingp) (prompt:cancel) (log:note "quit")))

(command:defcommand "recenter" ()
    (:describes "point to the middle of the window" :on '(text "C-l"))
  (let ((win (window:focused)))
    (setf (window:scroll win)
          (max 0 (- (text:at-line (%of)) (floor (window:lines win) 2))))))

(command:defcommand "scroll-up" ()
    (:describes "a screenful on" :on '(text "C-v" "PageDown"))
  (let* ((win (window:focused)) (document (%of))
         (step (max 1 (- (window:lines win) 2))))
    (setf (window:scroll win)
          (min (max 0 (1- (text:line-count document)))
               (+ (window:scroll win) step)))
    (text:goto document (+ (text:at-line document) step) (text:at-col document))))

(command:defcommand "scroll-down" ()
    (:describes "a screenful back" :on '(text "M-v" "PageUp"))
  (let* ((win (window:focused)) (document (%of))
         (step (max 1 (- (window:lines win) 2))))
    (setf (window:scroll win) (max 0 (- (window:scroll win) step)))
    (text:goto document (max 0 (- (text:at-line document) step))
              (text:at-col document))))
