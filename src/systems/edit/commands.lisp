(defpackage #:pine/edit/commands
  (:use #:pine/user)
  (:local-nicknames (#:node #:pine/fs/node) (#:d #:pine/data))
  (:export))
(in-package #:pine/edit/commands)

(named-readtables:in-readtable pine/fs/reader:syntax)

(defvar *kill-ring* nil)
(defvar *kill-kept* 60)
(defvar *count* nil)

(defun %clip ()
  (at "/dev/clip" "text"))

(defun kill (string)
  (let ((n (%clip)))
    (when n (attempt (lambda () (setf (contents n) string)) "copying")))
  (setf *kill-ring* (d:capped *kill-ring* string *kill-kept*))
  string)

(defun yank ()
  "What to put back: what the desktop is holding when that is not what pine killed
last, so what was copied in another program pastes here."
  (let* ((n (%clip))
         (theirs (and n (attempt (lambda () (contents n)) "pasting"))))
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

(defun %case-word (by)
  (let* ((document (current)) (line (at-line document)) (col (at-col document)))
    (multiple-value-bind (to-line to-col)
        (move-by :word (lines document) line col 1)
      (when (= line to-line)
        (let ((word (delete-region document line col to-line to-col)))
          (insert document (funcall by word)))))))

(defun %go (document kind)
  (or (motion document kind
                     (lambda (line col) (goto document line col)))
      (note "the parse says nothing there")))

(defun %laying-out (document)
  (lambda (targets)
    (loop :for (line . at) :in targets
          :do (indent-line document line at))))

(defun %indent (document from to)
  (indenting document from to (%laying-out document)))

(defcommand "forward-sexp" ()
    (:describes "over the form after point" :on '(code "C-M-f"))
  (%go (current) :forward-sexp))

(defcommand "backward-sexp" ()
    (:describes "back over the form before point" :on '(code "C-M-b"))
  (%go (current) :backward-sexp))

(defcommand "beginning-of-defun" ()
    (:describes "to the top of this definition")
  (%go (current) :beginning-of-defun))

(defcommand "end-of-defun" () (:describes "to the end of this definition")
  (%go (current) :end-of-defun))

(defcommand "mark-sexp" ()
    (:describes "the region is the form after point")
  (let ((document (current)))
    (motion document :forward-sexp
                   (lambda (line col)
                     (setf (mark document) (point document))
                     (goto document line col)))))

(defcommand "forward-char" ()
    (:describes "point one character on" :on '(text "C-f" "Right"))
  (move (current) :char (times)))

(defcommand "backward-char" ()
    (:describes "point one character back" :on '(text "C-b" "Left"))
  (move (current) :char (- (times))))

(defcommand "forward-word" ()
    (:describes "point one word on" :on '(text "M-f"))
  (move (current) :word (times)))

(defcommand "backward-word" ()
    (:describes "point one word back" :on '(text "M-b"))
  (move (current) :word (- (times))))

(defcommand "next-line" ()
    (:describes "point one line down" :on '(text "C-n" "Down"))
  (move (current) :line (times)))

(defcommand "previous-line" ()
    (:describes "point one line up" :on '(text "C-p" "Up"))
  (move (current) :line (- (times))))

(defcommand "beginning-of-line" ()
    (:describes "point to column zero" :on '(text "C-a" "Home"))
  (goto (current) (at-line (current)) 0))

(defcommand "end-of-line" ()
    (:describes "point to the end of the line" :on '(text "C-e" "End"))
  (goto (current) (at-line (current))
            (length (line (current) (at-line (current))))))

(defcommand "beginning-of-document" ()
    (:describes "point to the first line" :on '(text "M-<"))
  (goto (current) 0 0))

(defcommand "end-of-document" ()
    (:describes "point to the last line" :on '(text "M->"))
  (move (current) :text 1))

(defcommand "goto-line" (line)
    (:describes "point to a line by number"
     :asks '((:prompt "Line: " :as :integer))
     :on '(text "M-g g"))
  (let ((n (if (integerp line)
               line
               (parse-integer (princ-to-string line) :junk-allowed t))))
    (when n (goto (current) (max 0 (1- n)) 0))))

(defcommand "universal-argument" ()
    (:describes "the next command, four times" :on '(text "C-u"))
  (d:swap *count* (lambda (had)
                     (cond ((null had) :more)
                           ((eq had :more) 16)
                           ((integerp had) (* had 4))
                           (t :more))))
  (note "C-u~@[ ~a~]" (counting)))

(defcommand "negative-argument" ()
    (:describes "the next command, backwards" :on '(text "M--"))
  (d:swap *count* (lambda (had) (if (integerp had) (- had) :minus)))
  (note "C--"))

(macrolet ((digits ()
             `(progn
                ,@(loop :for n :from 0 :to 9
                        :collect
                        `(defcommand ,(format nil "digit-argument-~d" n) ()
                             (:describes "a digit of the count the next command takes"
                              :on '(text ,(format nil "M-~d" n)))
                           (d:swap *count*
                                    (lambda (had)
                                      (cond ((integerp had) (+ (* 10 had) ,n))
                                            ((eq had :minus) (- ,n))
                                            (t ,n))))
                           (note "C-u ~a" (counting)))))))
  (digits))

(defcommand "newline" ()
    (:describes "break the line at point" :on '(text "RET"))
  (let ((document (current)))
    (newline document)
    (%indent document (at-line document) (at-line document))
    (point document)))

(defcommand "indent-line" ()
    (:describes "indent this line as the parse says" :on '(text "TAB"))
  (%indent (current) (at-line (current)) (at-line (current))))

(defcommand "undo" ()
    (:describes "put back what the last edit changed" :on '(text "C-/"))
  (undo (current)))

(defcommand "redo" ()
    (:describes "do again what undo put back" :on '(text "C-?"))
  (redo (current)))

(defcommand "delete-backward-char" ()
    (:describes "take the character before point" :on '(text "DEL"))
  (delete-back (current)))

(defcommand "delete-char" ()
    (:describes "take the character at point" :on '(text "Delete" "C-d"))
  (let* ((document (current))
         (line (at-line document))
         (col (at-col document)))
    (if (< col (length (line document line)))
        (delete-region document line col line (1+ col))
        (when (< (1+ line) (line-count document))
          (delete-region document line col (1+ line) 0)))))

(defcommand "set-mark" ()
    (:describes "put the mark at point" :on '(text "C-SPC"))
  (setf (mark (current)) (point (current))))

(defcommand "kill-region" ()
    (:describes "take the region and keep it" :on '(text "C-w"))
  (let* ((document (current)) (taken (region-of document)))
    (when taken
      (kill taken)
      (destructuring-bind (line col) (mark document)
        (delete-region document line col
                           (at-line document) (at-col document))
        (setf (mark document) nil)))
    taken))

(defcommand "copy-region" ()
    (:describes "keep the region without taking it" :on '(text "M-w"))
  (let* ((document (current)) (taken (region-of document)))
    (when taken (kill taken) (setf (mark document) nil))
    taken))

(defcommand "yank" ()
    (:describes "put back what was killed" :on '(text "C-y"))
  (let ((held (yank)))
    (when held (insert (current) held))
    held))

(defcommand "yank-pop" ()
    (:describes "the kill before the one just put back" :on '(text "M-y"))
  (let ((held (second *kill-ring*)))
    (when held
      (setf *kill-ring* (append (rest *kill-ring*) (list (first *kill-ring*))))
      (insert (current) (first *kill-ring*)))
    held))

(defcommand "kill-line" ()
    (:describes "take the rest of the line and keep it" :on '(text "C-k"))
  (let* ((document (current))
         (line (at-line document))
         (col (at-col document))
         (text (line document line)))
    (kill (if (< col (length text))
              (delete-region document line col line (length text))
              (when (< (1+ line) (line-count document))
                (delete-region document line col (1+ line) 0))))))

(defcommand "kill-word" ()
    (:describes "take the word after point and keep it" :on '(text "M-d"))
  (let* ((document (current))
         (line (at-line document))
         (col (at-col document)))
    (multiple-value-bind (to-line to-col)
        (move-by :word (lines document) line col 1)
      (kill (delete-region document line col to-line to-col)))))

(defcommand "backward-kill-word" ()
    (:describes "take the word before point and keep it" :on '(text "M-DEL"))
  (let* ((document (current))
         (line (at-line document))
         (col (at-col document)))
    (multiple-value-bind (from-line from-col)
        (move-by :word (lines document) line col -1)
      (kill (delete-region document from-line from-col line col)))))

(defcommand "open-line" ()
    (:describes "a fresh line below, point where it is" :on '(text "C-o"))
  (let ((document (current)))
    (newline document)
    (move document :char -1)))

(defcommand "transpose-chars" ()
    (:describes "swap the two characters around point" :on '(text "C-t"))
  (let* ((document (current))
         (line (at-line document))
         (col (at-col document))
         (text (line document line)))
    (when (and (plusp col) (<= col (length text)))
      (let ((taken (delete-region document line (1- col) line col)))
        (goto document line (min (length (line document line)) col))
        (insert document taken)))))

(defcommand "mark-whole-document" ()
    (:describes "the region is everything" :on '(text "C-x h"))
  (let ((document (current)))
    (setf (mark document) (list 0 0))
    (move document :text 1)))

(defcommand "exchange-point-and-mark" ()
    (:describes "point and mark swap places" :on '(text "C-x C-x"))
  (let* ((document (current)) (mark (mark document)) (at (point document)))
    (when mark
      (setf (mark document) at)
      (goto document (first mark) (second mark)))))

(defcommand "indent-region" ()
    (:describes "indent every line of the region" :on '(text "C-M-\\"))
  (let* ((document (current)) (span (mark document)))
    (when span
      (%indent document
               (min (first span) (at-line document))
               (max (first span) (at-line document))))))

(defcommand "format-document" ()
    (:describes "indent every line of it" :on '(code "C-c TAB"))
  (let ((document (current)))
    (%indent document 0 (max 0 (1- (line-count document))))
    (line-count document)))

(defcommand "comment-line" ()
    (:describes "comment this line, or uncomment it" :on '(text "M-;"))
  (let* ((document (current))
         (line (at-line document))
         (text (line document line))
         (mark (says (mode-of document) :comment ";;"))
         (from (leading text))
         (body (subseq text from)))
    (goto document line 0)
    (delete-region document line 0 line (length text))
    (insert document
                (concatenate 'string
                             (make-string from :initial-element #\Space)
                             (if (and (>= (length body) (length mark))
                                      (string= mark body :end2 (length mark)))
                                 (string-left-trim " " (subseq body (length mark)))
                                 (concatenate 'string mark " " body))))))

(defcommand "upcase-word" ()
    (:describes "the word after point, in capitals" :on '(text "M-u"))
  (%case-word #'string-upcase))

(defcommand "downcase-word" ()
    (:describes "the word after point, in small letters" :on '(text "M-l"))
  (%case-word #'string-downcase))

(defcommand "capitalize-word" ()
    (:describes "the word after point, capitalised" :on '(text "M-c"))
  (%case-word #'string-capitalize))

(defcommand "insert-tab" () (:describes "a tab's worth of spaces")
  (let* ((document (current))
         (width (max 1 (says document :tab-width 8))))
    (insert document (make-string width :initial-element #\Space))
    width))

(defcommand "overwrite" ()
    (:describes "type over what is there" :on '(text "M-o"))
  (let* ((document (current)) (on (not (says document :overwrite nil))))
    (setf (setting document :overwrite) on)
    (note "overwrite is ~:[off~;on~]" on)
    on))

(defcommand "refresh" () (:describes "draw everything again")
  (let ((n 0))
    (dolist (each (surfaces) n)
      (attempt (lambda () (node:moved each)) (name each))
      (incf n))))

(defcommand "keyboard-quit" ()
    (:describes "drop the mark, the prompt and the pending chord"
     :on '(text "C-g" "Escape"))
  (setf (mark (current)) nil)
  (forget-spans (current))
  (if (askingp) (cancel) (note "quit")))

(defcommand "recenter" ()
    (:describes "point to the middle of the window" :on '(text "C-l"))
  (let ((win (focused)))
    (setf (scrolled win)
          (max 0 (- (at-line (current)) (floor (down win) 2))))
    (node:moved win)))

(defcommand "scroll-up" ()
    (:describes "a screenful on" :on '(text "C-v" "PageDown"))
  (let* ((win (focused)) (document (current))
         (step (max 1 (- (down win) 2))))
    (setf (scrolled win)
          (min (max 0 (1- (line-count document)))
               (+ (scrolled win) step)))
    (goto document (+ (at-line document) step) (at-col document))))

(defcommand "scroll-down" ()
    (:describes "a screenful back" :on '(text "M-v" "PageUp"))
  (let* ((win (focused)) (document (current))
         (step (max 1 (- (down win) 2))))
    (setf (scrolled win) (max 0 (- (scrolled win) step)))
    (goto document (max 0 (- (at-line document) step))
              (at-col document))))
