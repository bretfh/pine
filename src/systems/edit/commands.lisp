(defpackage #:pine/edit/commands
  (:use #:cl #:pine)
  (:shadowing-import-from #:pine #:read #:write #:map #:set)
  (:import-from #:pine/edit #:askingp #:cancel #:height #:focused #:scrolled #:indenting)
  (:import-from #:pine/mode #:says #:setting)
  (:import-from #:pine/text
   #:at-col #:at-line #:current #:delete-back #:delete-region #:buffer
   #:forget-spans #:goto #:indent-line #:insert #:leading #:line #:line-count
   #:lines #:mark #:mode-of #:motion #:move #:move-by #:newline #:point #:redo
   #:region-of #:span #:undo)
  (:import-from #:pine/ui #:surfaces)
  (:local-nicknames (#:fs #:pine/fs) (#:d #:pine/data))
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
  (let* ((n (%clip))
         (theirs (and n (attempt (lambda () (contents n)) "pasting"))))
    (if (and theirs (plusp (length theirs)) (not (equal theirs (first *kill-ring*))))
        theirs
        (first *kill-ring*))))

(defun counting () *count*)

(defun times (&optional (default 1))
  (let ((held (counting)))
    (setf *count* nil)
    (cond ((null held) default)
          ((eq held :more) 4)
          ((eq held :minus) -1)
          (t held))))

(defun %case-word (by)
  (let* ((buffer (current)) (line (at-line buffer)) (col (at-col buffer)))
    (multiple-value-bind (to-line to-col)
        (move-by :word (lines buffer) line col 1)
      (when (= line to-line)
        (let ((word (delete-region buffer line col to-line to-col)))
          (insert buffer (funcall by word)))))))

(defun %go (buffer kind)
  (or (motion buffer kind
                     (lambda (line col) (goto buffer line col)))
      (note "the parse says nothing there")))

(defun %laying-out (buffer)
  (lambda (targets)
    (loop :for (line . at) :in targets
          :do (indent-line buffer line at))))

(defun %indent (buffer from to)
  (indenting buffer from to (%laying-out buffer)))

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
  (let ((buffer (current)))
    (motion buffer :forward-sexp
                   (lambda (line col)
                     (setf (mark buffer) (point buffer))
                     (goto buffer line col)))))

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

(defcommand "beginning-of-buffer" ()
    (:describes "point to the first line" :on '(text "M-<"))
  (goto (current) 0 0))

(defcommand "end-of-buffer" ()
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
  (sb-ext:atomic-update *count* (lambda (had)
                     (cond ((null had) :more)
                           ((eq had :more) 16)
                           ((integerp had) (* had 4))
                           (t :more))))
  (note "C-u~@[ ~a~]" (counting)))

(defcommand "negative-argument" ()
    (:describes "the next command, backwards" :on '(text "M--"))
  (sb-ext:atomic-update *count* (lambda (had) (if (integerp had) (- had) :minus)))
  (note "C--"))

(macrolet ((digits ()
             `(progn
                ,@(loop :for n :from 0 :to 9
                        :collect
                        `(defcommand ,(format nil "digit-argument-~d" n) ()
                             (:describes "a digit of the count the next command takes"
                              :on '(text ,(format nil "M-~d" n)))
                           (sb-ext:atomic-update *count*
                                    (lambda (had)
                                      (cond ((integerp had) (+ (* 10 had) ,n))
                                            ((eq had :minus) (- ,n))
                                            (t ,n))))
                           (note "C-u ~a" (counting)))))))
  (digits))

(defcommand "newline" ()
    (:describes "break the line at point" :on '(text "RET"))
  (let ((buffer (current)))
    (newline buffer)
    (%indent buffer (at-line buffer) (at-line buffer))
    (point buffer)))

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
  (let* ((buffer (current))
         (line (at-line buffer))
         (col (at-col buffer)))
    (if (< col (length (line buffer line)))
        (delete-region buffer line col line (1+ col))
        (when (< (1+ line) (line-count buffer))
          (delete-region buffer line col (1+ line) 0)))))

(defcommand "set-mark" ()
    (:describes "put the mark at point" :on '(text "C-SPC"))
  (setf (mark (current)) (point (current))))

(defcommand "kill-region" ()
    (:describes "take the region and keep it" :on '(text "C-w"))
  (let* ((buffer (current)) (taken (region-of buffer)))
    (when taken
      (kill taken)
      (destructuring-bind (line col) (mark buffer)
        (delete-region buffer line col
                           (at-line buffer) (at-col buffer))
        (setf (mark buffer) nil)))
    taken))

(defcommand "copy-region" ()
    (:describes "keep the region without taking it" :on '(text "M-w"))
  (let* ((buffer (current)) (taken (region-of buffer)))
    (when taken (kill taken) (setf (mark buffer) nil))
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
  (let* ((buffer (current))
         (line (at-line buffer))
         (col (at-col buffer))
         (text (line buffer line)))
    (kill (if (< col (length text))
              (delete-region buffer line col line (length text))
              (when (< (1+ line) (line-count buffer))
                (delete-region buffer line col (1+ line) 0))))))

(defcommand "kill-word" ()
    (:describes "take the word after point and keep it" :on '(text "M-d"))
  (let* ((buffer (current))
         (line (at-line buffer))
         (col (at-col buffer)))
    (multiple-value-bind (to-line to-col)
        (move-by :word (lines buffer) line col 1)
      (kill (delete-region buffer line col to-line to-col)))))

(defcommand "backward-kill-word" ()
    (:describes "take the word before point and keep it" :on '(text "M-DEL"))
  (let* ((buffer (current))
         (line (at-line buffer))
         (col (at-col buffer)))
    (multiple-value-bind (from-line from-col)
        (move-by :word (lines buffer) line col -1)
      (kill (delete-region buffer from-line from-col line col)))))

(defcommand "open-line" ()
    (:describes "a fresh line below, point where it is" :on '(text "C-o"))
  (let ((buffer (current)))
    (newline buffer)
    (move buffer :char -1)))

(defcommand "transpose-chars" ()
    (:describes "swap the two characters around point" :on '(text "C-t"))
  (let* ((buffer (current))
         (line (at-line buffer))
         (col (at-col buffer))
         (text (line buffer line)))
    (when (and (plusp col) (<= col (length text)))
      (let ((taken (delete-region buffer line (1- col) line col)))
        (goto buffer line (min (length (line buffer line)) col))
        (insert buffer taken)))))

(defcommand "mark-whole-buffer" ()
    (:describes "the region is everything" :on '(text "C-x h"))
  (let ((buffer (current)))
    (setf (mark buffer) (list 0 0))
    (move buffer :text 1)))

(defcommand "exchange-point-and-mark" ()
    (:describes "point and mark swap places" :on '(text "C-x C-x"))
  (let* ((buffer (current)) (mark (mark buffer)) (at (point buffer)))
    (when mark
      (setf (mark buffer) at)
      (goto buffer (first mark) (second mark)))))

(defcommand "indent-region" ()
    (:describes "indent every line of the region" :on '(text "C-M-\\"))
  (let* ((buffer (current)) (span (mark buffer)))
    (when span
      (%indent buffer
               (min (first span) (at-line buffer))
               (max (first span) (at-line buffer))))))

(defcommand "format-buffer" ()
    (:describes "indent every line of it" :on '(code "C-c TAB"))
  (let ((buffer (current)))
    (%indent buffer 0 (max 0 (1- (line-count buffer))))
    (line-count buffer)))

(defcommand "comment-line" ()
    (:describes "comment this line, or uncomment it" :on '(text "M-;"))
  (let* ((buffer (current))
         (line (at-line buffer))
         (text (line buffer line))
         (mark (says (mode-of buffer) :comment ";;"))
         (from (leading text))
         (body (subseq text from)))
    (goto buffer line 0)
    (delete-region buffer line 0 line (length text))
    (insert buffer
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
  (let* ((buffer (current))
         (width (max 1 (says buffer :tab-width 8))))
    (insert buffer (make-string width :initial-element #\Space))
    width))

(defcommand "overwrite" ()
    (:describes "type over what is there" :on '(text "M-o"))
  (let* ((buffer (current)) (on (not (says buffer :overwrite nil))))
    (setf (setting buffer :overwrite) on)
    (note "overwrite is ~:[off~;on~]" on)
    on))

(defcommand "refresh" () (:describes "draw everything again")
  (let ((n 0))
    (dolist (each (surfaces) n)
      (attempt (lambda () (fs:touch each)) (name each))
      (incf n))))

(defcommand "keyboard-quit" ()
    (:describes "drop the mark, the prompt and the pending chord"
     :on '(text "C-g" "Escape"))
  (setf (mark (current)) nil)
  (forget-spans (current))
  (if (askingp) (cancel) (note "quit")))

(defcommand "recenter" ()
    (:describes "point to the middle of the pane" :on '(text "C-l"))
  (let ((win (focused)))
    (setf (scrolled win)
          (max 0 (- (at-line (current)) (floor (height win) 2))))
    (fs:touch win)))

(defcommand "scroll-up" ()
    (:describes "a screenful on" :on '(text "C-v" "PageDown"))
  (let* ((win (focused)) (buffer (current))
         (step (max 1 (- (height win) 2))))
    (setf (scrolled win)
          (min (max 0 (1- (line-count buffer)))
               (+ (scrolled win) step)))
    (goto buffer (+ (at-line buffer) step) (at-col buffer))))

(defcommand "scroll-down" ()
    (:describes "a screenful back" :on '(text "M-v" "PageUp"))
  (let* ((win (focused)) (buffer (current))
         (step (max 1 (- (height win) 2))))
    (setf (scrolled win) (max 0 (- (scrolled win) step)))
    (goto buffer (max 0 (- (at-line buffer) step))
              (at-col buffer))))
