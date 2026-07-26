(defpackage #:pine.editor.motion
  (:use #:cl)
  (:export #:buffer-package #:buffer-ts-language #:line-col->offset #:point->offset #:preceding-sexp-bounds #:sexp-delimiter-p #:move-sexp #:ts-runtime #:cur-buffer #:focused-snap #:move-chars #:move-lines #:move-words))

(in-package #:pine.editor.motion)

;;;; Motion / eval helpers (command implementations)

(defun focused-snap ()
  "The snapshot commands read for point/line context. While a prompt is active
this is the minibuffer's snapshot -- the minibuffer is the current buffer, so a
command like beginning-of-line must see its input, not the window behind it."
  (let ((c (pine.editor.frame:current-client)))
    (if (pine.editor.frame:prompt-active c)
        (pine.editor.frame:minibuffer-snap c)
        (let ((w (pine.editor.frame:focused-window c)))
          (when w (pine.text.window:snap w))))))

(defun cur-buffer () (pine.editor.frame:current-buffer (pine.editor.frame:current-client)))

(defun %fresh-snap ()
  (let ((buf (cur-buffer))) (when buf (pine.editor.ask:ask buf :snapshot))))

(defun buffer-ts-language ()
  (let ((mode (pine.editor.frame:current-buffer-mode)))
    (and (typep mode 'pine.editor.mode:major-mode) (pine.editor.mode:ts-language mode))))

(defun ts-runtime ()
  (pine.core.server:ts-runtime (pine.editor.frame:server-of (pine.editor.frame:current-client))))

(defun move-sexp (kind)
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

(defun move-words (n)
  "Move point N words (negative = backward) across line boundaries."
  (let ((buf (cur-buffer)))
    (when buf (sento.actor:tell buf (list :move-by :unit :word :n n)))))

(defun point->offset (snap)
  (let ((pl (pine.text.buffer:point-line snap))
        (pc (pine.text.buffer:point-col snap))
        (lines (pine.text.buffer:lines snap)))
    (+ (loop for i from 0 below pl sum (1+ (length (fset:@ lines i)))) pc)))

(defun %whitespace-p (c)
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline) (char= c #\Return)))

(defun sexp-delimiter-p (c)
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
    (loop while (and (>= i 0) (not (sexp-delimiter-p (char text i)))) do (decf i))
    (1+ i)))

(defun preceding-sexp-bounds (text end)
  (let ((i (1- end)))
    (loop while (and (>= i 0) (%whitespace-p (char text i))) do (decf i))
    (when (minusp i) (return-from preceding-sexp-bounds nil))
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

(defun buffer-package (state)
  (let ((inferred (%infer-package (pine.text.buffer:state->string state)))
        (name (pine.text.buffer:buffer-local state :package nil)))
    (or inferred (and name (find-package name)) (find-package :cl-user))))

(defun line-col->offset (text line col)
  "Character offset of LINE/COL in TEXT."
  (let ((i 0) (l 0) (n (length text)))
    (loop while (and (< i n) (< l line))
          do (when (char= (char text i) #\Newline) (incf l))
             (incf i))
    (min (+ i col) n)))
