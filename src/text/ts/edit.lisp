(in-package #:pine/text/ts/runtime)

(defun parse-lines! (ps lines &key edit from viewport)
  "Parse LINES into PS's tree, reading the bytes straight from the seq.

EDIT is (LINE OLD-LINES NEW-LINES BYTE-DELTA): at LINE, OLD-LINES lines became
NEW-LINES lines and the buffer grew by BYTE-DELTA bytes. Given one, the tree is
shifted and reused and the byte index is carried forward; without one, both are
built from scratch.

FROM is the lines the edit was computed against. A shift only means anything
from that state, and more than one thread writes a buffer, so an edit that
describes a step the tree did not take is dropped rather than applied.

VIEWPORT is the (FROM-LINE . TO-LINE) some window shows. Past
+WHOLE-FILE-LINES+ only a band around it is given to tree-sitter at all. The
tree, the index and every position they report are relative to the band's first
line; PS-OFFSET is that line."
  (let* ((band (%band lines viewport))
         (band-lines (%band-lines lines band))
         (same-band (equal band (ps-band ps)))
         (old-tree (ps-tree ps)))
    (when (and old-tree same-band (eq lines (ps-lines ps)) (null edit))
      (return-from parse-lines! ps))
    (%parse-band ps lines band band-lines same-band
                 (when (or (null from) (eq from (ps-lines ps))) edit))))

(defun parse-text! (ps text)
  "Parse TEXT from scratch by splitting it into lines. For callers holding a
string rather than a buffer: a tool, a test, a snippet."
  (parse-lines! ps (uiop:split-string text :separator '(#\Newline))))

(defun %outermost-path (node)
  "NODE, or the path it sits inside. A path's segments are named nodes so they
can be painted, but the reader sees one object, so a structural motion steps
over a whole path rather than through it."
  (loop :with out := node
        :for n := node :then (ts-node-parent n)
        :for depth :from 0 :below 64
        :until (ts-node-is-null n)
        :do (when (string= "ns_path" (ts-node-type n)) (setf out n))
        :finally (return out)))

(defun %forward-sexp-byte (root byte)
  (let ((cur (ts-node-named-descendant-for-byte-range root byte byte)))
    (cond
      ((ts-node-is-null cur) nil)
      ((<= byte (ts-node-start-byte cur)) (ts-node-end-byte cur))
      (t (loop for i from 0 below (ts-node-named-count cur)
               for node = (ts-node-named-nth cur i)
               when (>= (ts-node-start-byte node) byte)
                 return (ts-node-end-byte node)
               finally (return (ts-node-end-byte cur)))))))

(defun %backward-sexp-byte (root byte)
  (let ((cur (ts-node-named-descendant-for-byte-range root byte byte)))
    (cond
      ((ts-node-is-null cur) nil)
      ((>= byte (ts-node-end-byte cur)) (ts-node-start-byte cur))
      (t (loop for i from (1- (ts-node-named-count cur)) downto 0
               for node = (ts-node-named-nth cur i)
               when (<= (ts-node-end-byte node) byte)
                 return (ts-node-start-byte node)
               finally (return (ts-node-start-byte cur)))))))

(defun %defun-bytes (root byte)
  "Start and end bytes of the top-level form containing BYTE, or nil."
  (let ((cur (ts-node-named-descendant-for-byte-range root byte byte)))
    (cond
      ((ts-node-is-null cur) nil)

      (t (loop for p = (ts-node-parent cur)
               until (or (ts-node-is-null p) (ts-node-is-null (ts-node-parent p)))
               do (setf cur p))
         (values (ts-node-start-byte cur) (ts-node-end-byte cur))))))

(defun parse-motion (ps kind line col)
  "A structural target from PS's persistent tree at LINE/COL, no reparse. KIND
is :forward-sexp :backward-sexp :beginning-of-defun :end-of-defun. Answers
(values line col) or nil."
  (let* ((tree (ps-tree ps)) (src (ps-byte-index ps))
         (offset (ps-offset ps))
         (line (- line offset)))
    (when (and tree src (<= 0 line))
      (handler-case
          (let ((byte (pine/text/ts/index:source-byte src line col))
                (root (ts-tree-root-node tree)))
            (flet ((at (b) (when b
                             (multiple-value-bind (l c)
                                 (pine/text/ts/index:source-line-col src b)
                               (values (+ l offset) c)))))
              (ecase kind
                (:forward-sexp (at (%forward-sexp-byte root byte)))
                (:backward-sexp (at (%backward-sexp-byte root byte)))
                (:beginning-of-defun
                 (multiple-value-bind (s e) (%defun-bytes root byte)
                   (declare (ignore e))
                   (at s)))
                (:end-of-defun
                 (multiple-value-bind (s e) (%defun-bytes root byte)
                   (declare (ignore s))
                   (at e))))))

        (error (c)
          (pine/run/fault:report c (format nil "~a from line ~d" kind line))
          nil)))))

(defun char-byte-length (ch)
  "UTF-8 encoded length in bytes of the single character CH."
  (let ((code (char-code ch)))
    (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4))))

(defun byte-length (text)
  (length (sb-ext:string-to-octets text :external-format :utf-8)))

(defun build-line-index (text)
  "Vector of (start-byte . start-char) per line."
  (let ((index (list (cons 0 0))) (bpos 0) (ci 0))
    (loop for ch across text
          do (incf bpos (char-byte-length ch))
             (incf ci)
             (when (char= ch #\Newline) (push (cons bpos ci) index)))
    (coerce (nreverse index) 'vector)))

(defun line-of-byte (byte-pos index)
  "Greatest line whose start byte is <= BYTE-POS."
  (let ((lo 0) (hi (1- (length index))))
    (loop while (< lo hi)
          do (let ((mid (ceiling (+ lo hi) 2)))
               (if (<= (car (aref index mid)) byte-pos)
                   (setf lo mid)
                   (setf hi (1- mid)))))
    lo))

(defun byte-to-line-col (byte-pos index text)
  "Map a UTF-8 BYTE-POS to (values line char-col) using the line INDEX and TEXT."
  (let* ((line (line-of-byte byte-pos index))
         (b (car (aref index line)))
         (ci (cdr (aref index line)))
         (len (length text))
         (col 0))
    (loop while (and (< b byte-pos) (< ci len))
          do (incf b (char-byte-length (char text ci)))
             (incf ci)
             (incf col))
    (values line col)))

(defun pos-to-byte (text line col index)
  "UTF-8 byte offset of the character position LINE/COL."
  (let* ((line (min line (1- (length index))))
         (bpos (car (aref index line)))
         (ci (cdr (aref index line)))
         (len (length text)))
    (loop for k from 0 below col
          while (< ci len)
          do (incf bpos (char-byte-length (char text ci)))
             (incf ci))
    bpos))
