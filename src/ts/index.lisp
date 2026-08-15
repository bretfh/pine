(defpackage #:pine/ts/index
  (:use #:cl)
  (:export
   #:byte-index
   #:build-index
   #:byte-index-lines
   #:byte-index-pending
   #:index-total
   #:index-line-count
   #:line-start
   #:line-bytes
   #:byte-line
   #:index-edit
   #:compact-index
   #:string-bytes

   #:line-string
   #:source-line-col
   #:source-byte
   #:source-substring
   #:source-char-at)
  (:documentation "Byte offsets over a buffer's lines, without a flat string.

Tree-sitter asks for the bytes at an offset; the buffer is an fset seq of lines.
This is the map between them, and it has to survive an edit without walking the
file: an edit records a pending shift instead, and the base is rebuilt once the
pending list is long enough to be worth it."))
(in-package #:pine/ts/index)

(defparameter +compact-after+ 64
  "Pending edits to carry before rebuilding the base. A query costs one pass over
the pending list, and a rebuild costs a pass over the file, so this trades a
bounded per-query cost against an amortised per-edit one.")

(defstruct (byte-index (:constructor %make-index))

  (base-lines nil)

  (starts (make-array 0 :element-type '(unsigned-byte 62)) :type (simple-array (unsigned-byte 62) (*)))
  (base-total 0 :type (unsigned-byte 62))

  (lines nil)
  (pending nil :type list)
  (memo-line -1 :type fixnum)
  (memo-text "" :type string)
  (memo-offset 0 :type (unsigned-byte 62))
  (memo-col 0 :type (unsigned-byte 62)))

(declaim (inline %char-bytes))
(defun %char-bytes (ch)
  (let ((code (char-code ch)))
    (cond ((< code #x80) 1)
          ((< code #x800) 2)
          ((< code #x10000) 3)
          (t 4))))

(defun string-bytes (string)
  "STRING's length in UTF-8 bytes, counted rather than encoded."
  (let ((n 0))
    (declare (type (unsigned-byte 62) n))
    (loop :for ch :across string :do (incf n (%char-bytes ch)))
    n))

(defun %line-byte-length (line) (string-bytes line))

(defun build-index (lines)
  "A fresh index over LINES, with no pending edits."
  (let* ((n (pine/data:size lines))
         (starts (make-array (1+ n) :element-type '(unsigned-byte 62)))
         (offset 0)
         (i 0))
    (declare (type (unsigned-byte 62) offset))
    (pine/data:do-each (line lines)
      (setf (aref starts i) offset)
      (incf offset (%line-byte-length line))
      (when (< i (1- n)) (incf offset))
      (incf i))
    (setf (aref starts n) offset)
    (%make-index :base-lines lines :starts starts :base-total offset :lines lines)))

(defun index-line-count (index)
  (pine/data:size (byte-index-lines index)))

(defun %shift (index line)
  "The byte shift the pending edits apply at LINE, and the base line LINE came
from."
  (let ((shift 0) (base-line line))
    (declare (type integer shift))
    (dolist (edit (byte-index-pending index) (values shift base-line))
      (destructuring-bind (at byte-delta line-delta) edit
        (when (> base-line at)
          (incf shift byte-delta)
          (decf base-line line-delta))))))

(defun line-start (index line)
  "The byte offset LINE begins at."
  (let ((starts (byte-index-starts index)))
    (multiple-value-bind (shift base-line) (%shift index line)
      (let ((clamped (max 0 (min base-line (1- (length starts))))))
        (max 0 (+ (aref starts clamped) shift))))))

(defun index-total (index)
  "The whole buffer's byte length."
  (let ((shift 0))
    (dolist (edit (byte-index-pending index))
      (incf shift (second edit)))
    (max 0 (+ (byte-index-base-total index) shift))))

(defun line-bytes (index line)
  "LINE's own byte length, without its newline."
  (let ((lines (byte-index-lines index)))
    (if (< line (pine/data:size lines))
        (%line-byte-length (pine/data:at lines line))
        0)))

(defun byte-line (index byte)
  "The line BYTE falls in, and its byte offset within that line."
  (let ((n (index-line-count index)))
    (if (zerop n)
        (values 0 0)

        (let ((lo 0) (hi (1- n)))
          (loop :while (< lo hi)
                :do (let ((mid (floor (+ lo hi 1) 2)))
                      (if (<= (line-start index mid) byte)
                          (setf lo mid)
                          (setf hi (1- mid)))))
          (values lo (max 0 (- byte (line-start index lo))))))))

(defun %byte-offset-to-col (line offset)
  "(values COL BYTES): the character column OFFSET bytes into LINE, and the byte
offset COL itself begins at. The two differ when OFFSET falls inside a character,
and only the pair is safe to resume a count from."
  (let ((bytes 0) (col 0))
    (loop :for ch :across line
          :while (< bytes offset)
          :do (incf bytes (%char-bytes ch))
              (incf col))
    (values col bytes)))

(defun %col-to-byte-offset (line col)
  "The byte offset COL characters into LINE."
  (let ((bytes 0))
    (loop :for i :below (min col (length line))
          :do (incf bytes (%char-bytes (char line i))))
    bytes))

(defun line-string (index line)
  "LINE's text, or the empty string past the end. Memoised on the last line
asked for, which a walk asks about once per node."
  (if (= line (byte-index-memo-line index))
      (byte-index-memo-text index)
      (let* ((lines (byte-index-lines index))
             (text (if (and (>= line 0) (< line (pine/data:size lines)))
                       (pine/data:at lines line)
                       "")))
        (setf (byte-index-memo-line index) line
              (byte-index-memo-text index) text
              (byte-index-memo-offset index) 0
              (byte-index-memo-col index) 0)
        text)))

(defun %col-at (index line offset)
  "The character column OFFSET bytes into LINE.

Carried forward from the last column asked for on this line: a walk emits spans
left to right, so counting resumes rather than starting over."
  (let ((text (line-string index line)))
    (if (>= offset (byte-index-memo-offset index))
        (let ((bytes (byte-index-memo-offset index))
              (col (byte-index-memo-col index)))
          (loop :while (and (< bytes offset) (< col (length text)))
                :do (incf bytes (%char-bytes (char text col)))
                    (incf col))
          (setf (byte-index-memo-offset index) bytes
                (byte-index-memo-col index) col)
          col)
        (multiple-value-bind (col bytes) (%byte-offset-to-col text offset)
          (setf (byte-index-memo-offset index) bytes
                (byte-index-memo-col index) col)
          col))))

(defun source-line-col (index byte)
  "The (values line character-column) that BYTE falls at."
  (multiple-value-bind (line offset) (byte-line index byte)
    (values line (%col-at index line offset))))

(defun source-byte (index line col)
  "The byte offset of LINE at character column COL."
  (+ (line-start index line) (%col-to-byte-offset (line-string index line) col)))

(defun source-substring (index start-byte end-byte)
  "The characters between two byte offsets, spanning lines, newlines included."
  (if (>= start-byte end-byte)
      ""
      (multiple-value-bind (start-line start-offset) (byte-line index start-byte)
        (multiple-value-bind (end-line end-offset) (byte-line index end-byte)
          (when (= start-line end-line)
            (let ((text (line-string index start-line))
                  (from (%col-at index start-line start-offset))
                  (to (%col-at index start-line end-offset)))
              (return-from source-substring
                (subseq text (min from (length text)) (min to (length text))))))
          (with-output-to-string (out)
            (loop :for line :from start-line :to (min end-line (1- (index-line-count index)))
                  :for text = (line-string index line)
                  :do (let ((from (if (= line start-line)
                                      (%byte-offset-to-col text start-offset)
                                      0))
                            (to (if (= line end-line)
                                    (%byte-offset-to-col text end-offset)
                                    (length text))))
                        (write-string text out :start (min from (length text))
                                               :end (min to (length text)))
                        (when (< line end-line) (write-char #\Newline out)))))))))

(defun source-char-at (index byte)
  "The character at BYTE, or nil past the end."
  (multiple-value-bind (line offset) (byte-line index byte)
    (let* ((text (line-string index line))
           (col (%col-at index line offset)))
      (cond ((< col (length text)) (char text col))
            ((< line (1- (index-line-count index))) #\Newline)
            (t nil)))))

(defun forget-line (index)
  "Drop the memoised line, so the next question about one reads it afresh."
  (setf (byte-index-memo-line index) -1
        (byte-index-memo-text index) ""
        (byte-index-memo-offset index) 0
        (byte-index-memo-col index) 0)
  index)

(defun index-edit (index lines line byte-delta line-delta)
  "INDEX over LINES after an edit at LINE that changed the buffer by BYTE-DELTA
bytes and LINE-DELTA lines.

A pending shift can only move whole lines, so it describes an edit within one
line and nothing else. A newline splits a line and the new one starts in the
middle of the old, at an offset no shift can name; a join is the same in reverse.
Those rebuild the base. That is a pass over the file, but it happens on the
parser's thread and only when the line count actually changes, while ordinary
typing stays a cons."
  (if (not (zerop line-delta))
      (build-index lines)
      (let ((pending (cons (list line byte-delta line-delta)
                           (byte-index-pending index))))
        (if (>= (length pending) +compact-after+)
            (build-index lines)
            (progn (setf (byte-index-lines index) lines
                         (byte-index-pending index) pending)

                   (forget-line index)
                   index)))))

(defun compact-index (index)
  "Rebuild INDEX's base from the lines it now describes."
  (build-index (byte-index-lines index)))
