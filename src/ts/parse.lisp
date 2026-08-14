(in-package #:pine.ts.runtime)

(defun %grammar-of (language)
  (if *grammar-of* (funcall *grammar-of* language) (values nil nil)))

(defun load-language-entry (language library fn-name)
  (progn
    (unless library (return-from load-language-entry nil))
    (let ((lang (grammar-language-pointer library fn-name)))
      (when (and lang (not (cffi:null-pointer-p lang)))
        (let ((parser (ts-parser-new)))
          (when (claim-language parser lang language)
            (make-instance 'ts-entry :parser parser :language-ptr lang)))))))

(defun %grammars (runtime)
  (pine.data:held (grammars runtime)))

(defun %note-loaded (runtime language entry)
  (pine.data:swap!
   (grammars runtime)
   (lambda (state)
     (pl:with state :loaded
              (pl:with (pl:at state :loaded) language entry)))))

(defun %note-missing (runtime language)
  (pine.data:swap!
   (grammars runtime)
   (lambda (state)
     (pl:with state :missing
              (pl:with (pl:at state :missing) language)))))

(defun ensure-language (runtime language &optional lib fn)
  "LANGUAGE's ts-entry, loaded the first time it is asked for, or NIL when its
grammar is not here. A grammar already loaded is a slot read; only the loading
itself is one thread at a time, because the loader's table is one table.

The library and the C function come from the language's own declaration, so
what grammars exist is something written rather than a list compiled in here."
  (multiple-value-bind (library fn-name)
      (if lib (values lib fn) (%grammar-of language))
  (or (pl:at (pl:at (%grammars runtime) :loaded) language)
      (bordeaux-threads:with-recursive-lock-held ((loading runtime))
        (unless (libs-loaded runtime) (ensure-ts runtime))
        (when (libs-loaded runtime)
          (let ((state (%grammars runtime)))

            (or (pl:at (pl:at state :loaded) language)
                (unless (pl:contains (pl:at state :missing) language)
                  (let ((entry (load-language-entry language library fn-name)))
                    (cond
                      (entry (%note-loaded runtime language entry)
                             (pl:at (pl:at (%grammars runtime) :loaded)
                                          language))
                      (t (%note-missing runtime language)
                         (pine.run.fault:report
                          (make-condition 'simple-error
                                          :format-control "no ~(~a~) grammar to load"
                                          :format-arguments (list language))
                          "loading a grammar")
                         nil)))))))))))
(defun make-parse-state (runtime language &optional lib fn &key syntax package)
"A parse-state for LANGUAGE, or nil if the grammar is unavailable. SYNTAX is
the compiled rules the walk follows; PACKAGE is the one a head symbol resolves
in, so a macro defined in the buffer's own package is found."
  (let ((entry (ensure-language runtime language lib fn)))
    (when entry
      (let ((parser (ts-parser-new)))
        (when (claim-language parser (entry-language-ptr entry) language)
          (make-instance 'parse-state :language language :parser parser
                                      :syntax syntax :package package))))))

(defun free-parse-state (ps)
  (when ps
    (when (ps-tree ps) (ignore-errors (ts-tree-delete (ps-tree ps))) (setf (ps-tree ps) nil))
    (when (ps-parser ps) (ignore-errors (ts-parser-delete (ps-parser ps))) (setf (ps-parser ps) nil))
    (when (ps-scratch ps)
      (ignore-errors (cffi:foreign-free (ps-scratch ps)))
      (setf (ps-scratch ps) nil))))

(defun ps-read-buffer (ps)
  "PS's foreign read buffer, allocated on first use."
  (or (ps-scratch ps)
      (setf (ps-scratch ps) (cffi:foreign-alloc :unsigned-char :count +read-chunk+))))

(defun ps-offset (ps)
  "The buffer line PS's tree starts at."
  (let ((band (ps-band ps))) (if band (car band) 0)))

(defun %fill-scratch (lines index byte scratch size)
  "Copy up to SIZE bytes of LINES from byte offset BYTE into SCRATCH. Answers
how many were written, which is zero at the end of the buffer."
  (multiple-value-bind (line offset) (pine.ts.index:byte-line index byte)
    (let ((written 0)
          (n (pine.data:size lines)))
      (block filling
        (loop :while (< line n)
              :do (let* ((octets (sb-ext:string-to-octets (pine.data:at lines line)
                                                          :external-format :utf-8))
                         (len (length octets)))
                    (loop :for i :from (min offset len) :below len
                          :do (when (>= written size) (return-from filling))
                              (setf (cffi:mem-aref scratch :unsigned-char written)
                                    (aref octets i))
                              (incf written))

                    (when (< line (1- n))
                      (when (>= written size) (return-from filling))
                      (setf (cffi:mem-aref scratch :unsigned-char written) 10)
                      (incf written))
                    (setf offset 0)
                    (incf line))))
      written)))

(cffi:defcallback %ts-read :pointer
    ((payload :pointer) (byte :uint32) (point :uint64)
     (bytes-read (:pointer :uint32)))
  (declare (ignore payload point))

  (handler-case
      (destructuring-bind (&optional lines index scratch size) *reading*
        (if (null lines)
            (progn (setf (cffi:mem-ref bytes-read :uint32) 0) (cffi:null-pointer))
            (let ((written (%fill-scratch lines index byte scratch size)))
              (setf (cffi:mem-ref bytes-read :uint32) written)
              (if (zerop written) (cffi:null-pointer) scratch))))
    (error (c)
      (format *error-output* "pine: the parser read failed at byte ~d: ~a~%" byte c)
      (finish-output *error-output*)
      (setf (cffi:mem-ref bytes-read :uint32) 0)
      (cffi:null-pointer))))

(defun call-with-input (lines index scratch fn)
  "Call FN with a TSInput reading LINES through INDEX, as a foreign value."
  (let ((*reading* (list lines index scratch +read-chunk+)))
    (cffi:with-foreign-object (input '(:struct ts-input))
      (setf (cffi:foreign-slot-value input '(:struct ts-input) 'payload)
            (cffi:null-pointer)
            (cffi:foreign-slot-value input '(:struct ts-input) 'read)
            (cffi:callback %ts-read)
            (cffi:foreign-slot-value input '(:struct ts-input) 'encoding)
            +input-encoding-utf8+
            (cffi:foreign-slot-value input '(:struct ts-input) 'decode)
            (cffi:null-pointer))
      (funcall fn (cffi:mem-ref input '(:struct ts-input))))))

(defun %set-point (object type slot point)
  "Write POINT into OBJECT's TSPoint SLOT. TYPE is OBJECT's own foreign type:
ts-input-edit and ts-range both hold points, at different offsets."
  (let ((p (cffi:foreign-slot-pointer object type slot)))
    (setf (cffi:foreign-slot-value p '(:struct ts-point) 'row)    (car point)
          (cffi:foreign-slot-value p '(:struct ts-point) 'column) (cdr point))))

(defun %tree-edit (tree start-byte old-end-byte new-end-byte
                   start-row old-end-row new-end-row)
  "Shift TREE's positions for a change spanning whole lines. Columns are zero
because the span runs from the start of one line to the start of another."
  (cffi:with-foreign-object (edit '(:struct ts-input-edit))
    (setf (cffi:foreign-slot-value edit '(:struct ts-input-edit) 'start-byte) start-byte
          (cffi:foreign-slot-value edit '(:struct ts-input-edit) 'old-end-byte) old-end-byte
          (cffi:foreign-slot-value edit '(:struct ts-input-edit) 'new-end-byte) new-end-byte)
    (%set-point edit '(:struct ts-input-edit) 'start-point (cons start-row 0))
    (%set-point edit '(:struct ts-input-edit) 'old-end-point (cons old-end-row 0))
    (%set-point edit '(:struct ts-input-edit) 'new-end-point (cons new-end-row 0))
    (ts-tree-edit tree edit)))

(defun %changed-row-span (old-tree new-tree)
  "Union of changed line rows between OLD-TREE and NEW-TREE as (values lo hi),
or nil when tree-sitter reports no changed ranges. The edit span alone
under-approximates: an opened string quote recolours everything after it."
  (cffi:with-foreign-object (len :uint32)
    (let ((ranges (ts-tree-get-changed-ranges old-tree new-tree len)))
      (unwind-protect
           (let ((n (cffi:mem-ref len :uint32)))
             (when (and (plusp n) (not (cffi:null-pointer-p ranges)))
               (let ((lo most-positive-fixnum) (hi 0))
                 (dotimes (i n (values lo hi))
                   (let ((r (cffi:mem-aptr ranges '(:struct ts-range) i)))
                     (let ((sp (cffi:foreign-slot-pointer r '(:struct ts-range) 'start-point))
                           (ep (cffi:foreign-slot-pointer r '(:struct ts-range) 'end-point)))
                       (setf lo (min lo (cffi:foreign-slot-value sp '(:struct ts-point) 'row))
                             hi (max hi (cffi:foreign-slot-value ep '(:struct ts-point) 'row)))))))))
        (unless (cffi:null-pointer-p ranges)
          (cffi:foreign-free ranges))))))

(defun %record-hl-edit (ps old new start-row old-end-row new-end-row)
  "Note one edit for incremental highlighting: rows from tree-sitter's changed
ranges unioned with the raw edit's rows (a pure line insert shifts everything
below while changing no named ranges), plus the line delta. A second edit before
the next highlight call, or any failure here, marks the cache stale."
  (when (ps-hl-cache ps)
    (if (ps-hl-pending ps)
        (setf (ps-hl-stale ps) t)
        (handler-case
            (let ((lo start-row)
                  (hi new-end-row)
                  (delta (- new-end-row old-end-row)))
              (unless (cffi:pointer-eq new old)
                (multiple-value-bind (rlo rhi) (%changed-row-span old new)
                  (when rlo
                    (setf lo (min lo rlo) hi (max hi rhi)))))
              (setf (ps-hl-pending ps) (list lo hi delta)))
          (error () (setf (ps-hl-stale ps) t))))))

(defun %band (lines viewport)
  "The line band to parse for VIEWPORT over LINES, or NIL for all of them."
  (let ((n (pine.data:size lines)))
    (when (and viewport (> n +whole-file-lines+))
      (cons (max 0 (* +band-lines+ (floor (car viewport) +band-lines+)))
            (min (1- n) (1- (* +band-lines+ (ceiling (1+ (cdr viewport))
                                                     +band-lines+))))))))

(defun %band-lines (lines band)
  "The subsequence of LINES that BAND covers, or LINES itself when BAND is nil."
  (if band
      (pine.data:subseq lines (car band)
                        (min (pine.data:size lines) (1+ (cdr band))))
      lines))

(defun %parse-band (ps lines band band-lines same-band edit)
  (let* ((old-tree (ps-tree ps))
         (old-index (ps-byte-index ps))
         (shifted (and edit old-index old-tree same-band
                       (<= (car (or band '(0))) (first edit))
                       (< (first edit) (+ (or (and band (car band)) 0)
                                          (pine.data:size band-lines)))))
         (index nil))
    (when shifted
      (destructuring-bind (line old-lines new-lines byte-delta) edit
        (let* ((at (- line (if band (car band) 0)))
               (start (pine.ts.index:line-start old-index at))
               (old-end (pine.ts.index:line-start old-index (+ at old-lines))))
          (setf index (pine.ts.index:index-edit old-index band-lines at byte-delta
                                                (- new-lines old-lines)))
          (%tree-edit old-tree start old-end
                      (pine.ts.index:line-start index (+ at new-lines))
                      at (+ at old-lines) (+ at new-lines)))))
    (unless index (setf index (pine.ts.index:build-index band-lines)))
    (let ((incremental shifted))
      (let ((new (call-with-input
                  band-lines index (ps-read-buffer ps)
                  (lambda (input)
                    (ts-parser-parse (ps-parser ps)
                                     (if incremental old-tree (cffi:null-pointer))
                                     input)))))
        (cond

          ((or (null new) (cffi:null-pointer-p new))
           (pine.run.fault:report
            (make-condition 'simple-error
                            :format-control "the ~(~a~) parser answered no tree ~
                                             for ~d line~:p"
                            :format-arguments (list (ps-language ps)
                                                    (pine.data:size band-lines)))
            "parsing")
           nil)
          (t (if incremental
                 (destructuring-bind (line old-lines new-lines byte-delta) edit
                   (declare (ignore byte-delta))
                   (let ((at (- line (if band (car band) 0))))
                     (%record-hl-edit ps old-tree new at (+ at old-lines)
                                      (+ at new-lines))))
                 (setf (ps-hl-cache ps) nil
                       (ps-hl-lines ps) nil
                       (ps-hl-window ps) nil
                       (ps-hl-pending ps) nil
                       (ps-hl-stale ps) nil))
             (when (and old-tree (not (cffi:pointer-eq new old-tree)))
               (ts-tree-delete old-tree))
             (setf (ps-tree ps) new
                   (ps-byte-index ps) index
                   (ps-band ps) band
                   (ps-band-lines ps) band-lines
                   (ps-lines ps) lines)
             ps))))))
