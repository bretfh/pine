(in-package #:pine/text)

(defun %viewport-bytes (src from-line to-line)
  "The byte range covering lines FROM-LINE to TO-LINE inclusive."
  (let ((last (1- (index-line-count src))))
    (values (line-start src (max 0 (min from-line last)))
            (line-start src (1+ (max 0 (min to-line last)))))))

(defun %form-at-or-before (root byte count)
  "Index of the last of ROOT's named nodes starting at or before BYTE.

The nodes are in source order, so this is a binary search: finding the window
by descending from the root would touch every form in the file, and each node
call through the by-value TSNode binding allocates."
  (let ((lo 0) (hi (1- count)))
    (loop :while (< lo hi)
          :do (let ((mid (ceiling (+ lo hi) 2)))
                (if (<= (ts-node-start-byte (ts-node-named-nth root mid)) byte)
                    (setf lo mid)
                    (setf hi (1- mid)))))
    lo))

(defun %forms-in-window (root lo-byte hi-byte)
  "ROOT's top-level named nodes that intersect [LO-BYTE, HI-BYTE)."
  (let ((count (ts-node-named-count root)))
    (when (plusp count)
      (loop :for i :from (%form-at-or-before root lo-byte count) :below count
            :for form = (ts-node-named-nth root i)
            :while (< (ts-node-start-byte form) hi-byte)
            :when (> (ts-node-end-byte form) lo-byte)
              :collect form))))

(defun %hl-window (ps tree from-line to-line)
  "Walk the top-level forms covering lines FROM-LINE to TO-LINE, and cache them."
  (let ((src (ps-byte-index ps))
        (root (ts-tree-root-node tree)))
    (multiple-value-bind (lo-byte hi-byte) (%viewport-bytes src from-line to-line)
      (let ((hl (walk-highlights (ps-syntax ps) root src
                                 :lo-byte lo-byte :hi-byte hi-byte
                                 :package (ps-package ps)
                                 :forms (%forms-in-window root lo-byte hi-byte))))
        (setf (ps-hl-cache ps) hl (ps-hl-lines ps) (ps-lines ps)
              (ps-hl-window ps) (cons from-line to-line)
              (ps-hl-pending ps) nil (ps-hl-stale ps) nil)
        hl))))

(defun %hl-window-incremental (ps tree from-line to-line)
  "Re-walk only the forms the edit touched and keep the rest of the window's
cached tuples. Sound because the caller has established that no line moved, so
every cached tuple outside the re-walked lines still describes its own line."
  (destructuring-bind (lo hi delta) (ps-hl-pending ps)
    (declare (ignore delta))
    (let* ((src (ps-byte-index ps))
           (root (ts-tree-root-node tree))
           (last (1- (index-line-count src)))
           (lo-byte (line-start src (max 0 (min lo last))))
           (hi-byte (line-start src (1+ (max 0 (min hi last)))))
           (forms nil))

      (loop :for grew = nil
            :do (setf forms (%forms-in-window root lo-byte hi-byte))
                (dolist (form forms)
                  (let ((s (ts-node-start-byte form)) (e (ts-node-end-byte form)))
                    (when (< s lo-byte) (setf lo-byte s grew t))
                    (when (> e hi-byte) (setf hi-byte e grew t))))
                (let ((ll (line-start src (nth-value 0 (byte-line src lo-byte))))
                      (hh (line-start
                           src
                           (1+ (nth-value 0 (byte-line src (max lo-byte (1- hi-byte))))))))
                  (when (< ll lo-byte) (setf lo-byte ll grew t))
                  (when (> hh hi-byte) (setf hi-byte hh grew t)))
            :while grew)
      (let* ((lo-line (nth-value 0 (byte-line src lo-byte)))
             (hi-line (nth-value 0 (byte-line src (max lo-byte (1- hi-byte)))))
             (fresh (walk-highlights (ps-syntax ps) root src
                                     :lo-byte lo-byte :hi-byte hi-byte
                                     :package (ps-package ps) :forms forms))
             (kept (remove-if (lambda (tuple) (<= lo-line (first tuple) hi-line))
                              (ps-hl-cache ps)))
             (merged (nconc kept fresh)))
        (setf (ps-hl-cache ps) merged (ps-hl-lines ps) (ps-lines ps)
              (ps-hl-window ps) (cons from-line to-line)
              (ps-hl-pending ps) nil (ps-hl-stale ps) nil)
        merged))))

(defun %window-edit-is-local-p (ps from-line to-line)
  "True when the pending edit moved no line and lands inside FROM-LINE..TO-LINE,
which is what makes the window's cached tuples still usable."
  (let ((pending (ps-hl-pending ps)))
    (and pending
         (destructuring-bind (lo hi delta) pending
           (and (zerop delta) (>= lo from-line) (<= hi to-line))))))

(defun %shift-tuples (tuples offset)
  "TUPLES with every line moved from band-relative to buffer coordinates."
  (if (zerop offset)
      tuples
      (mapcar (lambda (tuple)
                (list (+ (first tuple) offset) (second tuple)
                      (third tuple) (fourth tuple)))
              tuples)))

(defun %hl-full (ps tree)

  (when (byte-index-pending (ps-byte-index ps))
    (setf (ps-byte-index ps) (compact-index (ps-byte-index ps))))
  (let ((hl (walk-highlights (ps-syntax ps) (ts-tree-root-node tree)
                             (ps-byte-index ps) :package (ps-package ps))))
    (setf (ps-hl-cache ps) hl (ps-hl-lines ps) (ps-lines ps)
          (ps-hl-window ps) nil
          (ps-hl-pending ps) nil (ps-hl-stale ps) nil)
    hl))

(defun %hl-incremental (ps tree)
  "Re-walk only the top-level forms covering the recorded edit and merge with
the cached tuples: keep lines above, shift lines below by the edit's delta.
The re-walk window is widened to whole lines and whole top-level forms (to a
fixpoint), so cached tuples are dropped exactly where fresh ones are emitted."
  (destructuring-bind (lo hi delta) (ps-hl-pending ps)
    (let* ((src (ps-byte-index ps))
           (root (ts-tree-root-node tree))
           (nlines (index-line-count src))
           (lo-byte (line-start src (min lo (1- nlines))))
           (hi-byte (line-start src (1+ hi))))

      (loop for grew = nil
            do (loop for i from 0 below (ts-node-named-count root)
                     for node = (ts-node-named-nth root i)
                     for s = (ts-node-start-byte node)
                     for e = (ts-node-end-byte node)
                     do (when (and (< s hi-byte) (> e lo-byte)
                                   (or (< s lo-byte) (> e hi-byte)))
                          (setf lo-byte (min lo-byte s)
                                hi-byte (max hi-byte e)
                                grew t)))
               (let ((ll (line-start src (nth-value 0 (byte-line src lo-byte))))
                     (hl (line-start
                          src
                          (1+ (nth-value 0 (byte-line src (max lo-byte (1- hi-byte))))))))
                 (when (< ll lo-byte) (setf lo-byte ll grew t))
                 (when (> hl hi-byte) (setf hi-byte hl grew t)))
            while grew)

      (when (>= (- hi-byte lo-byte) (* 3 (floor (max 1 (index-total src)) 4)))
        (return-from %hl-incremental (%hl-full ps tree)))
      (let* ((lo-line (nth-value 0 (byte-line src lo-byte)))
             (hi-line (nth-value 0 (byte-line src (max lo-byte (1- hi-byte)))))
             (old-hi-line (- hi-line delta))
             (fresh (walk-highlights (ps-syntax ps) root src
                                     :lo-byte lo-byte :hi-byte hi-byte :package (ps-package ps)))
             (merged nil))
        (dolist (tup (ps-hl-cache ps))
          (let ((line (first tup)))
            (when (< line lo-line)
              (push tup merged))))
        (setf merged (nreverse merged))
        (setf merged (nconc merged (copy-list fresh)))
        (let ((below nil))
          (dolist (tup (ps-hl-cache ps))
            (let ((line (first tup)))
              (when (> line old-hi-line)
                (push (list (+ line delta) (second tup) (third tup) (fourth tup))
                      below))))
          (setf merged (nconc merged (nreverse below))))
        (setf (ps-hl-cache ps) merged (ps-hl-lines ps) (ps-lines ps)
              (ps-hl-window ps) nil
              (ps-hl-pending ps) nil (ps-hl-stale ps) nil)
        merged))))

(defun parse-highlights (ps &key from-line to-line)
  "Highlights (line start-col end-col face) from PS's persistent tree, in the
buffer's own line numbers.

The source is PS's own lines and byte index, the ones its tree was parsed from,
so a caller cannot hand this a text the tree does not describe. Cache identity
is EQ on the  the seq is immutable, so an unchanged buffer is one
comparison rather than a walk over the file.

With FROM-LINE and TO-LINE, walks only the tree covering those lines. The
descent from the root still runs, so quote state, form depth and head kind stay
exact. Without a range, the whole buffer is walked and cached, and when exactly
one edit was recorded since the last call only the changed top-level forms are
re-walked. Anything unexpected falls back to the full walk."
  (let* ((tree (ps-tree ps))
         (lines (ps-lines ps))
         (offset (ps-offset ps))
         (from-line (and from-line (- from-line offset)))
         (to-line (and to-line (- to-line offset))))
    (when (and tree (ps-byte-index ps))
      (handler-case
          (%shift-tuples
           (if (and from-line to-line)
               (let ((same-window (equal (ps-hl-window ps) (cons from-line to-line))))
                 (cond
                   ((and (ps-hl-cache ps) same-window (not (ps-hl-stale ps))
                         (null (ps-hl-pending ps))
                         (eq lines (ps-hl-lines ps)))
                    (ps-hl-cache ps))
                   ((and (ps-hl-cache ps) same-window (not (ps-hl-stale ps))
                         (%window-edit-is-local-p ps from-line to-line))
                    (%hl-window-incremental ps tree from-line to-line))
                   (t (%hl-window ps tree from-line to-line))))
               (cond
                 ((and (ps-hl-cache ps) (null (ps-hl-pending ps)) (not (ps-hl-stale ps))
                       (eq lines (ps-hl-lines ps)))
                  (ps-hl-cache ps))
                 ((and (ps-hl-cache ps) (ps-hl-pending ps) (not (ps-hl-stale ps)))
                  (%hl-incremental ps tree))
                 (t (%hl-full ps tree))))
           offset)

        (error (c)
          (pine/run/fault:report
           c (format nil "highlighting ~a, retrying without the cache"
                     (ps-language ps)))
          (setf (ps-hl-cache ps) nil (ps-hl-lines ps) nil
                (ps-hl-pending ps) nil (ps-hl-stale ps) nil)
          (%shift-tuples
           (walk-highlights (ps-syntax ps) (ts-tree-root-node tree)
                            (ps-byte-index ps) :package (ps-package ps))
           offset))))))
