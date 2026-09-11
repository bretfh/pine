(in-package #:pine/text)

(defun %viewport-bytes (src from-line to-line)
  (let ((last (1- (index-line-count src))))
    (values (line-start src (max 0 (min from-line last)))
            (line-start src (1+ (max 0 (min to-line last)))))))

(defun %form-at-or-before (root byte count)
  (let ((lo 0) (hi (1- count)))
    (loop :while (< lo hi)
          :do (let ((mid (ceiling (+ lo hi) 2)))
                (if (<= (ts-node-start-byte (ts-node-named-nth root mid)) byte)
                    (setf lo mid)
                    (setf hi (1- mid)))))
    lo))

(defun %forms-in-window (root lo-byte hi-byte)
  (let ((count (ts-node-named-count root)))
    (when (plusp count)
      (loop :for i :from (%form-at-or-before root lo-byte count) :below count
            :for form = (ts-node-named-nth root i)
            :while (< (ts-node-start-byte form) hi-byte)
            :when (> (ts-node-end-byte form) lo-byte)
              :collect form))))

(defun %hl-window (ps tree from-line to-line)
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
  (let ((pending (ps-hl-pending ps)))
    (and pending
         (destructuring-bind (lo hi delta) pending
           (and (zerop delta) (>= lo from-line) (<= hi to-line))))))

(defun %shift-tuples (tuples offset)
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
