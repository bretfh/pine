(in-package #:pine/text)

(defun parse-lines! (ps lines &key edit from viewport)
  (let* ((band (%band lines viewport))
         (band-lines (%band-lines lines band))
         (same-band (equal band (ps-band ps)))
         (old-tree (ps-tree ps)))
    (when (and old-tree same-band (eq lines (ps-lines ps)) (null edit))
      (return-from parse-lines! ps))
    (%parse-band ps lines band band-lines same-band
                 (when (or (null from) (eq from (ps-lines ps))) edit))))

(defun parse-text! (ps text)
  (parse-lines! ps (d:as :seq (uiop:split-string text :separator '(#\Newline)))))

(defun %outermost-path (node)
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
  (let ((cur (ts-node-named-descendant-for-byte-range root byte byte)))
    (cond
      ((ts-node-is-null cur) nil)

      (t (loop for p = (ts-node-parent cur)
               until (or (ts-node-is-null p) (ts-node-is-null (ts-node-parent p)))
               do (setf cur p))
         (values (ts-node-start-byte cur) (ts-node-end-byte cur))))))

(defun parse-motion (ps kind line col)
  (let* ((tree (ps-tree ps)) (src (ps-byte-index ps))
         (offset (ps-offset ps))
         (line (- line offset)))
    (when (and tree src (<= 0 line))
      (handler-case
          (let ((byte (source-byte src line col))
                (root (ts-tree-root-node tree)))
            (flet ((at (b) (when b
                             (multiple-value-bind (l c)
                                 (source-line-col src b)
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
