(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.buf :in :pine)

(defmacro with-buf (&body body)
  "The live substrate, with /buf served over it."
  `(with-fixture substrate ()
     (pine.ns:with-space ()
       (pine.provider.buf:mount)
       ,@body)))

(defun quiet () (sleep 0.15))

;;;; reading a buffer is reading a path

(test every-buffer-is-under-buf
  (with-buf
    (is (member "scratch" (mapcar #'pine.path:leaf
                                  (pine.data:keys (pine.ns:read /buf/*)))
                :test #'string=))))

(test text-point-and-mode-are-places
  (with-buf
    (pine.ns:write /buf/scratch/text "one
two
three")
    (quiet)
    (is (= 3 (pine.ns:read /buf/scratch/lines)))
    (is (string= "two" (pine.ns:read /buf/scratch/line/1)))
    (is (eq :text-mode (pine.ns:read /buf/scratch/mode)))
    (pine.ns:write /buf/scratch/point [1 2])
    (quiet)
    (is (fset:equal? [1 2] (pine.ns:read /buf/scratch/point)))))

(test a-window-reads-the-range-it-shows
  "The band is not a policy in the parser; it is what the window asked for."
  (with-buf
    (pine.ns:write /buf/scratch/text (format nil "~{line ~d~^~%~}"
                                             (loop :for i :below 50 :collect i)))
    (quiet)
    (let ((band (pine.ns:read /buf/scratch/line/10..14)))
      (is (= 5 (fset:size band)))
      (is (string= "line 10" (fset:lookup band 0)))
      (is (string= "line 14" (fset:lookup band 4))))))

(test a-range-past-the-end-is-what-is-there
  (with-buf
    (pine.ns:write /buf/scratch/text "one")
    (quiet)
    (is (= 1 (fset:size (pine.ns:read /buf/scratch/line/0..99))))))

;;;; editing is writing

(test inserting-is-a-verb-on-the-text
  (with-buf
    (pine.ns:write /buf/scratch/text "")
    (quiet)
    (pine.ns:write /buf/scratch/point [0 0])
    (quiet)
    (pine.ns:write /buf/scratch/text [:insert "hello"])
    (quiet)
    (is (string= "hello" (pine.ns:read /buf/scratch/text)))))

(test point-moves-by-a-unit
  (with-buf
    (pine.ns:write /buf/scratch/text "one two")
    (quiet)
    (pine.ns:write /buf/scratch/point [0 0])
    (quiet)
    (pine.ns:write /buf/scratch/point [:move :word 1])
    (quiet)
    (is (plusp (fset:lookup (pine.ns:read /buf/scratch/point) 1)))))

(test an-edit-moves-the-tick-and-a-motion-does-not
  (with-buf
    (pine.ns:write /buf/scratch/text "one")
    (quiet)
    (let ((tick (pine.ns:read /buf/scratch/tick)))
      (pine.ns:write /buf/scratch/point [0 0])
      (quiet)
      (is (= tick (pine.ns:read /buf/scratch/tick)))
      (pine.ns:write /buf/scratch/text [:insert "x"])
      (quiet)
      (is (> (pine.ns:read /buf/scratch/tick) tick)))))

;;;; locals

(test a-buffer-local-is-a-place
  (with-buf
    (pine.ns:write /buf/scratch/tab-width 4)
    (quiet)
    (is (= 4 (pine.ns:read /buf/scratch/tab-width)))))

;;;; what it is

(test a-buffer-is-live-so-nothing-stores-its-text
  "The text of a buffer visiting a file came from the file."
  (with-buf
    (is (eq :live (pine.ns:kind /buf/scratch/text)))))
