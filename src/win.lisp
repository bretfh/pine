(defpackage #:pine.win
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:mount #:unmount #:windows #:focused #:focus #:stack-p
           #:buf-of #:scroll-of #:weight-of #:runs-of #:seed #:reset))

(in-package #:pine.win)
(named-readtables:in-readtable pine.path:syntax)

;;;; A window is pine's own view onto a buffer, and the arrangement is the
;;;; path. /win/0 is a window; splitting it makes it a stack, and its two
;;;; halves are /win/0/0 and /win/0/1. Nesting is nesting, so a row inside a
;;;; column is a directory inside a directory and there is nothing to
;;;; serialize: the arrangement is held paths and comes back with them.
;;;;
;;;;   /win/?n/buf      the buffer it shows
;;;;   /win/?n/scroll   the first line showing
;;;;   /win/?n/weight   its share of the stack it is in
;;;;   /win/?n/runs     :column or :row, on a stack
;;;;   /win/focused     the window that has the keyboard

(defun stack-p (path)
  "True when PATH is a stack rather than a window: it says which way it runs."
  (and (ns:read (p:child path "runs")) t))

(defun buf-of (path) (ns:read (p:child path "buf")))
(defun scroll-of (path) (or (ns:read (p:child path "scroll")) 0))
(defun weight-of (path) (or (ns:read (p:child path "weight")) 1))
(defun runs-of (path) (ns:read (p:child path "runs")))

(defun %parts (path)
  "PATH's children that are windows or stacks, in order. A window's own leaves
are not among them: a part is named by a number."
  (let ((held (ns:held path))
        (acc nil))
    (when (fset:map? held)
      (fset:do-map (key value held)
        (declare (ignore value))
        (let ((name (p:name key)))
          (when (every #'digit-char-p name)
            (push (cons (parse-integer name) (p:child path name)) acc)))))
    (mapcar #'cdr (sort acc #'< :key #'car))))

(defun windows (&optional (at /win))
  "Every window under AT, in tree order. A stack is not one; its parts are."
  (if (stack-p at)
      (loop :for part :in (%parts at) :append (windows part))
      (if (buf-of at)
          (list at)
          (loop :for part :in (%parts at) :append (windows part)))))

(defun focused ()
  "The window that has the keyboard, or the first one there is."
  (let ((named (ns:read /win/focused)))
    (if (and named (buf-of named))
        named
        (first (windows)))))

(defun focus (path)
  (ns:write /win/focused path)
  path)

;;;; The verbs

(defun %next-part (stack)
  (let ((parts (%parts stack)))
    (if parts
        (1+ (reduce #'max (mapcar (lambda (p) (parse-integer (p:leaf p))) parts)))
        0)))

(defun %split (path side)
  "Make PATH a stack of two: what was there, and a second window on the same
buffer. SIDE is :below or :above for a column, :beside or :right for a row.

One write, because moving a node down into itself is one new value: writing
the child and then clearing the parent would clear the child with it."
  (let* ((runs (if (member side '(:beside :right :left)) :row :column))
         (was (or (ns:held path) (fset:empty-map)))
         (fresh (fset:map ("buf" (buf-of path))
                          ("scroll" (scroll-of path))
                          ("weight" (weight-of path)))))
    (ns:write path (fset:map ("0" was) ("1" fresh) ("runs" runs)))
    (focus (p:child path (if (member side '(:above :left)) "0" "1")))))

(defun %parent (path)
  (let ((up (p:parent path)))
    (unless (p:rootp up) up)))

(defun %collapse (stack)
  "A stack with one part left is that part. One write, for the same reason a
split is one."
  (let ((parts (%parts stack)))
    (when (and (stack-p stack) (= 1 (length parts)))
      (let ((held (ns:held (first parts))))
        (ns:write stack held)
        (when (stack-p stack) (%collapse stack))))))

(defun %close (path)
  "Drop the window PATH. Its stack collapses when one part is left, and the
focus lands on whatever is still there."
  (let ((up (%parent path)))
    (when (and up (not (fset:equal? up /win)))
      (ns:write path nil)
      (%collapse up)
      (focus (or (first (windows up)) (first (windows)))))))

(defun %only (path)
  "PATH alone: what it shows becomes the whole arrangement."
  (let ((buf (buf-of path))
        (scroll (scroll-of path)))
    (when buf
      ;; each part, not /win itself: writing nothing where a provider is
      ;; mounted takes the provider off
      (dolist (part (%parts /win)) (ns:write part nil))
      (ns:write (fset:map (/win/0/buf buf)
                          (/win/0/scroll scroll)
                          (/win/0/weight 1)))
      (focus /win/0))))

(defun seed (buf)
  "One window on BUF, when there is no arrangement to come back to."
  (unless (windows)
    (ns:write (fset:map (/win/0/buf buf)
                        (/win/0/weight 1)))
    (focus /win/0))
  (focused))

(defun reset (buf)
  "One window on BUF, whatever was there: what a fresh session starts from."
  (dolist (part (%parts /win)) (ns:write part nil))
  (ns:write /win/focused nil)
  (seed buf))

(defun provider ()
  (ns:provider
   (/win/focused
    {:verbs {:split (pine.data:fn (&optional (side :below))
                      (let ((at (focused))) (when at (%split at side))))
             :close (pine.data:fn [] (let ((at (focused))) (when at (%close at))))
             :only (pine.data:fn [] (let ((at (focused))) (when at (%only at))))}
     :doc "the window with the keyboard; [:split :below] [:close] [:only]"})
   (/win/?@at
    {:doc "a window's buf, scroll and weight, or the two halves of a stack"})))

(defun mount ()
  (ns:write /win (provider)))

(defun unmount ()
  (ns:write /win nil))
