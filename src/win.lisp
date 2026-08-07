(defpackage #:pine.win
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:windows #:focused #:focus #:split-p #:parts #:provider
           #:buf-of #:scroll-of #:weight-of #:runs-of #:seed #:reset
           #:split #:close #:only)
  (:shadow #:close))

(in-package #:pine.win)
(named-readtables:in-readtable pine.path:syntax)

;;;; An arrangement is a subtree. /win/0 is a window; splitting it makes it a
;;;; stack, and its two halves are /win/0/0 and /win/0/1.
;;;;
;;;;   ?root/?n/buf      what it shows
;;;;   ?root/?n/scroll   the first line showing
;;;;   ?root/?n/weight   its share of the stack it is in
;;;;   ?root/?n/runs     :column or :row, on a stack
;;;;   ?root/focused     the one with the keyboard
;;;;
;;;; The root is an argument, so the editor's windows and the window manager's
;;;; are one arrangement over two subtrees.

(defun split-p (path)
  "True when PATH is a stack rather than a window: it says which way it runs."
  (and (ns:read (p:path path "runs")) t))

(defun buf-of (path) (ns:read (p:path path "buf")))
(defun scroll-of (path) (or (ns:read (p:path path "scroll")) 0))
(defun weight-of (path) (or (ns:read (p:path path "weight")) 1))
(defun runs-of (path) (ns:read (p:path path "runs")))

(defun parts (path)
  "The windows and stacks under PATH, in order. A window's own leaves
are not among them: a part is named by a number."
  (let ((held (ns:held path))
        (acc nil))
    (when (fset:map? held)
      (fset:do-map (key value held)
        (declare (ignore value))
        (let ((name (p:name key)))
          (when (every #'digit-char-p name)
            (push (cons (parse-integer name) (p:path path name)) acc)))))
    (mapcar #'cdr (sort acc #'< :key #'car))))

(defun windows (&optional (at /win))
  "Every window under AT, in tree order. A stack is not one; its parts are."
  (if (or (split-p at) (null (buf-of at)))
      (loop :for part :in (parts at) :append (windows part))
      (list at)))

(defun focused (&optional (root /win))
  "The window under ROOT with the keyboard, or the first one there is."
  (let ((named (ns:read (p:path root "focused"))))
    (if (and named (buf-of named))
        named
        (first (windows root)))))

(defun focus (path &optional (root /win))
  (ns:write (p:path root "focused") path)
  path)

(defun split (path side &optional (root /win))
  "Make PATH a stack of two: what was there, and a second window on the same
buffer. SIDE is :below or :above for a column, :beside or :right for a row.
One write, because writing the one below and clearing the one above clears both."
  (let* ((runs (if (member side '(:beside :right :left)) :row :column))
         (was (or (ns:held path) (fset:empty-map)))
         (fresh (fset:map ("buf" (buf-of path))
                          ("scroll" (scroll-of path))
                          ("weight" (weight-of path)))))
    (ns:write path (fset:seq :set (fset:map ("0" was) ("1" fresh)
                                            ("runs" runs))))
    (focus (p:path path (if (member side '(:above :left)) "0" "1")) root)))

(defun %parent (path)
  (let ((up (p:parent path)))
    (unless (p:rootp up) up)))

(defun %collapse (stack)
  "A stack with one part left is that part."
  (let ((its (parts stack)))
    (when (and (split-p stack) (= 1 (length its)))
      (ns:write stack (fset:seq :set (ns:held (first its))))
      (when (split-p stack) (%collapse stack)))))

(defun close (path &optional (root /win))
  "Drop the window PATH. Its stack collapses when one part is left, and the
focus lands on whatever is still there."
  (let ((up (%parent path)))
    (when (and up (not (fset:equal? up root)))
      (ns:write path nil)
      (%collapse up)
      (focus (or (first (windows up)) (first (windows root))) root))))

(defun only (path &optional (root /win))
  "PATH alone: what it shows becomes the whole arrangement."
  (let ((buf (buf-of path))
        (scroll (scroll-of path)))
    (when buf
      ;; each part, not the root: writing nothing at a mount takes it off
      (dolist (part (parts root)) (ns:write part nil))
      (let ((first (p:path root "0")))
        (ns:write (fset:map ((p:path first "buf") buf)
                            ((p:path first "scroll") scroll)
                            ((p:path first "weight") 1)))
        (focus first root)))))

(defun seed (buf &optional (root /win))
  "One window on BUF, when there is no arrangement to come back to."
  (unless (windows root)
    (let ((first (p:path root "0")))
      (ns:write (fset:map ((p:path first "buf") buf)
                          ((p:path first "weight") 1)))
      (focus first root)))
  (focused root))

(defun reset (buf &optional (root /win))
  "One window on BUF, whatever was there."
  (dolist (part (parts root)) (ns:write part nil))
  (ns:write (p:path root "focused") nil)
  (seed buf root))

(defun provider ()
  (ns:provider
   ;; a leaf under /win/focused is that window's leaf, the way a leaf under
   ;; /buf/current is the current buffer's. Without this a config writing
   ;; /win/focused/buf makes a leaf nothing reads, and the window it meant goes
   ;; on showing what it showed.
   (/win/focused/?leaf
    {:read (pine.data:fn [] (let ((at (focused)))
                              (and at (ns:read (p:path at leaf)))))
     :write (pine.data:fn [v] (let ((at (focused)))
                                (when at (ns:write (p:path at leaf) v))))
     :verbs {t (pine.data:fn [verb] (let ((at (focused)))
                                      (when at (ns:write (p:path at leaf) verb))))}
     :doc "a leaf of the window with the keyboard"})
   (/win/focused
    {:verbs {:split (pine.data:fn (&optional (side :below))
                      (let ((at (focused))) (when at (split at side))))
             :close (pine.data:fn [] (let ((at (focused))) (when at (close at))))
             :only (pine.data:fn [] (let ((at (focused))) (when at (only at))))}
     :doc "the window with the keyboard; [:split :below] [:close] [:only]"})
   (/win/?@at
    {:doc "a window's buf, scroll and weight, or the two halves of a stack"})))

(ns:serve :win
  {:at [/win]
   :doc "pine's own windows: the arrangement, as paths"
   :up (lambda () (ns:write /win (provider)))})
