(defpackage #:pine.provider.buf
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:b #:pine.text.buffer))
  (:export #:mount))

(in-package #:pine.provider.buf)
(named-readtables:in-readtable pine.path:syntax)

;;;; Buffers, as paths. A buffer's content, its point, its locals and its marks
;;;; are places, so nothing has to be exported for something else to read them
;;;; and another image reaches them without an accessor written for the
;;;; occasion.
;;;;
;;;; The band that made a million lines work is not a policy in the parser: it
;;;; is what the window read.

(defun %table ()
  (let ((server pine.core.server:*server*))
    (and server (b:buffer-table server))))

(defun %actor (name)
  (let ((table (%table)))
    (and table (gethash name table))))

(defun %names ()
  (let ((table (%table))
        (acc nil))
    (when table
      (maphash (lambda (name actor) (declare (ignore actor)) (push name acc)) table))
    (sort acc #'string<)))

(defun %snapshot (name)
  (let ((actor (%actor name)))
    (when actor
      (pine.core.actor:ask actor '(:get-snapshot) :timeout 5))))

(defun %tell (name message)
  (let ((actor (%actor name)))
    (when actor (sento.actor:tell actor message) t)))

(defun %lines (snapshot) (and snapshot (b:lines snapshot)))

(defun %range (name from to)
  "The lines FROM through TO, which is what a window asks for and the only
reason the whole file would ever be walked."
  (let* ((lines (%lines (%snapshot name)))
         (size (and lines (fset:size lines))))
    (when lines
      (fset:subseq lines
                   (max 0 (min from size))
                   (max 0 (min (1+ to) size))))))

(defun %span (segment)
  "FROM..TO, when a segment names a range rather than one line."
  (let ((dots (search ".." segment)))
    (when dots
      (let ((from (parse-integer segment :end dots :junk-allowed t))
            (to (parse-integer segment :start (+ dots 2) :junk-allowed t)))
        (when (and from to) (cons from to))))))

(defun provider ()
  (ns:provider
   (/buf/?name/line/?which
    {:read (pine.data:fn []
             (let ((span (%span which)))
               (if span
                   (%range name (car span) (cdr span))
                   (let ((n (parse-integer which :junk-allowed t)))
                     (when n
                       (let ((lines (%lines (%snapshot name))))
                         (and lines (fset:lookup lines n))))))))
     :doc "one line, or the range FROM..TO a window is showing"})
   (/buf/?name/text
    {:read (pine.data:fn []
             (let ((actor (%actor name)))
               (and actor (pine.core.actor:ask actor '(:get-text) :timeout 5))))
     :write (pine.data:fn [v] (%tell name (list :replace-content :content v)))
     :verbs {:insert (pine.data:fn [text] (%tell name (list :insert :text text)))
             :newline (pine.data:fn [] (%tell name '(:newline)))
             :backspace (pine.data:fn [] (%tell name '(:backspace)))
             :undo (pine.data:fn [] (%tell name '(:undo)))
             :redo (pine.data:fn [] (%tell name '(:redo)))}
     :doc "the whole string; [:insert TEXT] [:newline] [:backspace] [:undo]"})
   (/buf/?name/lines
    {:read (pine.data:fn [] (let ((s (%snapshot name))) (and s (b:line-count s))))
     :doc "how many lines there are"})
   (/buf/?name/point
    {:read (pine.data:fn []
             (let ((s (%snapshot name)))
               (when s (fset:seq (b:point-line s) (b:point-col s)))))
     :write (pine.data:fn [v]
              (%tell name (list :move-point :line (fset:lookup v 0)
                                            :col (fset:lookup v 1))))
     :verbs {:move (pine.data:fn [unit n]
                     (%tell name (list :move-by :unit unit :n n)))}
     :doc "[line col]; [:move :word 1] to step by something"})
   (/buf/?name/mode
    {:read (pine.data:fn []
             (let ((s (%snapshot name))) (and s (b:buffer-local s :mode))))
     :write (pine.data:fn [v] (%tell name (list :set-meta :key :mode :value v)))
     :doc "the mode keyword"})
   (/buf/?name/file
    {:read (pine.data:fn []
             (let ((s (%snapshot name))) (and s (b:buffer-local s :pathname))))
     :doc "the file it visits"})
   (/buf/?name/tick
    {:read (pine.data:fn [] (let ((s (%snapshot name))) (and s (b:tick s))))
     :doc "moves on an edit, not on a motion"})
   (/buf/?name/?local
    {:read (pine.data:fn []
             (let ((s (%snapshot name)))
               (and s (b:buffer-local s (intern (string-upcase local) :keyword)))))
     :write (pine.data:fn [v]
              (%tell name (list :set-meta
                                :key (intern (string-upcase local) :keyword)
                                :value v)))
     :doc "a buffer local; with no value here, the same leaf at the root"})
   (/buf/?name
    {:read (pine.data:fn []
             (let ((s (%snapshot name)))
               (when s
                 (fset:map (:lines (b:line-count s))
                           (:point (fset:seq (b:point-line s) (b:point-col s)))
                           (:mode (b:buffer-local s :mode))
                           (:file (b:buffer-local s :pathname))))))
     :doc "what is known about the buffer"})
   (/buf
    {:ls (pine.data:fn [] (%names))
     :doc "every buffer"})))

(defun mount ()
  (ns:write /buf (provider)))
