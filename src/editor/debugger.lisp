(defpackage #:pine.editor.debugger
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:err #:pine.err))
  (:shadow #:fault)
  (:export #:install #:ids #:fault #:at #:attended #:attend #:take #:next
           #:quit #:abort-attended #:restarts #:jobs #:jobs-builder #:text-layout))

(in-package #:pine.editor.debugger)
(named-readtables:in-readtable pine.path:syntax)


;;;; The debugger is a view over /err.
;;;;
;;;; Not a callback per fault and not a registry of its own: whatever put one
;;;; there -- an eval on its own thread, a parser actor that unwound, an agent
;;;; in another image -- reads the same, and a fault someone else decided
;;;; simply stops being in the list. Picking a restart is a write to the path,
;;;; so the same keystroke works on a fault in this image and one three hosts
;;;; away.
;;;;
;;;; What it holds is at /buf/*debugger*/..., because it is what that buffer is
;;;; showing and a buffer's own leaves are where a buffer's own state goes. Two
;;;; frontends attached to one daemon each look at their own space, and neither
;;;; is writing over the other's idea of which fault is up.
;;;;
;;;; The rows are an expression over /err. Nothing here renders: a fault
;;;; arriving moves a path, and what read that path is computed again, which is
;;;; the same rule as everything else in the tree. What the watch decides is
;;;; only which fault is attended and whether the buffer is in front, and both
;;;; of those are writes.

(defparameter +buffer+ "*debugger*")

(defun at (leaf)
  "Where the debugger keeps LEAF: a leaf of the buffer it is."
  (pine.buf:at +buffer+ leaf))

(defun ids ()
  "Every fault at /err, oldest first.

Reading /err itself first is what makes this a dependency: an expression built
from these is computed again when a fault arrives or is decided, because it
read the path that moved."
  (ns:read /err)
  (sort (loop :for path :in (ns:matches /err/*)
              :for id := (parse-integer (p:leaf path) :junk-allowed t)
              :when id :collect id)
        #'<))

(defun fault (id)
  "The map at /err/?id, or NIL when nothing is there any more."
  (let ((m (ns:read /err/${id})))
    (and (fset:map? m) m)))

(defun attended ()
  "The fault the *debugger* buffer is showing, or NIL."
  (ns:read (at :attended)))

(defun restarts (&optional (id (attended)))
  "The restart names ID can still be decided by."
  (let ((m (and id (fault id))))
    (when m
      (let ((names (fset:lookup m :restarts)))
        (and (fset:seq? names) (fset:convert 'list names))))))

(defun take (name)
  "Decide the attended fault: write the restart it should take.

Where that lands is the fault's business. A local evaluation is standing in it
and wakes up; an agent's fault goes back over remoting; one whose stack is
already gone simply stops being a fault."
  (let ((id (attended)))
    (cond ((null id) (pine.echo:message "no fault in the debugger") nil)
          (t (ns:write /err/${id} (fset:seq :restart name))
             (pine.echo:message (format nil "invoked ~a" name))
             t))))

;;;; What it looks like. The restart rows are selectable nodes whose activation
;;;; writes that restart to the fault's path, so Return / C-n / C-p are the
;;;; ordinary surface interaction and point->node needs no side table.

(defun %header (id m)
  (format nil "~a~@[: ~a~]" (or (fset:lookup m :label) (format nil "fault ~d" id))
          (let ((type (fset:lookup m :type)))
            (and (plusp (length (or type ""))) type))))

(defun %switcher (id live)
  "The row that says which of several faults is up, when there are several."
  (when (> (length live) 1)
    (list (pine.ui.build:label
           (format nil "fault ~d/~d  (Tab: next)   ~{~a~^  |  ~}"
                   (1+ (or (position id live) 0)) (length live)
                   (mapcar (lambda (other)
                             (let ((om (fault other)))
                               (if om (%header other om) (format nil "fault ~d" other))))
                           live))
           :class "dbg-switch")
          (pine.ui.build:label ""))))

(defun %backtrace (m)
  (let ((bt (fset:lookup m :backtrace)))
    (when (and bt (plusp (length bt)))
      (list* (pine.ui.build:label "")
             (pine.ui.build:label "Backtrace:" :class "dbg-note")
             (mapcar (lambda (l) (pine.ui.build:label l :class "dbg-bt"))
                     (pine.text:split-lines bt))))))

(defun %tree (name)
  "The *debugger* buffer's widget tree, as an expression over /err."
  (declare (ignore name))
  (let* ((live (ids))
         (id (attended))
         (m (and id (fault id))))
    (if (null m)
        (pine.ui.build:column :align :stretch
          (pine.ui.build:label "nothing has faulted" :class "dbg-note"))
        (apply #'pine.ui.build:column :align :stretch
               (append
                (%switcher id live)
                (list (pine.ui.build:label (%header id m) :class "dbg-header")
                      (pine.ui.build:label ""))
                (mapcar (lambda (l) (pine.ui.build:label l :class "dbg-cond"))
                        (pine.text:split-lines (or (fset:lookup m :condition) "")))
                (list (pine.ui.build:label "")
                      (pine.ui.build:label "Restarts (Return on a line):"
                                           :class "dbg-note"))
                (loop :for what :in (restarts id) :collect
                  (pine.ui.build:choice
                   :class "restart" :prefix-selected "" :prefix-unselected ""
                   :data (let ((nm what)) (lambda () (take nm)))
                   (pine.ui.build:label (format nil "  [~a]" (or what ""))
                                        :class "restart-lbl")))
                (%backtrace m))))))

;;;; Which one is up

(defun eval-notify (text)
  "Show TEXT in the echo area and repaint, safely from whatever thread faulted."
  (pine.echo:message text)
  (let ((r (ignore-errors (pine.editor.frame:renderer (pine.editor.frame:current-client)))))
    (when r (sento.actor:tell r '(:force-render)))))

(defun %switch-to (name)
  (let ((client (pine.editor.frame:current-client))
        (buf (pine.buf:live name)))
    (when buf
      (pine.editor.frame:switch-buffer name)
      (let ((r (ignore-errors (pine.editor.frame:renderer client))))
        (when r
          (sento.actor:tell r (list :switch-buffer :buffer buf :name name)))))))

(defun %say-attended (id open)
  "Say whether someone has ID open, so a thread standing in it knows whether a
decision is still coming. Written only when it is not already so: this runs
from a watch on /err, and a write that changed nothing would run it again."
  (when (and id (fault id)
             (not (eq (and open t) (and (ns:read /err/${id}/attended) t))))
    (ns:write /err/${id}/attended (and open t))))

(defun %target-of (m)
  "Where an eval should land while this fault is up: the image that broke, so
C-x C-e and recompile fix the defun where it is."
  (or (fset:lookup m :agent) :local))

(defun attend (id)
  "Show fault ID. The rows follow on their own; what this writes is which one."
  (let ((m (fault id)))
    (when m
      (unless (ns:read (at :return-to))
        (ns:write (at :return-to)
                  (or (ignore-errors (pine.buf:name-of
                                      (pine.editor.frame:current-buffer)))
                      "scratch"))
        (setf (pine.eval:target-was) (pine.eval:target)))
      (let ((was (attended)))
        (unless (eql id was) (%say-attended was nil)))
      (ns:write (at :attended) id)
      (setf (pine.eval:target) (%target-of m))
      (%say-attended id t)
      (%switch-to +buffer+)
      id)))

(defun quit ()
  "Hide the *debugger* without deciding anything. The faults stay at /err and
M-x debugger reopens the attended one."
  (let ((back (ns:read (at :return-to))))
    (when back (%switch-to back))))

(defun %dismiss ()
  "Nothing is faulted any more: put the eval target and the buffer back where
they were before the first one."
  (%say-attended (attended) nil)
  (ns:write (at :attended) nil)
  (ns:write (at :seen) 0)
  (setf (pine.eval:target) (pine.eval:target-was))
  (quit)
  (ns:write (at :return-to) nil))

(defun next ()
  "Page to the next live fault without deciding this one."
  (let ((live (ids)))
    (if (> (length live) 1)
        (let ((pos (or (position (attended) live) 0)))
          (attend (nth (mod (1+ pos) (length live)) live)))
        (pine.echo:message "only one fault"))))

(defun abort-attended ()
  "C-g while a fault is up: take its abort, and interrupt a runaway evaluation
into it rather than only asking."
  (let* ((id (attended))
         (ev (and id (err:find-evaluation id))))
    (when id
      (if ev (err:abort-evaluation ev) (take "ABORT"))
      t)))

;;;; The watch

(defun settle ()
  "Bring the surface to what /err holds, as writes and nothing else.

A new fault takes the buffer, a decided one gives it up, and nothing faulted
puts the editor back where it was. What the buffer shows is not settled here:
its rows read /err, so they follow whatever this decides."
  (let* ((live (ids))
         (newest (first (last live)))
         (seen (or (ns:read (at :seen)) 0)))
    (cond ((null live) (when (attended) (%dismiss)))
          ((> newest seen)
           ;; written first, so the write that says a fault is attended -- which
           ;; is a write under /err, and runs this again -- finds nothing to do
           (ns:write (at :seen) newest)
           (attend newest)
           (let ((m (fault newest)))
             (when m
               (eval-notify
                (format nil "~a  (0-9/Return picks a restart, q quits)"
                        (%header newest m))))))
          ((not (member (attended) live)) (attend newest)))))

(defun install ()
  "Make the editor the thing that looks at /err.

The buffer is opened once, and its rows are an expression over /err: a fault
arriving redraws it the way any other path does, so nothing renders inside the
watch that heard about it. pine.err also asks whether anything is watching
before it holds a thread on a decision, so this is what makes a fault in this
image stand with its restarts live instead of taking its abort for want of
anyone to ask."
  (pine.view:show +buffer+ #'%tree :mode :debugger :switch nil)
  (ns:watch /err (lambda (value) (declare (ignore value)) (settle))
            :as :debugger))

;;;; Other listings

(defun text-layout (text)
  "A read-only layout from TEXT: the first line styled as the heading, the
rest as entries."
  (lambda (state)
    (declare (ignore state))
    (let ((lines (pine.text:split-lines text)))
      (apply #'pine.ui.build:column :align :stretch
             (cons (pine.ui.build:label (or (first lines) "") :class "help-head")
                   (mapcar (lambda (l) (pine.ui.build:label l :class "help-entry"))
                           (rest lines)))))))

;;;; What is running, across every image.
;;;;
;;;; Read rather than reported: an evaluation is one this image started, and a
;;;; fault is at /err whichever image it happened in. There is no third list to
;;;; keep in step with those two.

(defun jobs ()
  "Every evaluation and every fault, as (ID WHERE STATE WHAT)."
  (let ((seen (fset:empty-set))
        (acc nil))
    (dolist (ev (err:list-evaluations))
      (setf seen (fset:with seen (err:evaluation-id ev)))
      (push (list (err:evaluation-id ev) "local"
                  (err:evaluation-status ev)
                  (princ-to-string (or (err:evaluation-form ev) "")))
            acc))
    (dolist (id (ids))
      (let ((m (fault id)))
        (when (and m (not (fset:contains? seen id)))
          (push (list (or (fset:lookup m :eval-id) id)
                      (or (fset:lookup m :agent) "local")
                      :error
                      (or (fset:lookup m :condition) ""))
                acc))))
    (sort acc #'< :key #'first)))

(defun %attend-job (where id)
  "Open the debugger on the fault this job is stopped at, if it is stopped."
  (let ((at (if (equal where "local")
                (and (fault id) id)
                (find-if (lambda (other)
                           (let ((m (fault other)))
                             (and m (equal where (fset:lookup m :agent))
                                  (eql id (fset:lookup m :eval-id)))))
                         (ids)))))
    (if at
        (attend at)
        (pine.echo:message (format nil "job ~a is not stopped" id)))))

(defun jobs-builder ()
  "The *jobs* layout: one selectable row each; Return attends a faulted one."
  (lambda (state)
    (declare (ignore state))
    (apply #'pine.ui.build:column :align :stretch
           (pine.ui.build:label "Jobs  (Return attends a faulted one)"
                                :class "help-head")
           (pine.ui.build:label (format nil "~4@a  ~10a ~9a ~a"
                                        "id" "where" "state" "form / condition")
                                :class "dbg-note")
           (loop :for (id where state what) :in (jobs) :collect
             (let ((id id) (where where))
               (pine.ui.build:choice
                :class "job-row" :prefix-selected "" :prefix-unselected ""
                :data (lambda () (%attend-job where id))
                (pine.ui.build:label
                 (format nil "~4@a  ~10a ~9a ~a" id where state what)
                 :class "help-entry")))))))
