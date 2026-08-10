(defpackage #:pine.ui.wire
  (:use #:cl #:pine.ui.node #:pine.ui.raster)
  (:local-nicknames (#:d #:pine.data))
  (:export #:apply-rows-patch #:arranged-p #:node->wire #:rows-patch
           #:scroll-to-selection #:wire->node #:wire-views #:wire-tag
           #:defwire))

(in-package #:pine.ui.wire)

(defvar *codec* (d:table)
  "What this image's widgets are, both ways: a wire tag to the function that
rebuilds a node from it, and a node class name to the tag it crosses as.

Filled by DEFWIRE as the files load and read from every thread after.")

(defparameter +base-skip+ '(:key :of :pad :margin)
  "Base initargs that do not cross: the identity the producer keeps, what a node
stands for, the shorthand that sets two others, and the one only the style pass
fills in.

What a node stands for stays here. A frontend paints and sends back the id of
what was interacted with; deciding what that meant is the business of the image
holding the thing it stands for.")

(defparameter +patched+ '(:rows :crow :ccol)
  "The props a rows patch can carry. Two forms that differ only in these can be
sent as a patch; anything else has to go whole.")

(defparameter +base+
  (let ((class (find-class 'pine.ui.node:node)))
    (c2mop:ensure-finalized class)
    (loop :for slot :in (c2mop:class-direct-slots class)
          :for key := (first (c2mop:slot-definition-initargs slot))
          :for reader := (first (c2mop:slot-definition-readers slot))
          :when (and key reader (not (member key +base-skip+)))
            :collect (list key reader
                           (let ((f (c2mop:slot-definition-initfunction slot)))
                             (and f (funcall f))))))
  "What every node carries, as (KEY READER DEFAULT), taken from NODE's own
slots. A style prop added to the base class crosses because it is there, not
because someone remembered to list it here.")

(defun base-props (n)
  "The non-default style props of node N as a plist, plus its arranged rect
when one was assigned, so an arranged tree's geometry crosses too."
  (let ((p nil))
    (dolist (spec +base+)
      (destructuring-bind (key reader default) spec
        (let ((v (funcall reader n)))
          (unless (equal v default) (setf p (list* key v p))))))
    (when (or (plusp (end-col n)) (plusp (end-line n)))
      (setf p (list* :rect (list (start-line n) (start-col n)
                                 (end-line n) (end-col n))
                     p)))
    p))

(defun prop (props key &optional default)
  (let ((v (getf props key)))
    (if (null v) default v)))

(defun props-without (props &rest drop)
  "PROPS as the argument list a constructor takes, less DROP: the base style
props, which are named for their own initargs."
  (loop :for (k v) :on props :by #'cddr
        :unless (member k drop) :append (list k v)))

(defun ordered (props)
  "PROPS in key order, so two forms saying the same thing are EQUAL.

Whether a frame may be sent as a patch is decided by comparing it with the one
before, and a form the frontend rebuilt from a patch has to be the form the
daemon built."
  (let ((pairs (loop :for (k v) :on props :by #'cddr :collect (cons k v))))
    (loop :for (k . v) :in (sort pairs #'string< :key (lambda (pair)
                                                        (symbol-name (car pair))))
          :append (list k v))))

(defun wire-tag (class)
  "The tag CLASS crosses as."
  (d:at (d:all *codec*) class))

(defun wire-default (tag key)
  "What TAG's KEY is when the wire does not carry it.

The codec leaves a default out, so anything that builds a form -- a patch
applied to an older one, say -- has to leave it out too, or two forms that say
the same thing are not the same form."
  (getf (d:at (d:all *codec*) (cons tag :defaults)) key))

(defun with-prop (props key value default)
  "PROPS with KEY set to VALUE, or without it when VALUE is the default."
  (let ((out (props-without props key)))
    (if (equal value default) out (list* key value out))))

(defgeneric node->wire (n &key on-action)
  (:documentation "Serialize node N to plain data.

ON-ACTION, given a handler closure, answers an id to embed. One method per
class, written by DEFWIRE.")
  (:method ((n null) &key on-action) (declare (ignore on-action)) nil))

(defmacro defwire (tag class (&rest props) &key nodes take build)
  "Say how CLASS crosses the wire, both ways, once.

TAG is what it is called on the wire. Each prop is (KEY OUT &key INITARG
DEFAULT ACTION IN): OUT is an expression over N answering the value, INITARG
takes it coming back, a value EQUAL to DEFAULT is left out, ACTION means it is
a closure and crosses as an id, and IN is an expression over PROPS when reading
it back is not just looking it up.

NODES is an expression over N answering the node nodes; TAKE is (INITARG
HOW) saying how they come back, with HOW :first or :list. BUILD replaces the
generated constructor for a class whose forms are not a flat list."
  (let ((keys (mapcar #'first props)))
    `(progn
       (d:keep! *codec* ',class ,tag)
       (d:keep! *codec* (cons ,tag :defaults)
                      (list ,@(loop :for (key nil . options) :in props
                                    :append (list key (getf options :default)))))
       (defmethod node->wire ((n ,class) &key on-action)
         (declare (ignorable on-action))
         (let ((p (base-props n)))
           ,@(loop :for (key out . options) :in props
                   :collect (destructuring-bind (&key default action &allow-other-keys)
                                options
                              `(let ((v ,(if action
                                             `(let ((cb ,out))
                                                (and cb on-action (funcall on-action cb)))
                                             out)))
                                 (setf p (with-prop p ,key v ,default)))))
           (list* ,tag (ordered p)
                  (mapcar (lambda (c) (node->wire c :on-action on-action))
                          ,(or nodes nil)))))
       (d:keep! *codec* ,tag
             (lambda (props forms on-action)
               (declare (ignorable props forms on-action))
               (let ((nodes (mapcar (lambda (c) (wire->node c :on-action on-action))
                                   forms)))
                 (declare (ignorable nodes))
                 ,(or build
                      `(apply #'make-instance ',class
                              ,@(loop :for (key nil . options) :in props
                                      :append
                                      (destructuring-bind
                                          (&key (initarg key) default action in
                                           &allow-other-keys)
                                          options
                                        (list initarg
                                              (cond (in in)
                                                    (action
                                                     `(let ((id (prop props ,key)))
                                                        (and id on-action
                                                             (funcall on-action id))))
                                                    (t `(prop props ,key ,default))))))
                              ,@(when take
                                  (list (first take)
                                        (if (eq (second take) :first) '(first nodes) 'nodes)))
                              (props-without props ,@keys))))))
       ,tag)))

(defwire :label text-node
  ((:content (content n) :default "")
   (:action (on-change n) :initarg :on-change :action t)))

(defwire :rule separator
  ((:char (char-code (sep-char n)) :default #x2500
          :in (code-char (prop props :char #x2500)))
   (:vertical (sep-vertical n) :initarg :vertical)))

(defwire :gap spacer ())

(defwire :slider slider
  ((:value (value n) :default 0)
   (:min (min-of n) :initarg :min :default 0)
   (:max (max-of n) :initarg :max :default 100)
   (:action (on-change n) :initarg :on-change :action t)))

(defwire :calendar calendar
  ((:year (cal-year n) :initarg :year :default 2000)
   (:month (cal-month n) :initarg :month :default 1)
   (:day (cal-day n) :initarg :day :default 1)))

(defwire :image picture
  ((:path (pic-path n) :initarg :path :default "")))

(defwire :view view-node
  ((:rows (view-rows n))
   (:crow (view-crow n) :default -1)
   (:ccol (view-ccol n) :default -1)
   (:opacity (view-opacity n) :default 1.0)
   (:base (view-base n))))

(defwire :ring pine.ui.node:ring
  ((:value (value n) :default 0)
   (:min (min-of n) :initarg :min :default 0)
   (:max (max-of n) :initarg :max :default 100)
   (:thickness (thickness n) :default 5)
   (:diameter (diameter n) :default 56)
   (:arc-face (arc-face n) :default :function-name)
   (:track-face (track-face n) :default :comment))
  :nodes (and (node n) (list (node n)))
  :take (:node :first))

(defwire :action action
  ((:action (callback n) :initarg :callback :action t))
  :nodes (and (node n) (list (node n)))
  :take (:node :first))

(defwire :choice selectable
  ((:selected (selectedp n) :initarg :selected)

   (:action (click n) :initarg :click :action t)
   (:prefix-selected (prefix-selected n) :default "> ")
   (:prefix-unselected (prefix-unselected n) :default "  "))
  :nodes (and (node n) (list (node n)))
  :take (:node :first))

(defwire :box box
  ((:width (width-of n) :initarg :width :default 0)
   (:align (align n) :default :left))
  :nodes (and (node n) (list (node n)))
  :take (:node :first))

(defwire :scroll scroll
  ((:height (vheight n) :initarg :height :default 10))
  :nodes (and (node n) (list (node n)))
  :take (:node :first))

(defwire :center center ()
  :nodes (and (node n) (list (node n)))
  :take (:node :first))

(defwire :column vstack
  ((:spacing (spacing n) :default 0)
   (:align (align n) :default :start))
  :nodes (nodes n)
  :take (:nodes :list))

(defwire :row hstack
  ((:spacing (spacing n) :default 1)
   (:align (align n) :default :start))
  :nodes (nodes n)
  :take (:nodes :list))

(defwire :stack stack ()
  :nodes (nodes n)
  :take (:nodes :list))

(defwire :list list-node ()
  :nodes (pine.ui.layout:list-items n)
  :build (apply #'make-instance 'vstack :nodes nodes :spacing 0 :align :start
                (props-without props)))

(defwire :centerbox pine.ui.node:centerbox
  ((:orient (cb-orient n) :initarg :orient :default :v))
  :nodes (list (cb-start n) (cb-center n) (cb-end n))
  :build (apply #'make-instance 'pine.ui.node:centerbox
                :orient (prop props :orient :v)
                :start (first nodes) :center (second nodes) :end (third nodes)
                (props-without props :orient)))

(defun wire->node (form &key on-action)
  "Rebuild a node from wire FORM, restoring its arranged rect when the wire
carries one. ON-ACTION, given an id, answers a handler -- the renderer's 'send
this id back'."
  (when form
    (destructuring-bind (tag props &rest forms) form
      (let ((rect (getf props :rect))
            (build (d:at (d:all *codec*) tag)))
        (unless build (error "No widget crosses the wire as ~s." tag))
        (let ((n (funcall build (props-without props :rect) forms on-action)))
          (when (and n rect)
            (destructuring-bind (sl sc el ec) rect
              (setf (start-line n) sl (start-col n) sc
                    (end-line n) el (end-col n) ec)))
          n)))))

(defun arranged-p (n)
  "True when N carries an arranged rect (from its own arrange, or the wire)."
  (or (plusp (end-col n)) (plusp (end-line n))))

(defun view-tag ()
  "What a window crosses as. Asked of the codec rather than written out, so a
window that changed its tag does not leave these walks matching a keyword
nothing uses."
  (wire-tag 'view-node))

(defun wire-views (form)
  "Every window form in FORM, in tree order."
  (let ((tag (view-tag))
        (acc nil))
    (labels ((walk (f)
               (when (and (consp f) (keywordp (first f)))
                 (when (eq (first f) tag) (push f acc))
                 (dolist (node (cddr f)) (walk node)))))
      (walk form))
    (nreverse acc)))

(defun wire-shape (form)
  "FORM with everything a patch can carry removed.

Two forms with the same shape differ only in what a patch can express, which is
the test for whether one may be sent."
  (if (and (consp form) (keywordp (first form)))
      (let ((props (second form)))
        (when (eq (first form) (view-tag))
          (setf props (apply #'props-without props +patched+)))
        (list* (first form) props (mapcar #'wire-shape (cddr form))))
      form))

(defun rows-patch (old new)
  "What changed between two wire forms, or NIL when a patch cannot say it.

The patch is one entry per window, (INDEX CROW CCOL (LINE . ROW)...), carrying
only the lines that differ. NIL means the caller must send FORM whole: a
different tree, a different number of lines, or no previous form at all."
  (when (and old new (equal (wire-shape old) (wire-shape new)))
    (let* ((tag (view-tag))
           (olds (wire-views old))
           (news (wire-views new)))
      (when (= (length olds) (length news))
        (loop :for o :in olds
              :for n :in news
              :for index :from 0
              :for o-rows := (prop (second o) :rows)
              :for n-rows := (prop (second n) :rows)
              :unless (= (length o-rows) (length n-rows))
                :do (return-from rows-patch nil)

              :collect (list index
                             (prop (second n) :crow (wire-default tag :crow))
                             (prop (second n) :ccol (wire-default tag :ccol))
                             (loop :for a :in o-rows
                                   :for b :in n-rows
                                   :for line :from 0
                                   :unless (equal a b)
                                     :collect (cons line b))))))))

(defun apply-rows-patch (form patch)
  "FORM with PATCH applied: a fresh wire form carrying the patched lines."
  (let ((tag (view-tag))
        (index -1))
    (labels ((patched (f)
               (if (and (consp f) (keywordp (first f)))
                   (if (eq (first f) tag)
                       (let* ((entry (assoc (incf index) patch))
                              (props (second f)))
                         (cond
                           ((null entry) f)
                           (t (destructuring-bind (crow ccol lines) (rest entry)
                                (let ((rows (copy-list (prop props :rows))))
                                  (dolist (line lines)
                                    (setf (nth (car line) rows) (cdr line)))
                                  (list* tag
                                         (ordered
                                          (with-prop
                                           (with-prop
                                            (with-prop props :rows rows
                                                       (wire-default tag :rows))
                                            :crow crow (wire-default tag :crow))
                                           :ccol ccol (wire-default tag :ccol)))
                                         (cddr f)))))))
                       (list* (first f) (second f) (mapcar #'patched (cddr f))))
                   f)))
      (patched form))))

(defun scroll-to-selection (sel offset max-vis)
  (when (minusp sel)
    (return-from scroll-to-selection (max 0 offset)))
  (let ((o offset))
    (when (>= sel (+ o max-vis)) (setf o (1+ (- sel max-vis))))
    (when (< sel o)              (setf o sel))
    (max 0 o)))
