(defpackage #:pine.echo
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:b #:pine.text))
  (:shadow #:complete #:abort #:next #:last)
  (:export #:server #:message #:current-message #:+buffer+
           #:prompt #:prompt-p #:prompt-text #:result
           #:accept #:abort #:complete-input #:changed
           #:next #:previous #:history-next #:history-previous
           #:said #:forget #:popup-rows #:input #:set-input
           #:complete-selection #:ensure #:snap
           #:candidate #:candidate-string #:candidate-value #:candidate-category
           #:candidate-annotation #:candidate-actions #:to-candidate
           #:complete #:rank #:register-source #:register-actions #:source-table
           #:popup #:widget))

(in-package #:pine.echo)
(named-readtables:in-readtable pine.path:syntax)

;;;; The minibuffer's own leaves live on its buffer: a prompt is a buffer and
;;;; its working out belongs to it, so two minibuffers do not share a selection.

(defparameter +buffer+ "*echo*")

(defparameter +said+
  '(:popup-rows :popup-tree :filtered :index :input :history-items
    :history-pos :back)
  "The leaves a prompt works with, so FORGET knows what to put down.")

(defparameter +separator+ " ")

(defvar *sources* (pine.data:table))

(defvar *actions* (pine.data:table))

(defun %default-directory ()
  (let* ((current (ns:read /buf/current))
         (file (and current (ns:read (p:child current "file")))))
    (if file (directory-namestring file) (namestring (uiop:getcwd)))))

(defun %prompt-in (v)
  "A prompt as it lands. A file prompt starts in the directory the current
buffer is in, unless the write said where, so putting one up is the write and
nothing else."
  (if (and (fset:map? v)
           (eq :file (fset:lookup v :complete))
           (null (fset:lookup v :initial)))
      (fset:with v :initial (%default-directory))
      v))

(defun provider ()
  (ns:provider
   (/echo/hint {:doc "the line pine is saying"})
   (/echo/result {:doc "what was typed"})
   (/echo/cols {:doc "how wide the echo pane is, said by whoever arranged it"})
   (/echo {:in (pine.data:fn [v] (%prompt-in v))
           :doc "the prompt that is up: :prompt :complete :history :initial :then"})))

(defun message (text)
  (ns:write /echo/hint (or text ""))
  nil)

(defun current-message ()
  (or (ns:read /echo/hint) ""))

(defun prompt ()
  (let ((value (ns:held /echo)))
    (when (and (fset:map? value) (fset:lookup value :prompt)) value)))

(defun prompt-p () (and (prompt) t))

(defun prompt-text ()
  (let ((up (prompt))) (and up (fset:lookup up :prompt))))

(defun result () (ns:read /echo/result))

(defun said (key &optional default)
  (ns:read (pine.buf:at +buffer+ key) default))

(defun (setf said) (value key)
  (ns:write (pine.buf:at +buffer+ key) value))

(defun forget ()
  (dolist (key +said+) (setf (said key) nil)))

(defun popup-rows () (said :popup-rows))

;;;; Candidates

(defstruct (candidate (:constructor %make-candidate) (:copier nil))
  (string "" :type string)
  (annotation nil)
  (preview nil)
  (action nil)
  (category nil)
  (source nil)
  (value nil)
  (spans nil)
  (score 0))

(defun candidate (string &key annotation preview action category source
                              (value string))
  (%make-candidate :string string :annotation annotation :preview preview
                   :action action :category category :source source
                   :value value))

(defgeneric to-candidate (x)
  (:documentation "X as something the minibuffer can offer.")
  (:method ((x candidate)) x))

(defmethod to-candidate ((x string))
  (%make-candidate :string x :value x))

(defun %fresh (c)
  (let ((cc (to-candidate c)))
    (%make-candidate :string (candidate-string cc)
                     :annotation (candidate-annotation cc)
                     :preview (candidate-preview cc)
                     :action (candidate-action cc)
                     :category (candidate-category cc)
                     :source (candidate-source cc)
                     :value (candidate-value cc))))

(defgeneric %query (table input)
  (:documentation "The candidates TABLE offers for INPUT, and whatever it says
about them.")
  (:method ((table function) input)
    (multiple-value-bind (cands meta) (funcall table input)
      (values (mapcar #'%fresh cands) meta)))
  (:method ((table list) input)
    (declare (ignore input))
    (values (mapcar #'%fresh table) nil))
  (:method ((table vector) input)
    (declare (ignore input))
    (values (map 'list #'%fresh table) nil)))

(defun literal-matcher (token string)
  (when (plusp (length token))
    (let ((pos (search token string :test #'char-equal)))
      (when pos (cons pos (+ pos (length token)))))))

(defun prefix-matcher (token string)
  (when (and (<= (length token) (length string))
             (string-equal token string :end2 (length token)))
    (cons 0 (length token))))

(defun %orderless-match (input cand matchers separator)
  (let ((s (candidate-string cand)) (spans nil) (score 0))
    (dolist (token (pine.data:split input :on separator :empties nil) (values t (nreverse spans) score))
      (let ((hit (loop :for m :in matchers :thereis (funcall m token s))))
        (if hit
            (progn (push hit spans) (incf score (car hit)))
            (return (values nil nil 0)))))))

(defun orderless (input candidates matchers separator)
  (let (out)
    (dolist (c candidates)
      (multiple-value-bind (ok spans score)
          (%orderless-match input c matchers separator)
        (when ok
          (setf (candidate-spans c) spans (candidate-score c) score)
          (push c out))))
    (nreverse out)))

(defun rank (candidates)
  (stable-sort (copy-list candidates)
               (lambda (a b)
                 (let ((sa (candidate-score a))
                       (sb (candidate-score b))
                       (la (length (candidate-string a)))
                       (lb (length (candidate-string b))))
                   (cond ((not (= sa sb)) (< sa sb))
                         ((not (= la lb)) (< la lb))
                         (t (string-lessp (candidate-string a)
                                          (candidate-string b))))))))

(defun complete (input table &key (styles (list #'orderless))
                                  (matchers (list #'literal-matcher))
                                  (separator +separator+))
  (multiple-value-bind (cands meta) (%query table input)
    (declare (ignore meta))
    (loop :for style :in styles
          :for matched = (funcall style input cands matchers separator)
          :when matched :return (rank matched)
          :finally (return nil))))

(defun register-source (name table) (pine.data:put *sources* name table))
(defun source-table (name) (pine.data:at *sources* name))

(defun register-actions (category alist) (pine.data:put *actions* category alist))
(defun candidate-actions (cand)
  (and cand (pine.data:at *actions* (candidate-category cand))))

(defun popup (visible)
  (apply #'pine.ui.build:column :align :stretch
         (if visible
             (mapcar (lambda (cand)
                       (pine.ui.build:choice
                        :class "cand-row" :prefix-selected "" :prefix-unselected ""
                        (pine.ui.build:row :spacing 1
                          (pine.ui.build:label (candidate-string cand) :class "cand")
                          (pine.ui.build:gap)
                          (let ((a (candidate-annotation cand)))
                            (when a (pine.ui.build:label a :class "cand-annot"))))))
                     visible)
             (list (pine.ui.build:label "(no matches)" :class "cand")))))

(defun %glyph (category)
  (case category
    (:command #x0F0493) (:buffer #x0F0219) (:file #x0F0210)
    (:window #x0F02D1) (:system #x0F0709) (t #x0F0766)))

(defun widget (candidates &key (query "") (selected 0) (title ""))
  (let* ((sel (and candidates
                   (nth (max 0 (min selected (1- (length candidates)))) candidates)))
         (acts (candidate-actions sel)))
    (pine.ui.build:column :class "netmenu" :align :stretch
      (pine.ui.build:row :class "nm-card nm-head" :align :center :spacing 10
        (pine.ui.build:icon #x0F0349 :class "nm-head-ico")
        (pine.ui.build:label query :class "nm-title")
        (pine.ui.build:label "|" :class "nm-title")
        (pine.ui.build:label title :class "nm-subhead"))
      (apply #'pine.ui.build:column :class "nm-card nm-list-card" :align :stretch
             :spacing 3
        (loop :for c :in candidates :for i :from 0 :collect
          (pine.ui.build:row :class (if (= i selected) "nm-row sel" "nm-row")
                             :align :center :spacing 12
            (pine.ui.build:icon (%glyph (candidate-category c)) :class "nm-sig")
            (pine.ui.build:label (candidate-string c) :expand 1
                                 :class (if (= i selected) "nm-name active" "nm-name"))
            (pine.ui.build:label (or (candidate-annotation c) "") :class "nm-lock"))))
      (apply #'pine.ui.build:row :class "nm-actions" :align :center :spacing 16
        (pine.ui.build:label "RET run" :class "nm-name active")
        (append
         (loop :for (label . nil) :in acts :collect
           (pine.ui.build:label label :class "nm-subhead"))
         (list (pine.ui.build:gap :expand 1)
               (pine.ui.build:label "actions" :class "nm-sig hi")))))))

;;;; The prompt over a buffer

(defun make ()
  (pine.buf:make +buffer+)
  +buffer+)

(defun %complete-with ()
  (let ((up (prompt))) (and up (fset:lookup up :complete))))

(defun completing-p () (and (%complete-with) t))
(defun file-p () (eq :file (%complete-with)))

(defun %filtered () (said :filtered))
(defun %index () (said :index -1))

(defun input ()
  (let ((lines (ns:held (pine.buf:at +buffer+ :text))))
    (if (and (fset:seq? lines) (plusp (fset:size lines)))
        (fset:lookup lines 0)
        "")))

(defun set-input (text)
  (ns:write (pine.buf:at +buffer+ :text) text)
  (ns:write (pine.buf:at +buffer+ :point) (fset:seq 0 (length text))))

(defun %expand-tilde (text)
  (cond
    ((string= text "~") (namestring (user-homedir-pathname)))
    ((and (>= (length text) 2) (string= (subseq text 0 2) "~/"))
     (concatenate 'string (namestring (user-homedir-pathname)) (subseq text 2)))
    (t text)))

(defun %split-path (path)
  (let ((slash (position #\/ path :from-end t)))
    (if slash
        (values (subseq path 0 (1+ slash)) (subseq path (1+ slash)))
        (values "" path))))

(defun %entries (dir)
  (handler-case
      (append
       (sort (mapcar (lambda (p)
                       (concatenate 'string (car (cl:last (pathname-directory p))) "/"))
                     (uiop:subdirectories dir))
             #'string<)
       (sort (mapcar #'file-namestring (uiop:directory-files dir)) #'string<))
    (error () nil)))

(defun %file-completions (text)
  (multiple-value-bind (dir base) (%split-path (%expand-tilde text))
    (let ((entries (%entries (if (string= dir "") "./" dir))))
      (if (string= base "")
          entries
          (remove-if-not
           (lambda (n) (and (>= (length n) (length base))
                            (string= base (subseq n 0 (length base)))))
           entries)))))

(defun %common-prefix (strings)
  (if (null strings)
      ""
      (let ((p (first strings)))
        (dolist (s (rest strings) p)
          (let ((n (min (length p) (length s))))
            (setf p (subseq p 0 (or (mismatch p s :end1 n :end2 n) n))))))))

(defun %show ()
  (let* ((most 12)
         (filtered (%filtered))
         (n (length filtered))
         (idx (max 0 (%index)))
         (off (pine.ui.wire:scroll-to-selection idx 0 most))
         (visible (subseq filtered (min off n) (min (+ off most) n)))
         (cols (or (ns:read /echo/cols) 80)))
    (multiple-value-bind (rows tree)
        (pine.ui.cells:render (popup visible) (max 10 cols)
                              :selection (and visible (- idx off)))
      (setf (said :popup-rows) rows
            (said :popup-tree) tree))))

(defun complete-input (text)
  (let ((cands (if (file-p)
                   (mapcar #'to-candidate (%file-completions text))
                   (complete text (%complete-with)))))
    (setf (said :input) text
          (said :filtered) cands
          (said :index) (if cands 0 -1))
    (%show)))

(defun next ()
  (when (%filtered)
    (setf (said :index) (min (1+ (%index)) (1- (length (%filtered)))))
    (%show)))

(defun previous ()
  (when (%filtered)
    (setf (said :index) (max 0 (1- (%index))))
    (%show)))

(defun %selected ()
  (let ((i (%index)) (f (%filtered)))
    (when (and (>= i 0) (< i (length f)))
      (candidate-string (nth i f)))))

(defun ensure ()
  (make))

(defun snap ()
  (b:state->snapshot (pine.buf:state +buffer+)))

(defun changed ()
  (when (prompt-p)
    (when (completing-p) (complete-input (input)))))

;;;; History

(defun %history () (let ((up (prompt))) (and up (fset:lookup up :history))))

(defun %history-at (name) (p:path /history name))

(defun %push-history (text)
  (let ((h (%history)))
    (when (and h (stringp text) (plusp (length text)))
      (ns:write (%history-at h) text :max 200 :keep t))))

(defun history-previous ()
  (let ((h (%history)))
    (when h
      (unless (said :history-items)
        (setf (said :history-items)
              (pine.data:vals (ns:read (p:child (%history-at h) (p:any))
                                       (fset:empty-map)))))
      (let* ((items (said :history-items))
             (pos (said :history-pos))
             (next (if pos (1+ pos) 0)))
        (when (< next (length items))
          (setf (said :history-pos) next)
          (set-input (nth next items)))))))

(defun history-next ()
  (let ((items (said :history-items))
        (pos (said :history-pos)))
    (when (and items pos)
      (if (plusp pos)
          (progn (setf (said :history-pos) (1- pos))
                 (set-input (nth (1- pos) items)))
          (progn (setf (said :history-pos) nil)
                 (set-input ""))))))

;;;; Accept, abort, complete

(defun %finish (text)
  (let ((then (let ((up (prompt))) (and up (fset:lookup up :then)))))
    (%push-history text)
    (ns:write /echo/result text)
    (ns:write /echo nil)
    (handler-case (cond ((null then) nil)
                        ((functionp then) (funcall then text))
                        (t (pine.cmd:run then)))
      (error (e) (message (format nil "error: ~a" e))))))

(defun %file-accept ()
  (let* ((typed (%expand-tilde (input)))
         (sel (%selected))
         (target (multiple-value-bind (dir base) (%split-path typed)
                   (declare (ignore base))
                   (if (and sel (not (uiop:file-exists-p typed))
                            (not (uiop:directory-exists-p typed)))
                       (concatenate 'string dir sel)
                       typed))))
    (if (uiop:directory-exists-p target)
        (set-input (if (and (plusp (length target))
                            (char= (char target (1- (length target))) #\/))
                       target
                       (concatenate 'string target "/")))
        (%finish target))))

(defun accept ()
  (cond
    ((file-p) (%file-accept))
    ((completing-p) (%finish (or (%selected) (input))))
    ((prompt) (%finish (input)))
    (t (ns:write /echo nil))))

(defun abort ()
  (when (prompt)
    (ns:write /echo nil)
    (message "quit")))

(defun complete-selection ()
  (cond ((file-p)
         (let ((cands (mapcar #'candidate-string (%filtered))))
           (multiple-value-bind (dir base) (%split-path (%expand-tilde (input)))
             (declare (ignore base))
             (when cands
               (set-input
                (concatenate 'string dir
                             (if (= 1 (length cands))
                                 (first cands)
                                 (%common-prefix cands))))))))
        ((completing-p)
         (let ((sel (%selected)))
           (when sel (set-input sel))))))

;;;; Opening and closing follow /echo.

(defun %open ()
  (let* ((up (prompt))
         (initial (or (fset:lookup up :initial) "")))
    (unless (said :back) (setf (said :back) (ns:read /buf/current)))
    (ns:write (pine.buf:at +buffer+ :mode) :text)
    (ns:write /buf/current (pine.buf:at +buffer+))
    (ns:write (pine.buf:at +buffer+ :minor) (fset:set :minibuffer))
    (message "")
    (set-input initial)
    (when (completing-p) (complete-input initial))))

(defun %close ()
  (let ((back (said :back)))
    (ns:write (pine.buf:at +buffer+ :minor) (fset:empty-set))
    (forget)
    (when (and back (not (fset:equal? back (pine.buf:at +buffer+))))
      (ns:write /buf/current back))))

(defclass server (ns:server) ()
  (:default-initargs :name :echo :serves (list /echo))
  (:documentation "The minibuffer: prompts, hints, results, and completion."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  (ns:write /echo (provider))
  (ns:watch (pine.buf:at +buffer+ :text)
            (pine.data:fn [v]
              (declare (ignore v))
              (changed)
              {})
            :as :echo-input)
  (ns:watch /echo
            (pine.data:fn [v]
              (declare (ignore v))
              (when (fset:equal? (ns:here) /echo)
                (if (prompt-p) (%open) (%close)))
              {})
            :as :echo)
  (ns:watch /echo/cols
            (pine.data:fn [v]
              (declare (ignore v))
              (when (and (prompt-p) (completing-p)) (%show))
              {})
            :as :echo-cols))

(ns:register (make-instance 'server))
