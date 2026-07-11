(in-package :pine.editor)

(defun start-editor ()
  (let* ((client (pine.client:current-client))
         (server (pine.client:server-of client)))
    (pine.buffer:start-buffer-registry server)
    (handler-case
        (let ((rt (pine.server:ts-runtime server)))
          (pine.ts:ensure-ts rt)
          (format *error-output* "tree-sitter: ~a~%" (pine.ts:ts-loaded-p rt))
          (force-output *error-output*))
      (error (c)
        (format *error-output* "tree-sitter init error: ~a~%" c)
        (force-output *error-output*)))
    (pine.render:start-renderer client)
    (install-default-commands)
    (install-default-bindings)
    (pine.mode:install-default-modes)
    (install-text-mode-bindings)
    (let ((buf (pine.buffer:make-buffer "scratch")))
      (pine.buffer:make-window buf "scratch"
                               :row 0 :col 0 :width 80 :height 29 :focused t)
      (pine.render:subscribe-to-buffer buf)
      (pine.mode:set-buffer-mode buf :text-mode)
      (pine.buffer:tell buf :set-local :key :package :value :pine-user))
    (pine.render:relayout)))

(defun focused-snap ()
  (let ((w (pine.client:focused-window (pine.client:current-client))))
    (when w (pine.buffer:snap w))))

(defun move-point-up ()
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client)))
    (when buf
      (let ((snap (focused-snap)))
        (when (and snap (plusp (pine.buffer:point-line snap)))
          (let* ((new-line (1- (pine.buffer:point-line snap)))
                 (new-col (min (pine.buffer:point-col snap)
                               (length (fset:@ (pine.buffer:lines snap) new-line)))))
            (sento.actor:tell buf (list :move-point :line new-line :col new-col))))))))

(defun move-point-down ()
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client)))
    (when buf
      (let ((snap (focused-snap)))
        (when (and snap (< (1+ (pine.buffer:point-line snap))
                           (pine.buffer:line-count snap)))
          (let* ((new-line (1+ (pine.buffer:point-line snap)))
                 (new-col (min (pine.buffer:point-col snap)
                               (length (fset:@ (pine.buffer:lines snap) new-line)))))
            (sento.actor:tell buf (list :move-point :line new-line :col new-col))))))))

(defun move-point-left ()
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client)))
    (when buf
      (let ((snap (focused-snap)))
        (when snap
          (cond
            ((plusp (pine.buffer:point-col snap))
             (sento.actor:tell buf
                               (list :move-point
                                     :line (pine.buffer:point-line snap)
                                     :col (1- (pine.buffer:point-col snap)))))
            ((plusp (pine.buffer:point-line snap))
             (let* ((new-line (1- (pine.buffer:point-line snap)))
                    (new-col (length (fset:@ (pine.buffer:lines snap) new-line))))
               (sento.actor:tell buf (list :move-point :line new-line :col new-col))))))))))

(defun move-point-right ()
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client)))
    (when buf
      (let ((snap (focused-snap)))
        (when snap
          (let ((line-len (length (fset:@ (pine.buffer:lines snap)
                                          (pine.buffer:point-line snap)))))
            (cond
              ((< (pine.buffer:point-col snap) line-len)
               (sento.actor:tell buf
                                 (list :move-point
                                       :line (pine.buffer:point-line snap)
                                       :col (1+ (pine.buffer:point-col snap)))))
              ((< (1+ (pine.buffer:point-line snap))
                  (pine.buffer:line-count snap))
               (sento.actor:tell buf
                                 (list :move-point
                                       :line (1+ (pine.buffer:point-line snap))
                                       :col 0))))))))))

(defun %point->offset (snap)
  "Char offset of point in the buffer text (newlines count as one char)."
  (let ((pl (pine.buffer:point-line snap))
        (pc (pine.buffer:point-col snap))
        (lines (pine.buffer:lines snap)))
    (+ (loop for i from 0 below pl
             sum (1+ (length (fset:@ lines i))))
       pc)))

(defun %whitespace-p (c)
  (or (char= c #\Space) (char= c #\Tab)
      (char= c #\Newline) (char= c #\Return)))

(defun %sexp-delim-p (c)
  (or (%whitespace-p c)
      (char= c #\() (char= c #\)) (char= c #\")))

(defun %match-paren-backward (text close-pos)
  "Given the offset of a closing paren, walk back to the matching open."
  (let ((depth 1) (i (1- close-pos)) (in-string nil))
    (loop while (>= i 0) do
      (let ((c (char text i)))
        (cond
          (in-string
           (when (and (char= c #\")
                      (or (zerop i)
                          (not (char= (char text (1- i)) #\\))))
             (setf in-string nil))
           (decf i))
          ((char= c #\")
           (setf in-string t)
           (decf i))
          ((char= c #\))
           (incf depth)
           (decf i))
          ((char= c #\()
           (decf depth)
           (when (zerop depth) (return-from %match-paren-backward i))
           (decf i))
          (t (decf i)))))
    nil))

(defun %match-string-backward (text close-pos)
  "Given the offset of a closing quote, walk back to the matching open quote."
  (let ((i (1- close-pos)))
    (loop while (>= i 0) do
      (let ((c (char text i)))
        (cond
          ((and (char= c #\")
                (or (zerop i)
                    (not (char= (char text (1- i)) #\\))))
           (return-from %match-string-backward i))
          (t (decf i)))))
    nil))

(defun %atom-start-backward (text end-inclusive)
  "Walk back over atom chars starting at END-INCLUSIVE; return first atom char."
  (let ((i end-inclusive))
    (loop while (and (>= i 0)
                     (not (%sexp-delim-p (char text i))))
          do (decf i))
    (1+ i)))

(defun %preceding-sexp-bounds (text end)
  "Return (values START END-EXCLUSIVE) of the sexp preceding position END,
skipping trailing whitespace. Returns NIL if nothing is there."
  (let ((i (1- end)))
    (loop while (and (>= i 0) (%whitespace-p (char text i)))
          do (decf i))
    (when (minusp i) (return-from %preceding-sexp-bounds nil))
    (let ((c (char text i)))
      (cond
        ((char= c #\))
         (let ((start (%match-paren-backward text i)))
           (when start (values start (1+ i)))))
        ((char= c #\")
         (let ((start (%match-string-backward text i)))
           (when start (values start (1+ i)))))
        (t
         (values (%atom-start-backward text i) (1+ i)))))))

(defun %buffer-package (state)
  (let ((name (pine.buffer:buffer-local state :package nil)))
    (or (and name (find-package name))
        (find-package :cl-user))))

(defun %show-eval-result (form thunk)
  (handler-case
      (let ((values (multiple-value-list (funcall thunk))))
        #+lqml (pine.qml:update-status-text
                (format nil "=> ~{~s~^, ~}" values))
        (values-list values))
    (error (c)
      #+lqml (pine.qml:update-status-text
              (format nil "error in ~s: ~a" form c))
      nil)))

(defun eval-last-sexp ()
  "Eval the sexp immediately preceding point — same semantics as Emacs:
paren-match back from the char before point, or walk back over an atom."
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (snap (pine.buffer:state->snapshot state))
             (offset (min (%point->offset snap) (length text))))
        (multiple-value-bind (start end) (%preceding-sexp-bounds text offset)
          (if start
              (let* ((*package* (%buffer-package state))
                     (form-text (subseq text start end))
                     (form (read-from-string form-text)))
                (%show-eval-result form (lambda () (eval form))))
              #+lqml (pine.qml:update-status-text "no form before point")))))))

(defun eval-buffer ()
  "Eval every top-level form in the current buffer."
  (let* ((client (pine.client:current-client))
         (buf (pine.client:current-buffer client)))
    (when buf
      (let* ((state (sento.actor:ask-s buf '(:get-state) :time-out 5))
             (text (pine.buffer:state->string state))
             (pos 0)
             (count 0)
             (*package* (%buffer-package state)))
        (handler-case
            (loop
              (multiple-value-bind (form new-pos)
                  (read-from-string text nil :eof :start pos)
                (when (eq form :eof) (return))
                (eval form)
                (incf count)
                (setf pos new-pos)))
          (error (c)
            #+lqml (pine.qml:update-status-text
                    (format nil "eval-buffer error at ~a: ~a" pos c))
            (return-from eval-buffer)))
        #+lqml (pine.qml:update-status-text
                (format nil "eval-buffer: ~a forms" count))))))

(defun scroll-window (delta)
  (let* ((client (pine.client:current-client))
         (w (pine.client:focused-window client))
         (buf (pine.client:current-buffer client)))
    (when (and w buf)
      (let ((snap (pine.buffer:snap w)))
        (when (and snap (typep snap 'pine.buffer:snapshot))
          (let* ((max-scroll (max 0 (- (pine.buffer:line-count snap)
                                       (pine.buffer:win-height w))))
                 (new-scroll (max 0 (min max-scroll
                                         (+ (pine.buffer:scroll-top w) delta))))
                 (h (pine.buffer:win-height w))
                 (pl (pine.buffer:point-line snap))
                 (pc (pine.buffer:point-col snap)))
            (setf (pine.buffer:scroll-top w) new-scroll)
            (cond
              ((< pl new-scroll)
               (sento.actor:tell buf
                                 (list :move-point
                                       :line new-scroll
                                       :col (min pc (length (fset:@ (pine.buffer:lines snap) new-scroll))))))
              ((>= pl (+ new-scroll h))
               (let ((target (+ new-scroll h -1)))
                 (sento.actor:tell buf
                                   (list :move-point
                                         :line target
                                         :col (min pc (length (fset:@ (pine.buffer:lines snap) target))))))))
            (sento.actor:tell (pine.client:renderer client) '(:force-render))))))))
