(in-package :pine.render)

(defun rs@ (key)
  (fset:@ (pine.client:render-state (pine.client:current-client)) key))

(defun rs-update (&rest pairs)
  (let* ((client (pine.client:current-client))
         (rs (pine.client:render-state client)))
    (loop for (k v) on pairs by #'cddr
          do (setf rs (fset:with rs k v)))
    (setf (pine.client:render-state client) rs)))


;;;; Text conversion for tree-sitter

(defun lines-to-string (ls)
  (let* ((n (fset:size ls))
         (total (loop for i from 0 below n
                      sum (length (fset:@ ls i))
                      sum (if (plusp i) 1 0)))
         (result (make-array total :element-type 'base-char))
         (pos 0))
    (dotimes (i n)
      (when (plusp i)
        (setf (aref result pos) #\Newline)
        (incf pos))
      (let ((line (fset:@ ls i)))
        (dotimes (j (length line))
          (let* ((ch (char line j))
                 (code (char-code ch)))
            (setf (aref result pos) (if (<= code 255) (code-char code) #\?))
            (incf pos)))))
    result))


;;;; Tree-sitter parse dispatch

(defun ts-request-parse (buffer-actor snap language)
  (let ((ts-actor (pine.client:ts-actor (pine.client:current-client))))
    (when (and ts-actor snap language (typep snap 'pine.buffer:snapshot))
      (let ((text (lines-to-string (pine.buffer:lines snap))))
        (sento.actor:tell ts-actor
                          (list :parse
                                :text text
                                :language language
                                :reply-to buffer-actor))))))


;;;; Tree-sitter actor (lives on server; shared across clients)

(defun start-ts-actor (server)
  (let ((ts-rt (pine.server:ts-runtime server))
        (sys (pine.server:actor-system server)))
    (when (and ts-rt (pine.ts:ts-loaded-p ts-rt))
      (sento.actor-context:actor-of sys
        :name "ts-parser"
        :receive
        (lambda (msg)
          (case (first msg)
            (:parse
             (destructuring-bind (&key text language reply-to) (rest msg)
               (let ((hl (handler-case
                             (pine.ts:compute-highlights ts-rt language text)
                           (error () nil))))
                 (when reply-to
                   (sento.actor:tell reply-to
                                     (list :highlights :highlights hl))))))))))))


;;;; Renderer actor (per-client)

(defun start-renderer (client)
  (let* ((server (pine.client:server-of client))
         (sys (pine.server:actor-system server)))
    (setf (pine.client:ts-actor client) (start-ts-actor server))
    (let ((renderer
            (sento.actor-context:actor-of sys
                                          :name (format nil "renderer-~a" (gensym "R"))
                                          :state nil
                                          :receive
                                          (let ((cli client))
                                            (lambda (msg)
                                              (let ((pine.client:*client* cli))
                                                (handler-case
                                                    (case (first msg)
                                                      (:snapshot
                                                       (destructuring-bind (&key snapshot) (rest msg)
                                                         (apply-snapshot snapshot)
                                                         (let ((w (pine.client:focused-window cli)))
                                                           (when w
                                                             (let* ((name (pine.buffer:name snapshot))
                                                                    (tick (pine.buffer:tick snapshot))
                                                                    (rs   (pine.client:render-state cli))
                                                                    (ticks (or (fset:@ rs :parsed-ticks)
                                                                               (fset:empty-map)))
                                                                    (last (or (fset:@ ticks name) -1))
                                                                    (mode-name (pine.buffer:buffer-local
                                                                                snapshot :mode :base-mode))
                                                                    (mode (pine.mode:find-mode mode-name))
                                                                    (lang (and mode
                                                                               (pine.mode:ts-language mode))))
                                                               (when (and lang (> tick last))
                                                                 (setf (pine.client:render-state cli)
                                                                       (fset:with rs :parsed-ticks
                                                                                  (fset:with ticks name tick)))
                                                                 (let ((buf (find-buffer-for-snap snapshot)))
                                                                   (when buf
                                                                     (ts-request-parse buf snapshot lang)))))))
                                                         (rs-update :dirty t)))

                                                      (:resize
                                                       (destructuring-bind (&key cols rows) (rest msg)
                                                         (let ((f (pine.client:frame cli)))
                                                           (setf (pine.buffer:frame-cols f) cols
                                                                 (pine.buffer:frame-rows f) rows))
                                                         (relayout)
                                                         (pine.buffer:ensure-frame-cells (pine.client:frame cli))
                                                         (pine.term:resize-active-terminal cols rows)
                                                         (rs-update :dirty t)
                                                         #+lqml (start-render-loop)))

                                                      (:switch-buffer
                                                       (destructuring-bind (&key buffer name) (rest msg)
                                                         (switch-window-buffer buffer name)))

                                                      (:force-render
                                                       (rs-update :dirty t)))
                                                  (error () nil))))))))
      (setf (pine.client:renderer client) renderer)
      renderer)))


;;;; Window management

(defun find-buffer-for-snap (snap)
  (when (typep snap 'pine.buffer:snapshot)
    (let ((snap-name (pine.buffer:name snap)))
      (loop for w in (pine.client:windows (pine.client:current-client))
            when (string= (pine.buffer:window-name w) snap-name)
              return (pine.buffer:buffer-ref w)))))

(defun apply-snapshot (snap)
  (when (typep snap 'pine.buffer:snapshot)
    (let ((snap-name (pine.buffer:name snap)))
      (dolist (w (pine.client:windows (pine.client:current-client)))
        (when (string= (pine.buffer:window-name w) snap-name)
          (setf (pine.buffer:snap w) snap))))))

(defun switch-window-buffer (buf name)
  (let ((w (pine.client:focused-window (pine.client:current-client))))
    (when w
      (setf (pine.buffer:buffer-ref w) buf
            (pine.buffer:window-name w) name
            (pine.buffer:scroll-top w) 0
            (pine.buffer:col w) 0
            (pine.buffer:snap w) nil
            (pine.buffer:win-display w) nil)))
  (rs-update :dirty t))

(defun relayout ()
  (let* ((client (pine.client:current-client))
         (f (pine.client:frame client))
         (cols (pine.buffer:frame-cols f))
         (rows (pine.buffer:frame-rows f)))
    (dolist (w (pine.client:windows client))
      (setf (pine.buffer:row w) 0
            (pine.buffer:col w) 0
            (pine.buffer:win-width w) cols
            (pine.buffer:win-height w) rows))))


;;;; Cell emission

(defun parse-hex-color (hex)
  (if (and hex (> (length hex) 6) (char= (char hex 0) #\#))
      (list (parse-integer hex :start 1 :end 3 :radix 16)
            (parse-integer hex :start 3 :end 5 :radix 16)
            (parse-integer hex :start 5 :end 7 :radix 16))
      (list 205 214 244)))

(defun face-rgb (face-name)
  (let ((f (when face-name (pine.buffer:find-face face-name))))
    (if (and f (pine.buffer:fg f))
        (parse-hex-color (pine.buffer:fg f))
        (list 205 214 244))))

(defun build-highlight-table (highlights scroll-top visible-rows)
  (let ((table (make-hash-table)))
    (dolist (h highlights)
      (let ((line (first h)) (sc (second h)) (ec (third h)) (face (fourth h)))
        (when (and (>= line scroll-top) (< line (+ scroll-top visible-rows)))
          (push (list sc ec face) (gethash (- line scroll-top) table)))))
    (maphash (lambda (k v)
               (setf (gethash k table) (nreverse v)))
             table)
    table))

(defun face-priority (face)
  (case face
    (:keyword        10)
    (:function-name   8)
    (:builtin         7)
    (:constant        6)
    (:escape          6)
    (:string          5)
    (:comment         5)
    (:type            4)
    (:variable-param  3)
    (:variable        1)
    (t                0)))

(defun build-face-slots (line-hl line-length)
  (let ((slots (make-array line-length :initial-element nil))
        (prios (make-array line-length :initial-element -1)))
    (dolist (h line-hl)
      (let* ((sc (first h))
             (ec (min (second h) line-length))
             (face (third h))
             (p (face-priority face)))
        (when face
          (loop for c from (max 0 sc) below ec
                when (> p (aref prios c))
                  do (setf (aref slots c) face
                           (aref prios c) p)))))
    slots))

(defun render-buffer-to-frame (w)
  (let ((f (pine.client:frame (pine.client:current-client))))
    (pine.buffer:ensure-frame-cells f)
    (let* ((s (pine.buffer:snap w))
           (dl (pine.buffer:win-display w))
           (wid (pine.buffer:win-width w))
           (cells (pine.buffer:frame-cells f))
           (left (pine.buffer:col w))
           (hl (pine.buffer:highlights s))
           (hl-table (when hl
                       (build-highlight-table hl (pine.buffer:scroll-top w) (length dl))))
           (off 0))
      (loop for d in dl
            for display-row from 0
            for row = (+ (pine.buffer:row w) display-row)
            for text = (pine.buffer:display-text d)
            for buf-line-idx = (+ display-row (pine.buffer:scroll-top w))
            do (let* ((line-hl (when hl-table (gethash display-row hl-table)))
                      (buf-line-len (if (< buf-line-idx (pine.buffer:line-count s))
                                        (length (fset:@ (pine.buffer:lines s) buf-line-idx))
                                        0))
                      (face-slots (when line-hl
                                    (build-face-slots line-hl buf-line-len))))
                 (loop for i from 0 below (min (length text) wid)
                       for ch = (char-code (char text i))
                       for col = i
                       for buf-col = (+ i left)
                       for face = (when (and face-slots (< buf-col (length face-slots)))
                                    (aref face-slots buf-col))
                       for rgb = (face-rgb face)
                       do (setf (svref cells (+ off 0)) row
                                (svref cells (+ off 1)) col
                                (svref cells (+ off 2)) ch
                                (svref cells (+ off 3)) (first rgb)
                                (svref cells (+ off 4)) (second rgb)
                                (svref cells (+ off 5)) (third rgb)
                                (svref cells (+ off 6)) -1
                                (svref cells (+ off 7)) -1
                                (svref cells (+ off 8)) -1
                                (svref cells (+ off 9)) nil)
                          (incf off 10))))
      (setf (pine.buffer:frame-cell-count f) off
            (pine.buffer:frame-cursor-row f)
            (max 0 (min (- (pine.buffer:point-line s) (pine.buffer:scroll-top w))
                        (1- (pine.buffer:win-height w))))
            (pine.buffer:frame-cursor-col f)
            (- (pine.buffer:point-col s) (pine.buffer:col w))
            (pine.buffer:frame-scroll-pixel f)
            (coerce (pine.buffer:scroll-top w) 'double-float)
            (pine.buffer:frame-dirtyp f) t))))


;;;; Render loop (Qt thread)
;;;;
;;;; Persistent self-perpetuating tick. Every render-tick resets the next
;;;; one via unwind-protect, so an error inside the body can never stall
;;;; the loop.

(defun start-render-loop ()
  #+lqml
  (let ((client (pine.client:current-client)))
    (unless (fset:@ (pine.client:render-state client) :loop-started)
      (setf (pine.client:render-state client)
            (fset:with (pine.client:render-state client) :loop-started t))
      (qml:qsingle-shot 16 #'render-tick))))

(defun render-tick ()
  #+lqml
  (unwind-protect
       (handler-case
           (let ((client pine.client:*client*))
             (when client
               (let ((rs (pine.client:render-state client))
                     (w (pine.client:focused-window client))
                     (f (pine.client:frame client)))
                 (when (and w (fset:@ rs :dirty))
                   (let ((term (pine.term:terminal-for-buffer
                                (pine.buffer:buffer-ref w))))
                     (cond
                       ((and term (pine.term:terminal-visible-p))
                        (setf (pine.client:render-state client)
                              (fset:with rs :dirty nil))
                        (pine.term:render-terminal-to-frame)
                        (pine.qml:push-frame)
                        (pine.qml:update-status-text
                         (format nil "*terminal*  |  L~a:C~a"
                                 (1+ (pine.buffer:frame-cursor-row f))
                                 (1+ (pine.buffer:frame-cursor-col f)))))
                       (t
                        (let ((s (pine.buffer:snap w)))
                          (when s
                            (setf (pine.client:render-state client)
                                  (fset:with rs :dirty nil))
                            (pine.buffer:ensure-point-visible w)
                            (pine.buffer:ensure-col-visible w)
                            (setf (pine.buffer:win-display w)
                                  (pine.buffer:window-display-lines w))
                            (render-buffer-to-frame w)
                            (pine.qml:push-frame)
                            (pine.qml:update-status-text
                             (format nil "~a  |  L~a:C~a"
                                     (pine.buffer:window-name w)
                                     (1+ (pine.buffer:point-line s))
                                     (1+ (pine.buffer:point-col s)))))))))))))
         (error () nil))
    (qml:qsingle-shot 16 #'render-tick)))


;;;; Subscription
;;;; Subscribers are identified by the client's renderer actor ref.
;;;; Buffers `tell` snapshots to the ref; unsubscribe matches by eq.

(defun subscribe-to-buffer (buffer-actor)
  (let ((renderer (pine.client:renderer (pine.client:current-client))))
    (when (and buffer-actor renderer)
      (sento.actor:tell buffer-actor
                        (list :subscribe :renderer renderer)))))

(defun unsubscribe-from-buffer (buffer-actor)
  (let ((renderer (pine.client:renderer (pine.client:current-client))))
    (when (and buffer-actor renderer)
      (sento.actor:tell buffer-actor
                        (list :unsubscribe :renderer renderer)))))
