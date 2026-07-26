(in-package :cl-user)

;;;; Headless eyes for the editor's split pipeline: a real session at pixel
;;;; geometry, the tree shipped through node->wire / wire->node and painted
;;;; with paint-arranged -- the frontend's exact path -- one PNG per step.

(defvar *cw* 9)
(defvar *ch* 19)
(defvar *cols* 84)
(defvar *rows* 30)

(let ((srv (pine.core.server:start-server)))
  (setf pine.core.server:*server* srv
        (pine.core.server:ts-runtime srv) (pine.ts:make-ts-runtime))
  (ignore-errors (pine.ts:ensure-ts (pine.core.server:ts-runtime srv)))
  (pine.core.event:make-event-bus srv)
  (pine.core.actor:start-agent-registry srv)
  (pine.core.actor:start-local-agent srv)
  (pine.text.buffer:start-buffer-registry srv)
  (let* ((sess (pine.editor:make-editor-session
                nil :sink (lambda (&rest _) (declare (ignore _)) nil)))
         (client (pine.editor::sess-client sess))
         (w (* *cols* *cw*))
         (h (* *rows* *ch*)))
    (let ((buf (pine.editor.frame:current-buffer client)))
      (pine.editor.ask:tell buf :replace-content
                        :content (format nil "(defun alpha (x)~%  (list x :a))~%~%(defun beta (y)~%  (* y 2))~%")))
    (pine.editor:session-feed sess (list :resize :cols *cols* :rows *rows*
                                         :width w :height h
                                         :cell-w *cw* :cell-h *ch*))
    (sleep 0.7)
    (labels ((dump (n depth)
               (format t "~&~v@T~a kind=~a expand=~a rect=(~a ~a ~a ~a) rows=~a~%"
                       (* 2 depth) (type-of n)
                       (and (typep n 'pine.ui.node:window-node)
                            (pine.ui.node:window-kind n))
                       (pine.ui.node:expand-of n)
                       (pine.ui.node:start-line n) (pine.ui.node:start-col n)
                       (pine.ui.node:end-line n) (pine.ui.node:end-col n)
                       (and (typep n 'pine.ui.node:window-node)
                            (length (pine.ui.node:window-rows n))))
               (dolist (c (pine.ui.node:nodes-of n)) (dump c (1+ depth))))
             (shot (name)
             (let* ((pine.editor.frame:*client* client)
                    (tree (pine.ui.render:refresh-editor-tree client))
                    (node (pine.ui.node:wire->node (pine.ui.node:node->wire tree))))
               (format t "~&== ~a~%" name)
               (dump tree 0)
               (pine.ui.node:with-cairo-layout
                 (let ((surface (cairo:create-image-surface :argb32 w h)))
                   (cairo:with-context ((cairo:create-context surface))
                     (cairo:set-source-rgb 0.10 0.09 0.11)
                     (cairo:paint)
                     (if (pine.ui.node:arranged-p node)
                         (pine.ui.node:paint-arranged node)
                         (pine.ui.node:paint-tree node w h)))
                   (cairo:surface-write-to-png
                    surface (format nil "/tmp/split-~a.png" name))
                   (format t "~&wrote /tmp/split-~a.png~%" name)))))
           (key (s &key ctrl)
             (pine.editor:session-feed sess (list :key :key-str s :ctrl ctrl))
             (sleep 0.25)))
      (shot "0-initial")
      (key "x" :ctrl t) (key "2") (sleep 0.3) (shot "1-cx2")
      (key "x" :ctrl t) (key "3") (sleep 0.3) (shot "2-cx3")
      (key "x" :ctrl t) (key "o")
      (key "z") (key "a") (key "p") (sleep 0.3) (shot "3-other-typed")
      (key "x" :ctrl t) (key "2") (sleep 0.3) (shot "4-cx2-again")
      (key "x" :ctrl t) (key "1") (sleep 0.3) (shot "5-cx1"))))
(finish-output)
(sb-ext:exit)
