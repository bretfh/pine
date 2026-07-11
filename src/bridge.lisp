(in-package :pine.qml)

(defvar *ui-ready* nil)

(defun init-ui (client)
  (declare (ignore client))
  (qml:qsingle-shot 100
    (lambda ()
      (setf *ui-ready* t)
      (let ((root (qml:find-quick-item "root")))
        (when root
          (qml:qml-set root "lispReady" t))))))

(defun find-item (name)
  (qml:find-quick-item name))

(defun set-property (item property value)
  (qml:qml-set item property value))


;;;; Frame push

(defun push-frame ()
  (let* ((f (pine.client:frame (pine.client:current-client)))
         (count (pine.buffer:frame-cell-count f)))
    (when (pine.buffer:frame-dirtyp f)
      (setf (pine.buffer:frame-dirtyp f) nil)
      (pine.term:ensure-pushframe)
      (let ((display (find-item "display")))
        (when display
          (let ((ptr (qml::qt-object-address display)))
            (qml:qrun
             (lambda ()
               (pine.term:push-frame-direct
                (pine.buffer:frame-cells f) count
                ptr
                (pine.buffer:frame-cursor-row f)
                (pine.buffer:frame-cursor-col f)
                (pine.buffer:frame-scroll-pixel f)))
             nil)))))))


(defun set-cursor (row col)
  (qml:qrun
   (lambda ()
     (let ((display (find-item "display")))
       (when display
         (set-property display "cursorRow" row)
         (set-property display "cursorCol" col))))
   nil))


;;;; Scroll

(defun on-scroll (lines)
  (pine.editor:scroll-window (truncate lines)))


;;;; Resize

(defun report-resize (cols rows)
  (let ((client pine.client:*client*))
    (when client
      (let ((renderer (pine.client:renderer client)))
        (when renderer
          (sento.actor:tell renderer
                            (list :resize :cols cols :rows rows)))))))


;;;; Status bar

(defun update-status-text (text)
  (qml:qrun
   (lambda ()
     (let ((item (find-item "statusText")))
       (when item (set-property item "text" text))))
   nil))

(defun show-status-input (prompt-text)
  (declare (ignore prompt-text))
  (qml:qrun*
   (let ((input (find-item "statusInput"))
         (status (find-item "statusText")))
     (when (and input status)
       (set-property status "visible" nil)
       (set-property input "visible" t)
       (set-property input "focus" t)))))

(defun hide-status-input ()
  (qml:qrun*
   (let ((input (find-item "statusInput"))
         (status (find-item "statusText")))
     (when (and input status)
       (set-property input "visible" nil)
       (set-property input "text" "")
       (set-property status "visible" t))))
  (qml:qrun*
   (let ((root (find-item "root")))
     (when root (set-property root "focus" t)))))


;;;; Completion area

(defun show-completion-area (text)
  (qml:qrun*
   (let ((area (find-item "completionArea"))
         (ct (find-item "completionText")))
     (when (and area ct)
       (set-property ct "text" text)
       (set-property area "visible" t)))))

(defun hide-completion-area ()
  (qml:qrun*
   (let ((area (find-item "completionArea")))
     (when area (set-property area "visible" nil)))))


;;;; Live input callback

(defun on-input-changed (text)
  (when (pine.editor:completing-read-active-p)
    (pine.editor:completion-update-input text)))
