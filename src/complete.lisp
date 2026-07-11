(in-package :pine.editor)

(defun completion ()
  (pine.client:completion-state (pine.client:current-client)))

(defun substring-match-p (input candidate)
  (search (string-downcase input) (string-downcase candidate)))

(defun filter-candidates (input candidates)
  (if (string= input "")
      candidates
      (remove-if-not (lambda (c) (substring-match-p input c)) candidates)))

(defun completing-read (prompt-text candidates cb)
  (let ((c (completion)))
    (setf (pine.client:active-p c) t
          (pine.client:candidates c) candidates
          (pine.client:input c) ""
          (pine.client:index c) (if candidates 0 -1)
          (pine.client:callback c) cb
          (pine.client:filtered c) (filter-candidates "" candidates)
          (pine.client:prompt c) prompt-text)
    (show-completions)
    #+lqml (pine.qml:show-status-input prompt-text)))

(defun completion-accept ()
  (let* ((c (completion))
         (result (if (and (>= (pine.client:index c) 0)
                          (< (pine.client:index c) (length (pine.client:filtered c))))
                     (nth (pine.client:index c) (pine.client:filtered c))
                     (pine.client:input c)))
         (cb (pine.client:callback c)))
    (completion-cleanup)
    (when cb
      (handler-case (funcall cb result)
        (error (c)
          #+lqml (pine.qml:update-status-text (format nil "error: ~a" c)))))))

(defun completion-cancel ()
  (completion-cleanup)
  #+lqml (pine.qml:update-status-text "cancelled"))

(defun completion-cleanup ()
  (let ((c (completion)))
    (setf (pine.client:active-p c) nil
          (pine.client:candidates c) nil
          (pine.client:filtered c) nil
          (pine.client:index c) -1
          (pine.client:input c) ""
          (pine.client:callback c) nil))
  #+lqml (pine.qml:hide-completion-area)
  #+lqml (pine.qml:hide-status-input))

(defun completion-next ()
  (let ((c (completion)))
    (when (pine.client:filtered c)
      (setf (pine.client:index c)
            (min (1+ (pine.client:index c)) (1- (length (pine.client:filtered c)))))
      (show-completions))))

(defun completion-prev ()
  (let ((c (completion)))
    (when (pine.client:filtered c)
      (setf (pine.client:index c) (max 0 (1- (pine.client:index c))))
      (show-completions))))

(defun completion-update-input (text)
  (let ((c (completion)))
    (setf (pine.client:input c) text
          (pine.client:filtered c) (filter-candidates text (pine.client:candidates c))
          (pine.client:index c) (if (pine.client:filtered c) 0 -1))
    (show-completions)))

(defun show-completions ()
  #+lqml
  (let* ((c (completion))
         (max-visible 12)
         (filtered (pine.client:filtered c))
         (n (length filtered))
         (idx (pine.client:index c))
         (visible (subseq filtered 0 (min max-visible n)))
         (lines (loop for cand in visible
                      for i from 0
                      collect (if (= i idx)
                                  (format nil "> ~a" cand)
                                  (format nil "  ~a" cand)))))
    (if lines
        (pine.qml:show-completion-area
         (format nil "~{~a~^~%~}" lines))
        (pine.qml:show-completion-area "(no matches)"))))

(defun completing-read-active-p ()
  (let ((cli pine.client:*client*))
    (and cli (pine.client:active-p (pine.client:completion-state cli)))))
