;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hemlock)

(defmode "Terminal" :major-p t)

(defvar *terminal-buffers* (make-hash-table :test 'eq))

(defstruct terminal-state
  (term nil)
  (process nil)
  (connection nil)
  (pty-fd nil :type (or null fixnum))
  (dirty nil :type boolean)
  (font-info-cache nil)
  (render-buf nil :type (or null simple-string))
  (drain-buf (make-array #x2000 :element-type '(unsigned-byte 8))
             :type (simple-array (unsigned-byte 8) (*)))
  (utf8-carry (make-array 4 :element-type '(unsigned-byte 8))
              :type (simple-array (unsigned-byte 8) (4)))
  (utf8-carry-len 0 :type fixnum))

(defun terminal-state-for-buffer (buffer)
  (gethash buffer *terminal-buffers*))

(defun terminal-state-for-current-buffer ()
  (terminal-state-for-buffer (current-buffer)))

(defun utf8-seq-len (byte)
  (cond ((<= byte #x7F) 1)
        ((<= #xC0 byte #xDF) 2)
        ((<= #xE0 byte #xEF) 3)
        ((<= #xF0 byte #xF7) 4)
        (t 1)))

(defun terminal-decode-utf8 (state bytes &optional (start 0) (end (length bytes)))
  (let* ((carry (terminal-state-utf8-carry state))
         (carry-len (terminal-state-utf8-carry-len state))
         (new-len (- end start))
         (total (+ carry-len new-len)))
    (when (zerop total)
      (return-from terminal-decode-utf8 ""))
    (let ((combined (if (zerop carry-len)
                        bytes
                        (let ((buf (make-array total :element-type '(unsigned-byte 8))))
                          (replace buf carry :end2 carry-len)
                          (replace buf bytes :start1 carry-len :start2 start :end2 end)
                          (setf start 0 end total)
                          buf))))
      (let ((safe-end end))
        (when (> (- end start) 0)
          (let* ((last-start nil)
                 (i start))
            (loop while (< i end) do
              (setf last-start i)
              (let ((needed (utf8-seq-len (aref combined i))))
                (incf i needed)))
            (when (and last-start (> i end))
              (setf safe-end last-start))))
        (let ((tail-len (- end safe-end)))
          (setf (terminal-state-utf8-carry-len state) tail-len)
          (when (plusp tail-len)
            (replace carry combined :start2 safe-end :end2 end))
          (if (= safe-end start)
              ""
              (hemlock.text:octets-to-string combined :external-format :utf-8
                                                :start start :end safe-end)))))))

(defun terminal-write-to-pty (state str)
  (let ((fd (terminal-state-pty-fd state)))
    (when fd
      (let ((bytes (hemlock.text:string-to-octets str :external-format :utf-8)))
        (hemlock.text:posix-write fd bytes 0 (length bytes))))))

(defun terminal-send (str)
  (let ((state (terminal-state-for-current-buffer)))
    (when (and state (terminal-state-pty-fd state))
      (terminal-write-to-pty state str)
      (dispatch-events-no-hang)
      (terminal-sync-buffer (current-buffer))
      (redisplay))))

(defconstant +terminal-drain-cap+ (* 256 1024))

(defun terminal-drain-pty (state)
  (let ((fd (terminal-state-pty-fd state))
        (term (terminal-state-term state))
        (buf (terminal-state-drain-buf state))
        (total 0))
    (loop
      (when (>= total +terminal-drain-cap+)
        (return))
      (unless (hemlock.io:fd-readable-p fd)
        (return))
      (let ((n (cffi:with-pointer-to-vector-data (ptr buf)
                 (cffi:foreign-funcall "read"
                                       :int fd :pointer ptr
                                       :size (length buf) :long))))
        (when (<= n 0) (return))
        (incf total n)
        (let ((str (terminal-decode-utf8 state buf 0 n)))
          (when (plusp (length str))
            (hemlock.term:term-process-output term str)))))
    (when (plusp total)
      (setf (terminal-state-dirty state) t))
    total))

(defun terminal-output-filter (buffer bytes)
  (let ((state (terminal-state-for-buffer buffer)))
    (when state
      (let ((str (terminal-decode-utf8 state bytes)))
        (when (plusp (length str))
          (hemlock.term:term-process-output (terminal-state-term state) str)))
      (terminal-drain-pty state)
      (setf (terminal-state-dirty state) t)
      nil)))

(defun terminal-font-changes-to-font-info (font-changes line-length)
  (let ((result nil))
    (loop for (fc . rest) on font-changes
          for x = (first fc)
          for plist = (second fc)
          for next-x = (if rest (first (first rest)) line-length)
          when (and plist (> next-x x))
          do (push (list* plist x next-x) result))
    (nreverse result)))

(defun terminal-sync-buffer (buffer)
  (let ((state (terminal-state-for-buffer buffer)))
    (when (and state (terminal-state-dirty state))
      (let* ((term (terminal-state-term state))
             (w (hemlock.term:term-width term))
             (h (hemlock.term:term-height term))
             (cache (or (terminal-state-font-info-cache state)
                        (setf (terminal-state-font-info-cache state)
                              (make-array h :initial-element nil))))
             (render-buf (let ((buf (terminal-state-render-buf state)))
                           (if (and buf (= (length buf) w))
                               buf
                               (setf (terminal-state-render-buf state)
                                     (make-string w :initial-element #\Space))))))
        (with-writable-buffer (buffer)
          (let ((line (mark-line (buffer-start-mark buffer)))
                (cy (hemlock.term:term-cursor-y term))
                (cx (hemlock.term:term-cursor-x term))
                (cursor-line nil))
            (dotimes (y h)
              (when line
                (fill render-buf #\Space)
                (multiple-value-bind (row-str font-changes)
                    (hemlock.term:term-render-line term y render-buf)
                  (unless (string= (line-string line) row-str)
                    (setf (line-string line) (copy-seq row-str)))
                  (let ((fi (terminal-font-changes-to-font-info
                             font-changes (length row-str))))
                    (setf (aref cache y) fi)
                    (setf (getf (line-plist line) 'hemlock.command::syntax-colors)
                          (loop for entry in fi
                                for plist = (car entry)
                                for start = (cadr entry)
                                for end = (cddr entry)
                                when (and plist (< start end))
                                collect (list start end plist)))))
                (when (= y cy)
                  (setf cursor-line line))
                (setf line (line-next line))))
            (when cursor-line
              (move-mark (buffer-point buffer)
                         (mark cursor-line
                               (min cx (line-length cursor-line)))))))
        (setf (terminal-state-dirty state) nil)))))

(defun terminal-font-info-for-dis-line (dis-line)
  (let* ((line (dis-line-line dis-line))
         (buffer (and line (line-buffer line)))
         (state (and buffer (terminal-state-for-buffer buffer))))
    (when state
      (let ((cache (terminal-state-font-info-cache state))
            (pos (dis-line-position dis-line)))
        (when (and cache (< pos (length cache)))
          (aref cache pos))))))

(defun terminal-inject-font-changes-for-window (window)
  (let* ((buffer (window-buffer window))
         (state (terminal-state-for-buffer buffer)))
    (when state
      (let ((cache (terminal-state-font-info-cache state)))
        (when cache
          (do ((dl (cdr (window-first-line window)) (cdr dl))
               (y 0 (1+ y)))
              ((or (eq dl the-sentinel) (>= y (length cache))))
            (let ((font-info (aref cache y))
                  (dis-line (car dl)))
              (let ((changes (dis-line-font-changes dis-line)))
                (when changes
                  (do ((prev changes current)
                       (current (font-change-next changes) (font-change-next current)))
                      ((null current)
                       (setf (dis-line-font-changes dis-line) nil)
                       (shiftf (font-change-next prev)
                               *free-font-changes* changes))
                    (setf (font-change-mark current) nil))))
              (let ((head nil) (tail nil))
                (dolist (fi font-info)
                  (let ((node (alloc-font-change
                               (cadr fi) (car fi) nil)))
                    (if tail
                        (setf (font-change-next tail) node
                              tail node)
                        (setf head node tail node))))
                (setf (dis-line-font-changes dis-line) head)))))))))

(defun terminal-resize-to-window (buffer window)
  (let ((state (terminal-state-for-buffer buffer)))
    (when state
      (let* ((new-h (window-height window))
             (new-w (window-width window))
             (term (terminal-state-term state))
             (old-h (hemlock.term:term-height term))
             (old-w (hemlock.term:term-width term)))
        (when (or (/= new-h old-h) (/= new-w old-w))
          (hemlock.term:term-resize term new-w new-h)
          (let ((fd (terminal-state-pty-fd state)))
            (when fd
              (hemlock.term:set-pty-size fd new-h new-w)))
          (with-writable-buffer (buffer)
            (delete-region (buffer-region buffer))
            (let ((point (buffer-point buffer)))
              (dotimes (y new-h)
                (insert-string point (make-string new-w :initial-element #\Space))
                (when (< y (1- new-h))
                  (insert-character point #\Newline)))))
          (dolist (w (buffer-windows buffer))
            (move-mark (window-display-start w) (buffer-start-mark buffer)))
          (setf (terminal-state-font-info-cache state)
                (make-array new-h :initial-element nil))
          (setf (terminal-state-dirty state) t))))))

(defun terminal-redisplay-hook (window)
  ;; Handle resize for the specific window being redisplayed
  (let* ((buffer (window-buffer window))
         (state (terminal-state-for-buffer buffer)))
    (when state
      (let ((term (terminal-state-term state)))
        (when (or (/= (window-height window) (hemlock.term:term-height term))
                  (/= (window-width window) (hemlock.term:term-width term)))
          (terminal-resize-to-window buffer window)))))
  ;; Sync ALL dirty terminal buffers so non-focused windows update too
  (maphash (lambda (buf state)
             (when (terminal-state-dirty state)
               (terminal-sync-buffer buf)))
           *terminal-buffers*))

(add-hook hemlock::redisplay-hook #'terminal-redisplay-hook)

(defun make-terminal-buffer (&key (command (value hemlock::shell-utility))
                                  (rows 24) (cols 80))
  (let* ((term (hemlock.term:make-term
                :width cols :height rows
                :input-fn (lambda (term str)
                            (declare (ignore term))
                            (let ((state (terminal-state-for-current-buffer)))
                              (when (and state (terminal-state-pty-fd state))
                                (terminal-write-to-pty state str))))
                :bell-fn (lambda (term)
                           (declare (ignore term)))))
         (name (format nil "Terminal ~D" (hash-table-count *terminal-buffers*))))
    (multiple-value-bind (master-fd proc-conn)
        (hemlock.term:spawn-pty-process command :rows rows :cols cols)
      (let* ((buffer (make-buffer name
                        :modes '("Terminal")
                        :delete-hook (list (lambda (buffer)
                                            (terminal-kill-process buffer)))))
             (conn (make-pipelike-connection
                    master-fd master-fd
                    :name name
                    :process-connection proc-conn
                    :filter (lambda (connection bytes)
                              (declare (ignore connection))
                              (terminal-output-filter buffer bytes))
                    :buffer nil))
             (state (make-terminal-state :term term
                                         :process nil
                                         :connection conn
                                         :pty-fd master-fd)))
        (setf (terminal-state-font-info-cache state)
              (make-array rows :initial-element nil))
        (setf (gethash buffer *terminal-buffers*) state)
        (with-writable-buffer (buffer)
          (let ((point (buffer-point buffer)))
            (dotimes (y rows)
              (insert-string point (make-string cols :initial-element #\Space))
              (when (< y (1- rows))
                (insert-character point #\Newline)))))
        (setf (buffer-writable buffer) nil)
        buffer))))

(defun terminal-kill-process (buffer)
  (let ((state (terminal-state-for-buffer buffer)))
    (when state
      (remhash buffer *terminal-buffers*)
      (let ((conn (terminal-state-connection state)))
        (when conn
          (setf (terminal-state-connection state) nil)
          (delete-connection conn))))))

(defcommand "Terminal Send Self" (p)
  "Send the last typed key to the terminal process."
  "Send the last typed key to the terminal process."
  (declare (ignore p))
  (let ((char (key-event-char *last-key-event-typed*)))
    (when char
      (terminal-send (string char)))))

(defcommand "Terminal Send Return" (p)
  "Send return to the terminal process."
  "Send return to the terminal process."
  (declare (ignore p))
  (terminal-send (string #\Return)))

(defcommand "Terminal Send Tab" (p)
  "Send tab to the terminal process."
  "Send tab to the terminal process."
  (declare (ignore p))
  (terminal-send (string #\Tab)))

(defcommand "Terminal Send Backspace" (p)
  "Send backspace to the terminal process."
  "Send backspace to the terminal process."
  (declare (ignore p))
  (terminal-send (string #\Rubout)))

(defcommand "Terminal Send Escape" (p)
  "Send escape to the terminal process."
  "Send escape to the terminal process."
  (declare (ignore p))
  (terminal-send (string #\Escape)))

(defcommand "Terminal Send Control" (p)
  "Send the control-character version of the last typed key to the terminal."
  "Send the control-character version of the last typed key to the terminal."
  (declare (ignore p))
  (let* ((keysym (key-event-keysym *last-key-event-typed*))
         (name (keysym-preferred-name keysym)))
    (when (and name (= (length name) 1))
      (let* ((ch (char-upcase (char name 0)))
             (code (- (char-code ch) 64)))
        (when (<= 1 code 31)
          (terminal-send (string (code-char code))))))))

(defcommand "Terminal Send Hyper As Control" (p)
  "Translate Hyper-<letter> (from C-c <letter>) into the corresponding control character and send it to the terminal."
  "Translate Hyper-<letter> (from C-c <letter>) into the corresponding control character and send it to the terminal."
  (declare (ignore p))
  (let* ((keysym (key-event-keysym *last-key-event-typed*))
         (name (keysym-preferred-name keysym)))
    (when (and name (= (length name) 1))
      (let* ((ch (char-upcase (char name 0)))
             (code (- (char-code ch) 64)))
        (when (<= 1 code 31)
          (terminal-send (string (code-char code))))))))

(defcommand "Terminal Quoted Send" (p)
  "Read the next key and send it literally to the terminal."
  "Read the next key and send it literally to the terminal."
  (declare (ignore p))
  (let* ((ev (get-key-event *editor-input* t))
         (char (key-event-char ev)))
    (when char
      (terminal-send (string char)))))

(defcommand "Terminal Send Arrow Up" (p)
  "Send up arrow to the terminal process."
  "Send up arrow to the terminal process."
  (declare (ignore p))
  (terminal-send (format nil "~C[A" #\Escape)))

(defcommand "Terminal Send Arrow Down" (p)
  "Send down arrow to the terminal process."
  "Send down arrow to the terminal process."
  (declare (ignore p))
  (terminal-send (format nil "~C[B" #\Escape)))

(defcommand "Terminal Send Arrow Right" (p)
  "Send right arrow to the terminal process."
  "Send right arrow to the terminal process."
  (declare (ignore p))
  (terminal-send (format nil "~C[C" #\Escape)))

(defcommand "Terminal Send Arrow Left" (p)
  "Send left arrow to the terminal process."
  "Send left arrow to the terminal process."
  (declare (ignore p))
  (terminal-send (format nil "~C[D" #\Escape)))

(defcommand "Terminal Quit" (p)
  "Kill the terminal process and buffer."
  "Kill the terminal process and buffer."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (terminal-kill-process buffer)
    (delete-buffer-if-possible buffer)))

(defcommand "Terminal" (p)
  "Open a terminal emulator buffer."
  "Open a terminal emulator buffer."
  (declare (ignore p))
  (let ((buffer (make-terminal-buffer
                 :rows (window-height *current-window*)
                 :cols (window-width *current-window*))))
    (change-to-buffer buffer)))

(defcommand "Terminal With Command" (p)
  "Open a terminal emulator buffer running a specific command."
  "Open a terminal emulator buffer running a specific command."
  (declare (ignore p))
  (let* ((command (prompt-for-string
                   :default (value hemlock::shell-utility) :trim t
                   :prompt "Command: "
                   :help "Command to run in terminal."))
         (buffer (make-terminal-buffer
                  :command command
                  :rows (window-height *current-window*)
                  :cols (window-width *current-window*))))
    (change-to-buffer buffer)))

;;; Key bindings — MUST come after defcommand forms
(do-alpha-key-events (key-event :both)
  (bind-key "Terminal Send Self" key-event :mode "Terminal"))
(dolist (ch (list #\Space #\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\) #\- #\_
                  #\= #\+ #\[ #\] #\{ #\} #\\ #\| #\; #\: #\' #\"
                  #\, #\. #\< #\> #\/ #\? #\` #\~
                  #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (let ((ke (char-key-event ch)))
    (when ke (bind-key "Terminal Send Self" ke :mode "Terminal"))))

(bind-key "Terminal Send Return" #k"return" :mode "Terminal")
(bind-key "Terminal Send Tab" #k"tab" :mode "Terminal")
(bind-key "Terminal Send Backspace" #k"backspace" :mode "Terminal")
(bind-key "Terminal Send Backspace" #k"delete" :mode "Terminal")
(let ((ctrl-bits (make-key-event-bits "Control"))
      (c-lower (name-keysym "c"))
      (c-upper (name-keysym "C"))
      (x-lower (name-keysym "x"))
      (x-upper (name-keysym "X")))
  (do-alpha-key-events (key-event :both)
    (let ((ks (key-event-keysym key-event)))
      (unless (or (= ks c-lower) (= ks c-upper)
                  (= ks x-lower) (= ks x-upper))
        (bind-key "Terminal Send Control"
                  (make-key-event key-event ctrl-bits)
                  :mode "Terminal")))))
(let ((hyper-bits (make-key-event-bits "Hyper"))
      (q-lower (name-keysym "q"))
      (q-upper (name-keysym "Q")))
  (do-alpha-key-events (key-event :both)
    (let ((ks (key-event-keysym key-event)))
      (unless (or (= ks q-lower) (= ks q-upper))
        (bind-key "Terminal Send Hyper As Control"
                  (make-key-event key-event hyper-bits)
                  :mode "Terminal")))))
(bind-key "Terminal Quoted Send" #k"hyper-q" :mode "Terminal")
(bind-key "Terminal Send Escape" #k"hyper-escape" :mode "Terminal")
(bind-key "Terminal Send Arrow Up" #k"uparrow" :mode "Terminal")
(bind-key "Terminal Send Arrow Down" #k"downarrow" :mode "Terminal")
(bind-key "Terminal Send Arrow Right" #k"rightarrow" :mode "Terminal")
(bind-key "Terminal Send Arrow Left" #k"leftarrow" :mode "Terminal")
