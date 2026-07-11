;;; term.lisp — Terminal emulation via libghostty-vt + POSIX PTY.
;;; Pure ECL FFI. Reader thread feeds only. QML timer triggers render.

(in-package :pine.term)

(ffi:clines "
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <util.h>
#include <errno.h>
#include <dlfcn.h>

static void *gterm_lib = NULL;
static void *gterm_sym(const char *name) {
    if (!gterm_lib) return NULL;
    return dlsym(gterm_lib, name);
}

typedef void* (*gterm_new_fn)(int, int);
typedef void  (*gterm_free_fn)(void*);
typedef void  (*gterm_feed_fn)(void*, const void*, int);
typedef int   (*gterm_render_fn)(void*, void*, int);
typedef int   (*gterm_cursor_fn)(void*);
typedef void  (*gterm_resize_fn)(void*, int, int);
")

;;; Library loading
(defvar *gterm-loaded* nil)
(defvar *pushframe-loaded* nil)

(defun load-gterm-lib (path)
  (ffi:c-inline (path) (:cstring) :int "
{ gterm_lib = dlopen(#0, RTLD_LAZY); @(return) = gterm_lib ? 1 : 0; }"
  :one-liner nil :side-effects t))

(defun ensure-gterm ()
  (unless *gterm-loaded*
    (let ((lib (namestring (merge-pathnames "lib/zig-out/lib/libpine-gterm.dylib"
                                            (asdf:system-source-directory :pine)))))
      (when (plusp (load-gterm-lib lib))
        (setf *gterm-loaded* t)))))

(defun ensure-pushframe ()
  (unless *pushframe-loaded*
    (let ((lib (namestring (merge-pathnames "lib/libpine-pushframe.dylib"
                                            (asdf:system-source-directory :pine)))))
      (when (plusp (ffi:c-inline (lib) (:cstring) :int "
{ void *h = dlopen(#0, RTLD_LAZY); @(return) = h ? 1 : 0; }"
        :one-liner nil :side-effects t))
        (setf *pushframe-loaded* t)))))

(defun push-frame-direct (vec count display-ptr cursor-row cursor-col scroll-pixel-y)
  (ffi:c-inline (vec count display-ptr cursor-row cursor-col scroll-pixel-y)
                (:object :int :unsigned-long :int :int :double) :int "
{ typedef int (*fn_t)(cl_object, int, unsigned long, int, int, double);
  fn_t fn = (fn_t)dlsym(RTLD_DEFAULT, \"pine_push_frame\");
  @(return) = (fn) ? fn(#0, #1, #2, #3, #4, #5) : 0; }"
  :one-liner nil :side-effects t))

;;; PTY
(defun pty-spawn (program rows cols)
  (ffi:c-inline (program rows cols) (:cstring :int :int) (values :int :int) "
{
    int master;
    struct winsize ws = {0};
    ws.ws_row = #1; ws.ws_col = #2;
    pid_t pid = forkpty(&master, NULL, NULL, &ws);
    if (pid < 0) { @(return 0) = -1; @(return 1) = -1; }
    else if (pid == 0) {
        setenv(\"TERM\", \"xterm-256color\", 1);
        setenv(\"COLORTERM\", \"truecolor\", 1);
        signal(SIGHUP, SIG_DFL); signal(SIGINT, SIG_DFL);
        signal(SIGQUIT, SIG_DFL); signal(SIGPIPE, SIG_DFL);
        signal(SIGCHLD, SIG_DFL); signal(SIGTSTP, SIG_DFL);
        char *argv[] = { (char*)#0, \"-l\", NULL };
        execvp(#0, argv);
        _exit(127);
    } else { @(return 0) = (int)pid; @(return 1) = master; }
}" :one-liner nil :side-effects t))

(defun fd-read-into (fd foreign-buf size)
  "Read into a pre-allocated foreign buffer. Returns byte count or -1."
  (ffi:c-inline (fd foreign-buf size) (:int :pointer-void :int) :int "
{ @(return) = (int)read(#0, #1, #2); }" :one-liner nil :side-effects t))

(defun fd-write-string (fd str)
  (ffi:c-inline (fd str) (:int :cstring) :int "
{ @(return) = (int)write(#0, #1, strlen(#1)); }" :one-liner nil :side-effects t))

(defun fd-close (fd)
  (ffi:c-inline (fd) (:int) :void "{ close(#0); }" :one-liner nil :side-effects t))

;;; gterm via dlsym — feed only needs the foreign buffer directly
(defun gterm-new (cols rows)
  (ffi:c-inline (cols rows) (:int :int) :pointer-void "
{ gterm_new_fn fn = (gterm_new_fn)gterm_sym(\"pine_gterm_new\");
  @(return) = fn ? fn(#0, #1) : NULL; }" :one-liner nil :side-effects t))

(defun gterm-free (h)
  (ffi:c-inline (h) (:pointer-void) :void "
{ gterm_free_fn fn = (gterm_free_fn)gterm_sym(\"pine_gterm_free\");
  if (fn && #0) fn(#0); }" :one-liner nil :side-effects t))

(defun gterm-feed-foreign (h buf len)
  "Feed from a foreign pointer directly — no Lisp vector involved."
  (ffi:c-inline (h buf len) (:pointer-void :pointer-void :int) :void "
{ gterm_feed_fn fn = (gterm_feed_fn)gterm_sym(\"pine_gterm_feed\");
  if (fn && #0) fn(#0, #1, #2); }" :one-liner nil :side-effects t))

(defun gterm-resize (h cols rows)
  (ffi:c-inline (h cols rows) (:pointer-void :int :int) :void "
{ gterm_resize_fn fn = (gterm_resize_fn)gterm_sym(\"pine_gterm_resize\");
  if (fn && #0) fn(#0, #1, #2); }" :one-liner nil :side-effects t))

(defun pty-set-size (fd rows cols)
  (ffi:c-inline (fd rows cols) (:int :int :int) :void "
{ struct winsize ws = {0};
  ws.ws_row = #1; ws.ws_col = #2;
  ioctl(#0, TIOCSWINSZ, &ws); }" :one-liner nil :side-effects t))

(defun gterm-render-cells16 (h out-buf out-size)
  "Render cells into foreign buffer. Returns cell count."
  (ffi:c-inline (h out-buf out-size) (:pointer-void :pointer-void :int) :int "
{ typedef int (*fn_t)(void*, void*, int);
  fn_t fn = (fn_t)gterm_sym(\"pine_gterm_render_cells16\");
  @(return) = (fn && #0) ? fn(#0, #1, #2) : 0; }" :one-liner nil :side-effects t))

(defun gterm-cursor-row (h)
  (ffi:c-inline (h) (:pointer-void) :int "
{ gterm_cursor_fn fn = (gterm_cursor_fn)gterm_sym(\"pine_gterm_cursor_row\");
  @(return) = (fn && #0) ? fn(#0) : 0; }" :one-liner nil :side-effects t))

(defun gterm-cursor-col (h)
  (ffi:c-inline (h) (:pointer-void) :int "
{ gterm_cursor_fn fn = (gterm_cursor_fn)gterm_sym(\"pine_gterm_cursor_col\");
  @(return) = (fn && #0) ? fn(#0) : 0; }" :one-liner nil :side-effects t))

;;; Alloc/free foreign buffers
(defun foreign-alloc (size)
  (ffi:c-inline (size) (:int) :pointer-void "
{ @(return) = malloc(#0); }" :one-liner nil :side-effects t))

(defun foreign-free (ptr)
  (ffi:c-inline (ptr) (:pointer-void) :void "
{ free(#0); }" :one-liner nil :side-effects t))

(defun foreign-byte (ptr offset)
  (ffi:c-inline (ptr offset) (:pointer-void :int) :int "
{ @(return) = ((unsigned char*)#0)[#1]; }" :one-liner nil :side-effects t))

(defun unpack-cells16 (src count dst)
  (ffi:c-inline (src count dst) (:pointer-void :int :object) :void "
{
  unsigned char *s = (unsigned char*)#0;
  int n = #1;
  cl_object vec = #2;
  for (int i = 0, idx = 0; i < n; i++) {
    unsigned char *c = s + i * 16;
    int has_bg = c[15];
    ecl_aset1(vec, idx++, ecl_make_fixnum(c[0] | (c[1] << 8)));
    ecl_aset1(vec, idx++, ecl_make_fixnum(c[2] | (c[3] << 8)));
    ecl_aset1(vec, idx++, ecl_make_fixnum(c[4] | (c[5] << 8) | (c[6] << 16)));
    ecl_aset1(vec, idx++, ecl_make_fixnum(c[8]));
    ecl_aset1(vec, idx++, ecl_make_fixnum(c[9]));
    ecl_aset1(vec, idx++, ecl_make_fixnum(c[10]));
    ecl_aset1(vec, idx++, ecl_make_fixnum(has_bg ? c[11] : -1));
    ecl_aset1(vec, idx++, ecl_make_fixnum(has_bg ? c[12] : -1));
    ecl_aset1(vec, idx++, ecl_make_fixnum(has_bg ? c[13] : -1));
    ecl_aset1(vec, idx++, c[14] ? ECL_T : ECL_NIL);
  }
}" :one-liner nil :side-effects t))

(defun vec-to-list (vec count)
  (ffi:c-inline (vec count) (:object :int) :object "
{
  cl_object v = #0;
  int n = #1;
  cl_object result = ECL_NIL;
  for (int i = n - 1; i >= 0; i--)
    result = ecl_cons(ecl_aref1(v, i), result);
  @(return) = result;
}" :one-liner nil :side-effects t))


;;; Terminal instance
(defclass terminal ()
  ((gterm   :initarg :gterm   :accessor gterm-handle  :initform nil)
   (master  :initarg :master  :accessor master-fd     :initform -1)
   (pid     :initarg :pid     :accessor child-pid     :initform -1)
   (cols    :initarg :cols    :accessor term-cols      :initform 80)
   (rows    :initarg :rows    :accessor term-rows      :initform 24)
   (buffer  :initarg :buffer  :accessor term-buffer    :initform nil)
   (reader  :initarg :reader  :accessor reader-thread  :initform nil)
   (alivep  :initarg :alive   :accessor alivep         :initform t)
   (lock    :initform (bordeaux-threads:make-lock "gterm") :accessor term-lock)
   (read-buf  :accessor read-buf  :initform nil)
   (cell-buf  :accessor cell-buf  :initform nil)))

(defun terminals-list (&optional (cli (pine.client:current-client)))
  (pine.client:terminals cli))

(defun terminal-map-table (&optional (cli (pine.client:current-client)))
  (or (pine.client:terminal-map cli)
      (setf (pine.client:terminal-map cli) (make-hash-table :test 'eq))))

(defun active-terminal (&optional (cli (pine.client:current-client)))
  (first (terminals-list cli)))

(defun render-terminal-to-frame ()
  (let* ((client (pine.client:current-client))
         (term (active-terminal client))
         (f (pine.client:frame client)))
    (when (and term (alivep term) (gterm-handle term))
      (pine.buffer:ensure-frame-cells f)
      (handler-case
          (bordeaux-threads:with-lock-held ((term-lock term))
            (let* ((h (gterm-handle term))
                   (cb (cell-buf term))
                   (buf-size (* (term-cols term) (term-rows term) 16))
                   (count (gterm-render-cells16 h cb buf-size)))
              (when (plusp count)
                (unpack-cells16 cb count (pine.buffer:frame-cells f))
                (setf (pine.buffer:frame-cell-count f) (* count 10)
                      (pine.buffer:frame-cursor-row f) (gterm-cursor-row h)
                      (pine.buffer:frame-cursor-col f) (gterm-cursor-col h)
                      (pine.buffer:frame-dirtyp f) t))))
        (error () nil)))))


;;; Screen extraction — terminal-mode's :get-text/:get-snapshot use this.

(defun gterm-text (term)
  (bordeaux-threads:with-lock-held ((term-lock term))
    (let* ((h (gterm-handle term))
           (cols (term-cols term))
           (rows (term-rows term)))
      (unless (and h (plusp cols) (plusp rows))
        (return-from gterm-text ""))
      (let* ((buf-size (* cols rows 16))
             (cb (cell-buf term))
             (n (gterm-render-cells16 h cb buf-size))
             (grid (make-array (list rows cols) :initial-element #\Space)))
        (dotimes (i n)
          (let* ((off (* i 16))
                 (row (logior (foreign-byte cb off)
                              (ash (foreign-byte cb (+ off 1)) 8)))
                 (col (logior (foreign-byte cb (+ off 2))
                              (ash (foreign-byte cb (+ off 3)) 8)))
                 (cp  (logior (foreign-byte cb (+ off 4))
                              (ash (foreign-byte cb (+ off 5)) 8)
                              (ash (foreign-byte cb (+ off 6)) 16))))
            (when (and (< row rows) (< col cols) (> cp 0))
              (setf (aref grid row col) (code-char cp)))))
        (with-output-to-string (s)
          (dotimes (r rows)
            (when (plusp r) (write-char #\Newline s))
            (dotimes (c cols)
              (write-char (aref grid r c) s))))))))

(defun gterm-cursor-row-of (term)
  (let ((h (gterm-handle term)))
    (if h (gterm-cursor-row h) 0)))

(defun gterm-cursor-col-of (term)
  (let ((h (gterm-handle term)))
    (if h (gterm-cursor-col h) 0)))


;;; PTY writing — called by terminal-mode's dispatch-message methods.

(defun term-write (buf text)
  (let ((term (terminal-for-buffer buf)))
    (when (and term (plusp (master-fd term)))
      (fd-write-string (master-fd term) text))))

(defun key-bytes (key mods)
  (declare (ignore mods))
  (case key
    (16777220 (string #\Return))
    (16777219 (string (code-char 127)))
    (16777223 (format nil "~c[3~" (code-char 27)))
    (16777216 (string (code-char 27)))
    (16777217 (string #\Tab))
    (16777235 (format nil "~c[A" (code-char 27)))
    (16777237 (format nil "~c[B" (code-char 27)))
    (16777236 (format nil "~c[C" (code-char 27)))
    (16777234 (format nil "~c[D" (code-char 27)))
    (16777232 (format nil "~c[H" (code-char 27)))
    (16777233 (format nil "~c[F" (code-char 27)))
    (16777238 (format nil "~c[5~" (code-char 27)))
    (16777239 (format nil "~c[6~" (code-char 27)))
    (t nil)))


;;; Open terminal
(defun open-terminal (&key cols rows (name "*terminal*") (shell "/bin/zsh"))
  (let* ((client (pine.client:current-client))
         (f (pine.client:frame client))
         (renderer (pine.client:renderer client)))
    (unless cols (setf cols (truncate (max 40 (pine.buffer:frame-cols f)))))
    (unless rows (setf rows (truncate (max 10 (pine.buffer:frame-rows f)))))

    (ensure-gterm)
    (unless *gterm-loaded* (error "libpine-gterm not available"))

    (let* ((h (gterm-new cols rows))
           (term (make-instance 'terminal :gterm h :cols cols :rows rows)))

      (setf (read-buf term) (foreign-alloc 16384))
      (setf (cell-buf term) (foreign-alloc (* cols rows 16)))

      (multiple-value-bind (pid master) (pty-spawn shell rows cols)
        (when (minusp pid) (error "pty-spawn failed"))
        (setf (master-fd term) master (child-pid term) pid))

      (let ((buf (pine.buffer:make-buffer name)))
        (setf (term-buffer term) buf)
        (setf (gethash buf (terminal-map-table client)) term)
        (pine.mode:set-buffer-mode buf :terminal-mode))

      (let ((captured-renderer renderer)
            (captured-term term))
        (setf (reader-thread term)
              (bordeaux-threads:make-thread
               (lambda ()
                 (let ((rb (read-buf captured-term)))
                   (loop while (alivep captured-term) do
                     (let ((n (fd-read-into (master-fd captured-term) rb 16384)))
                       (cond
                         ((<= n 0)
                          (setf (alivep captured-term) nil)
                          (return))
                         (t
                          (bordeaux-threads:with-lock-held ((term-lock captured-term))
                            (when (gterm-handle captured-term)
                              (gterm-feed-foreign (gterm-handle captured-term) rb n)))
                          (sento.actor:tell captured-renderer '(:force-render))))))))
               :name "pine-term-reader")))

      (push term (pine.client:terminals client))

      (pine.buffer:switch-buffer name)
      (sento.actor:tell renderer
                        (list :switch-buffer :buffer (term-buffer term) :name name))
      (pine.render:subscribe-to-buffer (term-buffer term))
      (pine.echo:message (format nil "terminal: ~a" name))
      term)))

(defun terminal-visible-p ()
  (let* ((client (pine.client:current-client))
         (term (active-terminal client))
         (w (pine.client:focused-window client)))
    (and term
         (alivep term)
         w
         (eq (pine.buffer:buffer-ref w) (term-buffer term)))))


(defun resize-active-terminal (cols rows)
  (let ((term (active-terminal)))
    (when (and term (alivep term) (gterm-handle term))
      (bordeaux-threads:with-lock-held ((term-lock term))
        (gterm-resize (gterm-handle term) cols rows)
        (setf (term-cols term) cols
              (term-rows term) rows)
        (let ((new-buf-size (* cols rows 16)))
          (when (cell-buf term) (foreign-free (cell-buf term)))
          (setf (cell-buf term) (foreign-alloc new-buf-size))))
      (when (plusp (master-fd term))
        (pty-set-size (master-fd term) rows cols)))))

(defun terminal-for-buffer (buf)
  (let ((client pine.client:*client*))
    (when client
      (let ((map (pine.client:terminal-map client)))
        (when map (gethash buf map))))))

(defun terminal-destroy (term)
  (let ((client (pine.client:current-client)))
    (setf (alivep term) nil)
    (when (gterm-handle term) (gterm-free (gterm-handle term)) (setf (gterm-handle term) nil))
    (when (plusp (master-fd term)) (fd-close (master-fd term)) (setf (master-fd term) -1))
    (when (read-buf term) (foreign-free (read-buf term)) (setf (read-buf term) nil))
    (when (cell-buf term) (foreign-free (cell-buf term)) (setf (cell-buf term) nil))
    (setf (pine.client:terminals client) (remove term (pine.client:terminals client)))))

;;; Stub for render.lisp compatibility
(defun with-gterm-cells (term callback)
  (declare (ignore term callback))
  nil)
