(in-package :cl-user)
(defvar *f* 0)
(defun chk (l g w) (if (equal g w) (format t "ok: ~a~%" l)
                       (progn (incf *f*) (format t "FAIL: ~a got ~s want ~s~%" l g w))))
(let ((srv (pine.server:start-server)))
  (setf pine.server:*server* srv)
  (setf (pine.server:ts-runtime srv) (pine.ts:make-ts-runtime))
  (pine.event:make-event-bus srv)
  (pine.actor:start-agent-registry srv) (pine.actor:start-local-agent srv)
  (pine.buffer:start-buffer-registry srv) (pine.buffer:install-default-faces)
  (pine.mode:install-default-modes) (pine.editor:install-commands) (pine.editor:install-bindings)
  (let ((client (pine.client:start-client srv)))
    (setf pine.client:*client* client)
    (pine.render:start-renderer client)
    (setf (pine.client:paint-sink client) (lambda (&rest _) (declare (ignore _)) nil))
    (let ((buf (pine.buffer:make-buffer "scratch")))
      (pine.buffer:make-window buf "scratch" :row 0 :col 0 :width 80 :height 29 :focused t)
      (setf (pine.client:current-buffer client) buf)
      (pine.mode:set-buffer-mode buf :text-mode))
    (pine.editor::ensure-minibuffer client)
    (sleep 0.2)
    (flet ((key (s) (pine.command:dispatch client (pine.key:parse-key s)) (sleep 0.06))
           (mbtext () (let ((mb (pine.client:minibuffer-buffer client)))
                        (sento.actor:ask-s mb '(:get-text) :time-out 5)))
           (mbcol () (pine.buffer:point-col
                      (sento.actor:ask-s (pine.client:minibuffer-buffer client)
                                         '(:get-snapshot) :time-out 5))))
      (let ((chosen :none))
        (pine.editor::completing-read "M-x " '("forward-word" "backward-word" "kill-line")
          (lambda (r) (setf chosen r)))
        (sleep 0.15)
        (chk "current-buffer is minibuffer"
             (eq (pine.client:current-buffer client) (pine.client:minibuffer-buffer client)) t)
        ;; type "abc"
        (key "a") (key "b") (key "c")
        (chk "typed abc" (mbtext) "abc")
        ;; C-a to start, C-f forward one, insert X -> aXbc
        (key "C-a") (key "C-f") (key "X")
        (chk "edit mid" (mbtext) "aXbc")
        (chk "point after X" (mbcol) 2)
        ;; C-e end, backspace removes c -> aXb
        (key "C-e") (key "BackSpace")
        (chk "backspace at end" (mbtext) "aXb")
        ;; C-a then C-k (kill-line) clears the input
        (key "C-a") (key "C-k")
        (chk "kill-line clears" (mbtext) "")
        ;; type "forward" -> filters to forward-word, Return accepts it
        (dolist (ch '("f" "o" "r" "w" "a" "r" "d")) (key ch))
        (sleep 0.15)
        (chk "filtered to one" (pine.client:filtered (pine.editor::completion)) '("forward-word"))
        (key "Return") (sleep 0.15)
        (chk "accepted candidate" chosen "forward-word")
        (chk "minibuffer deactivated" (eq (pine.client:current-buffer client)
                                          (pine.client:minibuffer-buffer client)) nil))
      ;; abort restores
      (let ((chosen2 :none))
        (pine.editor::completing-read "M-x " '("aaa" "bbb") (lambda (r) (setf chosen2 r)))
        (sleep 0.1) (key "a")
        (key "C-g") (sleep 0.1)
        (chk "abort no callback" chosen2 :none)
        (chk "abort restored buffer" (eq (pine.client:current-buffer client)
                                         (pine.client:minibuffer-buffer client)) nil))
      ;; --- error trapping: a failing edit surfaces through the debugger and the
      ;; buffer keeps its prior state, then recovers; a failing command routes by
      ;; :debug-on-error ---
      (let ((buf (pine.buffer:make-buffer "err-scratch")) (saw nil) (saw2 nil))
        (pine.mode:set-buffer-mode buf :text-mode)
        (sento.actor:tell buf (list :insert :text "hello"))
        (sleep 0.1)
        ;; global setf, not let: the actor runs on its own thread and reads the
        ;; global *on-debug*. The recorder picks ABORT so the parked actor thread
        ;; unwinds (as the live debugger would when the user presses a restart).
        (setf pine.eval:*on-debug*
              (lambda (ev) (setf saw ev) (pine.eval:pick-restart ev "ABORT")))
        ;; :insert with a non-string errors inside insert-string
        (sento.actor:tell buf (list :insert :text 42))
        (sleep 0.25)
        (chk "edit error surfaced to debugger" (and saw t) t)
        (chk "edit error status :error"
             (and saw (pine.eval:evaluation-status saw)) :error)
        (chk "buffer state preserved after edit error"
             (sento.actor:ask-s buf '(:get-text) :time-out 5) "hello")
        (chk "edit boundary offers a RETRY restart"
             (and (member "RETRY" (mapcar #'first (pine.eval:evaluation-restarts saw))
                          :test #'string=) t) t)
        (sento.actor:tell buf (list :insert :text "!"))
        (sleep 0.15)
        (let ((tx (sento.actor:ask-s buf '(:get-text) :time-out 5)))
          (chk "buffer recovers after edit error"
               (and (= 6 (length tx)) (and (search "hello" tx) t)) t))
        (pine.editor::install-variables)
        (pine.command:define-command "probe-boom" () (error "boom"))
        (setf pine.eval:*on-debug* (lambda (ev) (setf saw2 ev)))
        (pine.var:set-global :debug-on-error t)
        (pine.command:call-command "probe-boom")
        (chk "command error -> debugger when debug-on-error" (and saw2 t) t)
        (setf saw2 nil)
        (pine.var:set-global :debug-on-error nil)
        (pine.command:call-command "probe-boom")
        (chk "command error -> echo (no debugger) when off" saw2 nil)
        (setf pine.eval:*on-debug* nil)
        ;; --- debugger session registry: concurrent faults coexist and page ---
        (setf pine.editor::*debugger-sessions* nil pine.editor::*attended-session* nil)
        (pine.editor::%agent-debug-surface
         (list :agent-debug :agent "alpha" :eval-id 1 :condition "boom-a"
               :restarts (list "RETRY" "ABORT")))
        (pine.editor::%agent-debug-surface
         (list :agent-debug :agent "beta" :eval-id 2 :condition "boom-b"
               :restarts (list "ABORT")))
        (sleep 0.1)
        (chk "two live debugger sessions" (length pine.editor::*debugger-sessions*) 2)
        (chk "attended is newest fault"
             (pine.editor::dbg-session-agent pine.editor::*attended-session*) "beta")
        (chk "eval-target follows the attended fault" pine.editor::*eval-target* "beta")
        (pine.command:call-command "debugger-next-session") (sleep 0.05)
        (chk "Tab pages to the other session"
             (pine.editor::dbg-session-agent pine.editor::*attended-session*) "alpha")
        (chk "eval-target follows on page" pine.editor::*eval-target* "alpha")
        (pine.editor::invoke-pending-restart "ABORT") (sleep 0.05)
        (chk "resolve drops one, advances to remaining"
             (list (length pine.editor::*debugger-sessions*)
                   (pine.editor::dbg-session-agent pine.editor::*attended-session*))
             (list 1 "beta"))
        (pine.editor::invoke-pending-restart "ABORT") (sleep 0.05)
        (chk "last resolve empties the registry"
             (list (length pine.editor::*debugger-sessions*) pine.editor::*attended-session*)
             (list 0 nil))
        (chk "eval-target restored after the debugger closes" pine.editor::*eval-target* :local)
        ;; --- source fault boundary: record, back off, disable; no swallow ---
        (let ((s (pine.source::make-source :name "probe")))
          (pine.source::%source-fault s (make-condition 'simple-error :format-control "x"))
          (pine.source::%source-ok s)
          (chk "a successful refresh clears the fault count" (pine.source::source-faults s) 0)
          (dotimes (i 6)
            (pine.source::%source-fault s (make-condition 'simple-error :format-control "boom")))
          (chk "source disabled after the fault cap" (pine.source::source-stopped s) t)
          (chk "source records the last fault" (and (pine.source::source-last-fault s) t) t))))))
(format t "~&~d failures~%" *f*)
(sb-ext:exit :code (if (zerop *f*) 0 1))
