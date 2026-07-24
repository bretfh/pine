(in-package :cl-user)
;;;; Empirical proof of cross-image live redefinition: spawn a real :process
;;;; agent (a separate SBCL image), define a function in it, have it write the
;;;; function's result to a file FROM ITS OWN IMAGE, then REDEFINE the function
;;;; and write again. If the second write changed, the redefinition landed in the
;;;; agent's live heap over the eval path -- not the daemon's.

(defvar *f* 0)
(defun chk (l g w) (if (equal g w) (format t "ok: ~a~%" l)
                       (progn (incf *f*) (format t "FAIL: ~a got ~s want ~s~%" l g w))))

(defparameter *out* "/tmp/pine-xtest-out")

(let ((srv (pine.server:start-server :remoting-port 17055)))
  (setf pine.server:*server* srv)
  (pine.actor:start-agent-registry srv)
  (pine.actor:start-local-agent srv)
  (pine.actor:start-agent-debug srv)
  (ignore-errors (delete-file *out*))
  (handler-case
      (let ((info (pine.actor:spawn-agent srv "xtest")))
        (chk "process agent connected (its own SBCL image)" (and info t) t)
        (flet ((ev (form) (pine.actor:agent-eval srv "xtest" form) (sleep 0.6))
               (readf () (ignore-errors
                           (with-open-file (s *out* :if-does-not-exist nil)
                             (when s (read s nil nil))))))
          (ev "(defun cross-mark () 1)")
          (ev (format nil "(with-open-file (s ~s :direction :output :if-exists :supersede) (print (cross-mark) s))" *out*))
          (chk "agent ran the defun in its own image" (readf) 1)
          ;; live redefinition in the running agent image
          (ev "(defun cross-mark () 2)")
          (ev (format nil "(with-open-file (s ~s :direction :output :if-exists :supersede) (print (cross-mark) s))" *out*))
          (chk "live redefine took effect in the separate image" (readf) 2))
        (pine.actor:kill-agent srv "xtest"))
    (error (e) (incf *f*) (format t "FAIL: spawn/eval errored: ~a~%" e))))

(format t "~&~d failures~%" *f*)
(sb-ext:exit :code (if (zerop *f*) 0 1))
