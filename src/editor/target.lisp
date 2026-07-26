(in-package :pine.target)

;;;; Where an evaluation runs. One path serves C-x C-e, eval-defun and the
;;;; repl, so redirecting the target redirects all of them, and nothing above
;;;; here needs to know whether the image is this one or an agent's.

(defvar *eval-target* :local
  "Where C-x C-e / eval-defun run: :local (this image), or a registered agent
name (a :process agent's own image). The one eval path, target swappable.")

(defvar *eval-target-saved* :local
  "The eval target from before the debugger opened, restored when it closes:
while attending a fault the target follows the faulted image, so a fix compiles
into the image that broke.")

(defun eval-in-target (str package &key on-done bindings)
  "Evaluate STR in the current *eval-target* image over the one eval path: :local
runs through the local-agent (in-image), a named target through that agent, both
off the caller thread on the shared pine.eval engine. ON-DONE runs on the eval
thread for :local; for a remote agent the result comes home as an :agent-result
(the *jobs* surface) and BINDINGS do not cross the wire."
  (if (or (null *eval-target*) (eq *eval-target* :local))
      (if pine.actor:*local-agent*
          (pine.actor:agent-eval nil pine.actor:*local-agent* str
                                 :package package :bindings bindings :on-done on-done)
          (pine.eval:evaluate-string str :package package
                                     :bindings bindings :on-done on-done))
      (pine.actor:agent-eval (pine.client:server-of (pine.client:current-client))
                             *eval-target* str :package package :on-done on-done)))
