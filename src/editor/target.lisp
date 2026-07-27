(defpackage #:pine.editor.target
  (:use #:cl)
  (:export #:*eval-target* #:*eval-target-saved* #:eval-in-target)
  (:documentation "Which image an evaluation runs in. C-x C-e, eval-defun and
the repl share one path through here, so redirecting the target redirects all
of them."))

(in-package #:pine.editor.target)

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
off the caller thread on the shared pine.err engine. ON-DONE runs on the eval
thread for :local; for a remote agent the result comes home as an :agent-result
(the *jobs* surface) and BINDINGS do not cross the wire."
  (if (or (null *eval-target*) (eq *eval-target* :local))
      (if pine.core.actor:*local-agent*
          (pine.core.actor:agent-eval nil pine.core.actor:*local-agent* str
                                 :package package :bindings bindings :on-done on-done)
          (pine.err:evaluate-string str :package package
                                     :bindings bindings :on-done on-done))
      (pine.core.actor:agent-eval (pine.editor.frame:server-of (pine.editor.frame:current-client))
                             *eval-target* str :package package :on-done on-done)))
