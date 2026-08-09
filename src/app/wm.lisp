(defpackage #:pine.app.wm
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:tree #:pine.fs.tree)
                    (#:world #:pine.world.world) (#:wm #:pine.provider.wm)
                    (#:cmd #:pine.repl.command) (#:sh #:pine.provider.sh)
                    (#:log #:pine.run.log))
  (:export #:install #:root #:windows #:focused #:focus! #:step! #:title
           #:terminal))

(in-package #:pine.app.wm)

(defparameter +terminal+ "alacritty")

(defun root (&optional (w world:*world*))
  (tree:at (world:root w) "wm"))

(defun windows ()
  (let ((n (root)))
    (and n (node:contents (tree:at n "windows")))))

(defun focused ()
  (let ((n (root)))
    (and n (node:contents (tree:at n "focused")))))

(defun focus! (id)
  (let ((n (root)))
    (when n (setf (node:contents (tree:at n "focused")) (princ-to-string id)))))

(defun step! (by)
  (let* ((all (windows))
         (at (position (focused) all :test #'equal)))
    (when (and all at)
      (focus! (nth (mod (+ at by) (length all)) all)))))

(defun title (&optional (id (focused)))
  (let ((n (root)))
    (when (and n id)
      (getf (node:contents (tree:at n (format nil "windows/~a" id))) :title))))

(defun terminal ()
  (or (node:contents (world:ensure world:*world* "wm-terminal")) +terminal+))

(defun %verb (name)
  (let ((n (root)))
    (when n
      (let ((v (node:resolve n name)))
        (when v (setf (node:contents v) t))))))

(defun install ()
  (cmd:defcommand "wm-focus-next" () (:describes "the keyboard to the next window")
    (step! 1))
  (cmd:defcommand "wm-focus-previous" () (:describes "the keyboard to the window before")
    (step! -1))
  (cmd:defcommand "wm-close-window" () (:describes "close the focused window")
    (%verb "close"))
  (cmd:defcommand "wm-overview" () (:describes "the compositor's overview, either way")
    (%verb "overview"))
  (cmd:defcommand "wm-expel" () (:describes "the focused window out of its column")
    (%verb "expel"))
  (cmd:defcommand "wm-consume" () (:describes "the focused window into its column")
    (%verb "consume"))
  (cmd:defcommand "wm-exit" () (:describes "end the session")
    (%verb "exit"))
  (cmd:defcommand "wm-terminal" () (:describes "a terminal, the one this machine uses")
    (sh:launch (list (terminal)))
    (terminal))
  (cmd:defcommand "wm-windows" () (:describes "every window, with its title")
    (loop :for id :in (windows)
          :collect (list id (title id))))
  (cmd:defcommand "wm-title" () (:describes "what the focused window is called")
    (title))
  t)
