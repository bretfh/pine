(defpackage #:pine.app.wm
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:tree #:pine.fs.tree)
                    (#:world #:pine.world.world) (#:provider #:pine.provider.wm)
                    (#:cmd #:pine.repl.command) (#:sh #:pine.provider.sh))
  (:export #:install #:current #:root #:windows-of #:titled #:focused-of
           #:focus! #:step! #:close-window! #:overview! #:exit! #:split!
           #:terminal #:windows #:focused #:title))

(in-package #:pine.app.wm)

(defparameter +terminal+ "alacritty")

(defun root (&optional (w world:*world*))
  (tree:at (world:root w) "wm"))

(defun current (&optional (w world:*world*)) (root w))

(defgeneric windows-of (wm)
  (:method ((wm null)) nil))

(defgeneric titled (wm id)
  (:method ((wm null) id) (declare (ignore id)) nil))

(defgeneric focused-of (wm)
  (:method ((wm null)) nil))

(defgeneric focus! (wm id)
  (:method ((wm null) id) (declare (ignore id)) nil))

(defgeneric step! (wm by)
  (:method ((wm null) by) (declare (ignore by)) nil)
  (:method (wm by)
    (let* ((all (windows-of wm))
           (at (position (focused-of wm) all :test #'equal)))
      (when (and all at)
        (focus! wm (nth (mod (+ at by) (length all)) all))))))

(defgeneric close-window! (wm)
  (:method ((wm null)) nil))

(defgeneric overview! (wm)
  (:method ((wm null)) nil))

(defgeneric exit! (wm)
  (:method ((wm null)) nil))

(defgeneric split! (wm side)
  (:method ((wm null) side) (declare (ignore side)) nil))

(defmethod windows-of ((wm provider:wm-node))
  (node:contents (tree:at wm "windows")))

(defmethod titled ((wm provider:wm-node) id)
  (getf (node:contents (tree:at wm (format nil "windows/~a" id))) :title))

(defmethod focused-of ((wm provider:wm-node))
  (node:contents (tree:at wm "focused")))

(defmethod focus! ((wm provider:wm-node) id)
  (setf (node:contents (tree:at wm "focused")) (princ-to-string id)))

(defun %verb (wm name)
  (let ((v (node:resolve wm name)))
    (when v (setf (node:contents v) t))))

(defmethod close-window! ((wm provider:wm-node)) (%verb wm "close"))
(defmethod overview! ((wm provider:wm-node)) (%verb wm "overview"))
(defmethod exit! ((wm provider:wm-node)) (%verb wm "exit"))

(defmethod split! ((wm provider:wm-node) side)
  (%verb wm (if (member side '(:beside :right :row)) "expel" "consume")))

(defun windows () (windows-of (current)))
(defun focused () (focused-of (current)))
(defun title (&optional (id (focused))) (titled (current) id))

(defun terminal ()
  (or (node:contents (world:ensure world:*world* "wm-terminal")) +terminal+))

(defun install ()
  (cmd:defcommand "wm-focus-next" () (:describes "the keyboard to the next window")
    (step! (current) 1))
  (cmd:defcommand "wm-focus-previous" () (:describes "the keyboard to the window before")
    (step! (current) -1))
  (cmd:defcommand "wm-close-window" () (:describes "close the focused window")
    (close-window! (current)))
  (cmd:defcommand "wm-overview" () (:describes "the compositor's overview, either way")
    (overview! (current)))
  (cmd:defcommand "wm-exit" () (:describes "end the session")
    (exit! (current)))
  (cmd:defcommand "wm-terminal" () (:describes "a terminal, the one this machine uses")
    (sh:launch (list (terminal)))
    (terminal))
  (cmd:defcommand "wm-windows" () (:describes "every window, with its title")
    (loop :for id :in (windows)
          :collect (list id (title id))))
  (cmd:defcommand "wm-title" () (:describes "what the focused window is called")
    (title))
  t)
