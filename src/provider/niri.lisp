(defpackage #:pine.provider.niri
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path) (#:out #:pine.provider.out))
  (:export #:niri))

(in-package #:pine.provider.niri)
(named-readtables:in-readtable pine.path:syntax)

;;;; The compositor, when it is niri.
;;;;
;;;;   (write /wm (niri))
;;;;
;;;; Nothing above /wm learns which compositor answered: when pine is the
;;;; window manager the same paths are served by pine's own provider, and a bar
;;;; written against /wm/workspaces works under either.

(defun %json (command)
  (out:json (out:sh "niri msg --json ~a" command)))

(defun %vector (command)
  (let ((value (%json command)))
    (when (vectorp value) (coerce value 'list))))

(defun %workspaces () (%vector "workspaces"))
(defun %windows () (%vector "windows"))

(defun %named (things key name)
  (find-if (lambda (thing) (equal name (princ-to-string (gethash key thing ""))))
           things))

(defun %workspace (n) (%named (%workspaces) "idx" n))
(defun %window (id) (%named (%windows) "id" id))

(defun %focused-window ()
  (let ((found (find-if (lambda (w) (gethash "is_focused" w)) (%windows))))
    (when found (p:child /wm/windows (princ-to-string (gethash "id" found))))))

(defun niri ()
  "The compositor: its workspaces, its windows, and what it can be told."
  (ns:provider
   {:on /sh/${"niri msg --json event-stream"}
    :doc "niri, through niri msg"}

   (/wm/workspaces/?n/focused
    {:read (pine.data:fn []
             (let ((w (%workspace n))) (and w (gethash "is_focused" w) t)))
     :write (pine.data:fn [v]
              (when v (out:sh "niri msg action focus-workspace ~a" n))
              v)
     :doc "whether it has the keyboard; write true to focus it"})

   (/wm/workspaces/?n
    {:read (pine.data:fn []
             (let ((w (%workspace n)))
               (when w
                 (fset:map (:focused (and (gethash "is_focused" w) t))
                           (:urgent (and (gethash "is_urgent" w) t))))))
     :doc "{:focused .. :urgent ..}"})

   (/wm/workspaces
    {:ls (pine.data:fn []
           (mapcar (lambda (w) (princ-to-string (gethash "idx" w)))
                   (%workspaces)))
     :doc "every workspace there is"})

   (/wm/windows/?id
    {:read (pine.data:fn []
             (let ((w (%window id)))
               (when w
                 (fset:map (:title (gethash "title" w))
                           (:app (gethash "app_id" w))))))
     :doc "{:title .. :app ..}"})

   (/wm/windows
    {:ls (pine.data:fn []
           (mapcar (lambda (w) (princ-to-string (gethash "id" w))) (%windows)))
     :doc "every window there is"})

   (/wm/focused
    {:read (pine.data:fn [] (%focused-window))
     :write (pine.data:fn [v]
              (when (p:pathp v)
                (out:sh "niri msg action focus-window --id ~a" (p:leaf v)))
              v)
     :doc "the focused window's path; write one to focus it"})

   (/wm
    {:verbs {:overview (pine.data:fn [] (out:sh "niri msg action toggle-overview") t)
             :close (pine.data:fn [] (out:sh "niri msg action close-window") t)
             :split (pine.data:fn (&optional (side :below))
                      (out:sh "niri msg action ~a"
                              (if (eq side :below)
                                  "consume-or-expel-window-right"
                                  "consume-or-expel-window-left"))
                      t)
             :exit (pine.data:fn [] (out:sh "niri msg action quit --skip-confirmation") t)}
     :doc "the compositor; [:overview] [:close] [:split :below] [:exit]"})))
