(in-package :cl-user)

(defpackage :pine-user
  (:use :cl)
  (:import-from :pine.buffer
                #:buffer
                #:ask
                #:tell
                #:buffer-local
                #:meta
                #:make-buffer
                #:kill-buffer
                #:switch-buffer
                #:list-buffers
                #:buffer-count)
  (:import-from :pine.client
                #:*client*
                #:current-client
                #:current-buffer)
  (:import-from :pine.mode
                #:find-mode
                #:current-buffer-mode
                #:set-buffer-mode
                #:defmode)
  (:import-from :pine.editor
                #:eval-last-sexp
                #:eval-buffer
                #:run-command)
  (:documentation
   "Primary user facing functionality provided by Hecl."))
