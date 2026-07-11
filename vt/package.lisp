;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :cl-user)

(defpackage :hemlock.term
  (:use :common-lisp)
  (:export
   #:cell
   #:make-cell
   #:cell-char
   #:cell-face
   #:face-attrs
   #:make-face-attrs
   #:copy-face-attrs
   #:face-fg
   #:face-bg
   #:face-bold
   #:face-faint
   #:face-italic
   #:face-underline
   #:face-blink
   #:face-inverse
   #:face-conceal
   #:face-crossed
   #:face-underline-color
   #:term
   #:make-term
   #:term-width
   #:term-height
   #:term-grid
   #:term-cursor-x
   #:term-cursor-y
   #:term-cursor-visible
   #:term-cursor-style
   #:term-title
   #:term-cwd
   #:term-bracketed-paste
   #:term-input-fn
   #:term-bell-fn
   #:term-title-fn
   #:term-cwd-fn
   #:term-process-output
   #:term-resize
   #:term-reset
   #:term-grid-row
   #:color-index-to-rgb
   #:key-event-to-escape-sequence
   #:set-pty-size
   #:spawn-pty-process
   #:term-render-line
   #:term-dump-row-string
   #:term-dump-to-string
   #:char-display-width))
