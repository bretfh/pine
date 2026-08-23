(defpackage #:pine/edit/mode
  (:use #:cl)
  (:local-nicknames (#:mode #:pine/mode) (#:doc #:pine/text/document))
  (:export #:prompt #:listing #:debugger))
(in-package #:pine/edit/mode)

(defclass prompt (mode:text) ()
  (:documentation "The line you answer a question on. A mode rather than a flag laid
over another one: what RET means here is a method, and the keymap is this class's."))

(defclass listing (mode:text) ()
  (:documentation "Rows that stand for things. RET acts on the thing the row is
for, not on the text of it."))

(defclass debugger (mode:text) ()
  (:documentation "A fault, as something you act on: the restarts it offers are
numbered and typing one takes it."))

(defmethod mode:setting ((m prompt) key)
  (case key (:aside t) (t (call-next-method))))

(defmethod mode:setting ((m listing) key)
  (case key (:aside t) (t (call-next-method))))

(defmethod mode:insert ((m mode:text) document string)
  "Typing where the document is set to overwrite takes what was there first. A
setting rather than a mode laid over another: nothing about the text is different,
only what typing does to it."
  (when (doc:setting document :overwrite)
    (let ((line (doc:at-line document)) (col (doc:at-col document)))
      (when (< col (length (doc:line document line)))
        (doc:delete-region document line col line
                           (min (length (doc:line document line))
                                (+ col (length string)))))
      (doc:insert document string)
      t)))
