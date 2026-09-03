(in-package #:pine/edit)

(defclass prompt (mode:text) ()
  (:documentation "The line you answer a question on. A mode rather than a flag laid
over another one: what RET means here is a method, and the keymap is this class's."))

(defclass listing (mode:text)
  ((shown-rows :initarg :shown-rows :accessor shown-rows :initform nil)
   (on-enter   :initarg :on-enter   :accessor on-enter   :initform nil))
  (:documentation "Rows that stand for things: a row is a string, or (TEXT . PLACE)
where PLACE is what that row is about, so RET acts on the thing rather than on the
text of it."))

(defclass debugger (mode:text) ()
  (:documentation "A fault, as something you act on: the restarts it offers are
numbered and typing one takes it."))

(defmethod mode:setting ((m prompt) key)
  (case key (:aside t) (t (call-next-method))))

(defmethod mode:setting ((m listing) key)
  (case key (:aside t) (t (call-next-method))))

(defmethod mode:typing ((m mode:text) document string)
  "Typing where the document is set to overwrite takes what was there first. A
setting rather than a mode laid over another: nothing about the text is different,
only what typing does to it."
  (when (mode:says document :overwrite nil)
    (let ((line (text:at-line document)) (col (text:at-col document)))
      (when (< col (length (text:line document line)))
        (text:delete-region document line col line
                           (min (length (text:line document line))
                                (+ col (length string)))))
      (text:insert document string)
      t)))
