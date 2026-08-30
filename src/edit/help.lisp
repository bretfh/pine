(in-package #:pine/edit)

(defparameter +settings+
  '((:tab-width      . "how wide a tab is drawn")
    (:indent         . "how far a body indents")
    (:comment        . "what starts a comment on a line")
    (:grammar        . "which language the parse follows")
    (:overwrite      . "whether typing writes over what is there")
    (:aside          . "whether a window follows this document")))

(command:defcommand "describe-key" ()
    (:describes "what a chord runs" :on '(text "C-h k"))
  (ask "Describe key: "
              :then (lambda (chord)
                      (let ((c (mode:binding (text:mode-of (text:current)) chord)))
                        (log:note "~a: ~a" chord
                                  (if c
                                      (format nil "~a, ~a" (command:name c)
                                              (command:describes c))
                                      "undefined")))))
  :asking)

(command:defcommand "describe-bindings" ()
    (:describes "every chord in force here" :on '(text "C-h b"))
  (into
   "*help*"
   (cons (format nil "chords in force in ~a" (node:name (text:current)))
         (cons ""
               (loop :for (chord . name)
                       :in (sort (bindings (text:mode-of (text:current))) #'string< :key #'car)
                     :collect (cons (format nil "~16a ~a" chord name)
                                    (command:named name)))))
   (lambda (c)
     (when c (log:note "~a" (or (command:describes c) (command:name c)))))))

(command:defcommand "describe-mode" ()
    (:describes "what this document's mode is" :on '(text "C-h m"))
  (let ((m (text:mode-of (text:current))))
    (into
     "*help*"
     (list (mode:type m)
           ""
           (format nil "chain     ~{~(~a~)~^ -> ~}"
                   (mapcar #'class-name
                           (remove-if-not
                            (lambda (c) (subtypep (class-name c) 'mode:mode))
                            (c2mop:class-precedence-list (class-of m)))))
           (format nil "handles   ~a" (mode:handles m))))))

(command:defcommand "describe-command" (name)
    (:describes "what a command is for, and what it is bound to"
     :asks '((:prompt "Describe command: " :category :command :must-match t))
     :on '(text "C-h c" "C-h f"))
  (let ((c (command:named name)))
    (when c
      (log:note "~a: ~a~@[  (~a)~]" (command:name c) (command:describes c)
                (car (rassoc (command:name c) (bindings (text:mode-of (text:current)))
                             :test #'equal)))
      (command:describes c))))

(command:defcommand "describe-settings" ()
    (:describes "every setting, and what this document reads" :on '(text "C-h v"))
  (let ((document (text:current)))
    (into
     "*help*"
     (cons (format nil "settings in ~a" (node:name document))
           (cons ""
                 (loop :for (key . says) :in +settings+
                       :collect (format nil "~(~16a~) ~12a ~a" key
                                        (mode:says document key "")
                                        says)))))))

(command:defcommand "set-local" (key value)
    (:describes "a setting, for this document only"
     :asks '((:prompt "Setting: " :category :setting :must-match t)))
  (declare (ignore value))
  (let ((key (if (keywordp key)
                 key
                 (intern (string-upcase (princ-to-string key)) :keyword))))
    (ask (format nil "~(~a~) here: " key)
                :then (lambda (said)
                        (setf (mode:setting (text:current) key)
                              (let ((*package* (find-package :keyword)))
                                (handler-case (read-from-string said)
                                  (error () said))))))
    :asking))

(command:defcommand "list-documents" ()
    (:describes "every document there is" :on '(text "C-x C-b"))
  (show-listing "*documents*"
                (mapcar (lambda (d)
                          (cons (format nil "~a~30t~a" (node:name d)
                                        (or (text:file-of d) ""))
                                d))
                        (text:documents))
                (lambda (d) (when (node:nodep d) (setf (text:current) d)))))

(command:defcommand "list-jobs" ()
    (:describes "what this image is running" :on '(text "C-x j"))
  (into
   "*jobs*"
   (append
    (list (format nil "~16a ~10a ~a" "what" "state" "name") "")
    (loop :for j :in (job:jobs)
          :collect (cons (format nil "~16a ~10a ~a"
                                 (string-downcase (class-name (class-of j)))
                                 (job:state j) (job:name j))
                         j))
    (loop :for name :in (actors:ticks)
          :collect (format nil "~16a ~10a ~a" "tick" :running name)))
   (lambda (it) (log:note "~a" (or it "that row is a heading")))))

(command:defcommand "list-faults" ()
    (:describes "what has broken, and what stands")
  (into
   "*faults*"
   (loop :for f :in (fault:faults)
         :collect (cons (format nil "~10a ~a"
                                (if (fault:standingp f) :standing :done)
                                (fault:condition-of f))
                        f))
   (lambda (f) (when f (command:run "debugger")))))
