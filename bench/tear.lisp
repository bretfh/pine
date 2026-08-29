(require :asdf)
(asdf:load-system :pine/kernel)

(defpackage #:pine/bench/tear
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:place #:pine/kernel/place)
                    (#:graph #:pine/kernel/graph) (#:tree #:pine/kernel/tree)
                    (#:k #:pine/kernel/call) (#:bt #:bordeaux-threads)))
(in-package #:pine/bench/tear)

(defvar *stop* nil)
(defvar *width* 8)
(defvar *layers* 3)

(defun build ()
  (setf tree:*root* (tree:make-root))
  (k:write "/n" 0)
  (let ((below (loop :for i :below *width*
                     :collect (let ((name (format nil "/l0/~d" i)))
                                (k:make name :derived (lambda () (k:read "/n")))
                                name))))
    (loop :for layer :from 1 :below *layers*
          :do (setf below
                    (loop :for i :below *width*
                          :collect (let ((a (nth (mod (* 2 i) (length below)) below))
                                         (b (nth (mod (1+ (* 2 i)) (length below))
                                                 below))
                                         (name (format nil "/l~d/~d" layer i)))
                                     (k:make name :derived
                                             (lambda () (+ (k:read a) (k:read b))))
                                     name))))
    (let ((top below))
      (k:make "/all" :derived
              (lambda () (loop :for each :in top :sum (k:read each)))))))

(defun factor () (k:write "/n" 1) (k:read "/all"))

(defun look (all f)
  (format t "~&~%TORN: /all = ~d, which is not ~d times anything~%" all f)
  (format t "~&/n is ~d at version ~d~%"
          (k:read "/n") (place:version (tree:reach "/n")))
  (let ((top (tree:reach "/all")))
    (format t "~&/all holds ~a~%" (place:holds top))
    (loop :for (each . at) :in (place:worked-from (place:holds top))
          :do (format t "~&  read ~a at ~a, now mark ~a dirty ~a, value ~a~%"
                      (place:full-name each) at (place:mark each)
                      (place:dirty each)
                      (and (place:workedp (place:holds each))
                           (place:worked-value (place:holds each))))))
  (labels ((down (p at depth)
             (format t "~&~vt~a read at ~a, mark ~a, stands ~a~%"
                     depth (place:full-name p) at (place:mark p)
                     (graph:standsp p at))
             (let ((h (place:holds p)))
               (when (place:workedp h)
                 (format t "~&~vt  value ~a, checked ~a of ~a~%"
                         depth (place:worked-value h) (place:checked p)
                         place:*writes*)
                 (loop :for (each . a) :in (place:worked-from h)
                       :do (down each a (+ depth 4)))))))
    (let ((bad (loop :for (each . at) :in (place:worked-from (place:holds
                                                              (tree:reach "/all")))
                     :when (place:workedp (place:holds each))
                       :do (return (cons each at)))))
      (format t "~&~%the chain under ~a:~%" (place:full-name (car bad)))
      (down (car bad) (cdr bad) 0)))
  (sb-ext:exit :code 1))

(defun main ()
  (build)
  (let ((f (factor)))
    (format t "~&one writer, one reader, ~d layers of ~d, factor ~d~%"
            *layers* *width* f)
    (let ((w (bt:make-thread (lambda ()
                               (loop :until *stop*
                                     :do (k:write "/n" (1+ (random 1000))))))))
      (let ((seen 0))
        (loop :repeat 400000
              :do (let ((all (k:read "/all")))
                    (incf seen)
                    (unless (zerop (mod all f)) (setf *stop* t) (look all f))))
        (setf *stop* t)
        (bt:join-thread w)
        (format t "~&~:d reads, every one of them whole.~%" seen)))))

(main)
(sb-ext:exit)
