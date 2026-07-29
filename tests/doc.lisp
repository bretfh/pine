(in-package :pine.test)
(named-readtables:in-readtable pine.path:syntax)

(def-suite* :pine.doc :in :pine)

;;;; Output and documentation are both places to read.

(test a-line-written-to-log-is-the-last-line
  (pine.ns:with-space ()
    (pine.log:mount)
    (let ((*error-output* (make-broadcast-stream)))
      (pine.log:note "the first thing")
      (pine.log:note "the second thing"))
    (is (string= "the second thing" (pine.ns:read /log)))
    (is (string= "the first thing" (pine.ns:read /log/1)))))

(test the-log-is-bounded
  (pine.ns:with-space ()
    (pine.log:mount)
    (let ((*error-output* (make-broadcast-stream)))
      (dotimes (i (+ pine.log:*kept* 20))
        (pine.log:note "line ~d" i)))
    (is (= pine.log:*kept* (fset:size (pine.ns:read /log/*)))
        "the ring grew past its bound")))

(test what-pine-said-is-not-in-the-file
  "Output is what this image said, not what the world holds."
  (pine.ns:with-space ()
    (pine.log:mount)
    (is (null (pine.ns:setting /log :keep t)))))

(test anything-written-to-the-log-becomes-a-line
  (pine.ns:with-space ()
    (pine.log:mount)
    (pine.ns:write /log 42)
    (is (string= "42" (pine.ns:read /log)))))

(test a-provider-says-what-its-paths-are-for
  (pine.ns:with-space ()
    (pine.doc:mount)
    (pine.ns:write /audio
                   (pine.ns:provider
                    (/audio/volume {:read (pine.data:fn [] 40)
                                    :doc "0..100"})
                    (/audio/muted {:read (pine.data:fn [] nil)})))
    (is (string= "0..100" (pine.ns:read /doc/audio/volume)))))

(test a-path-nobody-documented-answers-nothing
  (pine.ns:with-space ()
    (pine.doc:mount)
    (pine.ns:write /tab-width 8)
    (is (null (pine.ns:read /doc/tab-width))
        "a held path has no doc, and saying so is the point")))

(test a-clause-with-no-doc-falls-back-to-its-providers
  (pine.ns:with-space ()
    (pine.doc:mount)
    (pine.ns:write /audio
                   (pine.ns:provider
                    {:doc "pipewire, through wpctl and pactl"}
                    (/audio/muted {:read (pine.data:fn [] nil)})))
    (is (string= "pipewire, through wpctl and pactl"
                 (pine.ns:read /doc/audio/muted)))))

(test every-provider-the-daemon-mounts-says-what-its-paths-are
  "Documentation is a handler, so an undocumented path is a provider that never
said what it was for. The list is read off the mounts rather than written here,
so a provider added tomorrow is held to it too."
  (with-fixture substrate ()
    (let ((silent (remove-if (lambda (at) (pine.ns:read (pine.path:child /doc at)))
                             (pine.ns:mounts))))
      (is (null silent) "these say nothing about themselves: ~{~a~^ ~}"
          (mapcar #'pine.path:text silent)))))
