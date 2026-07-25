(define-module (wayflan)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages lisp-xyz)
  #:export (sbcl-wayflan))

;; Two wire-level fixes against upstream wayflan, both exposed by river:
;; the enum decoder errors on values outside its protocol tables (river
;; advertises wl_shm formats -- drm fourcc codes -- the bundled tables
;; predate), and the string reader dies on null strings (length 0 on the
;; wire, sent for allow-null args like a window's unset app_id). Unknown
;; enum values decode to their raw integer, null strings to ""; encoding
;; stays strict.

(define-public sbcl-wayflan
  (package
    (inherit (@ (gnu packages lisp-xyz) sbcl-wayflan))
    (source
     (origin
       (inherit (package-source (@ (gnu packages lisp-xyz) sbcl-wayflan)))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (substitute* "src/client/client.lisp"
              (("\\(car \\(or \\(find value table :key #'cdr :test #'=\\)")
               "(car (or (find value table :key #'cdr :test #'=) (cons value value)"))
            (substitute* "src/wire.lisp"
              ((":max-chars \\(1- nul-length\\)")
               ":max-chars (max 0 (1- nul-length))"))))))))
