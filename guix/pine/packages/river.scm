(define-module (pine packages river)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system zig)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zig)
  #:use-module ((gnu packages zig-xyz) #:prefix upstream:)
  #:export (river-0.4
            zig-wayland-0.6
            zig-wlroots-0.20
            zig-xkbcommon-0.4
            zig-translate-c
            zig-aro))

;; river >= 0.4 splits the compositor from the window manager
;; (river-window-management-v1); guix upstream carries 0.3.12, the
;; pre-split architecture. These definitions cover pine's needs:
;; design/wm.org. Source URLs are the github mirrors; codeberg is the
;; canonical home.

(define-public zig-aro
  (package
    (name "zig-aro")
    (version "0.0.0-5f5a050")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Vexu/arocc")
                    (commit "5f5a050569a95ecc40a426f0c3666ae7ef987ede")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "073ky6pbs9z9v0vnjs972pxvrhx5jja0g31hv6d4irdifm4p9ikz"))))
    (build-system zig-build-system)
    (arguments (list #:skip-build? #t))
    (synopsis "C compiler frontend in Zig")
    (description "Aro is a C compiler frontend written in Zig, used by
translate-c for C-to-Zig translation.")
    (home-page "https://github.com/Vexu/arocc")
    (license license:expat)))

(define-public zig-translate-c
  (package
    (name "zig-translate-c")
    (version "0.0.0-57c559c")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ziglang/translate-c")
                    (commit "57c559cf581b1fcad90494eda219f98abeb155ce")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xmyakhvx7ami50c48di2h5cdhj60i2710r4cxacsibdzvcmdsgc"))))
    (build-system zig-build-system)
    (arguments (list #:skip-build? #t))
    (propagated-inputs (list zig-aro))
    (synopsis "Standalone C-to-Zig translation")
    (description "The translate-c implementation extracted from the Zig
compiler as a standalone package.")
    (home-page "https://codeberg.org/ziglang/translate-c")
    (license license:expat)))

(define-public zig-wayland-0.6
  (package
    (inherit upstream:zig-wayland)
    (name "zig-wayland")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ifreund/zig-wayland")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10chjkhahkjpl9wn308qdasa955rnipsmlrzp3wryl2rv16chvyy"))))
    (arguments (list #:skip-build? #t))))

(define-public zig-xkbcommon-0.4
  (package
    (inherit upstream:zig-xkbcommon)
    (name "zig-xkbcommon")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ifreund/zig-xkbcommon")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18cjrv4gzyihs6cvr1djkb74lj76yn84p65s71ihp11fywzjc2fd"))))))

(define-public zig-wlroots-0.20
  (package
    (inherit upstream:zig-wlroots)
    (name "zig-wlroots")
    (version "0.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/swaywm/zig-wlroots")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16cy0a65jddfhsvgvcmbj5ln7wx37nia6j6rr062k272dhkwgz3i"))))
    (arguments (list #:skip-build? #t))
    (propagated-inputs
     (list wlroots
           upstream:zig-pixman
           zig-wayland-0.6
           zig-xkbcommon-0.4))))

(define-public river-0.4
  (package
    (inherit upstream:river)
    (name "river")
    (version "0.4.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/riverwm/river")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "123bl6n2lcjl1z7ibgq681077xisf1hhcb892a0iyyvzpyb410mb"))))
    (arguments
     (list #:zig zig-0.16
           #:install-source? #f
           #:zig-release-type "safe"
           #:zig-build-flags
           #~(list "-Dpie" "-Dxwayland")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda _
                   (substitute* "build.zig"
                     (("/bin/sh") (which "sh")))))
               (add-after 'unpack 'prepare-build.zig.zon
                 (lambda _
                   (substitute* "build.zig.zon"
                     (("\\.pixman") ".@\"zig-pixman\"")
                     (("\\.wayland") ".@\"zig-wayland\"")
                     (("\\.wlroots") ".@\"zig-wlroots\"")
                     (("\\.xkbcommon") ".@\"zig-xkbcommon\"")
                     (("\\.translate_c") ".@\"zig-translate-c\""))))
               ;; translate-c reads includes only from pkg-config output
               ;; (-nostdlibinc, no C_INCLUDE_PATH), but guix's pkg-config
               ;; suppresses -I flags for dirs already on C_INCLUDE_PATH.
               (add-before 'build 'emit-system-cflags
                 (lambda _
                   (setenv "PKG_CONFIG_ALLOW_SYSTEM_CFLAGS" "1")))
               (add-before 'build 'revert-build.zig.zon
                 (lambda _
                   (substitute* "build.zig.zon"
                     (("\\.@\"zig-pixman\"") ".pixman")
                     (("\\.@\"zig-wayland\"") ".wayland")
                     (("\\.@\"zig-wlroots\"") ".wlroots")
                     (("\\.@\"zig-xkbcommon\"") ".xkbcommon")
                     (("\\.@\"zig-translate-c\"") ".translate_c"))))
               (add-after 'install 'install-wayland-session
                 (lambda _
                   (let ((wayland-sessions
                          (string-append #$output "/share/wayland-sessions")))
                     (mkdir-p wayland-sessions)
                     (install-file "contrib/river.desktop"
                                   wayland-sessions)))))))
    (inputs
     (list eudev
           libevdev
           libinput-minimal
           zig-wayland-0.6
           zig-wlroots-0.20
           zig-xkbcommon-0.4
           zig-translate-c))))
