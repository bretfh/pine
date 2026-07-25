;; The pine operating system, VM shape: river as the session compositor,
;; pine as the window manager, daemon, editor, and desktop.
;;
;;   make vm    (guix system vm -L guix guix/os.scm, then run the script)
;;
;; Auto-logs bfh in on tty1; the shell profile execs river with pine as its
;; init: `pine wm' binds window management, `pine start' brings up the
;; daemon which spawns the editor and desktop frontends.

(use-modules (gnu)
             (guix gexp)
             (pine)
             (river))
(use-service-modules dbus desktop networking ssh)
(use-package-modules fonts fontutils image terminals nss)

(define pine-river-init
  ;; river -c runs this once the compositor is up.
  (program-file "pine-river-init"
    #~(begin
        (setenv "PINE_FRAME_DUMP" "/tmp/pine-frame.png")
        (system (string-append #$pine "/bin/pine wm >/tmp/pine-wm.log 2>&1 &"))
        (execl (string-append #$pine "/bin/pine") "pine" "start"))))

(define pine-session-profile
  ;; exec river on the auto-logged-in tty; everything else is a normal shell.
  (mixed-text-file "pine-session.sh"
    "if [ \"$(tty)\" = /dev/tty1 ] && [ -z \"$WAYLAND_DISPLAY\" ]; then\n"
    "  export WLR_NO_HARDWARE_CURSORS=1\n"
    "  exec " river-0.4 "/bin/river -log-level debug -c " pine-river-init
    " > /tmp/river.log 2>&1\n"
    "fi\n"))

(operating-system
  (host-name "pine")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/vda"))))
  (file-systems (cons (file-system
                        (mount-point "/")
                        (device "/dev/vda1")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "bfh")
                (comment "pine")
                (group "users")
                (password (crypt "pine" "$6$pine.vm"))
                (supplementary-groups
                 '("wheel" "audio" "video" "input" "tty")))
               %base-user-accounts))

  (packages (cons* pine river-0.4
                   foot grim
                   (specification->package "font-maple-mono-nf")
                   font-dejavu fontconfig
                   nss-certs
                   %base-packages))

  (services
   (cons* (service elogind-service-type)
          (service dbus-root-service-type)
          (service dhcpcd-service-type)
          (service openssh-service-type
                   (openssh-configuration
                    (password-authentication? #t)))
          (simple-service 'pine-session etc-service-type
                          (list `("profile.d/pine-session.sh"
                                  ,pine-session-profile)))
          (modify-services %base-services
            (mingetty-service-type
             config =>
             (if (string= (mingetty-configuration-tty config) "tty1")
                 (mingetty-configuration
                  (inherit config)
                  (auto-login "bfh"))
                 config))))))
