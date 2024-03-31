;; PantherX OS Server Configuration

(use-modules (gnu)
             (gnu system)
             ;;
             (guix packages)
             (guix git-download)
             (guix build-system meson)
             (gnu packages wm)
             (gnu packages freedesktop)
             (gnu packages gtk)
             (gnu packages web)
             (gnu packages xorg)
             (gnu packages xdisorg)
             (gnu packages pcre)
             (gnu packages linux)
             (gnu packages gl)
             (gnu packages pkg-config)
             (gnu packages man)
             ;;
             (px system config))

(define %ssh-public-key
  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP7gcLZzs2JiEx2kWCc8lTHOC0Gqpgcudv0QVJ4QydPg franz")

;; Sway 1.9 is not compatible with wlroots 0.16
(define-public sway-legacy
  (package
    (inherit sway)
    (name "sway")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/sway")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y7brfrsjnm9gksijgnr6zxqiqvn06mdiwsk5j87ggmxazxd66av"))))
    (build-system meson-build-system)
    (arguments
     `(;; elogind is propagated by wlroots -> libseat
       ;; and would otherwise shadow basu.
       #:configure-flags '("-Dsd-bus-provider=basu")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hardcode path to swaybg.
             (substitute* "sway/config.c"
               (("strdup..swaybg..")
                (string-append "strdup(\"" (assoc-ref inputs "swaybg")
                               "/bin/swaybg\")")))
             ;; Hardcode path to scdoc.
             (substitute* "meson.build"
               (("scdoc.get_pkgconfig_variable..scdoc..")
                (string-append "'" (assoc-ref inputs "scdoc")
                               "/bin/scdoc'")))
             #t)))))
    (inputs (list basu
                  cairo
                  gdk-pixbuf
                  json-c
                  libevdev
                  libinput-minimal
                  libxkbcommon
                  pango
                  pcre2
                  swaybg
                  wayland
                  wlroots-0.16))
    (native-inputs
     (cons* linux-pam mesa pkg-config scdoc wayland-protocols
            (if (%current-target-system)
              (list pkg-config-for-build
                    wayland)
              '())))))

(px-desktop-os
 (operating-system
  (host-name "px-base")
  (timezone "Europe/Berlin")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/vda"))))
  
  (file-systems (cons (file-system
                       (device (file-system-label "my-root"))
                       (mount-point "/")
                       (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "panther")
                (comment "panther's account")
                (group "users")
                ;; Set the default password to 'pantherx'
                ;; Important: Change with 'passwd panther' after first login
                (password (crypt "pantherx" "$6$abc"))
                (supplementary-groups '("wheel"
                                        "audio" "video"))
                (home-directory "/home/panther"))
               %base-user-accounts))

  (packages %px-desktop-core-packages)

  (services (cons*
      (service greetd-service-type
             (greetd-configuration
             (greeter-supplementary-groups (list "video" "input"))
              (terminals
              (list
		            (greetd-terminal-configuration
                 (terminal-vt "1")
                 (terminal-switch #t)
                 (default-session-command
                            (greetd-wlgreet-sway-session
                             (sway sway-legacy)
                             (wlgreet-session
                              (greetd-wlgreet-session
                               (command (file-append sway-legacy "/bin/sway")))))))

                (greetd-terminal-configuration
                 (terminal-vt "2"))
                (greetd-terminal-configuration
                 (terminal-vt "3"))
                (greetd-terminal-configuration
                 (terminal-vt "4"))
                (greetd-terminal-configuration
                 (terminal-vt "5"))
                (greetd-terminal-configuration
                 (terminal-vt "6"))))))
  
  %px-desktop-minmal-services)))

 #:open-ports '(("tcp" "ssh"))
 #:authorized-keys `(("root" ,(plain-file "panther.pub" %ssh-public-key))))
