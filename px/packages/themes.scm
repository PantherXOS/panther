;;; Theme Packages Module for PantherX
;;; Hamzeh Nasajpour (h.nasajpour@pantherx.org)
;;;

(define-module (px packages themes)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web))

;; TODO: DROP IF OBSOLETE
(define-public px-widget-style
  (package
    (name "px-widget-style")
    (version "5.19.5-1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "0ihx39li1r266ankc1q4lvv2vfh9hbxjlyjddyy30rir8rbdfilp"))))
    (properties `((tags quote
                        ("Desktop" "KDE" "Plasma"))))
    (build-system cmake-build-system)
    (native-inputs `(("extra-cmake-modules" ,extra-cmake-modules)
                     ("pkg-config" ,pkg-config)))
    ;; TODO: Warning at /gnu/store/…-kpackage-5.34.0/…/KF5PackageMacros.cmake:
    ;; warnings during generation of metainfo for org.kde.breezedark.desktop:
    ;; Package type "Plasma/LookAndFeel" not found
    ;; TODO: Unknown property type for key "X-KDE-ParentApp",
    ;; "X-Plasma-MainScript"
    (inputs `(("kcmutils" ,kcmutils)
              ("kconfigwidgets" ,kconfigwidgets)
              ("kcoreaddons" ,kcoreaddons)
              ("kde-frameworkintegration" ,kde-frameworkintegration)
              ("kdecoration" ,kdecoration)
              ("kguiaddons" ,kguiaddons)
              ("ki18n" ,ki18n)
              ("kiconthemes" ,kiconthemes)
              ("kpackage" ,kpackage)
              ("kwayland" ,kwayland)
              ("kwindowsystem" ,kwindowsystem)
              ("plasma-framework" ,plasma-framework) ;missing in CMakeList.txt
              ;; ("qtbase" ,qtbase)
              ("qtdeclarative" ,qtdeclarative-5)
              ("qtx11extras" ,qtx11extras)))
    (propagated-inputs `(("qtbase" ,qtbase-5)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'fix-source
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "CMakeLists.txt"
                          (("include\\(KDEClangFormat\\)")
                           "#include(KDEClangFormat)"))
                        (substitute* "CMakeLists.txt"
                          (("kde_clang_format")
                           "#kde_clang_format"))
                        ;; Fixing the path of kde4breeze to pointing to the store of breeze theme
                        (substitute* "colors-px/breeze-default-colorscheme.desktop"
                          (("kde4breeze")
                           (string-append out
                                          "/lib/kconf_update_bin/kde4breeze")))
                        #t))))))
    (home-page "https://kde.org/plasma-desktop")
    (synopsis "Default Plasma theme (meta-package)")
    (description
     "Forked version of breeze style theme with additional color-schemes for PantherX.")
    (license license:gpl2+)))

(define-public lxqt-arc-dark-theme
  (package
    (name "lxqt-arc-dark-theme")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.pantherx.org/franz/lxqt-arc-dark-theme")
             (commit "928cddad613a4b28f5453e1c2414b6ce0438d9c8")))
       (sha256
        (base32 "0qi5vy2hjkkyydycmpilbig6rwf870n8yjy2knz9jm0y6a2bx07w"))
       (file-name (git-file-name name version))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (theme-dir (string-append %output
                                                    "/share/lxqt/themes/")))
                     (mkdir-p (string-append theme-dir "arc-dark"))
                     (copy-recursively (string-append source "/arc-dark")
                                       (string-append theme-dir "arc-dark"))))))
    (synopsis "LXQt Arc Dark Theme")
    (home-page "https://git.pantherx.org/franz/lxqt-arc-dark-theme")
    (description "LXQt Theme based on Arc by horst3180 and LXQt dark theme")
    (license license:gpl3+)))

(define-public px-openbox-theme
  (package
    (name "px-openbox-theme")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "19ir80lknf0lxb3qfy3s0zxl1ly1dxw83d33304zj2zpghd77i37"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis "Arc Openbox theme")
    (home-page "https://github.com/dglava/arc-openbox")
    (description
     "Openbox theme created to fit in nicely with the Arc GTK theme.")
    (license license:gpl3+)))

(define-public sddm-darkine-theme
  (package
    (name "sddm-darkine-theme")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Rokin05/darkine-kde.git")
             (commit "cd37f809b3f3ca7fd8865e6e91887a26b591c8d3")))
       (sha256
        (base32 "0wfq5ydlmf92y7xj8qad2rjvvicy1f97cd7n3j5i3pmq9pl74zys"))
       (file-name (git-file-name name version))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (theme-dir (string-append %output
                                      "/share/sddm/themes/darkine")))
                     (mkdir-p theme-dir)
                     (copy-recursively (string-append source
                                                      "/sddm/themes/darkine")
                                       theme-dir)))))
    (home-page "https://github.com/Rokin05/darkine-kde")
    (synopsis "SDDM theme from Darkine KDE collection")
    (description
     "SDDM theme from Darkine KDE collection, a pure QtQuick2 based SDDM login theme")
    (license license:expat)))

(define-public px-sddm-theme
  (package
    (name "px-sddm-theme")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1hjyi7mw8rkpkziq8wip2xsy8cwdjcwmyd7wjaq8892yir3rr66q"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let ((tar (assoc-ref %build-inputs "tar"))
                         (gzip (assoc-ref %build-inputs "gzip"))
                         (src (assoc-ref %build-inputs "source"))
                         (theme-dir (string-append %output
                                                   "/share/sddm/themes")))
                     (mkdir-p theme-dir)
                     (setenv "PATH"
                             (string-append gzip "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf" src "-C"
                             theme-dir) #t))))
    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)))
    (home-page "https://pantherx.org")
    (synopsis "PantherX login theme")
    (description "SDDM login theme for PantherX")
    (license license:expat)))

(define-public chilie-login-theme
  (package
    (name "chilie-login-theme")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/MarianArlt/sddm-chili/archive/"
             version ".tar.gz"))
       (sha256
        (base32 "03wswhqp9980blsrhk60jj49gbqmpkij85i71bzqyi0gyxxn1h53"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (tar (assoc-ref %build-inputs "tar"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (theme-dir (string-append %output
                                                    "/share/sddm/themes")))
                     (mkdir-p theme-dir)
                     (setenv "PATH"
                             (string-append gzip "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf" source "-C"
                             theme-dir)))))
    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)))
    (home-page "https://github.com/MarianArlt/sddm-chili")
    (synopsis "Chili login theme for SDDM")
    (description "Chili login theme for SDDM.")
    (license license:gpl3)))

(define-public paper-icon-theme
  (package
    (name "paper-icon-theme")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/snwh/paper-icon-theme/archive/v." version
             ".tar.gz"))
       (sha256
        (base32 "1klf545hk6g7dx9g0bkblrzd46kz6hr0yj1mqvjq3r7cjpz1vwk2"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'disable-post-install
                    (lambda _
                      (substitute* "meson.build"
                        (("meson.add_install_script.*")
                         "")))))))
    (synopsis "Paper icon theme")
    (home-page "https://snwh.org")
    (description "Paper is an open source FreeDesktop icon project")
    (license license:expat)))

;; TODO: DROP IF OBSOLETE
(define-public px-lxqt-themes
  (package
    (name "px-lxqt-themes")
    (version "1.3.0-u1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "06zzwym5id6a2nwwvxki7c4c1c7xf2lq03rm6924gylj047hyj5q"))))
    (build-system cmake-build-system)
    (native-inputs `(("lxqt-build-tools" ,lxqt-build-tools)
                     ("perl" ,perl)))
    (arguments
     `(#:tests? #f ;no tests
       #:phases (modify-phases %standard-phases
                  ;; !!! TODO I guess these Variables come from lxqt-build-tools, so maybe it would be better to update this package
                  ;; instead of these patches.
                  (add-after 'unpack 'patch-source
                    (lambda _
                      (substitute* '("CMakeLists.txt")
                        (("DESTINATION \"\\$\\{LXQT_GRAPHICS_DIR\\}")
                         "DESTINATION \"share/lxqt/graphics"))
                      (substitute* '("themes/CMakeLists.txt")
                        (("DESTINATION \"\\$\\{LXQT_SHARE_DIR\\}")
                         "DESTINATION \"share/lxqt"))
                      (substitute* '("wallpapers/CMakeLists.txt")
                        (("DESTINATION \"\\$\\{LXQT_SHARE_DIR\\}")
                         "DESTINATION \"share/lxqt"))
                      (substitute* '("palettes/CMakeLists.txt")
                        (("DESTINATION \"\\$\\{LXQT_SHARE_DIR\\}")
                         "DESTINATION \"share/lxqt")) #t)))))
    (home-page "https://lxqt-project.org/")
    (synopsis "Themes, graphics and icons for LXQt")
    (description "This package comprises a number of graphic files and themes
for LXQt.")
    ;; The whole package is released under LGPL 2.1+, while the LXQt logo is
    ;; licensed under CC-BY-SA 3.0.
    (license license:lgpl2.1+)))

(define-public px-settings-service-plugin-theme-dark-bright
  (package
    (name "px-settings-service-plugin-theme-dark-bright")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0svx2z0ws4ck5p1bzw57y4rasmjshna4wx2pqlmak621ghrrnfvn"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-screenshot-path
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "dark/theme.conf"
                          (("dark.jpg")
                           (string-append out "/share/px/themes/dark/dark.jpg")))
                        (substitute* "bright/theme.conf"
                          (("bright.jpg")
                           (string-append out
                            "/share/px/themes/bright/bright.jpg"))) #t))))))
    (synopsis "PantherX Dark and Bright theme")
    (home-page
     "https://git.pantherx.org/development/plugins/px-settings-service-plugin-theme-dark-bright")
    (description "PantherX Dark and Bright theme")
    (license license:gpl3+)))
