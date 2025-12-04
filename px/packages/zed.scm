;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages zed)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages vulkan))

(define-public zed
  (package
    (name "zed")
    (version "0.215.3")
    (source #f)
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan
       '(("opt/zed/libexec/zed-editor"
          ("gcc:lib" "glib" "gtk+" "libx11" "libxcb" "libxkbcommon"
           "fontconfig" "freetype" "mesa" "vulkan-loader" "alsa-lib"
           "zlib" "libxcursor" "libxi" "wayland" "sqlite")))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p "opt/zed")
             (invoke "tar"
                     "--strip-components=1"
                     "-xvf"
                     (assoc-ref inputs "zed")
                     "-C"
                     "opt/zed")
             #t))
         (add-after 'install 'create-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (zed-bin (string-append out "/opt/zed/libexec/zed-editor")))
               ;; Fix .desktop file
               (substitute* (string-append out "/opt/zed/share/applications/zed.desktop")
                 (("Exec=zed") (string-append "Exec=" out "/bin/zed"))
                 (("Icon=zed") (string-append "Icon=" out "/opt/zed/share/icons/hicolor/512x512/apps/zed.png")))
               ;; Create bin directory and wrapper
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/share/applications"))
               (mkdir-p (string-append out "/share/icons"))
               ;; Wrap the binary
               (wrap-program zed-bin
                 `("XKB_CONFIG_ROOT" ":" prefix
                   (,(string-append (assoc-ref inputs "xkeyboard-config") "/share/X11/xkb")))
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append out "/opt/zed/lib")
                    ,(string-append (assoc-ref inputs "vulkan-loader") "/lib")
                    ,(string-append (assoc-ref inputs "wayland") "/lib")
                    ,(string-append (assoc-ref inputs "libxkbcommon") "/lib"))))
               ;; Symlinks
               (symlink zed-bin (string-append out "/bin/zed"))
               (symlink (string-append out "/opt/zed/share/applications/zed.desktop")
                        (string-append out "/share/applications/zed.desktop"))
               (symlink (string-append out "/opt/zed/share/icons/hicolor")
                        (string-append out "/share/icons/hicolor")))
             #t)))))
    (native-inputs `(("tar" ,tar)))
    (inputs `(("zed"
               ,(origin
                  (method url-fetch)
                  (uri (string-append
                        "https://github.com/zed-industries/zed/releases/download/v"
                        version "/zed-linux-x86_64.tar.gz"))
                  (sha256
                   (base32 "0li9zdlx9g7pdmdvv9i21w5yalbmniwzw02iigv2dnh4hwyfaxng"))))
              ("bash-minimal" ,bash-minimal)
              ("glib" ,glib)
              ("gtk+" ,gtk+)
              ("libx11" ,libx11)
              ("libxcb" ,libxcb)
              ("libxcursor" ,libxcursor)
              ("libxi" ,libxi)
              ("libxkbcommon" ,libxkbcommon)
              ("fontconfig" ,fontconfig)
              ("freetype" ,freetype)
              ("mesa" ,mesa)
              ("vulkan-loader" ,vulkan-loader)
              ("alsa-lib" ,alsa-lib)
              ("zlib" ,zlib)
              ("wayland" ,wayland)
              ("sqlite" ,sqlite)
              ("xkeyboard-config" ,xkeyboard-config)
              ("gcc:lib" ,gcc "lib")))
    (supported-systems '("x86_64-linux"))
    (home-page "https://zed.dev/")
    (synopsis "High-performance multiplayer code editor")
    (description
     "Zed is a high-performance, multiplayer code editor from the creators
of Atom and Tree-sitter.  It features a fast GPU-accelerated UI, built-in
collaboration tools, and AI-powered code assistance.")
    (license license:gpl3+)))
