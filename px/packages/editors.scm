;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages editors)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses)
                :prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define* (make-vscode-release-asset version asset hash)
  (origin
    (method url-fetch)
    (uri (string-append "https://update.code.visualstudio.com/" version "/"
                        (match (or (%current-system)
                                   (%current-target-system))
                          ("x86_64-linux" "linux-x64")
                          ("aarch64-linux" "linux-arm64")
                          ("armhf-linux" "linux-armhf")) "/stable"))
    (sha256 (base32 hash))))

(define-public vscode
  (package
    (name "vscode")
    (version "1.106.3")
    (source
     #f)
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("opt/vscode/code" 
                          ("glib" "atk"
                            "libx11"
                            "dbus"
                            "gdk-pixbuf"
                            "gtk+"
                            "pango"
                            "fontconfig"
                            "cairo"
                            "libxcomposite"
                            "libxdamage"
                            "libxext"
                            "libxfixes"
                            "libxshmfence"
                            "libxkbfile"
                            "libxrandr"
                            "expat"
                            "libdrm"
                            "libxkbcommon"
                            "mesa"
                            "alsa-lib"
                            "cups"
                            "at-spi2-core"
                            "libxcb"
                            "at-spi2-atk"
                            "nspr"
                            "gcc"
                            "zlib"
                            "gcc:lib"
                            "libsecret"))
                         ("opt/vscode/chrome_crashpad_handler" 
                          ("gcc:lib")))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (mkdir-p "opt/vscode")
                      (invoke "tar"
                              "--strip-components=1"
                              "-xvf"
                              (assoc-ref inputs "vscode")
                              "-C"
                              "opt/vscode") #t))
                  (add-after 'install 'add-desktop-file
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (iconpath (string-append out
                                        "/share/icons/hicolor/512x512/apps/"))
                             (desktoppath (string-append out
                                           "/share/applications/"))
                             (desktopdata (string-append "[Desktop Entry]\n"
                                           "Name=Visual Studio Code\n"
                                           "Comment=Code Editing. Redefined.\n"
                                           "GenericName=Text Editor\n"
                                           "Exec="
                                           out
                                           "/opt/vscode/bin/code\n"
                                           "Icon=vscode\n"
                                           "Type=Application\n"
                                           "StartupNotify=true\n"
                                           "StartupWMClass=Code\n"
                                           "Categories=TextEditor;Development;IDE;\n"
                                           "Actions=new-empty-window;\n"
                                           "Keywords=vscode;\n\n")))
                        (mkdir-p iconpath)
                        (invoke "cp"
                         "opt/vscode/resources/app/resources/linux/code.png"
                         (string-append iconpath "vscode.png"))
                        (mkdir-p desktoppath)
                        (with-output-to-file (string-append desktoppath
                                                            ,name ".desktop")
                          (lambda _
                            (format #t desktopdata))))
                      (invoke "rm"
                              (string-append %output "/environment-variables"))
                      (mkdir-p (string-append %output "/bin"))
                      (invoke "ln" "-s"
                              (string-append %output "/opt/vscode/bin/code")
                              (string-append %output "/bin/"))
                      (invoke "ln" "-s"
                              (string-append %output "/opt/vscode/chrome_crashpad_handler")
                              (string-append %output "/bin/")) #t))
                  (add-after 'install 'wrap-where-patchelf-does-not-work
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out "/opt/vscode/code")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/opt/vscode/code")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs "nss")
                                                                "/lib/nss")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "eudev")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "gcc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libxkbfile")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "zlib")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libsecret")
                                                                "/lib")
                                                 (string-append out
                                                                "/opt/vscode")
                                                 out) ":"))))
                        (wrap-program (string-append out "/opt/vscode/chrome_crashpad_handler")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/opt/vscode/chrome_crashpad_handler")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs "nss")
                                                                "/lib/nss")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "eudev")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "gcc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libxkbfile")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "zlib")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libsecret")
                                                                "/lib")
                                                 (string-append out
                                                                "/opt/chrome_crashpad_handler")
                                                 out) ":"))))) #t)))))
    (native-inputs `(("tar" ,tar)))
    (inputs `(("vscode" ,(make-vscode-release-asset version "vscode"
                          "1kh7hrkyg30ralgidq3pk10061413046wfl10mi52sssjawygnsp"))
              ("gcc:lib" ,gcc "lib")
              ("gcc" ,gcc "lib")
              ("glib" ,glib)
              ("nss" ,nss)
              ("nspr" ,nspr)
              ("atk" ,atk)
              ("libx11" ,libx11)
              ("dbus" ,dbus)
              ("librsvg" ,librsvg)
              ("gtk+" ,gtk+)
              ("pango" ,pango)
              ("cairo" ,cairo)
              ("libxcomposite" ,libxcomposite)
              ("libxdamage" ,libxdamage)
              ("libxext" ,libxext)
              ("libxfixes" ,libxfixes)
              ("libxrandr" ,libxrandr)
              ("libxkbfile" ,libxkbfile)
              ("libxshmfence" ,libxshmfence)
              ("libsecret" ,libsecret)
              ("expat" ,expat)
              ("libdrm" ,libdrm)
              ("libxkbcommon" ,libxkbcommon)
              ("mesa" ,mesa)
              ("alsa-lib" ,alsa-lib)
              ("cups" ,cups)
              ("at-spi2-core" ,at-spi2-core)
              ("libxcb" ,libxcb)
              ("at-spi2-atk" ,at-spi2-atk)
              ("eudev" ,eudev)
              ("fontconfig" ,fontconfig)
              ("zlib" ,zlib)
              ("bash-minimal" ,bash-minimal)))
    (home-page "https://code.visualstudio.com/")
    (synopsis
     "Code editor written in TypeScript and Electron")
    (description
     "Visual Studio Code is a code editor redefined and optimized for
building and debugging modern web and cloud applications.")
    (license license:expat)))

(define-public cursor
  (package
    (name "cursor")
    (version "2.1.47")
    (source
     (origin
       (method url-fetch)
       (uri "https://downloads.cursor.com/production/2d3ce3499c15efd55b6b8538ea255eb7ba4266b2/linux/x64/deb/amd64/deb/cursor_2.1.47_amd64.deb")
       (sha256 (base32 "19hwja0fnvg42ykzzbdbi07923fpip43ydkjwl0nmy94zqjpqwq0"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("usr/share/cursor/cursor"
                          ("glib" "atk"
                           "libx11"
                           "dbus"
                           "gdk-pixbuf"
                           "gtk+"
                           "pango"
                           "fontconfig"
                           "cairo"
                           "libxcomposite"
                           "libxdamage"
                           "libxext"
                           "libxfixes"
                           "libxshmfence"
                           "libxkbfile"
                           "libxrandr"
                           "expat"
                           "libdrm"
                           "libxkbcommon"
                           "mesa"
                           "alsa-lib"
                           "cups"
                           "at-spi2-core"
                           "libxcb"
                           "at-spi2-atk"
                           "nspr"
                           "gcc"
                           "zlib"
                           "gcc:lib"
                           "libsecret"))
                         ("usr/share/cursor/chrome_crashpad_handler"
                          ("gcc:lib")))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (invoke "ar" "x" (assoc-ref inputs "source"))
                      (invoke "tar" "-xJf" "data.tar.xz")
                      (invoke "rm" "-rf" "control.tar.xz" "data.tar.xz" "debian-binary")
                      #t))
                  (add-after 'install 'add-desktop-file
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (iconpath (string-append out
                                                      "/share/icons/hicolor/512x512/apps/"))
                             (desktoppath (string-append out
                                                         "/share/applications/"))
                             (desktopdata (string-append "[Desktop Entry]\n"
                                                         "Name=Cursor\n"
                                                         "Comment=The AI Code Editor\n"
                                                         "GenericName=Text Editor\n"
                                                         "Exec="
                                                         out
                                                         "/usr/share/cursor/bin/cursor\n"
                                                         "Icon=cursor\n"
                                                         "Type=Application\n"
                                                         "StartupNotify=true\n"
                                                         "StartupWMClass=Cursor\n"
                                                         "Categories=TextEditor;Development;IDE;\n"
                                                         "Actions=new-empty-window;\n"
                                                         "Keywords=cursor;\n\n")))
                        (mkdir-p iconpath)
                        (invoke "cp"
                                "usr/share/pixmaps/co.anysphere.cursor.png"
                                (string-append iconpath "cursor.png"))
                        (mkdir-p desktoppath)
                        (with-output-to-file (string-append desktoppath
                                                            ,name ".desktop")
                          (lambda _
                            (format #t desktopdata))))
                      (invoke "rm"
                              (string-append %output "/environment-variables"))
                      (mkdir-p (string-append %output "/bin"))
                      (invoke "ln" "-s"
                              (string-append %output "/usr/share/cursor/bin/cursor")
                              (string-append %output "/bin/cursor"))
                      (invoke "ln" "-s"
                              (string-append %output "/usr/share/cursor/chrome_crashpad_handler")
                              (string-append %output "/bin/"))
                      #t))
                  (add-after 'install 'wrap-where-patchelf-does-not-work
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out "/usr/share/cursor/cursor")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/usr/share/cursor/cursor")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs "nss")
                                                                "/lib/nss")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "eudev")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "gcc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libxkbfile")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "zlib")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libsecret")
                                                                "/lib")
                                                 (string-append out
                                                                "/usr/share/cursor")
                                                 out) ":"))))
                        (wrap-program (string-append out "/usr/share/cursor/chrome_crashpad_handler")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/usr/share/cursor/chrome_crashpad_handler")
                          `("LD_LIBRARY_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs "nss")
                                                                "/lib/nss")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "eudev")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "gcc")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libxkbfile")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs "zlib")
                                                                "/lib")
                                                 (string-append (assoc-ref
                                                                 inputs
                                                                 "libsecret")
                                                                "/lib")
                                                 (string-append out
                                                                "/usr/share/cursor")
                                                 out) ":"))))
                        #t))))))
    (native-inputs `(("tar" ,tar)
                     ("binutils" ,binutils)))
    (inputs `(("gcc:lib" ,gcc "lib")
              ("gcc" ,gcc "lib")
              ("glib" ,glib)
              ("nss" ,nss)
              ("nspr" ,nspr)
              ("atk" ,atk)
              ("libx11" ,libx11)
              ("dbus" ,dbus)
              ("librsvg" ,librsvg)
              ("gtk+" ,gtk+)
              ("pango" ,pango)
              ("cairo" ,cairo)
              ("libxcomposite" ,libxcomposite)
              ("libxdamage" ,libxdamage)
              ("libxext" ,libxext)
              ("libxfixes" ,libxfixes)
              ("libxrandr" ,libxrandr)
              ("libxkbfile" ,libxkbfile)
              ("libxshmfence" ,libxshmfence)
              ("libsecret" ,libsecret)
              ("expat" ,expat)
              ("libdrm" ,libdrm)
              ("libxkbcommon" ,libxkbcommon)
              ("mesa" ,mesa)
              ("alsa-lib" ,alsa-lib)
              ("cups" ,cups)
              ("at-spi2-core" ,at-spi2-core)
              ("libxcb" ,libxcb)
              ("at-spi2-atk" ,at-spi2-atk)
              ("eudev" ,eudev)
              ("fontconfig" ,fontconfig)
              ("zlib" ,zlib)
              ("bash-minimal" ,bash-minimal)))
    (home-page "https://www.cursor.com/")
    (synopsis "AI-powered code editor")
    (description
     "Cursor is an AI-powered code editor built on VS Code that enhances
developer productivity through artificial intelligence integration.  It
features an AI agent capable of converting ideas into functional code,
predictive code completion, and integrations with development workflows.")
    (license license:expat)))

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
