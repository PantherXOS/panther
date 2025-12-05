;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages cursor)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses)
                :prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages gcc))

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
