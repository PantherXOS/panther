;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages antigravity)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses)
                :prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
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

(define-public antigravity
  (package
    (name "antigravity")
    (version "1.11.9-4787439284912128")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://edgedl.me.gvt1.com/edgedl/release2/j0qc3/antigravity/stable/"
             version "/linux-x64/Antigravity.tar.gz"))
       (sha256
        (base32
         "0ba79s00ijp1bm44kkk578frc2mbjbl715ijpipm5330v9hllfhr"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("Antigravity/antigravity"
                          ("glib"
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
                           "nspr"
                           "gcc"
                           "zlib"
                           "gcc:lib"
                           "libsecret"))
                         ("Antigravity/chrome_crashpad_handler"
                          ("gcc:lib"))
                         ("Antigravity/resources/app/extensions/antigravity/bin/language_server_linux_x64"
                          ()))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (invoke "tar" "xzf" (assoc-ref inputs "source"))
                      #t))
                  (add-after 'install 'add-desktop-file
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (iconpath (string-append out
                                        "/share/icons/hicolor/512x512/apps/"))
                             (desktoppath (string-append out
                                           "/share/applications/"))
                             (desktopdata (string-append "[Desktop Entry]\n"
                                           "Name=Google Antigravity\n"
                                           "Comment=Experience liftoff\n"
                                           "GenericName=IDE\n"
                                           "Exec="
                                           out
                                           "/Antigravity/bin/antigravity\n"
                                           "Icon=antigravity\n"
                                           "Type=Application\n"
                                           "StartupNotify=true\n"
                                           "StartupWMClass=Antigravity\n"
                                           "Categories=Development;IDE;\n"
                                           "Keywords=editor;ide;ai;\n\n")))
                        (mkdir-p iconpath)
                        (invoke "cp"
                         "Antigravity/resources/app/resources/linux/code.png"
                         (string-append iconpath "antigravity.png"))
                        (mkdir-p desktoppath)
                        (with-output-to-file (string-append desktoppath
                                                            ,name ".desktop")
                          (lambda _
                            (format #t desktopdata))))
                      (invoke "rm"
                              (string-append %output "/environment-variables"))
                      (mkdir-p (string-append %output "/bin"))
                      (invoke "ln" "-s"
                              (string-append %output "/Antigravity/bin/antigravity")
                              (string-append %output "/bin/antigravity")) #t))
                  (add-after 'install 'wrap-where-patchelf-does-not-work
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out "/Antigravity/antigravity")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/Antigravity/antigravity")
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
                                                                "/Antigravity")
                                                 out) ":"))))
                        (wrap-program (string-append out "/Antigravity/chrome_crashpad_handler")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/Antigravity/chrome_crashpad_handler")
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
                                                                "/Antigravity")
                                                 out) ":"))))) #t)))))
    (native-inputs `(("tar" ,tar)))
    (inputs `(("gcc:lib" ,gcc "lib")
              ("gcc" ,gcc "lib")
              ("glib" ,glib)
              ("nss" ,nss)
              ("nspr" ,nspr)
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
              ("eudev" ,eudev)
              ("fontconfig" ,fontconfig)
              ("zlib" ,zlib)
              ("bash-minimal" ,bash-minimal)))
    (home-page "https://antigravity.google")
    (synopsis
     "AI-powered IDE from Google with Gemini integration")
    (description
     "Google Antigravity is a next-generation AI-powered IDE built on Electron,
featuring deep integration with Gemini 3 for agentic development workflows,
intelligent code generation, and collaborative AI assistance.")
    (license license:expat)))
