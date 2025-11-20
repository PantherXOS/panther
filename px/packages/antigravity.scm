;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>
;;;
;;; NOTE: Google's Antigravity APT repository does not support direct HTTP downloads.
;;; To build this package, first download the .deb file using APT:
;;;
;;; Download command:
;;; podman run --rm -v /tmp:/output debian:bookworm bash -c "\
;;;   apt-get update -qq && \
;;;   apt-get install -y -qq curl gnupg ca-certificates && \
;;;   mkdir -p /etc/apt/keyrings && \
;;;   curl -fsSL https://us-central1-apt.pkg.dev/doc/repo-signing-key.gpg | \
;;;     gpg --dearmor -o /etc/apt/keyrings/antigravity-repo-key.gpg && \
;;;   echo 'deb [signed-by=/etc/apt/keyrings/antigravity-repo-key.gpg] \
;;;     https://us-central1-apt.pkg.dev/projects/antigravity-auto-updater-dev/ \
;;;     antigravity-debian main' > /etc/apt/sources.list.d/antigravity.list && \
;;;   apt-get update -qq && \
;;;   cd /output && \
;;;   apt-get download antigravity"
;;;
;;; This downloads: /tmp/antigravity_1.0.0-1763466940_amd64.deb (147MB)
;;; Then build: guix build -L ~/git/panther antigravity

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
    (version "1.0.0-1763466940")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "file:///tmp/antigravity_1.0.0-1763466940_amd64.deb"))
       (sha256
        (base32
         "0cl66cnji0r2xh60s28v87xapz0sxnwqzqhh3dwz6rha92zh2kxs"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("usr/share/antigravity/antigravity"
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
                         ("usr/share/antigravity/chrome_crashpad_handler"
                          ("gcc:lib")))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (invoke "ar" "x" (assoc-ref inputs "source"))
                      (invoke "tar" "xf" "data.tar.xz")
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
                                           "/usr/share/antigravity/bin/antigravity\n"
                                           "Icon=antigravity\n"
                                           "Type=Application\n"
                                           "StartupNotify=true\n"
                                           "StartupWMClass=Antigravity\n"
                                           "Categories=Development;IDE;\n"
                                           "Keywords=editor;ide;ai;\n\n")))
                        (mkdir-p iconpath)
                        (invoke "cp"
                         "usr/share/antigravity/resources/app/resources/linux/code.png"
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
                              (string-append %output "/usr/share/antigravity/bin/antigravity")
                              (string-append %output "/bin/antigravity")) #t))
                  (add-after 'install 'wrap-where-patchelf-does-not-work
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out "/usr/share/antigravity/antigravity")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/usr/share/antigravity/antigravity")
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
                                                                "/usr/share/antigravity")
                                                 out) ":"))))
                        (wrap-program (string-append out "/usr/share/antigravity/chrome_crashpad_handler")
                          `("FONTCONFIG_PATH" ":" prefix
                            (,(string-join (list (string-append (assoc-ref
                                                                 inputs
                                                                 "fontconfig")
                                                                "/etc/fonts")
                                                 out) ":"))))
                        (wrap-program (string-append out "/usr/share/antigravity/chrome_crashpad_handler")
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
                                                                "/usr/share/antigravity")
                                                 out) ":"))))) #t)))))
    (native-inputs `(("tar" ,tar)
                     ("binutils" ,binutils)))
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
intelligent code generation, and collaborative AI assistance.

Note: This package requires manual download of the .deb file via podman/apt-get
before building.  See package comments for download instructions.")
    (license license:expat)))
