(define-module (px packages programming)
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
    (version "1.86.0")
    (source
     #f)
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("opt/vscode/code" ("glib" "atk"
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
                                             "libsecret")))
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
                                                 out) ":"))))) #t)))))
    (native-inputs `(("tar" ,tar)))
    (inputs `(("vscode" ,(make-vscode-release-asset version "vscode"
                          "0qykchhd6cplyip4gp5s1fpv664xw2y5z0z7n6zwhwpfrld8piwb"))
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
