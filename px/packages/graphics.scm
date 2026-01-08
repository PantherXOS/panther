;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages graphics)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system meson)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages vulkan))

(define-public rapidraw
  (package
    (name "rapidraw")
    (version "1.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/CyberTimon/RapidRAW/releases/download/v"
             version "/03_RapidRAW_v" version "_ubuntu-24.04_amd64.deb"))
       (sha256
        (base32 "1hj0y5hiihxwfz0x8nav6nyc85a809scswm0bbx1f5wl2cr5whqf"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("usr/bin/RapidRAW"
                          ("glib" "gtk+" "gdk-pixbuf" "cairo"
                           "webkitgtk-for-gtk3" "libsoup" "openssl" "gcc:lib"
                           "vulkan-loader" "mesa"))
                         ("usr/lib/RapidRAW/resources/libonnxruntime.so"
                          ("gcc:lib" "glibc")))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "ar" "x" (assoc-ref inputs "source"))
             (invoke "tar" "-xzf" "data.tar.gz")
             (delete-file "control.tar.gz")
             (delete-file "data.tar.gz")
             (delete-file "debian-binary")))
         (add-after 'install 'install-extras
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib/RapidRAW"))
                    (share (string-append out "/share"))
                    (apps (string-append share "/applications"))
                    (icons (string-append share "/icons")))
               ;; Create directories
               (mkdir-p bin)
               (mkdir-p lib)
               (mkdir-p apps)
               ;; Copy resources
               (copy-recursively "usr/lib/RapidRAW" lib)
               ;; Install desktop file
               (copy-file "usr/share/applications/RapidRAW.desktop"
                          (string-append apps "/RapidRAW.desktop"))
               (substitute* (string-append apps "/RapidRAW.desktop")
                 (("Exec=RapidRAW")
                  (string-append "Exec=" out "/bin/rapidraw"))
                 (("Icon=RapidRAW")
                  (string-append "Icon=" out "/share/icons/hicolor/128x128/apps/RapidRAW.png")))
               ;; Install icons
               (copy-recursively "usr/share/icons" icons))))
         (add-after 'install-extras 'create-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib/RapidRAW")))
               (wrap-program (string-append out "/usr/bin/RapidRAW")
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append lib "/resources")
                    ,(string-append (assoc-ref inputs "vulkan-loader") "/lib")
                    ,(string-append (assoc-ref inputs "mesa") "/lib")
                    ,(string-append (assoc-ref inputs "webkitgtk-for-gtk3") "/lib")
                    ,(string-append (assoc-ref inputs "libsoup") "/lib")
                    ,(string-append (assoc-ref inputs "gtk+") "/lib")
                    ,(string-append (assoc-ref inputs "glib") "/lib")
                    ,(string-append (assoc-ref inputs "gdk-pixbuf") "/lib")
                    ,(string-append (assoc-ref inputs "cairo") "/lib")
                    ,(string-append (assoc-ref inputs "openssl") "/lib")
                    ,(string-append (assoc-ref inputs "gcc:lib") "/lib")))
                 ;; Point to hardware GPU Vulkan drivers (exclude llvmpipe)
                 `("VK_ICD_FILENAMES" ":" prefix
                   (,(string-append (assoc-ref inputs "mesa")
                                    "/share/vulkan/icd.d/radeon_icd.x86_64.json")
                    ,(string-append (assoc-ref inputs "mesa")
                                    "/share/vulkan/icd.d/intel_icd.x86_64.json")
                    ,(string-append (assoc-ref inputs "mesa")
                                    "/share/vulkan/icd.d/nouveau_icd.x86_64.json"))))
               ;; Create symlink in bin
               (symlink (string-append out "/usr/bin/RapidRAW")
                        (string-append bin "/rapidraw"))))))))
    (native-inputs `(("binutils" ,binutils)))
    (inputs `(("bash-minimal" ,bash-minimal)
              ("cairo" ,cairo)
              ("gdk-pixbuf" ,gdk-pixbuf)
              ("glib" ,glib)
              ("glibc" ,glibc)
              ("gcc:lib" ,gcc "lib")
              ("gtk+" ,gtk+)
              ("libsoup" ,libsoup)
              ("mesa" ,mesa)
              ("openssl" ,openssl)
              ("vulkan-loader" ,vulkan-loader)
              ("webkitgtk-for-gtk3" ,webkitgtk-for-gtk3)))
    (home-page "https://github.com/CyberTimon/RapidRAW")
    (synopsis "GPU-accelerated RAW image editor")
    (description
     "RapidRAW is a high-performance, non-destructive RAW image editor built
with Rust and Tauri.  It features GPU-accelerated processing via WGPU,
support for various camera RAW formats through the rawler library, and
optional AI-powered features using ONNX Runtime.")
    (license license:agpl3+)))

(define-public vipsdisp
  (package
    (name "vipsdisp")
    (version "4.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/libvips/vipsdisp/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "1hvnb7hjjh35gqsd5hha9v4fqmfp9fs7dsadkylsdbwk08jpqsv9"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t))
    (native-inputs
     (list `(,glib "bin")
           pkg-config
           python))
    (inputs
     (list gtk
           vips))
    (home-page "https://github.com/libvips/vipsdisp")
    (synopsis "Image viewer for libvips")
    (description
     "Vipsdisp is a lightweight image viewer powered by libvips and GTK4.
It can display very large images quickly and supports a wide range of image
formats including TIFF, PNG, JPEG, RAW, OpenEXR, FITS, and many scientific
and technical formats.  It handles various pixel types from 1-bit mono to
128-bit double precision complex.")
    (license license:expat)))
