;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages video)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust)
  #:use-module (px packages gstreamer)
  #:use-module (px self))

(define-public kooha
  (package
    (name "kooha")
    (version "2.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SeaDve/Kooha")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12z5fkm7ikkmh279j34akljcfmls09xd74175v9lyp397qx7mn8m"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:tests? #f
      #:imported-modules `(,@%meson-build-system-modules
                           ,@%cargo-build-system-modules)
      #:modules `(((guix build cargo-build-system) #:prefix cargo:)
                  (guix build meson-build-system)
                  (guix build utils))
      #:phases
      (with-extensions (list (cargo-guile-json))
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-for-build
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")
                (("update_desktop_database: true")
                 "update_desktop_database: false"))
              (delete-file "Cargo.lock")))
          (add-after 'configure 'prepare-cargo-build-system
            (lambda args
              (for-each
               (lambda (phase)
                 (format #t "Running cargo phase: ~a~%" phase)
                 (apply (assoc-ref cargo:%standard-phases phase)
                        #:vendor-dir "vendor"
                        #:cargo-target #$(cargo-triplet)
                        args))
               '(unpack-rust-crates
                 configure
                 check-for-pregenerated-files
                 patch-cargo-checksums))))
          (add-after 'glib-or-gtk-wrap 'wrap-gstreamer
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (gst-plugin-path
                     (string-join
                      (map (lambda (pkg)
                             (string-append (assoc-ref inputs pkg)
                                            "/lib/gstreamer-1.0"))
                           '("gstreamer" "gst-plugins-base"
                             "gst-plugins-good" "gst-plugins-bad"
                             "gst-plugins-ugly-full"))
                      ":")))
                (wrap-program (string-append out "/bin/kooha")
                  `("GST_PLUGIN_PATH" ":" prefix (,gst-plugin-path))))))))))
    (native-inputs
     (append
      (list gettext-minimal
            `(,glib "bin")
            pkg-config
            rust-1.88
            `(,rust-1.88 "cargo"))
      (or (and=> (%current-target-system)
                 (compose list make-rust-sysroot))
          '())))
    (inputs (cons* glib
                   gstreamer
                   gst-plugins-base
                   gst-plugins-good
                   gst-plugins-bad
                   gst-plugins-ugly-full
                   gtk
                   libadwaita
                   pipewire
                   (px-cargo-inputs 'kooha)))
    (home-page "https://github.com/SeaDve/Kooha")
    (synopsis "Screen recorder for GNOME")
    (description
     "Kooha is a minimalist screen recorder for GNOME.  It allows you to
capture your screen with a simple click, without having to configure
complicated settings.  It supports recording from microphones and desktop
audio simultaneously, and can save recordings in WebM, MP4, GIF, and
Matroska formats.")
    (license license:gpl3+)))
