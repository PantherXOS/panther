;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages bluetooth)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages rust)
  #:use-module (px packages rust-crates)
  #:use-module (ice-9 match))

(define-public overskride
  (package
    (name "overskride")
    (version "0.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kaii-lb/overskride")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c878kb47149dyz6ryx8n5p4nff1m2ygcc02y8b70ydq054raz5j"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:imported-modules (,@%meson-build-system-modules
                           ,@%cargo-build-system-modules)
       #:modules (((guix build cargo-build-system) #:prefix cargo:)
                  (guix build meson-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
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
                       args))
              '(unpack-rust-crates
                configure
                check-for-pregenerated-files
                patch-cargo-checksums)))))))
    (native-inputs
     (list blueprint-compiler
           desktop-file-utils
           `(,glib "bin")
           `(,gtk "bin")
           gettext-minimal
           pkg-config
           rust
           `(,rust "cargo")))
    (inputs
     (cons* bluez
            dbus
            gtk
            libadwaita
            pulseaudio
            (cargo-inputs 'overskride #:module '(px packages rust-crates))))
    (home-page "https://github.com/kaii-lb/overskride")
    (synopsis "Simple yet powerful Bluetooth client")
    (description
     "Overskride is a Bluetooth and OBEX client designed to be straight to the
point, desktop environment and window manager agnostic, and beautiful.  It
provides device enumeration, file transfer via OBEX, device pairing and
authentication, adapter management, battery polling, and audio profile
selection.")
    (license license:gpl3)))

(define-public goveebttemplogger
  (package
    (name "goveebttemplogger")
    (version "2.20231001.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/wcbonner/GoveeBTTempLogger/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "00hsyxz6v0ksq4x6199hv0da5rg4z6s9g7vnkw3r1yfv9cc8j7xx"))
       (patches (search-patches "goveebttemplogger-postbuild-sudo-fix.patch"))))
    (build-system cmake-build-system)
    (inputs (list bluez))
    (home-page "https://github.com/wcbonner/GoveeBTTempLogger")
    (synopsis "Temperature and Humidity Logger for Goove devices")
    (description
     "Govee H5074, H5075, H5100, H5174, H5177, H5179,
H5181, H5182, and H5183 Bluetooth Low Energy Temperature and Humidity Logger")
    (license license:expat)))