;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages audio)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system meson)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public wireplumber
  (package
    (name "wireplumber")
    (version "0.5.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://gitlab.freedesktop.org/pipewire/wireplumber.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dljz669ywy1lvvn0jh14ymynmbii45q5vay71zajpcg31249dyw"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dsystemd=disabled"
                           "-Dsystem-lua=true")))
    (native-inputs
     (list `(,glib "bin")
           pkg-config
           python))
    (inputs (list dbus elogind glib lua pipewire))
    (home-page "https://gitlab.freedesktop.org/pipewire/wireplumber")
    (synopsis "Session / policy manager implementation for PipeWire")
    (description "WirePlumber is a modular session / policy manager for
PipeWire and a GObject-based high-level library that wraps PipeWire's API,
providing convenience for writing the daemon's modules as well as external
tools for managing PipeWire.")
    (license license:expat)))
