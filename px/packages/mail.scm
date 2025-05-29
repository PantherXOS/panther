;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px packages mail)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages man)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-crypto)
  #:use-module (px packages crates-io))

(define-public pimsync 
  (package
    (name "pimsync")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~whynothugo/pimsync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "108lhzzxblf39qcn5zccz7vciznisp2dk39x7sfbwdn6425wcw0h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-base64" ,rust-base64-0.22)
        ("rust-camino" ,rust-camino-1)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-hyper" ,rust-hyper-1)
        ("rust-hyper-rustls" ,rust-hyper-rustls-0.26)
        ("rust-hyper-util" ,rust-hyper-util-0.1)
        ("rust-hyperlocal" ,rust-hyperlocal-0.9)
        ("rust-lexopt" ,rust-lexopt-0.3)
        ("rust-libdav" ,rust-libdav-0.9)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustix" ,rust-rustix-0.38)
        ("rust-rustls" ,rust-rustls-0.22)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
        ("rust-scfg" ,rust-scfg-0.3)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-simple-logger" ,rust-simple-logger-4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tower" ,rust-tower-0.5)
        ;; vstorage
        ("rust-sqlite" ,rust-sqlite-0.32)
        ("rust-vparser" ,rust-vparser-1)
        ("rust-inotify" ,rust-inotify-0.11)
        ("rust-tower-http" ,rust-tower-http-0.6))
       #:phases
       (modify-phases %standard-phases
         ;; vstorage not found
         (delete 'package)
         (add-after 'unpack 'set-shell-for-configure-script
           (lambda _
             (setenv "PIMSYNC_VERSION" "0.4.1"))))))
    (native-inputs
     (list git-minimal scdoc))
    (inputs
     (list sqlite))
    (home-page "https://pimsync.whynothugo.nl/")
    (synopsis "Synchronize calendars and contacts")
    (description
     "pimsync is a tool for synchronizing calendars and contacts between
local storage and remote servers using the CalDAV and CardDAV protocols.")
    (license (license:non-copyleft
              "https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12"))))