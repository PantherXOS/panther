;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages tools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix utils)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (px packages golang-xyz)
  #:use-module (px self))

(define-public broot
  (package
    (name "broot")
    (version "1.51.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Canop/broot/archive/refs/tags/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18dn36qwkgfy6fmjg1d6g2xavsrh9fi2170fn8q1md6idyk9l2b6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #t))
    (inputs
     (px-cargo-inputs 'broot))
    (home-page "https://dystroy.org/broot")
    (synopsis "Modern tree-like file navigator and fuzzy searcher")
    (description
     "Broot is a command-line tool for navigating directory trees and managing
files.  It provides fast fuzzy searching, file preview capabilities, Git status
integration, and customizable panels.  Broot helps you quickly overview and
navigate large directory structures.")
    (license license:expat)))

(define-public darkman
  (package
    (name "darkman")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.com/WhyNotHugo/darkman/-/archive/v"
                           version "/darkman-v" version ".tar.gz"))
       (sha256
        (base32 "1mqw6avadvx06sakbxf927kr6vxfq7vv2sw7k55li6pv01bfs1r0"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "gitlab.com/WhyNotHugo/darkman/cmd/darkman"
           #:unpack-path "gitlab.com/WhyNotHugo/darkman"
           #:install-source? #f
           #:build-flags
           #~(list "-ldflags"
                   #$(string-append "-X main.Version=" version))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'build-manpage
                 (lambda* (#:key unpack-path #:allow-other-keys)
                   (with-directory-excursion (string-append "src/" unpack-path)
                     (with-input-from-file "darkman.1.scd"
                       (lambda ()
                         (with-output-to-file "darkman.1"
                           (lambda ()
                             (invoke "scdoc"))))))))
               (add-after 'install 'install-manpage
                 (lambda* (#:key unpack-path outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (man (string-append out "/share/man/man1")))
                     (with-directory-excursion (string-append "src/" unpack-path)
                       (install-file "darkman.1" man))))))))
    (native-inputs
     (list scdoc))
    (propagated-inputs
     (list go-github-com-adrg-xdg
           go-github-com-goccy-go-yaml
           go-github-com-godbus-dbus-v5
           go-github-com-rxwycdh-rxhash
           go-github-com-sj14-astral
           go-github-com-spf13-cobra))
    (inputs
     (list geoclue))
    (home-page "https://darkman.whynothugo.nl/")
    (synopsis "Framework for dark-mode and light-mode transitions")
    (description
     "Darkman is a framework for managing dark mode and light mode transitions
on Unix-like desktops.  It runs in the background as a service and automatically
switches between dark and light themes based on the time of day, using your
location (via GeoClue) to determine sunrise and sunset times.  It can execute
custom scripts when switching themes, allowing integration with various
applications and desktop environments.")
    (license license:isc)))
