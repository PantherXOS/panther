;;; Package Repository for GNU Guix
;;; Copyright © 2021-2024 Hamzeh Nasajpour <h.nasajpour@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px packages document)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages base))

(define-public featherpad
  (package
    (name "featherpad")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/tsujan/FeatherPad/archive/V"
                           version ".tar.gz"))
       (sha256
        (base32 "1406a1appj3qf5545wfgvpskxm4r1vwxchd71pbvghxnic61c506"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("hunspell" ,hunspell)
                     ("qtsvg" ,qtsvg-5)
                     ("qtx11extras" ,qtx11extras)
                     ("qtbase" ,qtbase-5)))
    (home-page "https://github.com/tsujan/FeatherPad")
    (synopsis "FeatherPad is a lightweight Qt5 plain-text editor for Linux")
    (description "FeatherPad is a lightweight Qt5 plain-text editor for Linux")
    (license license:expat)))
