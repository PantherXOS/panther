;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px packages bluetooth)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (ice-9 match))

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