;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages setup)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages time)
  #:use-module (px packages common))

(define-public px-install
  (package
    (name "px-install")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1p4jmzxkpppx1hxxfnvwa2zqd7xqprg5pq3wy4n91rz0xknvlwhv"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("python-requests" ,python-requests)
              ("python-tqdm" ,python-tqdm)
              ("python-pytz" ,python-pytz)
              ("python-qrcode" ,python-qrcode)
              ("python-py-cpuinfo" ,python-py-cpuinfo)
              ("python-urllib3" ,python-urllib3)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX OS Installer")
    (description "A command line driven installer with sane defaults.")
    (license license:gpl3)))