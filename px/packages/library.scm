;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px packages library)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages qt)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public px-auth-library-cpp
  (package
    (name "px-auth-library-cpp")
    (version "0.0.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "1q3zzvk64c98i6h0rkfh2f4nclvcq2f1kp8z21bzck9gjx4zkd2a"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f))
    (inputs (list qtbase-5))
    (home-page "https://www.pantherx.org/")
    (synopsis "CIBA, QR and Device Authentication")
    (description "CIBA and QR flow and device authentication library")
    (license license:expat)))