;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px packages package-management)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config))

(define-public guix-tools
  (package
    (name "guix-tools")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/guix-tools_v" version
                           ".tgz"))
       (sha256
        (base32 "0b6z2fx5prkibdqsc2n13v322jcl9h29h9g0r67556h21x8f84n8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("guile-json" ,guile-json-1)
              ("guile" ,guile-3.0)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX guix tools to automate guix related tasks")
    (description "Automate `guix` package manager tasks using scheme scripts.
this tool is developed for PantherX team internal usage.")
    (license license:expat)))