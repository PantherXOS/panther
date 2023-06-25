;;; Settings Packages Module for PantherX
;;; Author: Hamzeh Nasajpour (h.nasajpour@pantherx.org)
;;;

(define-module (px packages secret)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages web)
  #:use-module (px packages common)
  #:use-module (gnu packages image)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz))


(define-public px-secret-service
  (package
    (name "px-secret-service")
    (version "v0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
          "https://source.pantherx.org/px-secret-service_" 
          version 
          ".tgz"))
        (sha256
          (base32
            "0jc3mcmq2232nfm52fp3ag1v7gj5yh7a432v4mxv5jkqm2h71nm1"))))
    (build-system cmake-build-system)
    (arguments
      `(#:tests? #f))
    (inputs `(("capnproto" ,capnproto-0.9)
              ("libsecret" ,libsecret)
              ("rapidjson" ,rapidjson)))
    (native-inputs 
            `(("pkg-config", pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Secret Service")
    (description "This package provides secret service to store confidential information on PantherX")
    (license license:gpl3)))

(define-public px-secret-sharing
  (package
    (name "px-secret-sharing")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "1w3lsi4km3xlwb1a4w5nb26pn7rkzmdvfh7isbpmhv6qfq9fim1a"))))
    (build-system python-build-system)
    (propagated-inputs 
      `(("steghide", steghide)
        ("python-appdirs", python-appdirs)))
    (home-page "https://git.pantherx.org/development/applications/px-secret-sharing")
    (synopsis "Automated, secure secrets sharing and reconstruction.")
    (description "CLI application that automates secret sharing and
reconstruction (Shamir's secret sharing scheme)")
    (license license:expat)))