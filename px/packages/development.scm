;;; Development Packages Module for PantherX
;;; Author: Hamzeh Nasajpour (h.nasajpour@pantherx.org)
;;;

(define-module (px packages development)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages databases)
  #:use-module (px packages library))


(define-public px-dev-environments
  (package
    (name "px-dev-environments")
    (version "v0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
          "https://source.pantherx.org/px-dev-environments_" 
          version 
          ".tgz"))
        (sha256
          (base32
            "0nspkyb67x3m6l2xgig4i4xbkyx6spsmwvw5vcy56ylh4d2px9jx"))))
    (build-system cmake-build-system)
    (arguments
      `(#:tests? #f))
    (inputs `(("libgit2", libgit2)))
    (propagated-inputs `(("recutils", recutils)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Setup Assistant")
    (description "This package provides command line application for creating development environments.")
    (license license:expat)))
