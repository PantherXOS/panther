;;; Settings Packages Module for PantherX
;;; Hamzeh Nasajpour (h.nasajpour@pantherx.org)
;;;

(define-module (px packages sentry)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages pkg-config))

(define-public sentry
  (package
    (name "sentry")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/getsentry/sentry-native")
             (commit version)
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rd069j2jrqaj67l32p005jmlfah6blcxrca7h2kqgc8nv33pd6j"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("curl" ,curl)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/getsentry/sentry-native")
    (synopsis "Official Sentry SDK for C/C++")
    (description "The Sentry Native SDK is an error and crash reporting client
for native applications, optimized for C and C++.")
    (license license:expat)))
