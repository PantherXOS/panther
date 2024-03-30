;;; Backup Packages Module for PantherX

(define-module (px packages backup)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages backup)
  #:use-module (px packages accounts)
  #:use-module (px packages library)
  #:use-module (px packages tarsnap))

(define-public px-backup
  (package
    (name "px-backup")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "0pzmq8kiw740nj1iswba5k7j0rqicrai27mbl2xiqf1k5r3406ra"))))
    (build-system python-build-system)
    (propagated-inputs `(("restic" ,restic)
                         ("python-psutil" ,python-psutil)
                         ("python-appdirs" ,python-appdirs)
                         ("python-pyyaml" ,python-pyyaml)
                         ("px-secret-library-python" ,px-secret-library-python)
                         ("px-accounts-library-python" ,px-accounts-library-python)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Client library and CLI tool for easy backup")
    (description "Python library that integrates with Accounts and Secrets
to provide a more automated backup experience. The CLI may be
accessed via: px-backup-cli")
    (license license:expat)))
