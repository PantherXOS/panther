;;; Backup Packages Module for PantherX

(define-module (px packages backup)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages openssl)
  #:use-module (px packages accounts)
  #:use-module (px packages library))

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

(define-public tarsnap
  (package
    (name "tarsnap")
    (version "1.0.39")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Tarsnap/tarsnap/archive/"
                           version ".tar.gz"))
       (sha256
        (base32 "0b91k0sg64nxidvgzpip5a1rz0cwmygsfr13ac1q7zmd59iz1cz2"))
       (patches (search-patches
                 "tarsnap-do-not-use-command-p-in-makefile.patch"))))
    ;; This official release includes a configure script but it will
    ;; try to invoke sh and that will not be found. Therefore it is
    ;; necessary to build the configure script with autoconf.
    (build-system gnu-build-system)
    (inputs `(("openssl" ,openssl)
              ("zlib" ,zlib)
              ("e2fsprogs" ,e2fsprogs)))
    (native-inputs `(("autoconf" ,autoconf)
                     ("autoconf-archive" ,autoconf-archive)
                     ;; necessary? I think this is added by the build system
                     ("automake" ,automake)
                     ("pkg-config" ,pkg-config)))
    (home-page "https://www.tarsnap.com/")
    (synopsis
     "Tarsnap is a secure, efficient online backup service: \"Online backups for
 the truly paranoid\".")
    (description #f)
    ;; See COPYING
    (license #f)))