;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages backup)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config))

(define-public tarsnap
  (package
    (name "tarsnap")
    (version "1.0.41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Tarsnap/tarsnap/archive/"
                           version ".tar.gz"))
       (sha256
        (base32 "10dwf0vgqj5vnsz6pfcn8mhazaj49xnz7zhfiwbsl19kpapglwja"))
       (patches (search-patches
                 "tarsnap-do-not-use-command-p-in-makefile.patch"))))
    ;; This official release includes a configure script but it will
    ;; try to invoke sh and that will not be found. Therefore it is
    ;; necessary to build the configure script with autoconf.
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "POSIX_SH=" (assoc-ref %build-inputs "bash") "/bin/sh"))))
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