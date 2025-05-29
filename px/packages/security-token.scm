;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px packages security-token)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages security-token)
  #:use-module (px packages python-xyz))

(define-public acsccid
  (package
    (name "acsccid")
    (version "1.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/acshk/acsccid/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32 "1ip7lrhnrnag96x29lfpb663i2y6y0631p7i14sialkz1sr5xlb8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--enable-usbdropdir=" %output
                                              "/pcsc/drivers"))
       #:phases (modify-phases %standard-phases
                  (add-after 'configure 'patch-Makefile
                    (lambda _
                      (substitute* "src/Makefile.in"
                        (("/bin/echo")
                         (which "echo"))) #t)))))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("flex" ,flex)
                     ("gettext" ,gettext-minimal)
                     ("libtool" ,libtool)
                     ("pcsc-lite" ,pcsc-lite) ;only required for headers
                     ("perl" ,perl)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("libusb" ,libusb)))
    (home-page "https://github.com/acshk/acsccid")
    (synopsis "ACS CCID PC/SC Driver for Linux/Mac OS X")
    (description
     "acsccid is a PC/SC driver for Linux/Mac OS X and it supports ACS CCID smart card
readers. This library provides a PC/SC IFD handler implementation and
communicates with the readers through the PC/SC Lite resource manager (pcscd).")
    (license license:lgpl2.1+)))

(define-public id-card-reader
  (package
    (name "id-card-reader")
    (version "0.1.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "0xkly0qhgr96y6qk4h24a5llm6yl650ad26i2nbilzx1csyxryf2"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("python-pillow" ,python-pillow)
              ("python-pyscard" ,python-pyscard)))
    (home-page "https://www.pantherx.org/")
    (synopsis " ")
    (description " ")
    (license license:expat)))

