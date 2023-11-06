;;; Imaging Packages Module for PantherX
;;; Reza Alizadeh Majd (r.majd@pantherx.org)

(define-module (px packages images)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages video))

(define-public qimgv
  (package
    (name "qimgv")
    (version "1.0.3-alpha")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/easymodo/" name "/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32 "1jcr2f6b2hjss4qiih9nvla1xryf9c4dnp3f1cfqkccxszikxh4l"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs (list exiv2
                         gcc
                         mpv
                         opencv
                         pkg-config
                         qtbase
                         qtsvg
                         qttools))
    (home-page "https://github.com/easymodo/qimgv")
    (synopsis "Qt5 image viewer with optional video support")
    (description
     "Qt5 image viewer. Fast, configurable, easy to use. Optional video support.")
    (license license:gpl3+)))
