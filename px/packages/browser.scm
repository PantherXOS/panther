(define-module (px packages browser)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public falkon
  (package
    (name "falkon")
    ;; 24.01.90 requires min. extra-cmake-modules 5.240.0
    (version "23.08.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://anongit.kde.org/falkon")
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "1vnhdyvc0rzhg7cm1ixych45hwhss9w6855m7irs5j3lcf7zqr22"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DBUILD_TESTS=FALSE")))
    (native-inputs (list extra-cmake-modules pkg-config qttools-5))
    (inputs (list karchive
                  libxcb
                  openssl
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols-5
                  qtwebchannel-5
                  qtx11extras
                  xcb-util
                  ki18n))
    (propagated-inputs (list qtwebengine-5))
    (home-page "https://falkon.org")
    (synopsis
     "Falkon is a KDE web browser. It uses QtWebEngine rendering engine")
    (description
     "Falkon is a KDE web browser using QtWebEngine rendering engine,
previously known as QupZilla. It aims to be a lightweight web browser
available through all major platforms.")
    (license license:gpl3)))
