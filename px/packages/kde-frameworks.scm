(define-module (px packages kde-frameworks)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1))

(define-public px-icons
  (package
    (name "px-icons")
    (version "5.70.0-21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1arkvx0n1gcihm74wx045xrvz75r0zcgcvg13gy69wznzq7p98b9"))))
    (build-system cmake-build-system)
    (native-inputs `(("extra-cmake-modules" ,extra-cmake-modules)
                     ("fdupes" ,fdupes)
                     ("libxml2" ,libxml2)))
    (inputs `(("qtbase" ,qtbase-5)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Default PantherX icon theme")
    (description "PantherX icons are a fork of KDE Breeze icon theme.")
    (license license:lgpl3+)))
