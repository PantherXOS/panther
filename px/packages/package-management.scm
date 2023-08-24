(define-module (px packages package-management)
  #:use-module (gnu packages)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public px
  (package
   (name "px")
   (version "0.0.19")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
     (sha256 (base32 "1yss5hmkrd80qa1gzd8l651fb0y1251cyvii03k0bimqf5vr4x4m"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'sanity-check))))
   (inputs
    `(("python-appdirs" ,python-appdirs)))
   (home-page "https://www.pantherx.org/")
   (synopsis "px is a guix overlay that aims to automate certain steps")
   (description "This package provides a number of helpers that combine repetitive
guix commands into one-liners; for ex. px update apply.")
   (license license:gpl3)))

(define-public px-unattended-upgrades
  (package
   (name "px-unattended-upgrades")
   (version "0.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
     (sha256 (base32 "0qzkx32wyfy8i12zcfx761kbd8nmxlw6ihcgqpycjkb3f2qgfs05"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'sanity-check))))
   (inputs
    `(("px" ,px)
      ("python-appdirs" ,python-appdirs)
      ("python-psutil" ,python-psutil)))
   (home-page "https://www.pantherx.org/")
   (synopsis "Unattended upgrades limited to run once per boot")
   (description "Unattended upgrades are to be run as root system service or via cron
and simply prevent running the upgrade twice before rebooting.")
   (license license:expat)))
