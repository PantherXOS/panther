(define-module (px packages package-management)
  #:use-module (guix build-system python)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages qt)
  #:use-module (px packages library))

(define-public px
  (package
    (name "px")
    (version "0.0.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1zinc00ys5byjhxx7gl2rspd572kqsvdvaxab7g5wajpknsnh87d"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))
    (inputs `(("python-appdirs" ,python-appdirs)))
    (home-page "https://www.pantherx.org/")
    (synopsis "px is a guix overlay that aims to automate certain steps")
    (description
     "This package provides a number of helpers that combine repetitive
guix commands into one-liners; for ex. px update apply.")
    (license license:gpl3)))

(define-public px-unattended-upgrades
  (package
    (name "px-unattended-upgrades")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "0qzkx32wyfy8i12zcfx761kbd8nmxlw6ihcgqpycjkb3f2qgfs05"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))
    (inputs `(("px" ,px)
              ("python-appdirs" ,python-appdirs)
              ("python-psutil" ,python-psutil)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Unattended upgrades limited to run once per boot")
    (description
     "Unattended upgrades are to be run as root system service or via cron
and simply prevent running the upgrade twice before rebooting.")
    (license license:expat)))

(define-public guix-tools
  (package
    (name "guix-tools")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/guix-tools_v" version
                           ".tgz"))
       (sha256
        (base32 "0b6z2fx5prkibdqsc2n13v322jcl9h29h9g0r67556h21x8f84n8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("guile-json" ,guile-json-1)
              ("guile" ,guile-3.0)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX guix tools to automate guix related tasks")
    (description "Automate `guix` package manager tasks using scheme scripts.
this tool is developed for PantherX team internal usage.")
    (license license:expat)))

(define-public px-software-assets-meta
  (package
    (name "px-software-assets-meta")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://assets.software.pantherx.org/px-software-assets_meta_"
             version ".tgz"))
       (sha256
        (base32 "0wd4gnsy9sw3cbm8b587qkscap7ljr0hbg6hk2c7pjy69rq0q5nr"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (tar (assoc-ref %build-inputs "tar"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (wiki-dir (string-append %output
                                     "/share/px-software-assets-meta")))
                     (mkdir-p wiki-dir)
                     (setenv "PATH"
                             (string-append gzip "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf" source "-C"
                             wiki-dir)))))
    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)))
    (inputs `(("bash" ,bash)))
    (home-page
     "https://git.pantherx.org/development/applications/px-software-assets/")
    (synopsis "PantherX Software Assets meta file.")
    (description "PantherX Software Assets meta file.")
    (license license:expat)))

(define-public px-software
  (package
    (name "px-software")
    (version "0.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/px-software_v" version
                           ".tgz"))
       (sha256
        (base32 "069hcfw6jm3yhb04n16yma9nh7zlm11szw0an6zhraz3rigr1w8w"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'set-executable
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (invoke "chmod" "755"
                                (string-append out
                                 "/bin/px-software-update-check")) #t))))))
    (inputs `(("yaml-cpp" ,yaml-cpp)))
    (native-inputs `(("guile" ,guile-3.0-latest)
                     ("recutils" ,recutils)
                     ("px-gui-library" ,px-gui-library)
                     ("qtbase" ,qtbase-5)
                     ("qtlinguist" ,qttools-5)
                     ("zlib" ,zlib)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Software")
    (description "Software Application for PantherX")
    (license license:gpl3)))


