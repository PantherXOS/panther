(define-module (px packages dav)
  #:use-module ((guix licenses)
                #:select (gpl3))
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages file)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages file))

(define-public px-cal-card-dav-lib
  (package
    (name "px-cal-card-dav-lib")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1273cy1yxx9i4723s88xrq1vf9448dk42i52rfc6245hnb83lcv2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("qtbase" ,qtbase-5)))
    (home-page "https://www.pantherx.org/")
    (synopsis
     "C++ CardDAV/CalDAV Client library")
    (description
     "C++ CardDAV/CalDAV Client library built with QT to connect to CardDAV/CalDAV Servers")
    (license license:expat)))

(define-public libccdav
  (package
    (name "libccdav")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FSajadi/libccdav")
             (commit "6ab7e1479b7b6696ca6be6491b527f41e799c0bc")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01s3rfmafvwadlqv6qx0dzlax0vga3g8w6wxkcasyxjdc1qygcx5"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'update-installation-path
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "CMakeLists.txt"
                          (("/usr")
                           out)) #t))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("qtbase" ,qtbase-5)))
    (inputs `(("qtbase" ,qtbase-5)))
    (home-page "https://github.com/FSajadi/libccdav")
    (synopsis
     "C++ CardDAV/CalDAV Client library")
    (description
     "C++ CardDAV/CalDAV Client library built with QT to connect to CardDAV/CalDAV Servers")
    (license gpl3)))

