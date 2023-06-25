(define-module (px packages password)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages serialization)
  #:use-module (px packages common))
  
(define-public px-pass-service
  (package
    (name "px-pass-service")
    (version "0.0.94")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/px_pass_service_" version
          ".tgz"))
        (sha256 
          (base32
            "08qx0dg2igibx0ifr55x5vcdac3giwln0grk86bkki1n0fykkfq3"))))
    (build-system gnu-build-system)
    (arguments
      `(
        #:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'unpack 'fix-source
              (lambda _ 
                (chdir "../")               ;; - guix automatically change cwd to first sub-directory, which is not desired.
                (delete-file "configure")   ;; - we need to delete old cofigure file inside package and allow autotools to 
                #t))                        ;; recreate that.
            (add-before 'install-license-files 'cleanup-license
              (lambda _
                (delete-file "COPYING"))))))
    (inputs `(("capnproto", capnproto-0.9)))
    (native-inputs `(
      ("autoconf", autoconf)
      ("automake", automake)
      ("pkg-config", pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Password Management Service")
    (description "Password manager service for pantherx operating system.")
    (license gpl3+)))
