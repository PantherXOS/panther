(define-module (px packages framework)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libftdi))

(define-public ectool
  (package
    (name "ectool")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.howett.net/DHowett/ectool")
             (commit "0ac6155abbb7d4622d3bcf2cdf026dde2f80dad7")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zwb2b8yldhxkwwh680mfb15mlk6n7b16zl0mmr4q1wnxj5abhqh"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "src/ectool" bin)
               #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libusb libftdi))
    (home-page "https://gitlab.howett.net/DHowett/ectool")
    (synopsis "ChromeOS EC Tool")
    (description
     "ECTool is a utility for interacting with the Embedded Controller (EC) 
      in ChromeOS devices and Framework laptops.")
    (license license:expat)))