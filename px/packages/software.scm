;;; Software Application for PantherX
;;; Author: Reza Alizadeh Majd (r.majd@pantherx.org)

(define-module (px packages software)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages qt)
  #:use-module (px  packages library)
  #:use-module (gnu packages serialization))


(define-public px-software-assets-meta
  (package
    (name "px-software-assets-meta")
    (version "0.1.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://assets.software.pantherx.org/px-software-assets_meta_" version ".tgz"))
             (sha256
              (base32
               "0wd4gnsy9sw3cbm8b587qkscap7ljr0hbg6hk2c7pjy69rq0q5nr"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source   (assoc-ref %build-inputs "source"))
                          (tar      (assoc-ref %build-inputs "tar"))
                          (gzip     (assoc-ref %build-inputs "gzip"))
                          (wiki-dir (string-append %output "/share/px-software-assets-meta")))
                     (mkdir-p wiki-dir)
                     (setenv "PATH" (string-append gzip "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf" source "-C" wiki-dir)))))
    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)))
    (inputs `(("bash" ,bash)))
    (home-page "https://git.pantherx.org/development/applications/px-software-assets/")
    (synopsis "PantherX Software Assets meta file.")
    (description
     "PantherX Software Assets meta file.")
    (license license:expat)))

(define-public px-software
  (package
    (name "px-software")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
                "https://source.pantherx.org/px-software_v"
                version
               ".tgz"))
        (sha256 (base32 "069hcfw6jm3yhb04n16yma9nh7zlm11szw0an6zhraz3rigr1w8w"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'set-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
                (invoke "chmod" "755" (string-append out "/bin/px-software-update-check"))
               #t))))))
    (inputs
      `(("yaml-cpp" ,yaml-cpp)))
    (native-inputs 
      `(("guile" ,guile-3.0-latest)
        ("recutils" ,recutils)
        ("px-gui-library" ,px-gui-library)
        ("qtbase" ,qtbase-5)
        ("qtlinguist" ,qttools-5)
        ("zlib" ,zlib)
        ))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Software")
    (description "Software Application for PantherX")
    (license license:gpl3)))

