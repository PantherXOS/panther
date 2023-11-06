(define-module (px packages machine-learning)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public dlib
  (package
    (name "dlib")
    (version "19.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://dlib.net/files/dlib-" version ".tar.bz2"))
       (sha256
        (base32 "0g7mgm6l6nw9hlw2zckf59jcd2y2lf907n88hjschkccmq1qlwmy"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Delete ~13MB of bundled dependencies.
                   (delete-file-recursively "dlib/external")
                   (delete-file-recursively "docs/dlib/external") #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-asserts
                    (lambda _
                      ;; config.h recommends explicitly enabling or disabling asserts
                      ;; when building as a shared library. By default neither is set.
                      (substitute* "dlib/config.h"
                        (("^//#define DLIB_DISABLE_ASSERTS")
                         "#define DLIB_DISABLE_ASSERTS")) #t))
                  (add-after 'disable-asserts 'disable-failing-tests
                    (lambda _
                      ;; One test times out on MIPS, so we need to disable it.
                      ;; Others are flaky on some platforms.
                      (let* ((system ,(or (%current-target-system)
                                          (%current-system)))
                             (disabled-tests (cond
                                               ((string-prefix? "mips64"
                                                                system)
                                                '("object_detector" ;timeout
                                                  "data_io"))
                                               ((string-prefix? "armhf" system)
                                                '("learning_to_track"))
                                               ((string-prefix? "i686" system)
                                                '("optimization"))
                                               (else '()))))
                        (for-each (lambda (test)
                                    (substitute* "dlib/test/makefile"
                                      (((string-append "SRC \\+= " test
                                                       "\\.cpp"))
                                       ""))) disabled-tests) #t)))
                  (replace 'check
                    (lambda _
                      ;; No test target, so we build and run the unit tests here.
                      (let ((test-dir (string-append "../dlib-"
                                                     ,version "/dlib/test")))
                        (with-directory-excursion test-dir
                          (invoke "make" "-j"
                                  (number->string (parallel-job-count)))
                          (invoke "./dtest" "--runall")) #t))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ;; For tests.
                     ("libnsl" ,libnsl)))
    (inputs `(("giflib" ,giflib)
              ("lapack" ,lapack)
              ("libjpeg" ,libjpeg-turbo)
              ("libpng" ,libpng)
              ("libx11" ,libx11)
              ("openblas" ,openblas)
              ("zlib" ,zlib)))
    (synopsis
     "Toolkit for making machine learning and data analysis applications in C++")
    (description
     "Dlib is a modern C++ toolkit containing machine learning algorithms and
tools.  It is used in both industry and academia in a wide range of domains
including robotics, embedded devices, mobile phones, and large high performance
computing environments.")
    (home-page "http://dlib.net")
    (license license:boost1.0)))
