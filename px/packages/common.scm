;;; Package Repository for GNU Guix
;;; Copyright © 2021-2023 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages common)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages check)
  #:use-module (gnu packages networking)
  #:use-module (px packages python-xyz))

(define-public capnproto-shared
  (package
    (inherit capnproto)
    (name "capnproto-shared")
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       ,@(package-arguments capnproto)))))

(define-public python-pycapnp
  (package
    (name "python-pycapnp")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/capnproto/pycapnp/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32 "0ysf179ki84maywgf8c4kaj7i8cjb9i92cyj2ygz4ggp2hfs53v9"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags '(@ ("force-system-libcapnp" . #t))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'force-system-library
                    (lambda _
                      (substitute* "setup.py"
                        (("need_build = True")
                         "need_build = False"))))
                  (delete 'sanity-check))))
    (native-inputs (list python-cython
                         python-pkgconfig
                         python-setuptools
                         python-wheel
                         capnproto-shared))
    (propagated-inputs (list python-jinja2))
    (home-page "https://github.com/capnproto/pycapnp")
    (synopsis "Cython wrapping of the C++ Cap'n Proto library")
    (description
     "This package provides a Cython wrapping of the C++ Cap'n Proto library,
enabling Python applications to use the Cap'n Proto serialization and RPC
system.")
    (license license:bsd-2)))

(define-public python-pynng
  (package
    (name "python-pynng")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pynng" version))
       (sha256
        (base32 "0621j0dmrhg8ydrpr3k5ia50hp73r9khrcbwvp43jb51igl6wwvc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-source
                    (lambda _
                      (substitute* '("setup.py" "build_nng.sh")
                        ;; Replace default shell path.
                        (("/bin/bash")
                         (which "sh"))) #t)))
       #:tests? #f))
    (native-inputs `(("python-pytest-runner" ,python-pytest-runner)
                     ("cmake" ,cmake)))
    (propagated-inputs `(("python-cffi" ,python-cffi)
                         ("python-sniffio" ,python-sniffio)))
    (home-page "https://github.com/codypiersall/pynng")
    (synopsis "Python bindings for Nanomsg Next Generation")
    (description
     "Ergonomic bindings for nanomsg next generation (nng), in Python.
pynng provides a nice interface on top of the full power of nng")
    (license license:expat)))

(define-public cpr
  (package
    (name "cpr")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/whoshuu/cpr/archive/v" version
                           ".tar.gz"))
       (sha256
        (base32 "18w0v6jhjz05c844wgsb07cxp4bbmcw0jiz9ka4hjsn6g5s3rmx6"))))
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DUSE_SYSTEM_CURL=ON" "-DBUILD_CPR_TESTS=OFF")))
    (build-system cmake-build-system)
    (native-inputs `(("curl" ,curl)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("curl" ,curl)))
    (home-page "https://whoshuu.github.io/cpr/")
    (synopsis "C++ Requests: Curl for People ")
    (description "C++ Requests is a simple wrapper around libcurl
  inspired by the excellent Python Requests project.")
    (license license:expat)))

(define-public restclient-cpp
  (package
    (name "restclient-cpp")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mrtazz/restclient-cpp/archive/"
             version ".tar.gz"))
       (sha256
        (base32 "1v35pkgqdcmyr1c91r9r312rjak6x24k4j1vslpnaf59z4cacayn"))))
    (arguments
     `(#:tests? #f))
    (build-system cmake-build-system)
    (native-inputs `(("libtool" ,libtool)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("curl" ,curl)))
    (home-page "https://github.com/mrtazz/restclient-cpp")
    (synopsis "C++ client")
    (description "C++ client for making HTTP/REST requests ")
    (license license:expat)))

