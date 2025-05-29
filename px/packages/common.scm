;;; Package Repository for GNU Guix
;;; Copyright © 2021-2023 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px packages common)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
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

(define-public capnproto-0.9
  (package
    (inherit capnproto)
    (name "capnproto")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://capnproto.org/capnproto-c++-" version
                           ".tar.gz"))
       (sha256
        (base32 "0hi5lpyhskdg99n9zgn0ffr79gn12m1j7igkp9wikklg2p4yjca0"))))))

(define-public python-pycapnp
  (package
    (name "python-pycapnp")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/capnproto/pycapnp/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32 "0kj9jpg6vpmlqgzqnxz2dsbihwhimq9xzq6yrkqvgdzz3sdlk8fh"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-3
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-source
                    (lambda _
                      (substitute* '("setup.py")
                        (("if need_build")
                         "if False")) #t))
                  (delete 'sanity-check))))
    (native-inputs `(("python-pkgconfig" ,python-pkgconfig)
                     ("python-setuptools" ,python-setuptools)
                     ("python-wrapper" ,python-wrapper)
                     ("python-cython" ,python-cython)
                     ("capnproto" ,capnproto)
                     ("python-sphinx" ,python-sphinx)
                     ("python-tox" ,python-tox)
                     ("python-wheel" ,python-wheel)))
    (propagated-inputs `(("python-jinja2" ,python-jinja2)))
    (home-page "http://jparyani.github.io/pycapnp")
    (synopsis "Capability-based RPC and serialization system")
    (description "This is a python3 wrapping of the C++
implementation of the Cap’n Proto library.")
    (license license:gpl2+)))

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

