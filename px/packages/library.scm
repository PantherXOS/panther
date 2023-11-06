;;; Library Packages Module for PantherX
;;; Hamzeh Nasajpour (h.nasajpour@pantherx.org)
;;;

(define-module (px packages library)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (px packages common)
  #:use-module (px packages python-xyz)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public px-auth-library-cpp
  (package
    (name "px-auth-library-cpp")
    (version "0.0.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "1q4ll8zdg7hrj0r1spdspp3zl40n69473qjib8c87a69yrr0j0id"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f))
    (inputs (list qtbase-5))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX GUI Library")
    (description "GUI Framework for PantherX applications as shared library")
    (license license:expat)))

(define-public px-gui-library
  (package
    (name "px-gui-library")
    (version "0.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "0205bbj1kz0fpgy93xyq2nvzwh3knvrj89akbwidsgik8dn5w3if"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs (list qtbase-5 qttools-5))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX GUI Library")
    (description "GUI Framework for PantherX applications as shared library")
    (license license:expat)))

(define-public px-claws-mail-parser
  (package
    (name "px-claws-mail-parser")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "0pn74mgbbz4v15dw9bmyb8lcxzknmwqnfy73m6b3vr70n52a528b"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))
    (inputs `(("python-pandas" ,python-pandas)
              ("python-pylint" ,python-pylint-2.5.3)
              ("python-toml" ,python-toml)
              ("python-lazy-object-proxy-1.4.0" ,python-lazy-object-proxy-1.4.0)
              ("python-ipaddress" ,python-ipaddress)
              ("python-wrapt" ,python-wrapt-1.12.1)))
    (propagated-inputs `(("python-simplejson" ,python-simplejson-3.17.0)
                         ("python-mail-parser" ,python-mail-parser-3.14.0)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Easy access to various APIs with unified return format.")
    (description
     "This python library provides easy, uniform access to various API ressources,
returning a unified HubMessage array.")
    (license license:expat)))

(define-public px-secret-library-python
  (package
    (name "px-secret-library-python")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "0iahzdbwps49bn9pnkiydp0gc8gry82bm8kjyj51mqjrnzr5pvsv"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs `(("python-secretstorage" ,python-secretstorage)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Easy access to secret service")
    (description
     "This python library provides easy, uniform access to secret service.")
    (license license:expat)))

(define-public px-lib-rw-guix-config
  (package
    (name "px-lib-rw-guix-config")
    (version "v0.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://source.pantherx.org/px-lib-rw-guix-config_" version
             ".tgz"))
       (sha256
        (base32 "1ij68sks9qphr12zpbp3skiqy2nw6jb4sazqmlcqlha7n953dql2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-source
                    (lambda _
                      (chdir "../"))))))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX config.scm parser/builder library")
    (description
     "This library provides a series of methods for add/remove/get users, service, ... in 
config.scm")
    (license license:expat)))

(define-public px-online-sources-library
  (package
    (name "px-online-sources-library")
    (version "0.0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0v5rj1gdq8gi42qpdq0lpgqydqiyx1zj3vfazkvb11mvp94ad3dd"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))
    (inputs `(("python-idna" ,python-idna)))
    (propagated-inputs `(("python-etherscan" ,python-etherscan)
                         ("python-requests" ,python-requests)
                         ("python-block-io" ,python-block-io)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Easy access to various APIs with unified return format.")
    (description
     "This python library provides easy, uniform access to various API ressources,
returning a unified HubMessage array.")
    (license license:expat)))

