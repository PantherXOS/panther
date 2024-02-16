(define-module (px packages python-xyz)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages time)
  #:use-module (guix utils)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages django)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages check)
  #:use-module (gnu packages web)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression))

(define-public pybind11-2.6.2
  (package
    (name "pybind11")
    (version "2.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pybind/pybind11")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1lsacpawl2gb5qlh0cawj9swsyfbwhzhwiv6553a7lsigdbadqpy"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs `(("python" ,python-wrapper)
                     
                     ;; The following dependencies are used for tests.
                     ("python-pytest" ,python-pytest)
                     ("catch" ,catch2-1)
                     ("eigen" ,eigen)))
    (arguments
     `(#:configure-flags (list (string-append "-DCATCH_INCLUDE_DIR="
                                              (assoc-ref %build-inputs "catch")
                                              "/include/catch"))

       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-python
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (with-directory-excursion "../source"
                          (setenv "PYBIND11_USE_CMAKE" "yes")
                          (invoke "python"
                                  "setup.py"
                                  "install"
                                  "--single-version-externally-managed"
                                  "--root=/"
                                  (string-append "--prefix=" out)))))))

       #:test-target "check"))
    (home-page "https://github.com/pybind/pybind11/")
    (synopsis "Seamless operability between C++11 and Python")
    (description
     "@code{pybind11} is a lightweight header-only library that exposes C++
types in Python and vice versa, mainly to create Python bindings of existing
C++ code.  Its goals and syntax are similar to the @code{Boost.Python}
library: to minimize boilerplate code in traditional extension modules by
inferring type information using compile-time introspection.")
    (license license:bsd-3)))

(define-public python-maestral-qt
  (package
    (name "python-maestral-qt")
    (version "1.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/SamSchott/maestral-qt/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "1m5lvgjyad8lvkvx0fx9xnjakg6ljxp3kbfhhw9fsk7cbkc1l70k"))))
    (build-system python-build-system)
    (native-inputs `(("python-click" ,python-click-8)
                     ("python-pyqt" ,python-pyqt)
                     ("python-markdown2" ,python-markdown2)
                     ("python-wheel" ,python-wheel)
                     ("python-packaging" ,python-packaging)
                     ("python-importlib-resources" ,python-importlib-resources)
                     ("python-maestral" ,python-maestral)))
    (home-page "https://github.com/SamSchott/maestral-qt")
    (synopsis "A Qt user interface for the Maestral Daemon.")
    (description "A Qt user interface for the Maestral Daemon.")
    (license license:asl2.0)))

(define-public python-markdown2
  (package
    (name "python-markdown2")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/trentm/python-markdown2/archive/refs/tags/"
             version ".tar.gz"))
       (sha256
        (base32 "0259g4wq124fxzrqs50frkc03zldy916g68xn7n9vpw1z0iznva5"))))
    (build-system python-build-system)
    (home-page "https://github.com/trentm/python-markdown2")
    (synopsis
     "Markdown is a light text markup format and a processor to convert that to HTML.")
    (description
     "Markdown is a light text markup format and a processor to convert that to HTML.")
    (license license:asl2.0)))

(define-public python-maestral
  (package
    (name "python-maestral")
    (version "1.6.4") ;1.7.2 fails with no setup.py found
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "maestral" version))
       (sha256
        (base32 "1ik6hff1vc8swmpwzlv6bhzca4yha4mjk9j78jbyf7nnc5mnl0xs"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-click" ,python-click-8)
                         ("python-desktop-notifier" ,python-desktop-notifier)
                         ("python-dropbox" ,python-dropbox)
                         ("python-fasteners" ,python-fasteners)
                         ("python-importlib-metadata" ,python-importlib-metadata)
                         ("python-keyring" ,python-keyring)
                         ("python-keyrings.alt" ,python-keyrings.alt)
                         ("python-packaging" ,python-packaging)
                         ("python-pathspec" ,python-pathspec)
                         ("python-pyro5" ,python-pyro5)
                         ("python-requests" ,python-requests)
                         ("python-sdnotify" ,python-sdnotify)
                         ("python-survey" ,python-survey)
                         ("python-watchdog" ,python-watchdog)))
    (native-inputs `(("python-black" ,python-black)
                     ("python-bump2version" ,python-bump2version)
                     ("python-flake8" ,python-flake8)
                     ("python-mypy" ,python-mypy)
                     ("python-pre-commit" ,python-pre-commit)
                     ("python-pytest" ,python-pytest)
                     ("python-wheel" ,python-wheel)
                     ("python-pytest-benchmark" ,python-pytest-benchmark)
                     ("python-pytest-cov" ,python-pytest-cov)
                     ("python-pytest-rerunfailures" ,python-pytest-rerunfailures)
                     ("python-types-requests" ,python-types-requests)))
    (home-page "https://maestral.app")
    (synopsis "Open-source Dropbox client for macOS and Linux.")
    (description "Open-source Dropbox client for macOS and Linux.")
    (license license:expat)))

(define-public python-dbus-next
  (package
    (name "python-dbus-next")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/altdesktop/python-dbus-next/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0nggps85gq1f2lp9gf8kv1am3zihdk65zdg8hy2xs3p51ndnh6fv"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/altdesktop/python-dbus-next")
    (synopsis "A zero-dependency DBus library for Python with asyncio support")
    (description
     "A zero-dependency DBus library for Python with asyncio support")
    (license license:expat)))

(define-public python-desktop-notifier
  (package
    (name "python-desktop-notifier")
    (version "3.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "desktop-notifier" version))
       (sha256
        (base32 "0wrqgbvhi0implwqzy5as0hk3lh7cc83h9942rz1jrdw797vvmhd"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-dbus-next" ,python-dbus-next)
                         ("python-importlib-resources" ,python-importlib-resources)
                         ("python-packaging" ,python-packaging)
                         ;; ("python-rubicon-objc" ,python-rubicon-objc)
                         ))
    (native-inputs `(("python-black" ,python-black)
                     ("python-bump2version" ,python-bump2version)
                     ("python-wheel" ,python-wheel)
                     ("python-flake8" ,python-flake8)
                     ("python-mypy" ,python-mypy)
                     ("python-pre-commit" ,python-pre-commit)
                     ("python-pytest" ,python-pytest)
                     ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://github.com/samschott/desktop-notifier")
    (synopsis "Python library for cross-platform desktop notifications")
    (description "Python library for cross-platform desktop notifications")
    (license license:expat)))

(define-public python2-pymongo
  (package-with-python2 python-pymongo))

(define-public python-pyro5
  (package
    (name "python-pyro5")
    (version "5.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pyro5" version))
       (sha256
        (base32 "1qpjyh7l5l9p6mhsb77m84ch6hkvr74hay4ldyi0rnj1qdbnjvk1"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-serpent" ,python-serpent)))
    (home-page "https://github.com/irmen/Pyro5")
    (synopsis "Remote object communication library, fifth major version")
    (description "Remote object communication library, fifth major version")
    (license license:expat)))

(define-public python-sdnotify
  (package
    (name "python-sdnotify")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sdnotify" version))
       (sha256
        (base32 "1wdrdg2j16pmqhk0ify20s5pngijh7zc6hyxhh8w8v5k8v3pz5vk"))))
    (build-system python-build-system)
    (home-page "https://github.com/bb4242/sdnotify")
    (synopsis
     "A pure Python implementation of systemd's service notification protocol (sd_notify)")
    (description
     "A pure Python implementation of systemd's service notification protocol (sd_notify)")
    (license #f)))

(define-public python-survey
  (package
    (name "python-survey")
    (version "3.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "survey" version))
       (sha256
        (base32 "1qpg5alg6m9kfj5y90chgr66g0f33r3xqrrx2d496vipwsm3vbsc"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-wrapio" ,python-wrapio)))
    (home-page "https://github.com/Exahilosys/survey")
    (synopsis "A simple library for creating beautiful interactive prompts.")
    (description
     "A simple library for creating beautiful interactive prompts.")
    (license license:expat)))

(define-public python-wrapio
  (package
    (name "python-wrapio")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wrapio" version))
       (sha256
        (base32 "1s696hblap6qz8pc6zspg75bnvh7d48ww1qh1d8d5zjrnwhiqjh9"))))
    (build-system python-build-system)
    (home-page "https://github.com/Exahilosys/wrapio")
    (synopsis "Handling event-based streams.")
    (description "Handling event-based streams.")
    (license license:expat)))

(define-public python-pytest-rerunfailures
  (package
    (name "python-pytest-rerunfailures")
    (version "10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-rerunfailures" version))
       (sha256
        (base32 "0ws2hbgh00nd6xchyi9ymyxfpg5jpxsy5mxdz4nxvriyw5nw05vn"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/pytest-dev/pytest-rerunfailures")
    (synopsis "pytest plugin to re-run tests to eliminate flaky failures")
    (description "pytest plugin to re-run tests to eliminate flaky failures")
    (license #f)))

(define-public python-types-requests
  (package
    (name "python-types-requests")
    (version "2.25.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "types-requests" version))
       (sha256
        (base32 "1vh203dppi6457lwv7z46dc8rpanjlahk4v3394nq1jwyp0425g2"))))
    (build-system python-build-system)
    (home-page "https://github.com/python/typeshed")
    (synopsis "Typing stubs for requests")
    (description "Typing stubs for requests")
    (license #f)))

(define-public python-mail-parser
  (package
    (name "python-mail-parser")
    (version "3.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mail-parser" version))
       (sha256
        (base32 "11q384z2pa3i4w0d07mnqd25r16sdgvgd8m75ysff6w0kr3vhxvm"))))
    (build-system python-build-system)
    (inputs `(("python-simplejson" ,python-simplejson)
              ("python-six" ,python-six)))
    (home-page "https://github.com/SpamScope/mail-parser")
    (synopsis
     "mail-parser is not only a wrapper for email Python Standard Library.")
    (description
     "mail-parser is not only a wrapper for email Python Standard Library.
It give you an easy way to pass from raw mail to Python object that you can
use in your code.")
    (license license:asl2.0)))

(define-public python-mail-parser-3.14.0
  (package
    (inherit python-mail-parser)
    (name "python-mail-parser-3.14.0")
    (version "3.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mail-parser" version))
       (sha256
        (base32 "11q384z2pa3i4w0d07mnqd25r16sdgvgd8m75ysff6w0kr3vhxvm"))))))

(define-public python-mail-parser-3.12.0
  (package
    (inherit python-mail-parser)
    (name "python-mail-parser-3.12.0")
    (version "3.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mail-parser" version))
       (sha256
        (base32 "052j6iwsfjg472wb74c9n9i9cgqjm6lsrkb9zs3hljkxnb24mzz8"))))))

(define-public python-pylint-2.5.3
  (package
    (inherit python-pylint)
    (name "python-pylint")
    (version "2.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PyCQA/pylint")
             (commit (string-append "pylint-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04cgbh2z1mygar63plzziyz34yg6bdr4i0g63jp256fgnqwb1bi3"))))
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))))

(define-public python-simplejson-3.17.0
  (package
    (inherit python-simplejson)
    (name "python-simplejson-3.17.0")
    (version "3.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "simplejson" version))
       (sha256
        (base32 "108yf3252fy4ndqab7h46raksxfhcn113bzy2yd8369vidrjnjrb"))))))

(define-public python-wrapt-1.12.1
  (package
    (inherit python-wrapt)
    (name "python-wrapt-1.12.1")
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wrapt" version))
       (sha256
        (base32 "1mwpkdnqxf0yxj1a0hsnl06lf287qi5ap7309wm37x45zf0zlbxn"))))))

(define-public python-pyscard-1.9.9
  (package
    (inherit python-pyscard)
    (name "python-pyscard-1.9.9")
    (version "1.9.9")
    (source
     (origin
       (method url-fetch)
       ;; The maintainer publishes releases on various sites, but
       ;; SourceForge is apparently the only one with a signed release.
       (uri (string-append "mirror://sourceforge/pyscard/pyscard/pyscard%20"
                           version "/pyscard-" version ".tar.gz"))
       (sha256
        (base32 "082cjkbxadaz2jb4rbhr0mkrirzlqyqhcf3r823qb0q1k50ybgg6"))))))

(define-public python-shortuuid-v1
  (package
    (inherit python-shortuuid)
    (name "python-shortuuid")
    (version "1.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "shortuuid" version))
       (sha256
        (base32 "12gph4sgmhzqp5pzrm8gxxkcni9pa4x1nl5i9j75m08lb5hz4xgw"))))
    (build-system python-build-system)
    (native-inputs (list python-pep8 python-django))
    (home-page "https://github.com/skorokithakis/shortuuid")
    (synopsis "Generator library for concise, unambiguous and URL-safe UUIDs")
    (description
     "@code{shortuuid} is a Python library for generating concise, unambiguous
and URL-safe UUIDs.  UUIDs are generated using the built-in Python @code{uuid}
module and then similar looking characters are removed.")
    (license license:bsd-3)))

(define-public python-pyyaml-v5.3.1
  (package
    (name "python-pyyaml-v5.3.1")
    (version "5.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyYAML" version))
       (sha256
        (base32 "0pb4zvkfxfijkpgd1b86xjsqql97ssf1knbd1v53wkg1qm9cgsmq"))))
    (build-system python-build-system)
    (inputs `(("libyaml" ,libyaml)))
    (home-page "https://pyyaml.org")
    (synopsis "YAML parser and emitter for Python")
    (description
     "PyYAML is a YAML parser and emitter for Python.  PyYAML features a
complete YAML 1.1 parser, Unicode support, pickle support, capable extension
API, and sensible error messages.  PyYAML supports standard YAML tags and
provides Python-specific tags that represent an arbitrary Python object.")
    (license license:expat)))

(define-public python-jose-3.1.0
  (package
    (name "python-jose-3.1.0")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-jose" version))
       (sha256
        (base32 "1kmz7bzwvjk0nnzcpfqhbnlvv4ngkr380xnf5hj9sblnnvyvg144"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))

    (inputs `(("python-pytest-runner" ,python-pytest-runner)
              ("python-pyasn1" ,python-pyasn1)
              ("python-rsa" ,python-rsa)
              ("python-ecdsa" ,python-ecdsa)))
    (propagated-inputs `(("python-pyasn1" ,python-pyasn1)
                         ("python-rsa" ,python-rsa)
                         ("python-ecdsa" ,python-ecdsa)
                         ;; ("python-urllib3" ,python-urllib3)
                         ))

    (home-page "")
    (synopsis "")
    (description "todo.")
    (license license:psfl)))

(define-public python-exitstatus-2.0.1
  (package
    (name "python-exitstatus-2.0.1")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "exitstatus" version))
       (sha256
        (base32 "193x2775ffy9qkaz267g2qjagd132xprh5nv4iq7a4kq61a26yjf"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))

    ;; (native-inputs `(
    ;; ("python-cryptography" ,python-cryptography)))
    
    (home-page "")
    (synopsis "")
    (description "todo.")
    (license license:psfl)))

(define-public python-authlib-0.14.3
  (package
    (name "python-authlib-0.14.3")
    (version "0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Authlib" version))
       (sha256
        (base32 "1xljzkzhhc27rbr9zj37iv7cbwf6wqgbyq9gmki3svwrks790lnc"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))

    (native-inputs `(("python-cryptography" ,python-cryptography)))

    (home-page "")
    (synopsis "")
    (description "todo.")
    (license license:psfl)))

(define-public python-requests-2.23
  (package
    (name "python-requests")
    (version "2.23")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests" version))
       (sha256
        (base32 "1rhpg0jb08v0gd7f19jjiwlcdnxpmqi1fhvw7r4s9avddi4kvx5k"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-certifi" ,python-certifi)
                         ("python-chardet" ,python-chardet)
                         ("python-idna" ,python-idna)
                         ("python-urllib3" ,python-urllib3)))
    (arguments
     ;; FIXME: Some tests require network access.
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))
    (home-page "http://python-requests.org/")
    (synopsis "Python HTTP library")
    (description
     "Requests is a Python HTTP client library.  It aims to be easier to use
than Python’s urllib2 library.")
    (license license:asl2.0)))

(define-public python-requests-cache
  (package
    (name "python-requests-cache")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/reclosedev/requests-cache/archive/v" version
             ".tar.gz"))
       (sha256
        (base32 "05r26hb1ck0q4ffm334x078nq97hz6cg0nr8nlw1vvh9rl4g0ikq"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs `(("python-requests" ,python-requests)))
    (home-page "https://github.com/reclosedev/requests-cache")
    (synopsis
     "Python-requests-cache is a transparent persistent cache for python-requests (version >= 1.1.0) library.")
    (description
     "Python-requests-cache uses python-requests, so it needs an available installation of python-requests.")
    (license license:expat)))

(define-public python-etherscan
  (package
    (name "python-etherscan")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/d0/7a/d2c0cc7ce6c54854931f0e069997f1b82dafeff1bdd302e34153ccca9f59/etherscan-"
             version ".tar.gz"))
       (sha256
        (base32 "1qqgiy6q8kdsl3gvclgvbnzni8wm3yccsxsq85dlc7mw2dfzhapp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs `(("python-requests-cache" ,python-requests-cache)))
    (home-page "https://github.com/neoctobers/etherscan")
    (synopsis "Python-etherscan provides a wrapper for Etherscan.io API.")
    (description
     "Python-etherscan uses python-requests-cache, so it needs an available installation of python-requests-cache.")
    (license license:expat)))

(define-public python-block-io
  (package
    (name "python-block-io")
    (version "1.1.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/BlockIo/block_io-python/archive/" version
             ".tar.gz"))
       (sha256
        (base32 "15n8qapdlx8n9f27vj9qr34wgfahwgnqwh039plc6abs4flqjjrb"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))
    (propagated-inputs `(("python-requests" ,python-requests)
                         ("python-pycryptodome" ,python-pycryptodome)
                         ("python-ecdsa" ,python-ecdsa)
                         ("python-six" ,python-six)
                         ("python-base58" ,python-base58)))
    (home-page "https://github.com/BlockIo/block_io-python")
    (synopsis
     "This Python package is the official reference client for the Block.io payments API.")
    (description "Please use Python2.7+. Also compatible with Python 3.0+.")
    (license license:expat)))

(define-public python-appdirs-1.4.3
  (package
    (inherit python-appdirs)
    (name "python-appdirs")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "appdirs" version))
       (sha256
        (base32 "14id6wxi12lgyw0mg3bcfnf888ad07jz9yj46gfzhn186z8rcn4y"))))))

(define-public python-pyrsistent-0.14.11
  (package
    (inherit python-pyrsistent)
    (name "python-pyrsistent")
    (version "0.14.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyrsistent" version))
       (sha256
        (base32 "1qkh74bm296mp5g3r11lgsksr6bh4w1bf8pji4nmxdlfj542ga1w"))))))

(define-public python-click-8
  (package
    (inherit python-click)
    (name "python-click")
    (version "8.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "click" version))
       (sha256
        (base32 "0ymdyf37acq4qxh038q0xx44qgj6y2kf0jd0ivvix6qij88w214c"))))))

(define-public python-itsdangerous-1.1.0
  (package
    (inherit python-itsdangerous)
    (name "python-itsdangerous")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "itsdangerous" version))
       (sha256
        (base32 "068zpbksq5q2z4dckh2k1zbcq43ay74ylqn77rni797j0wyh66rj"))))))

(define-public python-markupsafe-1.1.1
  (package
    (inherit python-markupsafe)
    (name "python-markupsafe")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MarkupSafe" version))
       (sha256
        (base32 "0sqipg4fk7xbixqd8kq6rlkxj664d157bdwbh93farcphf92x1r9"))))))

(define-public python-pytz-2018.9
  (package
    (inherit python-pytz)
    (name "python-pytz")
    (version "2018.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytz" version))
       (sha256
        (base32 "0k1pqnq3c4gvcspjxf3fschdjn71fk89i4wz801rxqh7f145xw6m"))))))

(define-public python-mypy
  (package
    (name "python-mypy")
    (version "0.701")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mypy" version))
       (sha256
        (base32 "05479r3gbq17r22hyhxjg49smx5q864pgx8ayy23rsdj4w6z2r2p"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))
    (inputs `(("python-typed-ast" ,python-typed-ast)
              ("python-mypy-extensions" ,python-mypy-extensions)))
    (home-page "http://www.mypy-lang.org/")
    (synopsis "Optional static typing for Python (mypyc-compiled version)")
    (description
     "Add type annotations to your Python programs, and use mypy to type check them.
	 Mypy is essentially a Python linter on steroids, and it can catch many programming errors by analyzing your program,
	 without actually having to run it. Mypy has a powerful type system with features such as type inference,
	 gradual typing, generics and union types.")
    (license license:expat)))

(define-public python-persist-queue
  (package
    (name "python-persist-queue")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "persist-queue" version))
       (sha256
        (base32 "0xhvj26jkc4fk0yjzn47is6wh8figyp5cralj8d56r4bidn78wp2"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/peter-wangxu/persist-queue")
    (synopsis "A thread-safe disk based persistent queue in Python.")
    (description
     "persist-queue implements a file-based queue and a serial of sqlite3-based queues")
    (license license:bsd-2)))

(define-public python-persist-queue-0.3.5
  (package
    (inherit python-persist-queue)
    (name "python-persist-queue")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "persist-queue" version))
       (sha256
        (base32 "1bdz04ybjqczlp3x4wc4jq2dbr6r6zqbxak0l27av82irg91m2wn"))))))

(define-public strict-rfc3339
  (package
    (name "strict-rfc3339")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "strict-rfc3339" version))
       (sha256
        (base32 "0xqjbsn0g1h88rayh5yrpdagq60zfwrfs3yvk6rmgby3vyz1gbaw"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/danielrichman/strict-rfc3339")
    (synopsis "Strict, simple, lightweight RFC3339 functions.")
    (description "Strict, simple, lightweight RFC3339 functions.")
    (license license:gpl3+)))

(define-public python-json-logger-0.1.10
  (package
    (inherit python-json-logger)
    (name "python-json-logger")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-json-logger" version))
       (sha256
        (base32 "1plcfi4z3rpch29l64hpjxwxwi829pg44qjvj6ag7vyj9grslb6g"))))))

(define-public python-supervisor
  (package
    (name "python-supervisor")
    (version "4.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "supervisor" version))
       (sha256
        (base32 "04mw7vnhzizzprk83h1k06djrwpv0zxial8s52983i933ap1nxil"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs (list python-setuptools))
    (native-inputs (list python-pytest python-pytest-cov))
    (home-page "http://supervisord.org/")
    (synopsis "A system for controlling process state under UNIX")
    (description
     "This package provides a system for controlling process state under UNIX")
    (license #f)))

(define-public python-takethetime
  (package
    (name "python-takethetime")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "TakeTheTime" version))
       (sha256
        (base32 "1y9gzqb9l1f2smx8783ccjzjvby5mphshgrfks7s75mml59h9qyv"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/ErikBjare/TakeTheTime")
    (synopsis "Take The Time, a time-taking library for Python")
    (description "Take The Time, a time-taking library for Python.")
    (license license:expat)))

(define-public python-lazy-object-proxy-1.4.0
  (package
    (inherit python-lazy-object-proxy)
    (name "python-lazy-object-proxy-1.4.0")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lazy-object-proxy" version))
       (sha256
        (base32 "1znridhk878rpgn92jvyra2gg70bi4l03ciz1i5lwdwsv4rivcbz"))))))

(define-public python-ckcc-protocol
  (package
    (name "python-ckcc-protocol")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ckcc-protocol" version))
       (sha256
        (base32 "1rgcpckyz38nwx8szh8cdgcny9m82d35pgm619ca93ihwg9x94yd"))))
    (build-system python-build-system)
	(inputs (list python-hidapi
	              python-ecdsa
				  python-pyaes
				  python-click))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/Coldcard/ckcc-protocol")
    (synopsis "Python library for Coldcard")
    (description "Python library and command line tool for 
communicating with your Coldcard over USB")
    (license license:expat)))

(define-public px-python-shared
  (package
    (name "px-python-shared")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "18cm0wpvc1hxs8v80v220kjws5wll4c1jl47zbwrw492y2sczakz"))))
    (build-system python-build-system)
    (native-inputs 
     (list python-requests))
    (home-page "https://www.pantherx.org/")
    (synopsis "Stuff that's shared across px-projects.")
    (description "Stuff like well known applications that's shared across px-projects.")
    (license license:expat)))