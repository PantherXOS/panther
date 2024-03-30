(define-module (px packages etesync)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dav)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (guix git-download)
  #:use-module (px packages python-xyz))

(define-public python-etebase
  (package
    (name "python-etebase")
    (version "0.31.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/etesync/etebase-py/releases/download/v"
             version
             "/etebase-"
             version
             ;; This fails because releases are only available up to python 3.9
             ;; "-cp" (string-replace-substring (version-major+minor (package-version python)) "." "")
             ;; "-cp" (string-replace-substring (version-major+minor (package-version python)) "." "") "-manylinux2010_"
             "-cp39-cp39-manylinux2010_"
             (match (or (%current-system)
                        (%current-target-system))
               ("x86_64-linux" "x86_64")
               ("i686-linux" "i686")
               ("aarch64-linux" "aarch64"))
             ".whl"))
       (sha256
        (base32 "19gf7zriarcac3l6hx4zw7gl175fxaz457gkl4kkjdfcq6g5pxas"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:validate-runpath? #f
       #:phases (modify-phases %standard-phases
                  (delete 'unpack)
                  (delete 'configure)
                  (delete 'build)
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((source (assoc-ref %build-inputs "source"))
                             (python (assoc-ref inputs "python"))
                             (out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin/"))
                             (target (string-append %output "/lib/python"
                                                    ,(version-major+minor (package-version
                                                                           python))
                                                    "/site-packages/")))
                        (mkdir-p target)
                        (invoke "unzip" source "-d" target) #t))))))
    (native-inputs `(("coreutils" ,coreutils)
                     ("python" ,python-3)
                     ("unzip" ,unzip)))
    (propagated-inputs `(("python-msgpack" ,python-msgpack)))
    (home-page "https://www.etesync.com/")
    (synopsis "A Python library for Etebase")
    (description
     "This package is implemented in Rust and exposes a Python API for people to use.")
    (license license:bsd-3)))

(define-public python-etesync
  (package
    (name "python-etesync")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/etesync/pyetesync/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32 "18z7fh8mg3h5zdp4zjshg29p495n03i6hh8nghpc9n79a6a4s8k3"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs `(("python" ,python)
                     ("python-appdirs" ,python-appdirs)
                     ("python-asn1crypto" ,python-asn1crypto)
                     ("python-attrs" ,python-attrs)
                     ("python-certifi" ,python-certifi)
                     ("python-cffi" ,python-cffi)
                     ("python-chardet" ,python-chardet)
                     ("python-idna" ,python-idna)
                     ("python-orderedmultidict" ,python-orderedmultidict)
                     ("python-packaging" ,python-packaging)
                     ("python-py" ,python-py)
                     ("python-pyasn1" ,python-pyasn1)
                     ("python-pycparser" ,python-pycparser)
                     ("python-pyparsing" ,python-pyparsing)
                     ("python-dateutil" ,python-dateutil)
                     ("python-pytz" ,python-pytz)
                     ("python-six" ,python-six)
                     ("python-urllib3" ,python-urllib3)))
    (propagated-inputs `(("python-cryptography" ,python-cryptography)
                         ("python-furl" ,python-furl)
                         ("python-peewee" ,python-peewee)
                         ("python-requests" ,python-requests)
                         ("python-vobject" ,python-vobject)))
    (home-page "https://github.com/etesync/pyetesync")
    (synopsis "A python client library for EteSync.")
    (description
     "This module provides a python API to interact with an EteSync server. 
It currently implements AddressBook and Calendar access, and supports two-way sync (both push and pull) to the server.")
    (license license:gpl3)))

(define-public python-speaklater
  (package
    (name "python-speaklater")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "speaklater" version))
       (sha256
        (base32 "1ab5dbfzzgz6cnz4xlwx79gz83id4bhiw67k1cgqrlzfs0va7zjr"))))
    (build-system python-build-system)
    (home-page "http://github.com/mitsuhiko/speaklater")
    (synopsis
     "implements a lazy string for python useful for use with gettext")
    (description
     "implements a lazy string for python useful for use with gettext")
    (license #f)))

;; python-flask-wtf tests require python-flask-babelex
(define-public python-flask-babelex
  (package
    (name "python-flask-babelex")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-BabelEx" version))
       (sha256
        (base32 "09yfr8hlwvpgvq8kp1y7qbnnl0q28hi0348bv199ssiqx779r99r"))))
    (build-system python-build-system)
    (propagated-inputs (list python-babel python-flask python-jinja2
                             python-speaklater))
    (home-page "http://github.com/mrjoes/flask-babelex")
    (synopsis "Adds i18n/l10n support to Flask applications")
    (description "Adds i18n/l10n support to Flask applications")
    (license license:bsd-3)))

;; etesync-dav: Requirement.parse('Radicale==3.0.3'), {'etesync-dav'})
(define-public radicale-3.0.3
  (package
    (name "radicale")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Radicale" version))
       (sha256
        (base32 "08m2lg4z0gr5n8qj54sblld5pqwcqccx1whw2mi74zx4zanrb69x"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ;The tests are not distributed in the PyPi release.
    (native-inputs `(("python-dateutil" ,python-dateutil)
                     ("python-defusedxml" ,python-defusedxml)
                     ("python-passlib" ,python-passlib)
                     ("python-vobject" ,python-vobject)))
    (propagated-inputs
     ;; TODO: Add python-pam
     `(("python-requests" ,python-requests)))
    (synopsis "Basic CalDAV and CardDAV server")
    (description
     "Radicale is a CalDAV and CardDAV server for UNIX-like
platforms.  Calendars and address books are available for both local and remote
access, possibly limited through authentication policies.  They can be viewed
and edited by calendar and contact clients on mobile phones or computers.

Radicale intentionally does not fully comply with the CalDAV and CardDAV RFCs.
Instead, it supports the CalDAV and CardDAV implementations of popular
clients.")
    (home-page "https://radicale.org/")
    (license license:gpl3+)))

(define-public etesync-dav
  (package
    (name "etesync-dav")
    (version "0.32.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32 "086pjji6897y157idlnls87qi2f2bq0787lwqfdd4m97jf1yxqm4"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ;The tests are not distributed in the PyPi release.
    (propagated-inputs `(("python-appdirs" ,python-appdirs)
                         ("python-asn1crypto" ,python-asn1crypto)
                         ("python-certifi" ,python-certifi)
                         ("python-cffi" ,python-cffi)
                         ("python-chardet" ,python-chardet)
                         ("python-click" ,python-click)
                         ("python-cryptography" ,python-cryptography)
                         ("python-defusedxml" ,python-defusedxml)
                         ("python-etebase" ,python-etebase)
                         ("python-etesync" ,python-etesync)
                         ("python-exitstatus" ,python-exitstatus-2.0.1)
                         ("python-flask" ,python-flask)
                         ("python-furl" ,python-furl)
                         ("python-idna" ,python-idna)
                         ("python-itsdangerous" ,python-itsdangerous)
                         ("python-flask-wtf" ,python-flask-wtf-0.15.1)
                         ("python-jinja2" ,python-jinja2)
                         ("python-markupsafe" ,python-markupsafe)
                         ("python-msgpack" ,python-msgpack)
                         ("python-orderedmultidict" ,python-orderedmultidict)
                         ("python-packaging" ,python-packaging)
                         ("python-passlib" ,python-passlib)
                         ("python-peewee" ,python-peewee)
                         ("python-py" ,python-py)
                         ("python-pyasn1" ,python-pyasn1)
                         ("python-pycparser" ,python-pycparser)
                         ("python-pyparsing" ,python-pyparsing)
                         ("python-dateutil" ,python-dateutil)
                         ("python-pysocks" ,python-pysocks)
                         ("python-pytz" ,python-pytz)
                         ("python-requests" ,python-requests)
                         ("python-six" ,python-six)
                         ("python-urllib3" ,python-urllib3)
                         ("python-vobject" ,python-vobject)
                         ("python-werkzeug" ,python-werkzeug)
                         ("python-wtforms" ,python-wtforms)
                         ("radicale" ,radicale-3.0.3)))
    (home-page "https://github.com/etesync/etesync-dav")
    (synopsis "CalDAV and CardDAV adapter for EteSync")
    (description
     "This package provides a local CalDAV and CardDAV server that acts as an EteSync compatibility layer (adapter). 
              It's meant for letting desktop CalDAV and CardDAV clients such as Thunderbird, Outlook and Apple Contacts connect with EteSync.")
    (license license:gpl3)))

