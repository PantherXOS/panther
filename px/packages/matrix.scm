;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px packages matrix)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages matrix)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public synapse
  (package
    (name "synapse")
    (version "1.45.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "matrix-synapse" version))
       (sha256
        (base32 "0dmps93cy4b2g73kxvbnsnci1vxj38m2jcaqyxrb9ind0rsik5zi"))))
    (build-system python-build-system)
    ;; TODO Run tests with ‘PYTHONPATH=. trial3 tests’.
    (propagated-inputs `(("python-simplejson" ,python-simplejson)
                          ;not attested but required
                         ;; requirements (synapse/python_dependencies.py)
                         ("python-jsonschema" ,python-jsonschema)
                         ("python-frozendict" ,python-frozendict)
                         ("python-unpaddedbase64" ,python-unpaddedbase64)
                         ("python-canonicaljson" ,python-canonicaljson)
                         ("python-signedjson" ,python-signedjson)
                         ("python-pynacl" ,python-pynacl)
                         ("python-idna" ,python-idna)
                         ("python-service-identity" ,python-service-identity)
                         ("python-twisted" ,python-twisted)
                         ("python-treq" ,python-treq)
                         ("python-pyopenssl" ,python-pyopenssl)
                         ("python-pyyaml" ,python-pyyaml)
                         ("python-pyasn1" ,python-pyasn1)
                         ("python-pyasn1-modules" ,python-pyasn1-modules)
                         ("python-daemonize" ,python-daemonize)
                         ("python-bcrypt" ,python-bcrypt)
                         ("python-pillow" ,python-pillow)
                         ("python-sortedcontainers" ,python-sortedcontainers)
                         ("python-pymacaroons" ,python-pymacaroons)
                         ("python-msgpack" ,python-msgpack)
                         ("python-phonenumbers" ,python-phonenumbers)
                         ("python-six" ,python-six)
                         ("python-prometheus-client" ,python-prometheus-client)
                         ("python-attrs" ,python-attrs)
                         ("python-netaddr" ,python-netaddr)
                         ("python-jinja2" ,python-jinja2)
                         ("python-bleach" ,python-bleach)
                         ("python-typing-extensions" ,python-typing-extensions)
                         ;; conditional requirements (synapse/python_dependencies.py)
                         ;; ("python-hiredis" ,python-hiredis)
                         ("python-matrix-synapse-ldap3" ,python-matrix-synapse-ldap3)
                         ("python-psycopg2" ,python-psycopg2)
                         ("python-jinja2" ,python-jinja2)
                         ("python-txacme" ,python-txacme)
                         ("python-pysaml2" ,python-pysaml2)
                         ("python-lxml" ,python-lxml)
                         ("python-packaging" ,python-packaging)
                         ;; sentry-sdk, jaeger-client, and opentracing could be included, but
                         ;; all are monitoring aids and not essential.
                         ("python-pyjwt" ,python-pyjwt)))
    (native-inputs `(("python-mock" ,python-mock)
                     ("python-parameterized" ,python-parameterized)))
    (home-page "https://github.com/matrix-org/synapse")
    (synopsis "Matrix reference homeserver")
    (description
     "Synapse is a reference \"homeserver\" implementation of
Matrix from the core development team at matrix.org, written in
Python/Twisted.  It is intended to showcase the concept of Matrix and let
folks see the spec in the context of a codebase and let you run your own
homeserver and generally help bootstrap the ecosystem.")
    (license license:asl2.0)))
