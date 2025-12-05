;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

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
