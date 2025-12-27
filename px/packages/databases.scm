;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages databases)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public sqlitecpp
  (package
    (name "sqlitecpp")
    (version "3.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/SRombauts/SQLiteCpp/archive/"
                           version ".tar.gz"))
       (sha256
        (base32 "1inav751a06khmgikd8iyl3phpnhcjz45s4fj8bk3i1vv1r47g9k"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("sqlite" ,sqlite)))
    (home-page "https://github.com/SRombauts/SQLiteCpp")
    (synopsis
     "SQLiteC++ (SQLiteCpp) is a smart and easy to use C++ SQLite3 wrapper.")
    (description
     "SQLiteC++ offers an encapsulation around the native C APIs of
     SQLite, with a few intuitive and well documented C++ classes.")
    (license license:expat)))