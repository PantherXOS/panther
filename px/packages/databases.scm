(define-module (px packages databases)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public sqlitecpp
  (package
    (name "sqlitecpp")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/SRombauts/SQLiteCpp/archive/"
                           version ".tar.gz"))
       (sha256
        (base32 "14yn8bdnmh5bv2k85acm0yqjzgr90dmqxgvs5bw60x64h1b7vikh"))))
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

(define-public px-database-utility
  (package
    (name "px-database-utility")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "0qqgr8nvgxf3dp1wcaciiv4n4xr5akf3f7k2aiiihfa89h2v7kq1"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("python-psycopg2" ,python-psycopg2)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Database Utility")
    (description
     "Easily dump (backup), restore and list PostgreSQL databases.")
    (license license:expat)))