;;; Databases Packages Module for PantherX
;;; Hamzeh Nasajpour (h.nasajpour@pantherx.org)
;;; Reza Alizadeh Majd (r.majd@pantherx.org)

(define-module (px packages databases)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages tls)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build-system scons)
  #:use-module (guix utils)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (nonguix build-system binary)
  #:use-module (px packages common)
  #:use-module (px packages python-xyz)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

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

(define-public wiredtiger-2.9.2
  (package
    (name "wiredtiger")
    (version "2.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://source.wiredtiger.com/releases/wiredtiger-"
             version ".tar.bz2"))
       (sha256
        (base32 "1n8qlhk9fcnan6w38i0lqdwm02bxm91nlzq1976qqmmv989nmmi2"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-lz4" "--with-builtins=snappy,zlib"
                           "--enable-verbose")
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'disable-test/fops
                    (lambda _
                      ;; XXX: timed out after 3600 seconds of silence
                      (substitute* "Makefile"
                        (("test/fops")
                         "")) #t)))))
    (inputs `(("lz4" ,lz4)
              ("zlib" ,zlib)
              ("snappy" ,snappy)))
    (home-page "http://source.wiredtiger.com/")
    (synopsis "NoSQL data engine")
    (description
     "WiredTiger is an extensible platform for data management.  It supports
row-oriented storage (where all columns of a row are stored together),
column-oriented storage (where columns are stored in groups, allowing for
more efficient access and storage of column subsets) and log-structured merge
trees (LSM), for sustained throughput under random insert workloads.")
    (license license:gpl3) ;or GPL-2
    ;; configure.ac: WiredTiger requires a 64-bit build.
    (supported-systems '("x86_64-linux" "mips64el-linux" "aarch64-linux"))))
