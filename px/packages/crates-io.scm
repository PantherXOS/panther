(define-module (px packages crates-io)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-windows)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public rust-iso8601-0.4
  (package
    (name "rust-iso8601")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "iso8601" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15nfg6d4qlniw4gk7039s5y07lzgr1dp9snsw63lsxarnyz4zfg5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-nom" ,rust-nom-7))))
    (home-page "https://github.com/badboy/iso8601")
    (synopsis "Parsing ISO8601 dates using nom")
    (description "Parsing ISO8601 dates using nom")
    (license license:expat)))

(define-public rust-lock-api-0.4.6
  (package
    (inherit rust-lock-api-0.4)
    (name "rust-lock-api")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lock_api" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0frbbqqiwngg33xrc69xagi4rqqk62msllr7z95mlbjaxzbkv548"))))))

(define-public rust-parking-lot-0.12-patched
  (package
    (name "rust-parking-lot")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "parking_lot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13r2xk7mnxfc5g0g6dkdxqdqad99j7s7z8zhzz4npw5r0g0v4hip"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:cargo-inputs `(("rust-instant" ,rust-instant-0.1)
                       ("rust-lock-api" ,rust-lock-api-0.4.6)
                       ("rust-parking-lot-core" ,rust-parking-lot-core-0.8))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "Efficient implementations of the standard synchronization primitives")
    (description
     "This package provides more compact and efficient implementations
of the standard synchronization primitives.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-tokio-1-patched
  (package
    (inherit rust-tokio-1)
    (arguments
     (list
      #:skip-build? #t
      #:cargo-inputs `(("rust-autocfg" ,rust-autocfg-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12-patched)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-tokio-macros" ,rust-tokio-macros-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-xmlrpc-0.15
  (package
    (name "rust-xmlrpc")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xmlrpc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xviwks6c4w408kbi419wa8fn3lfbizz226qffxg04m4hqpcsdlb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-iso8601" ,rust-iso8601-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-xml-rs" ,rust-xml-rs-0.8))
       #:cargo-development-inputs (("rust-version-sync" ,rust-version-sync-0.9))))
    (home-page "https://github.com/jonas-schievink/xml-rpc-rs.git")
    (synopsis "An XML-RPC implementation for Rust")
    (description "An XML-RPC implementation for Rust")
    (license license:cc0)))
