;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px packages crates-io)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages certs) ;; rust-libdav
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-tls)
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

(define-public rust-oauth2-5
  (package
    (name "rust-oauth2")
    (version "5.0.0-rc.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oauth2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gw5cxhkkf65skz1ggkhfpgh8rn3f7nvww1gdl1jjbb07kd8blr3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-http" ,rust-http-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-reqwest" ,rust-reqwest-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-hex" ,rust-hex-0.4)
                                   ("rust-hmac" ,rust-hmac-0.12)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/ramosbugs/oauth2-rs")
    (synopsis "An extensible, strongly-typed implementation of OAuth2")
    (description
     "This package provides An extensible, strongly-typed implementation of OAuth2.")
    (license (list license:expat license:asl2.0))))

(define-public rust-oauth2-4
  (package
    (name "rust-oauth2")
    (version "4.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oauth2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zwkmwxwygl4fwghgyanixzqgn7yvkwwwacdghz7x124v36l3263"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-hex" ,rust-hex-0.4)
                                   ("rust-hmac" ,rust-hmac-0.12)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-uuid" ,rust-uuid-0.8))))
    (home-page "https://github.com/ramosbugs/oauth2-rs")
    (synopsis "An extensible, strongly-typed implementation of OAuth2")
    (description
     "This package provides An extensible, strongly-typed implementation of OAuth2.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-plain-1
  (package
    (name "rust-serde-plain")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_plain" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l4d4nbw00pz6n43icrc605bhgynfmlyq39sn8i10qasnrnzrqcw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://docs.rs/serde_plain")
    (synopsis "restricted plain text serializer for serde")
    (description
     "This package provides a restricted plain text serializer for serde.")
    (license (list license:expat license:asl2.0))))

(define-public rust-retry-1
  (package
    (name "rust-retry")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "retry" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xm3p41ygijbjpyj81psqhb2r3rdcqwlk5pl48lgsqwsjh5cd5dc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/jimmycuadra/retry")
    (synopsis "Utilities for retrying operations that can fail")
    (description
     "This package provides Utilities for retrying operations that can fail.")
    (license license:expat)))

(define-public rust-openidconnect-4
  (package
    (name "rust-openidconnect")
    (version "4.0.0-rc.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openidconnect" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11gas2z9lvswdm4qiv3rsmra0v3kjzpws15ipy39jr0bkmw50fm9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-dyn-clone" ,rust-dyn-clone-1)
                       ("rust-ed25519-dalek" ,rust-ed25519-dalek-2)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-http" ,rust-http-1)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-oauth2" ,rust-oauth2-5)
                       ("rust-p256" ,rust-p256-0.13)
                       ("rust-p384" ,rust-p384-0.13)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rsa" ,rust-rsa-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-value" ,rust-serde-value-0.7)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-serde-plain" ,rust-serde-plain-1)
                       ("rust-serde-with" ,rust-serde-with-3)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-color-backtrace" ,rust-color-backtrace-0.5)
                                   ("rust-env-logger" ,rust-env-logger-0.9)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-reqwest" ,rust-reqwest-0.12)
                                   ("rust-retry" ,rust-retry-1))))
    (home-page "https://github.com/ramosbugs/openidconnect-rs")
    (synopsis "OpenID Connect library")
    (description "This package provides @code{OpenID} Connect library.")
    (license license:expat)))

(define-public rust-openidconnect-3
  (package
    (name "rust-openidconnect")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openidconnect" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "140mv5kfc4ca9g0znlsmavb73292vmz9h7n457fn4i5frylq0zpl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-dyn-clone" ,rust-dyn-clone-1)
                       ("rust-ed25519-dalek" ,rust-ed25519-dalek-2)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-http" ,rust-http-1)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-oauth2" ,rust-oauth2-4)
                       ("rust-p256" ,rust-p256-0.13)
                       ("rust-p384" ,rust-p384-0.13)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rsa" ,rust-rsa-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-value" ,rust-serde-value-0.7)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
                       ("rust-serde-plain" ,rust-serde-plain-1)
                       ("rust-serde-with" ,rust-serde-with-3)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-color-backtrace" ,rust-color-backtrace-0.5)
                                   ("rust-env-logger" ,rust-env-logger-0.9)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-reqwest" ,rust-reqwest-0.12)
                                   ("rust-retry" ,rust-retry-1))))
    (home-page "https://github.com/ramosbugs/openidconnect-rs")
    (synopsis "OpenID Connect library")
    (description "This package provides @code{OpenID} Connect library.")
    (license license:expat)))

(define-public rust-libyml-0.0.5
  (package
    (name "rust-libyml")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libyml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "106963pwg1gc3165bdlk8bbspmk919gk10vshhqglks3z8m700ik"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-version-check" ,rust-version-check-0.9))
       #:cargo-development-inputs (("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "https://libyml.com")
    (synopsis
     "safe and efficient Rust library for parsing, emitting, and manipulating YAML data.")
    (description
     "This package provides a safe and efficient Rust library for parsing, emitting,
and manipulating YAML data.")
    (license license:expat)))

(define-public rust-serde-yml-0.0.12
  (package
    (name "rust-serde-yml")
    (version "0.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_yml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p8xwz4znd6fj962y22fdvvv16gb8c0hx4iv5hjplngiidcdvqjr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libyml" ,rust-libyml-0.0.5)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-version-check" ,rust-version-check-0.9))
       #:cargo-development-inputs (("rust-indoc" ,rust-indoc-2)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serdeyml.com")
    (synopsis
     "robust Rust library that simplifies the serialization and deserialization of Rust data structures to and from YAML format using the widely-used Serde framework.")
    (description
     "This package provides a robust Rust library that simplifies the serialization
and deserialization of Rust data structures to and from YAML format using the
widely-used Serde framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hyperlocal-0.9
  (package
    (name "rust-hyperlocal")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyperlocal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1iy8rhsap5iyigj6s86nk449zl5bahjycy2mswy6nlllp7imqv4q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-hex" ,rust-hex-0.4)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tower-service" ,rust-tower-service-0.3))
       #:cargo-development-inputs (("rust-thiserror" ,rust-thiserror-1)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/softprops/hyperlocal")
    (synopsis "Hyper bindings for Unix domain sockets")
    (description
     "This package provides Hyper bindings for Unix domain sockets.")
    (license license:expat)))

(define-public rust-prost-types-0.11
  (package
    (name "rust-prost-types")
    (version "0.11.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prost-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04ryk38sqkp2nf4dgdqdfbgn6zwwvjraw6hqq6d9a6088shj4di1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-prost" ,rust-prost-0.11))
       #:cargo-development-inputs (("rust-proptest" ,rust-proptest-1))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "Prost definitions of Protocol Buffers well known types")
    (description
     "This package provides Prost definitions of Protocol Buffers well known types.")
    (license license:asl2.0)))

(define-public rust-crossbeam-utils-0.8.16
  (package
    (name "rust-crossbeam-utils")
    (version "0.8.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "153j0gikblz7n7qdvdi8pslhi008s1yp9cmny6vw07ad7pbb48js"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-loom" ,rust-loom-0.5))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8)
                                   ("rust-rustversion" ,rust-rustversion-1))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description "This package provides Utilities for concurrent programming.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rayon-1.6
  (package
    (name "rust-rayon")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rayon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dsr0yyfgdsg8ggh37kq678mfa5j3js6p16ksb7knazhml9s5cvd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-either" ,rust-either-1)
                       ("rust-rayon-core" ,rust-rayon-core-1))
       #:cargo-development-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rand-xorshift" ,rust-rand-xorshift-0.3))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Simple work-stealing parallelism for Rust")
    (description
     "This package provides Simple work-stealing parallelism for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-channel-0.5.8
  (package
    (name "rust-crossbeam-channel")
    (version "0.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-channel" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "004jz4wxp9k26z657i7rsh9s7586dklx2c5aqf1n3w1dgzvjng53"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--release" "--" "--skip=select2::main")
       #:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8.16))
       #:cargo-development-inputs (("rust-num-cpus" ,rust-num-cpus-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-signal-hook" ,rust-signal-hook-0.3))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-channel")
    (synopsis "Multi-producer multi-consumer channels for message passing")
    (description
     "This package provides multi-producer multi-consumer channels for
message passing.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-epoch-0.9.15
  (package
    (name "rust-crossbeam-epoch")
    (version "0.9.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-epoch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ixwc3cq816wb8rlh3ix4jnybqbyyq4l61nwlx0mfm3ck0s148df"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs
       (("rust-memoffset" ,rust-memoffset-0.9)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
        ("rust-loom" ,rust-loom-0.7))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-epoch")
    (synopsis "Epoch-based garbage collection")
    (description "This package provides an Epoch-based garbage collection.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-deque-0.8.3
  (package
    (name "rust-crossbeam-deque")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crossbeam-deque" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vqczbcild7nczh5z116w8w46z991kpjyw7qxkf24c14apwdcvyf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9.15)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8.16))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
    (synopsis "Concurrent work-stealing deque")
    (description "This package provides a concurrent work-stealing deque.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rayon-core-1.10
  (package
    (name "rust-rayon-core")
    (version "1.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rayon-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nw3ds7agdc9a3swyjhzw9ndr60ic54apk8108676kwmy4jhcsim"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5.8)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8.3)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8.16)
                       ("rust-num-cpus" ,rust-num-cpus-1))
       #:cargo-development-inputs (("rust-libc" ,rust-libc-0.2)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
                                   ("rust-scoped-tls" ,rust-scoped-tls-1))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Core APIs for Rayon")
    (description "This package provides Core APIs for Rayon.")
    (license (list license:expat license:asl2.0))))

(define-public rust-byteorder-1.4
  (package
    (name "rust-byteorder")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "byteorder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0456lv9xi1a5bcm32arknf33ikv76p3fr9yzki4lb2897p2qkh8l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-0.9)
                                   ("rust-rand" ,rust-rand-0.7))))
    (home-page "https://github.com/BurntSushi/byteorder")
    (synopsis
     "Library for reading/writing numbers in big-endian and little-endian")
    (description
     "This package provides Library for reading/writing numbers in big-endian and little-endian.")
    (license (list license:unlicense license:expat))))

(define-public rust-proptest-1.0
  (package
    (name "rust-proptest")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proptest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rdhjnf0xma5rmsq04d31n2vq1pgbm42pjc6jn3jsj8qgz09q38y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f 
       #:cargo-inputs (("rust-bit-set" ,rust-bit-set-0.5)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-quick-error" ,rust-quick-error-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rand-chacha" ,rust-rand-chacha-0.3)
                       ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.6)
                       ("rust-rusty-fork" ,rust-rusty-fork-0.3)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-x86" ,rust-x86-0.33))
       #:cargo-development-inputs (("rust-regex" ,rust-regex-1))))
    (home-page "https://proptest-rs.github.io/proptest/proptest/index.html")
    (synopsis "Hypothesis-like property-based testing and shrinking.")
    (description
     "This package provides Hypothesis-like property-based testing and shrinking.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tempfile-3.6
  (package
    (name "rust-tempfile")
    (version "3.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mm6n3ijfsnk7grbbws3fc9qy4y5n3pshixa19wmhzimfqj47h1i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-fastrand" ,rust-fastrand-1)
                       ("rust-redox-syscall" ,rust-redox-syscall-0.3)
                       ("rust-rustix" ,rust-rustix-0.37)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "library for managing temporary files and directories.")
    (description
     "This package provides a library for managing temporary files and directories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-average-0.14
  (package
    (name "rust-average")
    (version "0.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "average" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "002sav5a1n5ligv4sl6gii2wp95k2ylrx8nb7vnbwbm1zk3v22f3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8.16)
                       ("rust-easy-cast" ,rust-easy-cast-0.5)
                       ("rust-float-ord" ,rust-float-ord-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rayon" ,rust-rayon-1.6)
                       ("rust-rayon-core" ,rust-rayon-core-1.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-big-array" ,rust-serde-big-array-0.5)
                       ("rust-serde-derive" ,rust-serde-derive-1))
       #:cargo-development-inputs (("rust-bencher" ,rust-bencher-0.1)
                                   ("rust-byteorder" ,rust-byteorder-1.4)
                                   ("rust-proptest" ,rust-proptest-1.0)
                                   ("rust-quantiles" ,rust-quantiles-0.7)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rand-distr" ,rust-rand-distr-0.4)
                                   ("rust-rand-xoshiro" ,rust-rand-xoshiro-0.6)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-streaming-stats" ,rust-streaming-stats-0.2)
                                   ("rust-tempfile" ,rust-tempfile-3.6))))
    (home-page "https://github.com/vks/average")
    (synopsis "Calculate statistics iteratively")
    (description "This package provides Calculate statistics iteratively.")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-foundation-sys-0.7
  (package
    (name "rust-core-foundation-sys")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b5qfnnmg49sawwfsb0c0wbj81bqi7h7lh68pmhbidf0jjs1m9xk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description
     "This package provides Bindings to Core Foundation for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-foundation-0.7
  (package
    (name "rust-core-foundation")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wbias8f0m5kyn2pcksi0h58fdslams6nmf16w78fgn42dx4rljp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.7)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-uuid" ,rust-uuid-0.5))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description
     "This package provides Bindings to Core Foundation for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-text-15
  (package
    (name "rust-core-text")
    (version "15.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-text" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07d6lbxrhzicmh3c6iyl0ky4mih8c3dlzylqngrbjpdxz38ky6qk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-core-foundation" ,rust-core-foundation-0.7)
                       ("rust-core-graphics" ,rust-core-graphics-0.19)
                       ("rust-foreign-types" ,rust-foreign-types-0.3)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to the Core Text framework")
    (description "This package provides Bindings to the Core Text framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-servo-freetype-sys-4
  (package
    (name "rust-servo-freetype-sys")
    (version "4.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "servo-freetype-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z0dvnakans4vn4vlpx4nxg984427lh8dskxxz9pglij1mnwnk1c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cmake" ,rust-cmake-0.1)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "http://www.freetype.org/")
    (synopsis
     "FreeType is a freely available software library to render fonts")
    (description
     "This package provides @code{FreeType} is a freely available software library to render fonts.")
    (license (list license:freetype license:gpl2))))

(define-public rust-freetype-0.4
  (package
    (name "rust-freetype")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "freetype" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a70x03n68997f08bi3n47q9wyi3pv5s9v4rjc79sihb84mnp4hi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-servo-freetype-sys" ,rust-servo-freetype-sys-4))))
    (home-page "https://github.com/servo/rust-freetype")
    (synopsis "Bindings for Freetype used by Servo")
    (description "This package provides Bindings for Freetype used by Servo.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-servo-fontconfig-sys-4
  (package
    (name "rust-servo-fontconfig-sys")
    (version "4.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "servo-fontconfig-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v0mbicy74wd6cjd5jyqnm4nvrrr5lmg053cn16kylhg8mkf3cv2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-expat-sys" ,rust-expat-sys-2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-servo-freetype-sys" ,rust-servo-freetype-sys-4))))
    (home-page "http://fontconfig.org")
    (synopsis "Font configuration and customization library")
    (description
     "This package provides Font configuration and customization library.")
    (license license:expat)))

(define-public rust-servo-fontconfig-0.4
  (package
    (name "rust-servo-fontconfig")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "servo-fontconfig" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nach6s4hdf86jz5hlm4p5r7vin91cs7gg89mr533id5fpbzi250"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-servo-fontconfig-sys" ,rust-servo-fontconfig-sys-4))))
    (home-page "https://github.com/servo/rust-fontconfig/")
    (synopsis "Rust bindings for fontconfig")
    (description "This package provides Rust bindings for fontconfig.")
    (license (list license:expat license:asl2.0))))

(define-public rust-font-kit-0.7
  (package
    (name "rust-font-kit")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "font-kit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kxas4dzv5n5p881yx20c6liwf69rnhsyfmjmi4mqm1bg7f2ravn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-core-foundation" ,rust-core-foundation-0.7)
                       ("rust-core-graphics" ,rust-core-graphics-0.19)
                       ("rust-core-text" ,rust-core-text-15)
                       ("rust-dirs" ,rust-dirs-2)
                       ("rust-dwrote" ,rust-dwrote-0.11)
                       ("rust-float-ord" ,rust-float-ord-0.2)
                       ("rust-freetype" ,rust-freetype-0.4)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pathfinder-geometry" ,rust-pathfinder-geometry-0.5)
                       ("rust-pathfinder-simd" ,rust-pathfinder-simd-0.5)
                       ("rust-servo-fontconfig" ,rust-servo-fontconfig-0.4)
                       ("rust-servo-fontconfig" ,rust-servo-fontconfig-0.5)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-2)
                                   ("rust-colored" ,rust-colored-1)
                                   ("rust-pbr" ,rust-pbr-1)
                                   ("rust-prettytable-rs" ,rust-prettytable-rs-0.8))))
    (home-page "https://github.com/servo/font-kit")
    (synopsis "cross-platform font loading library")
    (description
     "This package provides a cross-platform font loading library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-palette-derive-0.5
  (package
    (name "rust-palette-derive")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "palette_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x5icddb877923rpl27bg4cjsf1x0d3layxmgwa3mpb01rh5yjqb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/Ogeon/palette")
    (synopsis "Automatically implement traits from the palette crate")
    (description
     "This package provides Automatically implement traits from the palette crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-palette-0.5
  (package
    (name "rust-palette")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "palette" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nfc4ycdsx2qgf2wkcpxqxc0vmx7188jjjx3ppgs8qlf8qs06p50"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-approx" ,rust-approx-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-palette-derive" ,rust-palette-derive-0.5)
                       ("rust-phf" ,rust-phf-0.8)
                       ("rust-phf-codegen" ,rust-phf-codegen-0.8)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-2)
                                   ("rust-csv" ,rust-csv-1)
                                   ("rust-image" ,rust-image-0.22)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/Ogeon/palette")
    (synopsis
     "Convert and manage colors with a focus on correctness, flexibility and ease of use")
    (description
     "This package provides Convert and manage colors with a focus on correctness, flexibility and ease of
use.")
    (license (list license:expat license:asl2.0))))

(define-public rust-piston-0.49
  (package
    (name "rust-piston")
    (version "0.49.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "piston" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y0rbw92mzagqmwk79wv9axq0m7aid0s0d5cppyzh33wrxhdl3xj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-pistoncore-event-loop" ,rust-pistoncore-event-loop-0.49)
                       ("rust-pistoncore-input" ,rust-pistoncore-input-0.28)
                       ("rust-pistoncore-window" ,rust-pistoncore-window-0.44))))
    (home-page "https://github.com/PistonDevelopers/piston")
    (synopsis "The Piston game engine core libraries")
    (description
     "This package provides The Piston game engine core libraries.")
    (license license:expat)))

(define-public rust-interpolation-0.2
  (package
    (name "rust-interpolation")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "interpolation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00icvvgc72zdgyrwwg2p0wad4hry4d2vd6l9iqpyjpmw5dykbdyk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/pistondevelopers/interpolation")
    (synopsis "library for interpolation")
    (description "This package provides a library for interpolation.")
    (license license:expat)))

(define-public rust-read-color-1
  (package
    (name "rust-read-color")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "read_color" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1np0pk31ak7hni4hri3m75mbf8py1wdfjshmrj5krbd4p9c8hk4z"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/pistondevelopers/read_color")
    (synopsis "simple library for reading hex colors")
    (description
     "This package provides a simple library for reading hex colors.")
    (license (list license:expat license:asl2.0))))

(define-public rust-vecmath-1
  (package
    (name "rust-vecmath")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vecmath" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0shmj76rj7rqv377vy365xwr5rx23kxqgkqxxrymdjjvv3hf2slm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-piston-float" ,rust-piston-float-1))))
    (home-page "https://github.com/pistondevelopers/vecmath")
    (synopsis
     "simple and type agnostic library for vector math designed for reexporting")
    (description
     "This package provides a simple and type agnostic library for vector math
designed for reexporting.")
    (license license:expat)))

(define-public rust-piston2d-graphics-0.36
  (package
    (name "rust-piston2d-graphics")
    (version "0.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "piston2d-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wqzrs8qqksr8z3d3jizj1r57mxc4xmdagfasdrh8pnzyqv7qz0v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fnv" ,rust-fnv-1)
                       ("rust-interpolation" ,rust-interpolation-0.2)
                       ("rust-piston-texture" ,rust-piston-texture-0.8)
                       ("rust-piston-viewport" ,rust-piston-viewport-1)
                       ("rust-read-color" ,rust-read-color-1)
                       ("rust-rusttype" ,rust-rusttype-0.8)
                       ("rust-vecmath" ,rust-vecmath-1))))
    (home-page "https://github.com/pistondevelopers/graphics")
    (synopsis "library for 2D graphics that works with multiple back-ends")
    (description
     "This package provides a library for 2D graphics that works with multiple
back-ends.")
    (license license:expat)))

(define-public rust-gl-generator-0.10
  (package
    (name "rust-gl-generator")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gl_generator" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0146yd4i9wbgfrhnkc04w7n7civbanznc0q87skp6v7p7hbszzx0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-khronos-api" ,rust-khronos-api-3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-xml-rs" ,rust-xml-rs-0.8))))
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis
     "Code generators for creating bindings to the Khronos OpenGL APIs")
    (description
     "This package provides Code generators for creating bindings to the Khronos @code{OpenGL} APIs.")
    (license license:asl2.0)))

(define-public rust-wayland-sys-0.20
  (package
    (name "rust-wayland-sys")
    (version "0.20.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xr2g7k766glpilasnzwzqf943838bj247q6szgpyrdab3k2xj47"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dlib" ,rust-dlib-0.4)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "FFI bindings to the various libwayland-*.so libraries. You should only need this crate if you are working on custom wayland protocol extensions. Look at the crate wayland-client for usable bindings")
    (description
     "This package provides FFI bindings to the various libwayland-*.so libraries.  You should only need
this crate if you are working on custom wayland protocol extensions.  Look at
the crate wayland-client for usable bindings.")
    (license license:expat)))

(define-public rust-wayland-commons-0.20
  (package
    (name "rust-wayland-commons")
    (version "0.20.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-commons" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ynl274rqvhzrjcaqspckr92sbvibban9c7kwjx9iwavp5crsq6q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-downcast-rs" ,rust-downcast-rs-1)
                       ("rust-wayland-sys" ,rust-wayland-sys-0.20))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Common types and structures used by wayland-client and wayland-server")
    (description
     "This package provides Common types and structures used by wayland-client and wayland-server.")
    (license license:expat)))

(define-public rust-wayland-scanner-0.20
  (package
    (name "rust-wayland-scanner")
    (version "0.20.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b85ip7gwfi1aby32fnfhzzjz85740p8yk9pj32vnz66x5ddhx76"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-xml-rs" ,rust-xml-rs-0.7))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Wayland Scanner for generating rust APIs from XML wayland protocol files")
    (description
     "This package provides Wayland Scanner for generating rust APIs from XML wayland protocol files.")
    (license license:expat)))

(define-public rust-tempfile-2
  (package
    (name "rust-tempfile")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q61byf232rra0vqxp4qp10wwwqsqqd45qjj80ql5f34vgljzkhi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-rand" ,rust-rand-0.3)
                       ("rust-redox-syscall" ,rust-redox-syscall-0.1)
                       ("rust-winapi" ,rust-winapi-0.2))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "library for managing temporary files and directories.")
    (description
     "This package provides a library for managing temporary files and directories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wayland-client-0.20
  (package
    (name "rust-wayland-client")
    (version "0.20.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04jbg7p8naww501hcrhka277319clnk7av26dlpbsmcs84inllg7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-wayland-commons" ,rust-wayland-commons-0.20)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.20)
                       ("rust-wayland-sys" ,rust-wayland-sys-0.20))
       #:cargo-development-inputs (("rust-byteorder" ,rust-byteorder-1)
                                   ("rust-tempfile" ,rust-tempfile-2))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Bindings to the standard C implementation of the wayland protocol, client side")
    (description
     "This package provides Bindings to the standard C implementation of the wayland protocol, client side.")
    (license license:expat)))

(define-public rust-core-graphics-0.16
  (package
    (name "rust-core-graphics")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0md0pdaxbnikz3rw5f91qdghi60hw1r2m17d37k1lc56is81r04j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-core-foundation" ,rust-core-foundation-0.6)
                       ("rust-foreign-types" ,rust-foreign-types-0.3)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Graphics for macOS")
    (description
     "This package provides Bindings to Core Graphics for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cocoa-0.17
  (package
    (name "rust-cocoa")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08zz8qbv1r4s01v4hh2qp4rx8ydprjyb5vfzwm0rrpmjhgximkgm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-block" ,rust-block-0.1)
                       ("rust-core-graphics" ,rust-core-graphics-0.16)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Cocoa for macOS")
    (description "This package provides Bindings to Cocoa for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-image-0.19
  (package
    (name "rust-image")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sksvf3i9cnl1bbvhkf080xl7bw65dijmbg8pn4h1qq4my8zgpzb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-gif" ,rust-gif-0.10)
                       ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.1)
                       ("rust-lzw" ,rust-lzw-0.10)
                       ("rust-num-derive" ,rust-num-derive-0.2)
                       ("rust-num-iter" ,rust-num-iter-0.1)
                       ("rust-num-rational" ,rust-num-rational-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-png" ,rust-png-0.12)
                       ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1))
       #:cargo-development-inputs (("rust-glob" ,rust-glob-0.2)
                                   ("rust-num-complex" ,rust-num-complex-0.1)
                                   ("rust-quickcheck" ,rust-quickcheck-0.6))))
    (home-page "https://github.com/image-rs/image")
    (synopsis
     "Imaging library. Provides basic image processing and encoders/decoders for common image formats")
    (description
     "This package provides Imaging library.  Provides basic image processing and encoders/decoders for
common image formats.")
    (license license:expat)))

(define-public rust-parking-lot-core-0.3
  (package
    (name "rust-parking-lot-core")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "parking_lot_core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "130by1bszx1iaav33sz3i6lzfx01c9dsb1ybzpvdz7n7pmp7wzxd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-petgraph" ,rust-petgraph-0.4)
                       ("rust-rand" ,rust-rand-0.5)
                       ("rust-rustc-version" ,rust-rustc-version-0.2)
                       ("rust-smallvec" ,rust-smallvec-0.6)
                       ("rust-thread-id" ,rust-thread-id-3)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis "An advanced API for creating custom synchronization primitives")
    (description
     "This package provides An advanced API for creating custom synchronization primitives.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-parking-lot-0.6
  (package
    (name "rust-parking-lot")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "parking_lot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rfd4h24kvvxhjzaz53ycqqwql9y65wpxp2nlwdjjfq017zjp07h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lock-api" ,rust-lock-api-0.1)
                       ("rust-parking-lot-core" ,rust-parking-lot-core-0.3))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.5))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "More compact and efficient implementations of the standard synchronization primitives")
    (description
     "This package provides More compact and efficient implementations of the standard synchronization
primitives.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-nix-0.11
  (package
    (name "rust-nix")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p06wvrg172gb4z59nvsnab9xkxqqq74ibf773px471gcrynbjxy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-void" ,rust-void-1))
       #:cargo-development-inputs (("rust-bytes" ,rust-bytes-0.4)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-rand" ,rust-rand-0.4)
                                   ("rust-sysctl" ,rust-sysctl-0.1)
                                   ("rust-tempdir" ,rust-tempdir-0.3)
                                   ("rust-tempfile" ,rust-tempfile-2))))
    (home-page "https://github.com/nix-rust/nix")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description "This package provides Rust friendly bindings to *nix APIs.")
    (license license:expat)))

(define-public rust-nix-0.10
  (package
    (name "rust-nix")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17w38rr50nrnijwhxma4ln4rl9si41glxgfgc9j69nizs60mdzdp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-0.4)
                       ("rust-cfg-if" ,rust-cfg-if-0.1)
                       ("rust-gcc" ,rust-gcc-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-void" ,rust-void-1))
       #:cargo-development-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-rand" ,rust-rand-0.4)
                                   ("rust-tempdir" ,rust-tempdir-0.3)
                                   ("rust-tempfile" ,rust-tempfile-2))))
    (home-page "https://github.com/nix-rust/nix")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description "This package provides Rust friendly bindings to *nix APIs.")
    (license license:expat)))

(define-public rust-wayland-server-0.20
  (package
    (name "rust-wayland-server")
    (version "0.20.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mhkcqrlrl6s6vmg87zkin0hhn8rhbsin7xmdc26nznzi5gddb05"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.10)
                       ("rust-wayland-commons" ,rust-wayland-commons-0.20)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.20)
                       ("rust-wayland-sys" ,rust-wayland-sys-0.20))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Bindings to the standard C implementation of the wayland protocol, server side")
    (description
     "This package provides Bindings to the standard C implementation of the wayland protocol, server side.")
    (license license:expat)))

(define-public rust-wayland-protocols-0.20
  (package
    (name "rust-wayland-protocols")
    (version "0.20.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c4jgr0ksi5gzvcfrbwjwldy1bidmzsh0lpkvfnxpv76dflk2kdx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-wayland-client" ,rust-wayland-client-0.20)
                       ("rust-wayland-commons" ,rust-wayland-commons-0.20)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.20)
                       ("rust-wayland-server" ,rust-wayland-server-0.20)
                       ("rust-wayland-sys" ,rust-wayland-sys-0.20))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Generated API for the officials wayland protocol extensions")
    (description
     "This package provides Generated API for the officials wayland protocol extensions.")
    (license license:expat)))

(define-public rust-smithay-client-toolkit-0.3
  (package
    (name "rust-smithay-client-toolkit")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-client-toolkit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01fnff8185f9w6z22cqwi1q4sxp12sp813b47hd9k8xwss1r0q7i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-dlib" ,rust-dlib-0.4)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-memmap" ,rust-memmap-0.6)
                       ("rust-nix" ,rust-nix-0.11)
                       ("rust-rand" ,rust-rand-0.5)
                       ("rust-wayland-client" ,rust-wayland-client-0.20)
                       ("rust-wayland-commons" ,rust-wayland-commons-0.20)
                       ("rust-wayland-protocols" ,rust-wayland-protocols-0.20))
       #:cargo-development-inputs (("rust-byteorder" ,rust-byteorder-1)
                                   ("rust-image" ,rust-image-0.19)
                                   ("rust-wayland-client" ,rust-wayland-client-0.20))))
    (home-page "https://github.com/smithay/client-toolkit")
    (synopsis "Toolkit for making client wayland applications")
    (description
     "This package provides Toolkit for making client wayland applications.")
    (license license:expat)))

(define-public rust-winit-0.17
  (package
    (name "rust-winit")
    (version "0.17.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yj3840jklw58vdzifrca6h68ix5zpbbajl9h73ss6wqdcqcyi5s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-android-glue" ,rust-android-glue-0.2)
                       ("rust-cocoa" ,rust-cocoa-0.17)
                       ("rust-core-foundation" ,rust-core-foundation-0.6)
                       ("rust-core-graphics" ,rust-core-graphics-0.17)
                       ("rust-image" ,rust-image-0.19)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-parking-lot" ,rust-parking-lot-0.6)
                       ("rust-percent-encoding" ,rust-percent-encoding-1)
                       ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.3)
                       ("rust-wayland-client" ,rust-wayland-client-0.20)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-x11-dl" ,rust-x11-dl-2))))
    (home-page "https://github.com/rust-windowing/winit")
    (synopsis "Cross-platform window creation library")
    (description
     "This package provides Cross-platform window creation library.")
    (license license:asl2.0)))

(define-public rust-khronos-api-2
  (package
    (name "rust-khronos-api")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "khronos_api" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m5mpi5zyzzbsjkfymfkzib577ii8lk3l5p9sgxvarrzqdrb8yh3"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis "The Khronos XML API Registry, exposed as byte string constants")
    (description
     "This package provides The Khronos XML API Registry, exposed as byte string constants.")
    (license license:asl2.0)))

(define-public rust-gl-generator-0.9
  (package
    (name "rust-gl-generator")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gl_generator" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02lx6zfvpszp43161645hvj06smfbi9dgmjqm9xmlnyqrdq52ybs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-khronos-api" ,rust-khronos-api-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-xml-rs" ,rust-xml-rs-0.7))))
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis
     "Code generators for creating bindings to the Khronos OpenGL APIs")
    (description
     "This package provides Code generators for creating bindings to the Khronos @code{OpenGL} APIs.")
    (license license:asl2.0)))

(define-public rust-glutin-0.18
  (package
    (name "rust-glutin")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zpaqq9zi8r7ldp7df5vp00hi75xwmwfg6rjn7jcvk0x5j2lps0b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-android-glue" ,rust-android-glue-0.2)
                       ("rust-cgl" ,rust-cgl-0.2)
                       ("rust-cocoa" ,rust-cocoa-0.18)
                       ("rust-core-foundation" ,rust-core-foundation-0.6)
                       ("rust-core-graphics" ,rust-core-graphics-0.17)
                       ("rust-gl-generator" ,rust-gl-generator-0.9)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-osmesa-sys" ,rust-osmesa-sys-0.1)
                       ("rust-shared-library" ,rust-shared-library-0.1)
                       ("rust-wayland-client" ,rust-wayland-client-0.21)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-winit" ,rust-winit-0.17)
                       ("rust-x11-dl" ,rust-x11-dl-2))))
    (home-page "https://github.com/rust-windowing/glutin")
    (synopsis "Cross-platform OpenGL context provider")
    (description
     "This package provides Cross-platform @code{OpenGL} context provider.")
    (license license:asl2.0)))

(define-public rust-gl-0.11
  (package
    (name "rust-gl")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wcqpyhck0xriffkmgmldy33lwk2044hb4l02d44vm4fbvicin6p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gl-generator" ,rust-gl-generator-0.10))
       #:cargo-development-inputs (("rust-glutin" ,rust-glutin-0.18))))
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis "OpenGL bindings")
    (description "This package provides @code{OpenGL} bindings.")
    (license license:asl2.0)))

(define-public rust-pistoncore-glutin-window-0.63
  (package
    (name "rust-pistoncore-glutin-window")
    (version "0.63.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pistoncore-glutin_window" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dhbyxarv5i742d400bmqdqq3f8c25kcgcg0xavrc18dc913rixc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gl" ,rust-gl-0.11)
                       ("rust-glutin" ,rust-glutin-0.21)
                       ("rust-pistoncore-input" ,rust-pistoncore-input-0.28)
                       ("rust-pistoncore-window" ,rust-pistoncore-window-0.44)
                       ("rust-shader-version" ,rust-shader-version-0.6))))
    (home-page "https://github.com/pistondevelopers/glutin_window")
    (synopsis "Piston window back-end using the Glutin library")
    (description
     "This package provides a Piston window back-end using the Glutin library.")
    (license license:expat)))

(define-public rust-shader-version-0.6
  (package
    (name "rust-shader-version")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "shader_version" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yk651xc9irl3pl0rlplypzyzy44d0j03ji0j7hjjdjknwzpi3j7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-piston-graphics-api-version" ,rust-piston-graphics-api-version-0.2))))
    (home-page "https://github.com/pistondevelopers/shader_version")
    (synopsis "helper library for detecting and picking compatible shaders")
    (description
     "This package provides a helper library for detecting and picking compatible
shaders.")
    (license license:expat)))

(define-public rust-piston-gfx-texture-0.41
  (package
    (name "rust-piston-gfx-texture")
    (version "0.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "piston-gfx_texture" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z699bgwyv5llkb604ajm50fqrxnlayaakrgghcp95qgcabi33fy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gfx" ,rust-gfx-0.18)
                       ("rust-gfx-core" ,rust-gfx-core-0.9)
                       ("rust-image" ,rust-image-0.23)
                       ("rust-piston-texture" ,rust-piston-texture-0.8))))
    (home-page "https://github.com/pistondevelopers/gfx_texture")
    (synopsis
     "Gfx texture representation that works nicely with Piston libraries")
    (description
     "This package provides a Gfx texture representation that works nicely with Piston
libraries.")
    (license license:expat)))

(define-public rust-find-folder-0.3
  (package
    (name "rust-find-folder")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "find_folder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06xr2sjiyjrwxlgjpf8ig6pq0ayfjv6qxmmfakw5j2ssp67h2vcz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/PistonDevelopers/find_folder")
    (synopsis
     "simple tool for finding the absolute path to a folder with a given name.")
    (description
     "This package provides a simple tool for finding the absolute path to a folder
with a given name.")
    (license license:expat)))

(define-public rust-piston2d-gfx-graphics-0.68
  (package
    (name "rust-piston2d-gfx-graphics")
    (version "0.68.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "piston2d-gfx_graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14kh0dwx5hl3035bx5v371jicwdld3bplrpn0xasrlivk88l5fyj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-draw-state" ,rust-draw-state-0.8)
                       ("rust-gfx" ,rust-gfx-0.18)
                       ("rust-piston-gfx-texture" ,rust-piston-gfx-texture-0.41)
                       ("rust-piston-shaders-graphics2d" ,rust-piston-shaders-graphics2d-0.3)
                       ("rust-piston2d-graphics" ,rust-piston2d-graphics-0.36)
                       ("rust-shader-version" ,rust-shader-version-0.6))
       #:cargo-development-inputs (("rust-find-folder" ,rust-find-folder-0.3)
                                   ("rust-gfx-device-gl" ,rust-gfx-device-gl-0.16)
                                   ("rust-image" ,rust-image-0.23)
                                   ("rust-piston" ,rust-piston-0.49)
                                   ("rust-pistoncore-glutin-window" ,rust-pistoncore-glutin-window-0.63))))
    (home-page "https://github.com/PistonDevelopers/gfx_graphics")
    (synopsis "Gfx 2D back-end for the Piston game engine")
    (description
     "This package provides a Gfx 2D back-end for the Piston game engine.")
    (license license:expat)))

(define-public rust-piston-window-0.108
  (package
    (name "rust-piston-window")
    (version "0.108.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "piston_window" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w0k1dwx9ncyxc46qmff04v3vssafhjlbs1rv82hf89w0ydwvidq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gfx" ,rust-gfx-0.18)
                       ("rust-gfx-device-gl" ,rust-gfx-device-gl-0.16)
                       ("rust-piston" ,rust-piston-0.49)
                       ("rust-piston-texture" ,rust-piston-texture-0.8)
                       ("rust-piston2d-gfx-graphics" ,rust-piston2d-gfx-graphics-0.68)
                       ("rust-piston2d-graphics" ,rust-piston2d-graphics-0.36)
                       ("rust-pistoncore-glutin-window" ,rust-pistoncore-glutin-window-0.63)
                       ("rust-shader-version" ,rust-shader-version-0.6))))
    (home-page "https://github.com/pistondevelopers/piston_window")
    (synopsis "The official Piston window wrapper for the Piston game engine")
    (description
     "This package provides The official Piston window wrapper for the Piston game engine.")
    (license license:expat)))

(define-public rust-gfx-gl-0.6
  (package
    (name "rust-gfx-gl")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gfx_gl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ppzj4bgjawdqz3fvnscqk8lnmgh95pwzh0v96vwy809cxj83lzj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gl-generator" ,rust-gl-generator-0.14))))
    (home-page "https://github.com/gfx-rs/gfx_gl")
    (synopsis "OpenGL bindings for gfx, based on gl-rs")
    (description
     "This package provides @code{OpenGL} bindings for gfx, based on gl-rs.")
    (license license:asl2.0)))

(define-public rust-gfx-device-gl-0.16
  (package
    (name "rust-gfx-device-gl")
    (version "0.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gfx_device_gl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g5yg19jvxdmviljyakhd6253bnb2qg7v8iscf48ihc0ldgki70h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gfx-core" ,rust-gfx-core-0.9)
                       ("rust-gfx-gl" ,rust-gfx-gl-0.6)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/gfx-rs/gfx")
    (synopsis "OpenGL backend for gfx-rs")
    (description "This package provides @code{OpenGL} backend for gfx-rs.")
    (license license:asl2.0)))

(define-public rust-plotters-0.2
  (package
    (name "rust-plotters")
    (version "0.2.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "plotters" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fzn4h0mk5d8z5wpnggkajypbb2mv6lrsqih0gg37fmywzxqa5hd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.8)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-font-kit" ,rust-font-kit-0.7)
                       ("rust-gif" ,rust-gif-0.10)
                       ("rust-image" ,rust-image-0.23)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-palette" ,rust-palette-0.5)
                       ("rust-piston-window" ,rust-piston-window-0.108)
                       ("rust-rusttype" ,rust-rusttype-0.8)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-itertools" ,rust-itertools-0.9)
                                   ("rust-rand" ,rust-rand-0.7)
                                   ("rust-rand-distr" ,rust-rand-distr-0.2)
                                   ("rust-rand-xorshift" ,rust-rand-xorshift-0.2)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://plotters-rs.github.io/")
    (synopsis
     "Rust drawing library focus on data plotting for both WASM and native applications")
    (description
     "This package provides a Rust drawing library focus on data plotting for both
WASM and native applications.")
    (license license:expat)))

(define-public rust-criterion-0.3.3
  (package
    (inherit rust-criterion-0.4)
    (name "rust-criterion")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "criterion" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n24l95pgjig4nfhgm3vn9gxb49ky5ylr8390scl7wbcxk7agnkh"))))
    (arguments
     `(#:tests? #f
       #:cargo-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-cast" ,rust-cast-0.3)
        ("rust-clap" ,rust-clap-2)
        ("rust-criterion-plot" ,rust-criterion-plot-0.4)
        ("rust-csv" ,rust-csv-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-itertools" ,rust-itertools-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-oorandom" ,rust-oorandom-11)
        ("rust-plotters" ,rust-plotters-0.2)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-cbor" ,rust-serde-cbor-0.11)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-smol" ,rust-smol-1)
        ("rust-tinytemplate" ,rust-tinytemplate-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs
       (("rust-approx" ,rust-approx-0.5)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-quanta-0.12
  (package
    (name "rust-quanta")
    (version "0.12.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quanta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03kwh0xb7gr461jcjhrxvcj9157k1jyg2gyy0f4579nf4ilgxl9v"))))
    (build-system cargo-build-system)
    (arguments
       ;; t1 should be greater than t0 by at least 14ms, was only 0ns (t0: 72038116156784, t1: 72038116152466)
     `(#:tests? #f
       #:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8.16)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-prost-types" ,rust-prost-types-0.11)
                       ("rust-raw-cpuid" ,rust-raw-cpuid-11)
                       ("rust-wasi" ,rust-wasi-0.11)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-average" ,rust-average-0.14)
                                   ("rust-criterion" ,rust-criterion-0.3.3)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/metrics-rs/quanta")
    (synopsis "high-speed timing library")
    (description "This package provides high-speed timing library.")
    (license license:expat)))

(define-public rust-moka-0.12
  (package
    (name "rust-moka")
    (version "0.12.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "moka" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09h9ww66vxrkizma99n7ib2fm91crkw4msp650j2i94lr911ccm9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("future")
       #:cargo-test-flags '("--features" "future")
       #:cargo-inputs (("rust-async-lock" ,rust-async-lock-3)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-event-listener" ,rust-event-listener-5)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-loom" ,rust-loom-0.7)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-portable-atomic" ,rust-portable-atomic-1)
                       ("rust-quanta" ,rust-quanta-0.12)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-tagptr" ,rust-tagptr-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs (("rust-actix-rt" ,rust-actix-rt-2)
                                   ("rust-ahash" ,rust-ahash-0.8)
                                   ("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-env-logger" ,rust-env-logger-0.10)
                                   ("rust-getrandom" ,rust-getrandom-0.2)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-paste" ,rust-paste-1)
                                   ("rust-reqwest" ,rust-reqwest-0.11)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/moka-rs/moka")
    (synopsis "fast and concurrent cache library inspired by Java Caffeine")
    (description
     "This package provides a fast and concurrent cache library inspired by Java
Caffeine.")
    (license (list license:expat license:asl2.0))))

(define-public rust-octseq-0.5
  (package
    (name "rust-octseq")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "octseq" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04pycbrcxlmhxqmrs4jgd0kqjk9pwjil6zr4fp2wwi4wgjikqv0j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-heapless" ,rust-heapless-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/NLnetLabs/octets/")
    (synopsis "Abstractions for types representing octet sequences")
    (description
     "This package provides Abstractions for types representing octet sequences.")
    (license license:bsd-3)))

(define-public rust-siphasher-1
  (package
    (name "rust-siphasher")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "siphasher" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://docs.rs/siphasher")
    (synopsis "SipHash-2-4, SipHash-1-3 and 128-bit variants in pure Rust")
    (description
     "This package provides @code{SipHash-2-4}, @code{SipHash-1-3} and 128-bit variants in pure Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mock-instant-0.5
  (package
    (name "rust-mock-instant")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mock_instant" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sly2w66y6gi6d071ir94h2kiyx7rrcwxng1qv5fsn438524q7af"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/museun/mock_instant")
    (synopsis "a simple way to mock an std::time::Instant")
    (description
     "This package provides a simple way to mock an std::time::Instant.")
    (license license:bsd-0)))

(define-public rust-tokio-tfo-0.2
  (package
    (name "rust-tokio-tfo")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-tfo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17mb78dhn7j5d09777zjasjxhf1xpac37ivdajqckrid20zl62zk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-socket2" ,rust-socket2-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/zonyitoo/tokio-tfo")
    (synopsis "TCP Fast Open (TFO) in Rust for tokio")
    (description
     "This package provides TCP Fast Open (TFO) in Rust for tokio.")
    (license license:expat)))

(define-public rust-domain-0.10
  (package
    (name "rust-domain")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "domain" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13f2l5g9890v1ilgn6z16y09p1as7a7ssa6dcf5aidpkv5k8c034"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-heapless" ,rust-heapless-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-moka" ,rust-moka-0.12)
                       ("rust-octseq" ,rust-octseq-0.5)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-ring" ,rust-ring-0.17)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-siphasher" ,rust-siphasher-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.26)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))
       #:cargo-development-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-mock-instant" ,rust-mock-instant-0.5)
                                   ("rust-rstest" ,rust-rstest-0.19)
                                   ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-serde-test" ,rust-serde-test-1)
                                   ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                                   ("rust-socket2" ,rust-socket2-0.5)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tokio-rustls" ,rust-tokio-rustls-0.26)
                                   ("rust-tokio-test" ,rust-tokio-test-0.4)
                                   ("rust-tokio-tfo" ,rust-tokio-tfo-0.2)
                                   ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/nlnetlabs/domain/")
    (synopsis "DNS library for Rust.")
    (description "This package provides a DNS library for Rust.")
    (license license:bsd-3)))

(define-public rust-libdav-0.9
  (package
    (name "rust-libdav")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libdav" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1646mcnalav3jiprn3xyslyncmcvn34jzw5qn0h4k1x0bppczqhm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-domain" ,rust-domain-0.10)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-roxmltree" ,rust-roxmltree-0.20)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tower-service" ,rust-tower-service-0.3))
       #:cargo-development-inputs (("rust-hyper-rustls" ,rust-hyper-rustls-0.26)
                                   ("rust-hyper-util" ,rust-hyper-util-0.1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-tower-http" ,rust-tower-http-0.6))))
    (native-inputs (list nss-certs-for-test))
    (home-page "https://sr.ht/~whynothugo/vdirsyncer-rs/")
    (synopsis "CalDAV and CardDAV client implementations")
    (description
     "This package provides @code{CalDAV} and @code{CardDAV} client implementations.")
    (license license:isc)))

(define-public rust-scfg-0.3
  (package
    (name "rust-scfg")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "scfg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xfqn2yy75jg0jzwh9x4bxfi575csgrjjym32sf93hhg9nmknf59"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-shell-words" ,rust-shell-words-1))))
    (home-page "https://git.sr.ht/~cdv/scfg-rs")
    (synopsis "An scfg parser")
    (description "This package provides An scfg parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sqlite3-src-0.5
  (package
    (name "rust-sqlite3-src")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m74wrkpify3z0xvrw4i2yssn9m9sjwqa5ipk6aq6f7fl58mmjdz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/stainless-steel/sqlite3-src")
    (synopsis "The package provides SQLite")
    (description "This package provides The package provides SQLite.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sqlite3-sys-0.15
  (package
    (name "rust-sqlite3-sys")
    (version "0.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fq6m21dnd5yqrzknsmnl2565nahdwa29s7x12xhxr1kjik2qxgj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-sqlite3-src" ,rust-sqlite3-src-0.5))
       #:cargo-development-inputs (("rust-temporary" ,rust-temporary-0.6))))
    (home-page "https://github.com/stainless-steel/sqlite3-sys")
    (synopsis "The package provides bindings to SQLite")
    (description
     "This package provides The package provides bindings to SQLite.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sqlite-0.32
  (package
    (name "rust-sqlite")
    (version "0.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rpqpkpxn2qdvghsnak2b73cn5ca37p6ri0ylyjdcmrq3481r003"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-sqlite3-sys" ,rust-sqlite3-sys-0.15))
       #:cargo-development-inputs (("rust-temporary" ,rust-temporary-0.6))))
    (home-page "https://github.com/stainless-steel/sqlite")
    (synopsis "The package provides an interface to SQLite")
    (description
     "This package provides The package provides an interface to SQLite.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-vparser-1
  (package
    (name "rust-vparser")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yjszxiqz9bwxd5qx4w8k1gcbgf1mi9wrk75d89443najyl3klzr"))))
    (build-system cargo-build-system)
    (home-page "https://sr.ht/~whynothugo/vdirsyncer-rs")
    (synopsis "Low-level non-validating icalendar/vcard parser")
    (description
     "This package provides Low-level non-validating icalendar/vcard parser.")
    (license license:isc)))

(define-public rust-manganis-macro-0.6
  (package
    (name "rust-manganis-macro")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "manganis-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rfhscidn851g1b99cydkqrd1ni7n6ygq34z9lbl0g4i209zgx6r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dunce" ,rust-dunce-1)
                       ("rust-manganis-core" ,rust-manganis-core-0.6)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://dioxuslabs.com")
    (synopsis
     "Ergonomic, automatic, cross crate asset collection and optimization")
    (description
     "This package provides Ergonomic, automatic, cross crate asset collection and optimization.")
    (license (list license:expat license:asl2.0))))

(define-public rust-manganis-core-0.6
  (package
    (name "rust-manganis-core")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "manganis-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06b5l0p04wyfwavky172vgxfi2vwyn8b5nxm4fx2nnvjrijyx2y3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-const-serialize" ,rust-const-serialize-0.6)
                       ("rust-dioxus-cli-config" ,rust-dioxus-cli-config-0.6)
                       ("rust-dioxus-core-types" ,rust-dioxus-core-types-0.6)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://dioxuslabs.com")
    (synopsis "core for manganis")
    (description "This package provides core for manganis.")
    (license (list license:expat license:asl2.0))))

(define-public rust-const-serialize-macro-0.6
  (package
    (name "rust-const-serialize-macro")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "const-serialize-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sznai9igkv86cvwrc9q0wdxyfgg4sha37mln7v4lhzpkl6jsf04"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://dioxuslabs.com/learn/0.5/getting_started")
    (synopsis "macro to derive const serialize")
    (description "This package provides a macro to derive const serialize.")
    (license (list license:expat license:asl2.0))))

(define-public rust-const-serialize-0.6
  (package
    (name "rust-const-serialize")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "const-serialize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19lkkxb7z18rfc4gckwnizvcbrd9ldj3sjnb4r45qw9csrv9j988"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-const-serialize-macro" ,rust-const-serialize-macro-0.6)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://dioxuslabs.com/learn/0.5/getting_started")
    (synopsis "serialization framework that works in const contexts")
    (description
     "This package provides a serialization framework that works in const contexts.")
    (license (list license:expat license:asl2.0))))

(define-public rust-manganis-0.6
  (package
    (name "rust-manganis")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "manganis" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wzglhz8mfkciqqrnv0m80q7acdn7fd489a5y22mnq772m5z8yii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-const-serialize" ,rust-const-serialize-0.6)
                       ("rust-manganis-core" ,rust-manganis-core-0.6)
                       ("rust-manganis-macro" ,rust-manganis-macro-0.6))))
    (home-page "https://dioxuslabs.com")
    (synopsis
     "Ergonomic, automatic, cross crate asset collection and optimization")
    (description
     "This package provides Ergonomic, automatic, cross crate asset collection and optimization.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-router-macro-0.6
  (package
    (name "rust-dioxus-router-macro")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-router-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lkg18zvh2drcdfzj4k0fg84gh56lbp6szf8g7bk6zcsksvzyhr7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://dioxuslabs.com")
    (synopsis "Macro for Dioxus Router")
    (description "This package provides Macro for Dioxus Router.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-router-0.6
  (package
    (name "rust-dioxus-router")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-router" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r33z1mqsx79nh460yk2nr98j5ppzzn1l7cx95b1zag4r5psfrkj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dioxus-cli-config" ,rust-dioxus-cli-config-0.6)
                       ("rust-dioxus-history" ,rust-dioxus-history-0.6)
                       ("rust-dioxus-lib" ,rust-dioxus-lib-0.6)
                       ("rust-dioxus-router-macro" ,rust-dioxus-router-macro-0.6)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-urlencoding" ,rust-urlencoding-2))))
    (home-page "https://dioxuslabs.com")
    (synopsis "Cross-platform router for Dioxus apps")
    (description
     "This package provides Cross-platform router for Dioxus apps.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tracing-wasm-0.2
  (package
    (name "rust-tracing-wasm")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tracing-wasm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01vfcarjds5n94vz72fxnzxz4nznd3zhhhcgsyi0yhkll5iwcxa5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rayon" ,rust-rayon-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/storyai/tracing-wasm")
    (synopsis "tracing subscriber for browser WASM")
    (description "This package provides tracing subscriber for browser WASM.")
    (license (list license:expat license:asl2.0))))

(define-public rust-console-error-panic-hook-0.1
  (package
    (name "rust-console-error-panic-hook")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "console_error_panic_hook" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g5v8s0ndycc10mdn6igy914k645pgpcl8vjpz6nvxkhyirynsm0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/rustwasm/console_error_panic_hook")
    (synopsis
     "panic hook for `wasm32-unknown-unknown` that logs panics to `console.error`")
    (description
     "This package provides a panic hook for `wasm32-unknown-unknown` that logs panics
to `console.error`.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dioxus-logger-0.6
  (package
    (name "rust-dioxus-logger")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-logger" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ccrx2iz5q99bc1y5yfjdbdhd88qicy1p5blq9cvzj7nabkn2nal"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-console-error-panic-hook" ,rust-console-error-panic-hook-0.1)
                       ("rust-dioxus-cli-config" ,rust-dioxus-cli-config-0.6)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-tracing-wasm" ,rust-tracing-wasm-0.2))))
    (home-page "https://github.com/dioxuslabs/dioxus")
    (synopsis
     "logging utility to provide a standard interface whether you're targeting web desktop, fullstack, and more.")
    (description
     "This package provides a logging utility to provide a standard interface whether
you're targeting web desktop, fullstack, and more.")
    (license license:expat)))

(define-public rust-dioxus-liveview-0.6
  (package
    (name "rust-dioxus-liveview")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-liveview" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w2y3iyf6ii6n9bl56y2qidaz1f0b7894y3dx4wyp4ymnn88vdr7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-axum" ,rust-axum-0.7)
                       ("rust-dioxus-cli-config" ,rust-dioxus-cli-config-0.6)
                       ("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-dioxus-devtools" ,rust-dioxus-devtools-0.6)
                       ("rust-dioxus-document" ,rust-dioxus-document-0.6)
                       ("rust-dioxus-history" ,rust-dioxus-history-0.6)
                       ("rust-dioxus-html" ,rust-dioxus-html-0.6)
                       ("rust-dioxus-interpreter-js" ,rust-dioxus-interpreter-js-0.6)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-generational-box" ,rust-generational-box-0.6)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://dioxuslabs.com/learn/0.6/getting_started")
    (synopsis "Build server-side apps with Dioxus")
    (description "This package provides Build server-side apps with Dioxus.")
    (license (list license:expat license:asl2.0))))

(define-public rust-server-fn-macro-default-0.6
  (package
    (name "rust-server-fn-macro-default")
    (version "0.6.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "server_fn_macro_default" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h14ai1gf16f2648acj5y9j9a2c0y03zvh8a5scig2jmkc8shakz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-server-fn-macro" ,rust-server-fn-macro-0.6)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/leptos-rs/leptos")
    (synopsis
     "The default implementation of the server_fn macro without a context")
    (description
     "This package provides The default implementation of the server_fn macro without a context.")
    (license license:expat)))

(define-public rust-serde-qs-0.12
  (package
    (name "rust-serde-qs")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_qs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "031kgpxbqkkxnql0k7sd80lyp98x7jc92311chrkc7k5d1as6c84"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-web" ,rust-actix-web-2)
                       ("rust-actix-web" ,rust-actix-web-3)
                       ("rust-actix-web" ,rust-actix-web-4)
                       ("rust-axum" ,rust-axum-0.6)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-warp" ,rust-warp-0.3))))
    (home-page "https://github.com/samscott89/serde_qs")
    (synopsis "Querystrings for Serde")
    (description "This package provides Querystrings for Serde.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-lite-derive-0.5
  (package
    (name "rust-serde-lite-derive")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-lite-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02vnsrc7gpd8ksa6bvlgbrfqyaqkb1fagxhw619hilfqwf26mqkw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/operutka/serde-lite")
    (synopsis
     "Implementation of #[derive(Deserialize, Serialize, Update)] for serde-lite")
    (description
     "This package provides Implementation of #[derive(Deserialize, Serialize, Update)] for serde-lite.")
    (license license:expat)))

(define-public rust-serde-lite-0.5
  (package
    (name "rust-serde-lite")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-lite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v01mq8z1a1nbnrjsr5zrhmkfmml31gnf9nnm2dhv7qsp5x2gs7r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-lite-derive" ,rust-serde-lite-derive-0.5))))
    (home-page "https://github.com/operutka/serde-lite")
    (synopsis "general-purpose serialization/de-serialization library")
    (description
     "This package provides a general-purpose serialization/de-serialization library.")
    (license license:expat)))

(define-public rust-gloo-utils-0.2
  (package
    (name "rust-gloo-utils")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1am31cd6889shb7158bg9zzsjcpvyzxrhfhxgia8rc8k84smam8b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "Convenience crate for common `web_sys` features")
    (description
     "This package provides Convenience crate for common `web_sys` features.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gloo-net-0.6
  (package
    (name "rust-gloo-net")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gloo-net" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1005q761m8kbifc01pvjyjfpj0qs3szh8qaxni13vjjq39xn4vy0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-gloo-utils" ,rust-gloo-utils-0.2)
                       ("rust-http" ,rust-http-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://github.com/rustwasm/gloo")
    (synopsis "HTTP requests library for WASM Apps")
    (description "This package provides HTTP requests library for WASM Apps.")
    (license (list license:expat license:asl2.0))))

(define-public rust-server-fn-0.6
  (package
    (name "rust-server-fn")
    (version "0.6.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "server_fn" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gifsvi9j42brnzlwycbmzzqlbw5nigc9iijp8s5lbm370q7mbjg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-actix-web" ,rust-actix-web-4)
                       ("rust-axum" ,rust-axum-0.7)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-ciborium" ,rust-ciborium-0.2)
                       ("rust-const-format" ,rust-const-format-0.2)
                       ("rust-dashmap" ,rust-dashmap-5)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-gloo-net" ,rust-gloo-net-0.6)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-inventory" ,rust-inventory-0.3)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-multer" ,rust-multer-3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-reqwest" ,rust-reqwest-0.12)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-rmp-serde" ,rust-rmp-serde-1)
                       ("rust-send-wrapper" ,rust-send-wrapper-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-lite" ,rust-serde-lite-0.5)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-qs" ,rust-serde-qs-0.12)
                       ("rust-server-fn-macro-default" ,rust-server-fn-macro-default-0.6)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-wasm-streams" ,rust-wasm-streams-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-xxhash-rust" ,rust-xxhash-rust-0.8))))
    (home-page "https://github.com/leptos-rs/leptos")
    (synopsis "RPC for any web framework")
    (description "This package provides RPC for any web framework.")
    (license license:expat)))

(define-public rust-server-fn-macro-0.6
  (package
    (name "rust-server-fn-macro")
    (version "0.6.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "server_fn_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "188176yyj2nic8y1yh0zh5ak8nmvi93i01kw2xwfyylnqr4gdaps"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-const-format" ,rust-const-format-0.2)
                       ("rust-convert-case" ,rust-convert-case-0.6)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-xxhash-rust" ,rust-xxhash-rust-0.8))))
    (home-page "https://github.com/leptos-rs/leptos")
    (synopsis "RPC for any web framework")
    (description "This package provides RPC for any web framework.")
    (license license:expat)))

(define-public rust-dioxus-server-macro-0.6
  (package
    (name "rust-dioxus-server-macro")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus_server_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15ys5lxlq0873nma45acbak603jxyyrpgscja0yba1lsk0hmn6ip"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-server-fn-macro" ,rust-server-fn-macro-0.6)
                       ("rust-syn" ,rust-syn-2))))
    (home-page
     "https://dioxuslabs.com/docs/0.5/guide/en/getting_started/fullstack.html")
    (synopsis "Server function macros for Dioxus")
    (description "This package provides Server function macros for Dioxus.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-wasm-bindgen-0.5
  (package
    (name "rust-serde-wasm-bindgen")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-wasm-bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03m01y4l2kqz63pb1bip52j8bqilzlhhsa7asfdanmrwhgi47cgk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/RReverser/serde-wasm-bindgen")
    (synopsis "Native Serde adapter for wasm-bindgen")
    (description
     "This package provides Native Serde adapter for wasm-bindgen.")
    (license license:expat)))

(define-public rust-dioxus-web-0.6
  (package
    (name "rust-dioxus-web")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-web" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l9yr1hwb1azgk9swq6pvjvwpjfzns7bdqdgp1c00dixbi3i4z3y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-ciborium" ,rust-ciborium-0.2)
                       ("rust-dioxus-cli-config" ,rust-dioxus-cli-config-0.6)
                       ("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-dioxus-core-types" ,rust-dioxus-core-types-0.6)
                       ("rust-dioxus-devtools" ,rust-dioxus-devtools-0.6)
                       ("rust-dioxus-document" ,rust-dioxus-document-0.6)
                       ("rust-dioxus-history" ,rust-dioxus-history-0.6)
                       ("rust-dioxus-html" ,rust-dioxus-html-0.6)
                       ("rust-dioxus-interpreter-js" ,rust-dioxus-interpreter-js-0.6)
                       ("rust-dioxus-signals" ,rust-dioxus-signals-0.6)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-generational-box" ,rust-generational-box-0.6)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-lazy-js-bundle" ,rust-lazy-js-bundle-0.6)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-wasm-bindgen" ,rust-serde-wasm-bindgen-0.5)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://dioxuslabs.com/learn/0.6/getting_started")
    (synopsis
     "Web-sys renderer for Dioxus: Build fullstack web, desktop, and mobile apps with a single codebase")
    (description
     "This package provides Web-sys renderer for Dioxus: Build fullstack web, desktop, and mobile apps with
a single codebase.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-ssr-0.6
  (package
    (name "rust-dioxus-ssr")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-ssr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08pinsqcw2qq0c3ifhkf4y0jkglkhfv48pg1x72cw777jz0gmq9q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-askama-escape" ,rust-askama-escape-0.10)
                       ("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-dioxus-core-types" ,rust-dioxus-core-types-0.6)
                       ("rust-rustc-hash" ,rust-rustc-hash-1))))
    (home-page "https://github.com/DioxusLabs/dioxus/")
    (synopsis "Dioxus render-to-string")
    (description "This package provides Dioxus render-to-string.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-mobile-0.6
  (package
    (name "rust-dioxus-mobile")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-mobile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lm97zngv2qvx2h5y4hgrvb9i1hg3rbjmwwki5sjgx09i4w9a4kd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dioxus-cli-config" ,rust-dioxus-cli-config-0.6)
                       ("rust-dioxus-desktop" ,rust-dioxus-desktop-0.6)
                       ("rust-dioxus-lib" ,rust-dioxus-lib-0.6)
                       ("rust-jni" ,rust-jni-0.21)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://dioxuslabs.com/learn/0.6/getting_started")
    (synopsis "Mobile-compatible renderer for Dioxus")
    (description
     "This package provides Mobile-compatible renderer for Dioxus.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-lib-0.6
  (package
    (name "rust-dioxus-lib")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-lib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0whv62vwlvncg1c33gcjfwksaz116hpz2a17sbhc7c5qm4dbf1al"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dioxus-config-macro" ,rust-dioxus-config-macro-0.6)
                       ("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-dioxus-core-macro" ,rust-dioxus-core-macro-0.6)
                       ("rust-dioxus-document" ,rust-dioxus-document-0.6)
                       ("rust-dioxus-history" ,rust-dioxus-history-0.6)
                       ("rust-dioxus-hooks" ,rust-dioxus-hooks-0.6)
                       ("rust-dioxus-html" ,rust-dioxus-html-0.6)
                       ("rust-dioxus-rsx" ,rust-dioxus-rsx-0.6)
                       ("rust-dioxus-signals" ,rust-dioxus-signals-0.6)
                       ("rust-warnings" ,rust-warnings-0.2))))
    (home-page "https://dioxuslabs.com/learn/0.6/")
    (synopsis
     "Build fullstack web, desktop, and mobile apps with a single codebase")
    (description
     "This package provides Build fullstack web, desktop, and mobile apps with a single codebase.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-isrg-0.6
  (package
    (name "rust-dioxus-isrg")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-isrg" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rkas9frgb3q6rk92p4s07y0ar153jvzv6aa1kfi9qlqlh0ifzpz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-http" ,rust-http-1)
                       ("rust-lru" ,rust-lru-0.12)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://dioxuslabs.com")
    (synopsis "CLI Configuration for dioxus-cli")
    (description "This package provides CLI Configuration for dioxus-cli.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-fullstack-0.6
  (package
    (name "rust-dioxus-fullstack")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-fullstack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a6iqpg591y5vpi38z958wxhsfw6zn03xgf4nn2w7vj82f5b96gy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-aws-lc-rs" ,rust-aws-lc-rs-1)
                       ("rust-axum" ,rust-axum-0.7)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-ciborium" ,rust-ciborium-0.2)
                       ("rust-dioxus-cli-config" ,rust-dioxus-cli-config-0.6)
                       ("rust-dioxus-desktop" ,rust-dioxus-desktop-0.6)
                       ("rust-dioxus-devtools" ,rust-dioxus-devtools-0.6)
                       ("rust-dioxus-history" ,rust-dioxus-history-0.6)
                       ("rust-dioxus-interpreter-js" ,rust-dioxus-interpreter-js-0.6)
                       ("rust-dioxus-isrg" ,rust-dioxus-isrg-0.6)
                       ("rust-dioxus-lib" ,rust-dioxus-lib-0.6)
                       ("rust-dioxus-mobile" ,rust-dioxus-mobile-0.6)
                       ("rust-dioxus-ssr" ,rust-dioxus-ssr-0.6)
                       ("rust-dioxus-web" ,rust-dioxus-web-0.6)
                       ("rust-dioxus-server-macro" ,rust-dioxus-server-macro-0.6)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-generational-box" ,rust-generational-box-0.6)
                       ("rust-http" ,rust-http-1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.27)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-rustls" ,rust-rustls-0.23)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-server-fn" ,rust-server-fn-0.6)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-tower-http" ,rust-tower-http-0.5)
                       ("rust-tower-layer" ,rust-tower-layer-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-futures" ,rust-tracing-futures-0.2)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://dioxuslabs.com")
    (synopsis
     "Fullstack utilities for Dioxus: Build fullstack web, desktop, and mobile apps with a single codebase")
    (description
     "This package provides Fullstack utilities for Dioxus: Build fullstack web, desktop, and mobile apps
with a single codebase.")
    (license (list license:expat license:asl2.0))))

(define-public rust-webview2-com-sys-0.33
  (package
    (name "rust-webview2-com-sys")
    (version "0.33.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webview2-com-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11hq04494njp374f8khpgmzd1cwfcqa70xzrjcf3d0lgnppf58x3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror" ,rust-thiserror-1)
                       ("rust-windows" ,rust-windows-0.58)
                       ("rust-windows-core" ,rust-windows-core-0.58))))
    (home-page "https://github.com/wravery/webview2-rs")
    (synopsis
     "Bindings generated with the windows crate for the WebView2 COM APIs")
    (description
     "This package provides Bindings generated with the windows crate for the @code{WebView2} COM APIs.")
    (license license:expat)))

(define-public rust-webview2-com-macros-0.8
  (package
    (name "rust-webview2-com-macros")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webview2-com-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cg430mp9a7lrhl533dl2yylaid2dzkbxpcbvrnxbfd3pcaqy8hx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/wravery/webview2-rs")
    (synopsis
     "Macros which generate callback implementations for WebView2 COM APIs")
    (description
     "This package provides Macros which generate callback implementations for @code{WebView2} COM APIs.")
    (license license:expat)))

(define-public rust-webview2-com-0.33
  (package
    (name "rust-webview2-com")
    (version "0.33.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webview2-com" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b5f2mrfbhrz60wz6af31p8h49ysrwxfn50v8v5yzr0fklyzyqbg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-webview2-com-macros" ,rust-webview2-com-macros-0.8)
                       ("rust-webview2-com-sys" ,rust-webview2-com-sys-0.33)
                       ("rust-windows" ,rust-windows-0.58)
                       ("rust-windows-core" ,rust-windows-core-0.58)
                       ("rust-windows-implement" ,rust-windows-implement-0.58)
                       ("rust-windows-interface" ,rust-windows-interface-0.58))))
    (home-page "https://github.com/wravery/webview2-rs")
    (synopsis "Rust bindings for the WebView2 COM APIs")
    (description
     "This package provides Rust bindings for the @code{WebView2} COM APIs.")
    (license license:expat)))

(define-public rust-webkit2gtk-sys-2
  (package
    (name "rust-webkit2gtk-sys")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webkit2gtk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z6mlrkw5syy0k6k1kikxmygfgwrslq3ssdq2a7iykaizj5a7nk2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.18)
                       ("rust-gdk-sys" ,rust-gdk-sys-0.18)
                       ("rust-gio-sys" ,rust-gio-sys-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-gtk-sys" ,rust-gtk-sys-0.18)
                       ("rust-javascriptcore-rs-sys" ,rust-javascriptcore-rs-sys-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-soup3-sys" ,rust-soup3-sys-0.5)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://github.com/tauri-apps/webkit2gtk-rs")
    (synopsis "Rust binding for webkit-gtk library")
    (description "This package provides Rust binding for webkit-gtk library.")
    (license license:expat)))

(define-public rust-webkit2gtk-2
  (package
    (name "rust-webkit2gtk")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webkit2gtk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06inf9qy6hpymyhk9mpj3bxmi8rba4w0p78pkwgdm0f5ahgbrcbn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cairo-rs" ,rust-cairo-rs-0.18)
                       ("rust-gdk" ,rust-gdk-0.18)
                       ("rust-gdk-sys" ,rust-gdk-sys-0.18)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-gio-sys" ,rust-gio-sys-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-gtk" ,rust-gtk-0.18)
                       ("rust-gtk-sys" ,rust-gtk-sys-0.18)
                       ("rust-javascriptcore-rs" ,rust-javascriptcore-rs-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-soup3" ,rust-soup3-0.5)
                       ("rust-webkit2gtk-sys" ,rust-webkit2gtk-sys-2))))
    (home-page "https://github.com/tauri-apps/webkit2gtk-rs")
    (synopsis "Rust bindings for webkit-gtk library")
    (description "This package provides Rust bindings for webkit-gtk library.")
    (license license:expat)))

(define-public rust-soup3-sys-0.5
  (package
    (name "rust-soup3-sys")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "soup3-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09vcp2m0hcddjqsv979d4jnribxp1pvipgjyy4j2z8c0lr88kgky"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gio-sys" ,rust-gio-sys-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://gtk-rs.org")
    (synopsis "FFI bindings of Soup 3")
    (description "This package provides FFI bindings of Soup 3.")
    (license license:expat)))

(define-public rust-soup3-0.5
  (package
    (name "rust-soup3")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "soup3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17sgrkvx5jy6r6pyyhh8cl5mrm96rf0yfl3lqypm24pk815947s7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-soup3-sys" ,rust-soup3-sys-0.5))))
    (home-page "https://gitlab.gnome.org/World/Rust/soup3-rs")
    (synopsis "Soup crate for Rust")
    (description "This package provides Soup crate for Rust.")
    (license license:expat)))

(define-public rust-kuchikiki-0.8
  (package
    (name "rust-kuchikiki")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kuchikiki" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1a3d2byx4hzbm23mlkfrgfvmiv9g5g220x8aa5ph95drnxalg7pj"))
       (snippet
        '(begin
           (use-modules (guix build utils))
           ;; Update Cargo.toml to use selectors 0.23 instead of 0.22
           (substitute* "Cargo.toml"
             (("0.22") "0.23"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cssparser" ,rust-cssparser-0.27)
                       ("rust-html5ever" ,rust-html5ever-0.26)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-matches" ,rust-matches-0.1)
                       ("rust-selectors" ,rust-selectors-0.23))))
    (home-page "https://github.com/brave/kuchikiki")
    (synopsis "(口利き) HTML tree manipulation library")
    (description
     "This package provides (口利き) HTML tree manipulation library.")
    (license license:expat)))

(define-public rust-javascriptcore-rs-sys-1
  (package
    (name "rust-javascriptcore-rs-sys")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "javascriptcore-rs-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "092igagxm561lx65sin2z18jpxzyg0288cfzcrdvg97z2j6yf6xg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://github.com/tauri-apps/javascriptcore-rs")
    (synopsis
     "Sys functions for the Rust bindings of the javacriptcore library")
    (description
     "This package provides Sys functions for the Rust bindings of the javacriptcore library.")
    (license license:expat)))

(define-public rust-javascriptcore-rs-1
  (package
    (name "rust-javascriptcore-rs")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "javascriptcore-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k3z4pmg46znxfmjqvx63d5zr9vdj070f97wgajzp3yfzzlp2mna"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-javascriptcore-rs-sys" ,rust-javascriptcore-rs-sys-1))))
    (home-page "https://github.com/tauri-apps/javascriptcore-rs")
    (synopsis "Rust bindings for the javacriptcore library")
    (description
     "This package provides Rust bindings for the javacriptcore library.")
    (license license:expat)))

(define-public rust-gdkx11-0.18
  (package
    (name "rust-gdkx11")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdkx11" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zpvndnqasyk9gfnh8mwkb27gsr70dlkcg1v334bpgji8ghh1aiw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gdk" ,rust-gdk-0.18)
                       ("rust-gdkx11-sys" ,rust-gdkx11-sys-0.18)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-x11" ,rust-x11-2))))
    (home-page "https://gtk-rs.org/")
    (synopsis
     "UNMAINTAINED Rust bindings for the GDK X11 library (use gdk4-x11 instead)")
    (description
     "This package provides UNMAINTAINED Rust bindings for the GDK X11 library (use gdk4-x11 instead).")
    (license license:expat)))

(define-public rust-wry-0.45
  (package
    (name "rust-wry")
    (version "0.45.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wry" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1c2w3f9z938iyvm2lxwnf7ivn3lqd0664pvbq9abz7w26sirj05c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.22)
                       ("rust-block" ,rust-block-0.1)
                       ("rust-cocoa" ,rust-cocoa-0.26)
                       ("rust-core-graphics" ,rust-core-graphics-0.24)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-dpi" ,rust-dpi-0.1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-gdkx11" ,rust-gdkx11-0.18)
                       ("rust-gtk" ,rust-gtk-0.18)
                       ("rust-html5ever" ,rust-html5ever-0.26)
                       ("rust-http" ,rust-http-1)
                       ("rust-javascriptcore-rs" ,rust-javascriptcore-rs-1)
                       ("rust-jni" ,rust-jni-0.21)
                       ("rust-kuchikiki" ,rust-kuchikiki-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-ndk" ,rust-ndk-0.9)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-objc-id" ,rust-objc-id-0.1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.6)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-soup3" ,rust-soup3-0.5)
                       ("rust-tao-macros" ,rust-tao-macros-0.1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-webkit2gtk" ,rust-webkit2gtk-2)
                       ("rust-webkit2gtk-sys" ,rust-webkit2gtk-sys-2)
                       ("rust-webview2-com" ,rust-webview2-com-0.33)
                       ("rust-windows" ,rust-windows-0.58)
                       ("rust-windows-core" ,rust-windows-core-0.58)
                       ("rust-windows-version" ,rust-windows-version-0.1)
                       ("rust-x11-dl" ,rust-x11-dl-2))))
    (home-page "https://github.com/tauri-apps/wry")
    (synopsis "Cross-platform WebView rendering library")
    (description
     "This package provides Cross-platform @code{WebView} rendering library.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-objc2-uniform-type-identifiers-0.3
  (package
    (name "rust-objc2-uniform-type-identifiers")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-uniform-type-identifiers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "118wd3r2gm72vbdcbpyz877r4rzc5k3abm65cdwrm2dmg1bshr6r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the UniformTypeIdentifiers framework")
    (description
     "This package provides Bindings to the @code{UniformTypeIdentifiers} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-quartz-core-0.3
  (package
    (name "rust-objc2-quartz-core")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-quartz-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mfcnbgs4akjwb2xxqmkfixpz98j1db8hhrkck4js62zrnhbdzwh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-core-video" ,rust-objc2-core-video-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the QuartzCore/CoreAnimation framework")
    (description
     "This package provides Bindings to the @code{QuartzCore/CoreAnimation} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-video-0.3
  (package
    (name "rust-objc2-core-video")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-video" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00v3zcxl0xv8q964888il96d23nh3jbg9rm91fmqr5vydkkw728r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-io-surface" ,rust-objc2-io-surface-0.3)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreVideo framework")
    (description
     "This package provides Bindings to the @code{CoreVideo} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-ml-0.3
  (package
    (name "rust-objc2-core-ml")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-ml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11kiimdf1qd4cb7hs59qkv6y1yqda16rx540v28bjvwy7x4vgfj3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-core-video" ,rust-objc2-core-video-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreML framework")
    (description
     "This package provides Bindings to the @code{CoreML} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-image-0.3
  (package
    (name "rust-objc2-core-image")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-image" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zh0ihcb46lh09azhr5hfn74rx576hdmjmy1477nqsrqqh6drcvr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-core-ml" ,rust-objc2-core-ml-0.3)
                       ("rust-objc2-core-video" ,rust-objc2-core-video-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-objc2-io-surface" ,rust-objc2-io-surface-0.3)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreImage framework")
    (description
     "This package provides Bindings to the @code{CoreImage} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-metal-0.3
  (package
    (name "rust-objc2-metal")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-metal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x588xxxlsp4b061vgbmj4jx8h10mcspnic22ymhlm1r68c6q93z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-dispatch2" ,rust-dispatch2-0.3)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-objc2-io-surface" ,rust-objc2-io-surface-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Metal framework")
    (description "This package provides Bindings to the Metal framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-io-surface-0.3
  (package
    (name "rust-objc2-io-surface")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-io-surface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g0c89swz8hgfrh0j1iqhcz3ig7cyhavn3p9gi2s77sjjanfk0kj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the IOSurface framework")
    (description "This package provides Bindings to the IOSurface framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-graphics-0.3
  (package
    (name "rust-objc2-core-graphics")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "197mf2a4yvvigkd9hsp8abbpip7rn3mmc55psv1ba89hq5l6r74q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-dispatch2" ,rust-dispatch2-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-io-surface" ,rust-objc2-io-surface-0.3)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreGraphics framework")
    (description
     "This package provides Bindings to the @code{CoreGraphics} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-data-0.3
  (package
    (name "rust-objc2-core-data")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-data" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "179j6k4a7m5780qn41xnsh7p0x4w4gvpqhc6hs7531wjsbvvn7r9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-cloud-kit" ,rust-objc2-cloud-kit-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreData framework")
    (description
     "This package provides Bindings to the @code{CoreData} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-security-0.3
  (package
    (name "rust-objc2-security")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-security" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i0sfdwjfynwvf5vpbfqpdsabh3fp96v7p244v20hsxn7bpy1y71"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Security framework")
    (description "This package provides Bindings to the Security framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-services-0.3
  (package
    (name "rust-objc2-core-services")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-services" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r6n0qdf0337l12y1kciaq726m8qlwmam3brdlipsv1nrpjfd6qm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-dispatch2" ,rust-dispatch2-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-security" ,rust-objc2-security-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreServices framework")
    (description
     "This package provides Bindings to the @code{CoreServices} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-foundation-0.3
  (package
    (name "rust-objc2-core-foundation")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rn19d70mwxyv74kx7aqm5in6x320vavq9v0vrm81vbg9a4w440w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-dispatch2" ,rust-dispatch2-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreFoundation framework")
    (description
     "This package provides Bindings to the @code{CoreFoundation} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-foundation-0.3
  (package
    (name "rust-objc2-foundation")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g5hl47dxzabs7wndcg6kz3q137v9hwfay1jd2da1q9gglj3224h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-services" ,rust-objc2-core-services-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Foundation framework")
    (description "This package provides Bindings to the Foundation framework.")
    (license license:expat)))

(define-public rust-objc2-contacts-0.3
  (package
    (name "rust-objc2-contacts")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-contacts" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gdp7mrip1dfar97drqkkmmyarkzwn015v501swrn233ljcbs58a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Contacts framework")
    (description "This package provides Bindings to the Contacts framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-dispatch2-0.3
  (package
    (name "rust-dispatch2")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dispatch2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v1ak9w0s8z1g13x4mj2y5im9wmck0i2vf8f8wc9l1n6lqi9z849"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings and wrappers for Apple's Grand Central Dispatch (GCD)")
    (description
     "This package provides Bindings and wrappers for Apple's Grand Central Dispatch (GCD).")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-location-0.3
  (package
    (name "rust-objc2-core-location")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-location" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h2p2cpd16pghqzr15nzznva19r6fkdvbfs3hihrvajq4mwpa3xc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.6)
                       ("rust-dispatch2" ,rust-dispatch2-0.3)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-contacts" ,rust-objc2-contacts-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreLocation framework")
    (description
     "This package provides Bindings to the @code{CoreLocation} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-cloud-kit-0.3
  (package
    (name "rust-objc2-cloud-kit")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-cloud-kit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pd1iq5gw1c024gipy3x7al8p40g2p8b3pqp27zyc4dlv7f4yq8p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-location" ,rust-objc2-core-location-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CloudKit framework")
    (description
     "This package provides Bindings to the @code{CloudKit} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-block2-0.6
  (package
    (name "rust-block2")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wnwha7wjjqiamj9abq5l45fyzdxna2k2la0rp9w2hravc5jy39l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-objc2" ,rust-objc2-0.6))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Apple's C language extension of blocks")
    (description
     "This package provides Apple's C language extension of blocks.")
    (license license:expat)))

(define-public rust-objc2-app-kit-0.3
  (package
    (name "rust-objc2-app-kit")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-app-kit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k4vz0s63rpp1yyhx96mh9nndn1zzv2cwxzpvw6rnigcidb9zwp6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-cloud-kit" ,rust-objc2-cloud-kit-0.3)
                       ("rust-objc2-core-data" ,rust-objc2-core-data-0.3)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-core-image" ,rust-objc2-core-image-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-objc2-quartz-core" ,rust-objc2-quartz-core-0.3)
                       ("rust-objc2-uniform-type-identifiers" ,rust-objc2-uniform-type-identifiers-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the AppKit framework")
    (description
     "This package provides Bindings to the @code{AppKit} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-proc-macros-0.2
  (package
    (name "rust-objc2-proc-macros")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-proc-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zi9y5bb6igas980il7x0d5wijy959v69hhzzffmf17fii5h6hkw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Procedural macros for the objc2 project")
    (description
     "This package provides Procedural macros for the objc2 project.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-exception-helper-0.1
  (package
    (name "rust-objc2-exception-helper")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-exception-helper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12nrg6fhhp2rzmnym6s37h7w9v9sa9wbaixvfsq3axrdnzxwb8f7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "External helper function for catching Objective-C exceptions")
    (description
     "This package provides External helper function for catching Objective-C exceptions.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-encode-4
  (package
    (name "rust-objc2-encode")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-encode" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Objective-C type-encoding representation and parsing")
    (description
     "This package provides Objective-C type-encoding representation and parsing.")
    (license license:expat)))

(define-public rust-objc2-0.6
  (package
    (name "rust-objc2")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l85a8r77i8i183fqyx55kqm2nh9rzg2z3z59kjb4fj92iz5kil8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-objc2-encode" ,rust-objc2-encode-4)
                       ("rust-objc2-exception-helper" ,rust-objc2-exception-helper-0.1)
                       ("rust-objc2-proc-macros" ,rust-objc2-proc-macros-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Objective-C interface and runtime bindings")
    (description
     "This package provides Objective-C interface and runtime bindings.")
    (license license:expat)))

(define-public rust-muda-0.15
  (package
    (name "rust-muda")
    (version "0.15.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "muda" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "110lisi6hf685nbzqnq2i9a60k0h4anyh9f6radmgh0wwq09rbpx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-dpi" ,rust-dpi-0.1)
                       ("rust-gtk" ,rust-gtk-0.18)
                       ("rust-keyboard-types" ,rust-keyboard-types-0.7)
                       ("rust-libxdo" ,rust-libxdo-0.6)
                       ("rust-objc2" ,rust-objc2-0.5)
                       ("rust-objc2-app-kit" ,rust-objc2-app-kit-0.2)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-png" ,rust-png-0.17)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/amrbashir/muda")
    (synopsis "Menu Utilities for Desktop Applications")
    (description
     "This package provides Menu Utilities for Desktop Applications.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libappindicator-sys-0.9
  (package
    (name "rust-libappindicator-sys")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libappindicator-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bsw2mcxil3zm4zzdir76i7xnaqaq30cd9qpviccrvdb70hwb7kf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gtk-sys" ,rust-gtk-sys-0.18)
                       ("rust-libloading" ,rust-libloading-0.7)
                       ("rust-once-cell" ,rust-once-cell-1))))
    (home-page "")
    (synopsis "Rust sys bindings for libappindicator")
    (description
     "This package provides Rust sys bindings for libappindicator.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libappindicator-0.9
  (package
    (name "rust-libappindicator")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libappindicator" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02nwjmm5qqbkvzbz4j1dd50xs0ywr0i2l2scwmxcqs680yb9nn03"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib" ,rust-glib-0.18)
                       ("rust-gtk" ,rust-gtk-0.18)
                       ("rust-gtk-sys" ,rust-gtk-sys-0.18)
                       ("rust-libappindicator-sys" ,rust-libappindicator-sys-0.9)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "")
    (synopsis "Rust safe bindings for libappindicator")
    (description
     "This package provides Rust safe bindings for libappindicator.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-redox-users-0.5
  (package
    (name "rust-redox-users")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "redox_users" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0awxx66izdw6kz97r3zxrl5ms5f6dqi5l0f58mlsvlmx8wyrsvyx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-libredox" ,rust-libredox-0.1)
                       ("rust-rust-argon2" ,rust-rust-argon2-0.8)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://gitlab.redox-os.org/redox-os/users")
    (synopsis "Rust library to access Redox users and groups functionality")
    (description
     "This package provides a Rust library to access Redox users and groups
functionality.")
    (license license:expat)))

(define-public rust-dirs-sys-0.5
  (package
    (name "rust-dirs-sys")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dirs-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aqzpgq6ampza6v012gm2dppx9k35cdycbj54808ksbys9k366p0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-option-ext" ,rust-option-ext-0.2)
                       ("rust-redox-users" ,rust-redox-users-0.5)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/dirs-dev/dirs-sys-rs")
    (synopsis
     "System-level helper functions for the dirs and directories crates")
    (description
     "This package provides System-level helper functions for the dirs and directories crates.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dirs-6
  (package
    (name "rust-dirs")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dirs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0knfikii29761g22pwfrb8d0nqpbgw77sni9h2224haisyaams63"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dirs-sys" ,rust-dirs-sys-0.5))))
    (home-page "https://github.com/soc/dirs-rs")
    (synopsis
     "tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (description
     "This package provides a tiny low-level library that provides platform-specific
standard locations of directories for config, cache and other data on Linux,
Windows, @code{macOS} and Redox by leveraging the mechanisms defined by the XDG
base/user directory specifications on Linux, the Known Folder API on Windows,
and the Standard Directory guidelines on @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tray-icon-0.19
  (package
    (name "rust-tray-icon")
    (version "0.19.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tray-icon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12mlyz7qbsnraa27hf5x7blw63096gsna8wvl7m1699f03spbpga"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-dirs" ,rust-dirs-6)
                       ("rust-libappindicator" ,rust-libappindicator-0.9)
                       ("rust-muda" ,rust-muda-0.15)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-app-kit" ,rust-objc2-app-kit-0.3)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-png" ,rust-png-0.17)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/tauri-apps/tray-icon")
    (synopsis "Create tray icons for desktop applications")
    (description
     "This package provides Create tray icons for desktop applications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tao-macros-0.1
  (package
    (name "rust-tao-macros")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tao-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zbx0ifpn6xi4sc6vp7wq0sbpgrdwr0cm2xbisr7vh9aigmnpqgl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/tauri-apps/tao")
    (synopsis "Proc macros for tao")
    (description "This package provides Proc macros for tao.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gdkx11-sys-0.18
  (package
    (name "rust-gdkx11-sys")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdkx11-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13a2yjqac7i6bqxkpdjfa5rf33v0v06jdnq12vqjdb01zr2p8bkf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.18)
                       ("rust-gdk-sys" ,rust-gdk-sys-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6)
                       ("rust-x11" ,rust-x11-2))))
    (home-page "https://gtk-rs.org/")
    (synopsis
     "UNMAINTAINED FFI binding for libgdkx11 (use gdk4-x11-sys instead)")
    (description
     "This package provides UNMAINTAINED FFI binding for libgdkx11 (use gdk4-x11-sys instead).")
    (license license:expat)))

(define-public rust-gdkwayland-sys-0.18
  (package
    (name "rust-gdkwayland-sys")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdkwayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sgyipcl2k39ybw7mk6mii17ggdgaphva2cz5dbzf8yj0vap200l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gdk-sys" ,rust-gdk-sys-0.18)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://gtk-rs.org/")
    (synopsis
     "UNMAINTAINED FFI bindings to libgdk-3-wayland (use gdk4-wayland-sys instead)")
    (description
     "This package provides UNMAINTAINED FFI bindings to libgdk-3-wayland (use gdk4-wayland-sys instead).")
    (license license:expat)))

(define-public rust-dlopen2-derive-0.4
  (package
    (name "rust-dlopen2-derive")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dlopen2_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x6hr5la6yqjqyr3f9zwx3n1cjwhflk6l77kga2xv76y63xn10bq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/OpenByteDev/dlopen2")
    (synopsis "Derive macros for the dlopen2 crate")
    (description "This package provides Derive macros for the dlopen2 crate.")
    ;; unknown-license
    (license license:expat)))

(define-public rust-dlopen2-0.7
  (package
    (name "rust-dlopen2")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dlopen2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19hmafkfsqbgdw7pvm4s3w4b2l1d9hlydkjgfa2ymfib7l89f4ly"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dlopen2-derive" ,rust-dlopen2-derive-0.4)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/OpenByteDev/dlopen2")
    (synopsis
     "Library for opening and operating on dynamic link libraries (also known as shared objects or shared libraries)")
    (description
     "This package provides Library for opening and operating on dynamic link libraries (also known as
shared objects or shared libraries).")
    (license license:expat)))

(define-public rust-cocoa-foundation-0.2
  (package
    (name "rust-cocoa-foundation")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03cvlw5w94i28nf272bwd6lj4fl6bxc8qprn27ya3s8fqmkijhc1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block" ,rust-block-0.1)
                       ("rust-core-foundation" ,rust-core-foundation-0.10)
                       ("rust-core-graphics-types" ,rust-core-graphics-types-0.2)
                       ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Cocoa Foundation for macOS")
    (description
     "This package provides Bindings to Cocoa Foundation for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cocoa-0.26
  (package
    (name "rust-cocoa")
    (version "0.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k5nn6jffngwfbxv4vxpsk6c6wbmqb683nv8zsfia5kyxdx50dmd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block" ,rust-block-0.1)
                       ("rust-cocoa-foundation" ,rust-cocoa-foundation-0.2)
                       ("rust-core-foundation" ,rust-core-foundation-0.10)
                       ("rust-core-graphics" ,rust-core-graphics-0.24)
                       ("rust-foreign-types" ,rust-foreign-types-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Cocoa for macOS")
    (description "This package provides Bindings to Cocoa for @code{macOS}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tao-0.30
  (package
    (name "rust-tao")
    (version "0.30.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tao" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nkjp2mcn1bbv97bspmpxnsjifbsj51abl10pnibic5symya10k6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cocoa" ,rust-cocoa-0.26)
                       ("rust-core-foundation" ,rust-core-foundation-0.10)
                       ("rust-core-graphics" ,rust-core-graphics-0.24)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-dispatch" ,rust-dispatch-0.2)
                       ("rust-dlopen2" ,rust-dlopen2-0.7)
                       ("rust-dpi" ,rust-dpi-0.1)
                       ("rust-gdkwayland-sys" ,rust-gdkwayland-sys-0.18)
                       ("rust-gdkx11-sys" ,rust-gdkx11-sys-0.18)
                       ("rust-gtk" ,rust-gtk-0.18)
                       ("rust-instant" ,rust-instant-0.1)
                       ("rust-jni" ,rust-jni-0.21)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-ndk" ,rust-ndk-0.9)
                       ("rust-ndk-context" ,rust-ndk-context-0.1)
                       ("rust-ndk-sys" ,rust-ndk-sys-0.6)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.4)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.6)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
                       ("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tao-macros" ,rust-tao-macros-0.1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-windows" ,rust-windows-0.58)
                       ("rust-windows-core" ,rust-windows-core-0.58)
                       ("rust-windows-version" ,rust-windows-version-0.1)
                       ("rust-x11-dl" ,rust-x11-dl-2))))
    (home-page "https://github.com/tauri-apps/tao")
    (synopsis "Cross-platform window manager library")
    (description
     "This package provides Cross-platform window manager library.")
    (license license:asl2.0)))

(define-public rust-pipewire-sys-0.8
  (package
    (name "rust-pipewire-sys")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pipewire-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04hiy3rl8v3j2dfzp04gr7r8l5azzqqsvqdzwa7sipdij27ii7l4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-libspa-sys" ,rust-libspa-sys-0.8)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://pipewire.org")
    (synopsis "Rust FFI bindings for PipeWire")
    (description
     "This package provides Rust FFI bindings for @code{PipeWire}.")
    (license license:expat)))

(define-public rust-libspa-sys-0.8
  (package
    (name "rust-libspa-sys")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libspa-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07yh4i5grzbxkchg6dnxlwbdw2wm5jnd7ffbhl77jr0388b9f3dz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://pipewire.org")
    (synopsis "Rust FFI bindings for libspa")
    (description "This package provides Rust FFI bindings for libspa.")
    (license license:expat)))

(define-public rust-libspa-0.8
  (package
    (name "rust-libspa")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libspa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "044qs48yl0llp2dmrgwxj9y1pgfy09i6fhq661zqqb9a3fwa9wv5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-convert-case" ,rust-convert-case-0.6)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libspa-sys" ,rust-libspa-sys-0.8)
                       ("rust-nix" ,rust-nix-0.27)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://pipewire.org")
    (synopsis "Rust bindings for libspa")
    (description "This package provides Rust bindings for libspa.")
    (license license:expat)))

(define-public rust-pipewire-0.8
  (package
    (name "rust-pipewire")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pipewire" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nldg1hz4v0qr26lzdxqpvrac4zbc3pb6436sl392425bjx4brh8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libspa" ,rust-libspa-0.8)
                       ("rust-libspa-sys" ,rust-libspa-sys-0.8)
                       ("rust-nix" ,rust-nix-0.27)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pipewire-sys" ,rust-pipewire-sys-0.8)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://pipewire.org")
    (synopsis "Rust bindings for PipeWire")
    (description "This package provides Rust bindings for @code{PipeWire}.")
    (license license:expat)))

(define-public rust-gdk4-x11-sys-0.8
  (package
    (name "rust-gdk4-x11-sys")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-x11-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gg2197l3wlgmxiffpqnszck8f2wxk5kr560fbxv5dvpxj3ykd0b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gdk4-sys" ,rust-gdk4-sys-0.8)
                       ("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://gtk-rs.org/gtk4-rs")
    (synopsis "FFI bindings of GDK4 X11")
    (description "This package provides FFI bindings of GDK4 X11.")
    (license license:expat)))

(define-public rust-gdk4-x11-0.8
  (package
    (name "rust-gdk4-x11")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-x11" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1axpg2jdyn5sfnmvdk2dlyi9d5n4jsaf3byx4x5w21vhabis6vgc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gdk4" ,rust-gdk4-0.8)
                       ("rust-gdk4-x11-sys" ,rust-gdk4-x11-sys-0.8)
                       ("rust-gio" ,rust-gio-0.19)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-khronos-egl" ,rust-khronos-egl-6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-x11" ,rust-x11-2))))
    (home-page "https://gtk-rs.org/gtk4-rs")
    (synopsis "Rust bindings of the GDK4 X11 library")
    (description
     "This package provides Rust bindings of the GDK4 X11 library.")
    (license license:expat)))

(define-public rust-khronos-egl-6
  (package
    (name "rust-khronos-egl")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "khronos-egl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xnzdx0n1bil06xmh8i1x6dbxvk7kd2m70bbm6nw1qzc43r1vbka"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-libloading" ,rust-libloading-0.8)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/timothee-haudebourg/khronos-egl")
    (synopsis "Rust bindings for EGL")
    (description "This package provides Rust bindings for EGL.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gdk4-wayland-sys-0.8
  (package
    (name "rust-gdk4-wayland-sys")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00qqwqkwd3jzha64cpb4kf8g1ixxpbp289kix2q88wcigj9d6pvs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.19)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-system-deps" ,rust-system-deps-6))))
    (home-page "https://gtk-rs.org/gtk4-rs")
    (synopsis "FFI bindings of GDK4 Wayland")
    (description "This package provides FFI bindings of GDK4 Wayland.")
    (license license:expat)))

(define-public rust-gdk4-wayland-0.8
  (package
    (name "rust-gdk4-wayland")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk4-wayland" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w8kkdgwj0s5r92r4ml1k8dwn6gyrvmnpvvbxhzlwmscpvna087n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gdk4" ,rust-gdk4-0.8)
                       ("rust-gdk4-wayland-sys" ,rust-gdk4-wayland-sys-0.8)
                       ("rust-gio" ,rust-gio-0.19)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-khronos-egl" ,rust-khronos-egl-6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-client" ,rust-wayland-client-0.31))))
    (home-page "https://gtk-rs.org/gtk4-rs")
    (synopsis "Rust bindings of the GDK 4 Wayland library")
    (description
     "This package provides Rust bindings of the GDK 4 Wayland library.")
    (license license:expat)))

(define-public rust-ashpd-0.8
  (package
    (name "rust-ashpd")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ashpd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14yh6v12bgngawzds6i252n4zzq9rnqz659p5h898yl7f9y4v26x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-fs" ,rust-async-fs-2)
                       ("rust-async-net" ,rust-async-net-2)
                       ("rust-enumflags2" ,rust-enumflags2-0.7)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gdk4-wayland" ,rust-gdk4-wayland-0.8)
                       ("rust-gdk4-x11" ,rust-gdk4-x11-0.8)
                       ("rust-glib" ,rust-glib-0.19)
                       ("rust-gtk4" ,rust-gtk4-0.8)
                       ("rust-pipewire" ,rust-pipewire-0.8)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-client" ,rust-wayland-client-0.31)
                       ("rust-wayland-protocols" ,rust-wayland-protocols-0.31)
                       ("rust-zbus" ,rust-zbus-4))))
    (home-page "https://github.com/bilelmoussaoui/ashpd")
    (synopsis "XDG portals wrapper in Rust using zbus")
    (description
     "This package provides XDG portals wrapper in Rust using zbus.")
    (license license:expat)))

(define-public rust-rfd-0.14
  (package
    (name "rust-rfd")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rfd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lcjf6kcq2c2022bbnj29q8pp1si3x9fqxnavxp3c97w6xrkm9r5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ashpd" ,rust-ashpd-0.8)
                       ("rust-block" ,rust-block-0.1)
                       ("rust-dispatch" ,rust-dispatch-0.2)
                       ("rust-glib-sys" ,rust-glib-sys-0.18)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.18)
                       ("rust-gtk-sys" ,rust-gtk-sys-0.18)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-objc-foundation" ,rust-objc-foundation-0.1)
                       ("rust-objc-id" ,rust-objc-id-0.1)
                       ("rust-pollster" ,rust-pollster-0.3)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.6)
                       ("rust-urlencoding" ,rust-urlencoding-2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-windows-sys" ,rust-windows-sys-0.48))))
    (home-page "https://github.com/PolyMeilex/rfd")
    (synopsis "Rusty File Dialog")
    (description "This package provides Rusty File Dialog.")
    (license license:expat)))

(define-public rust-libxdo-sys-0.11
  (package
    (name "rust-libxdo-sys")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libxdo-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04ljl0lmirg8a9q7w8ib2sybx35nnzpbw2xciayip0xpwbkvj8yv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-x11" ,rust-x11-2))))
    (home-page "https://github.com/crumblingstatue/rust-libxdo-sys")
    (synopsis "FFI bindings to libxdo")
    (description "This package provides FFI bindings to libxdo.")
    (license license:expat)))

(define-public rust-libxdo-0.6
  (package
    (name "rust-libxdo")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libxdo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nqlina6li1bmap0144h4hdsczyyfyinf87qvrw8xlm3as3kncq0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libxdo-sys" ,rust-libxdo-sys-0.11))))
    (home-page "https://github.com/crumblingstatue/rust-libxdo")
    (synopsis "Bindings to libxdo")
    (description "This package provides Bindings to libxdo.")
    (license license:expat)))

(define-public rust-gtk3-macros-0.18
  (package
    (name "rust-gtk3-macros")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk3-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "179yszj83hgfxl4h4g2zfbsyn9a2zc5zrp6nzqv0fkzi45dkrzsj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://gtk-rs.org/")
    (synopsis
     "UNMAINTAINED Rust bindings for the GTK 3 library (use gtk4-macros instead)")
    (description
     "This package provides UNMAINTAINED Rust bindings for the GTK 3 library (use gtk4-macros instead).")
    (license license:expat)))

(define-public rust-gdk-0.18
  (package
    (name "rust-gdk")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14967h4pac5gjyrd47yls4wbicrzhbwnd4ajisfwjyk2ijalbwnr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cairo-rs" ,rust-cairo-rs-0.18)
                       ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.18)
                       ("rust-gdk-sys" ,rust-gdk-sys-0.18)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.18))))
    (home-page "https://gtk-rs.org/")
    (synopsis
     "UNMAINTAINED Rust bindings for the GDK 3 library (use gdk4 instead)")
    (description
     "This package provides UNMAINTAINED Rust bindings for the GDK 3 library (use gdk4 instead).")
    (license license:expat)))

(define-public rust-atk-0.18
  (package
    (name "rust-atk")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jw2n5xln62px4dh0hxdzbkbfraznkjakwznwhxrjbh72c9646r4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atk-sys" ,rust-atk-sys-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://gtk-rs.org/")
    (synopsis "UNMAINTAINED Rust bindings for the ATK library")
    (description
     "This package provides UNMAINTAINED Rust bindings for the ATK library.")
    (license license:expat)))

(define-public rust-gtk-0.18
  (package
    (name "rust-gtk")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sjh12mvvcmkz54nn30lb2xrzxagshbz1x2i4xfvshpwgccznmpx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atk" ,rust-atk-0.18)
                       ("rust-cairo-rs" ,rust-cairo-rs-0.18)
                       ("rust-field-offset" ,rust-field-offset-0.3)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-gdk" ,rust-gdk-0.18)
                       ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.18)
                       ("rust-gio" ,rust-gio-0.18)
                       ("rust-glib" ,rust-glib-0.18)
                       ("rust-gtk-sys" ,rust-gtk-sys-0.18)
                       ("rust-gtk3-macros" ,rust-gtk3-macros-0.18)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pango" ,rust-pango-0.18)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://gtk-rs.org/")
    (synopsis
     "UNMAINTAINED Rust bindings for the GTK+ 3 library (use gtk4 instead)")
    (description
     "This package provides UNMAINTAINED Rust bindings for the GTK+ 3 library (use gtk4 instead).")
    (license license:expat)))

(define-public rust-muda-0.11
  (package
    (name "rust-muda")
    (version "0.11.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "muda" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lrli6kb67jqd0cbkz1r4v0h6as1n97z793f4br1mz4hb5iffisc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cocoa" ,rust-cocoa-0.25)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-gtk" ,rust-gtk-0.18)
                       ("rust-keyboard-types" ,rust-keyboard-types-0.7)
                       ("rust-libxdo" ,rust-libxdo-0.6)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-png" ,rust-png-0.17)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/amrbashir/muda")
    (synopsis "Menu Utilities for Desktop Applications")
    (description
     "This package provides Menu Utilities for Desktop Applications.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-infer-0.11
  (package
    (name "rust-infer")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "infer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0178zsl23s9a9ymss06x72z39c6y9japy7cvxfp2cnv63aqicv0a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfb" ,rust-cfb-0.7))))
    (home-page "https://github.com/bojand/infer")
    (synopsis
     "Small crate to infer file type based on magic number signatures")
    (description
     "This package provides Small crate to infer file type based on magic number signatures.")
    (license license:expat)))

(define-public rust-global-hotkey-0.5
  (package
    (name "rust-global-hotkey")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "global-hotkey" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02fd1akcc64pdymjbsqv7bsn69ykpar9gh6xgwxmxc4q2lyhjdml"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cocoa" ,rust-cocoa-0.25)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-keyboard-types" ,rust-keyboard-types-0.7)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.52)
                       ("rust-x11-dl" ,rust-x11-dl-2))))
    (home-page "https://github.com/amrbashir/global-hotkey")
    (synopsis "Global hotkeys for Desktop Applications")
    (description
     "This package provides Global hotkeys for Desktop Applications.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sledgehammer-utils-0.3
  (package
    (name "rust-sledgehammer-utils")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sledgehammer_utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bk19982s789bf7ihfni9zrd43winf1m7id3rs1ik5i46nwd9gfy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-hash" ,rust-rustc-hash-1))))
    (home-page "https://github.com/demonthos/sledgehammer_utils/")
    (synopsis "Utilities for sledgehammer-bindgen")
    (description "This package provides Utilities for sledgehammer-bindgen.")
    (license license:expat)))

(define-public rust-sledgehammer-bindgen-macro-0.6
  (package
    (name "rust-sledgehammer-bindgen-macro")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sledgehammer_bindgen_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mag0zbkc69kwcbka7cdhhl5ipzqx7y7ix4f95zjy8kh0gdhcbzn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/demonthos/sledgehammer_bindgen/")
    (synopsis "Fast batched js bindings")
    (description "This package provides Fast batched js bindings.")
    (license license:expat)))

(define-public rust-sledgehammer-bindgen-0.6
  (package
    (name "rust-sledgehammer-bindgen")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sledgehammer_bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cs1zin58x9skf41nir5v77c7ylajp8cyaf15g4mjr0pilbkxs29"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-sledgehammer-bindgen-macro" ,rust-sledgehammer-bindgen-macro-0.6)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/demonthos/sledgehammer_bindgen/")
    (synopsis "Fast batched js bindings")
    (description "This package provides Fast batched js bindings.")
    (license license:expat)))

(define-public rust-dioxus-interpreter-js-0.6
  (package
    (name "rust-dioxus-interpreter-js")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-interpreter-js" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08ksnl4ilg1nmrvq4shic9z0vk8p0bcgirgr0pmv0p571jqhf1rk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-dioxus-core-types" ,rust-dioxus-core-types-0.6)
                       ("rust-dioxus-html" ,rust-dioxus-html-0.6)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-lazy-js-bundle" ,rust-lazy-js-bundle-0.6)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sledgehammer-bindgen" ,rust-sledgehammer-bindgen-0.6)
                       ("rust-sledgehammer-utils" ,rust-sledgehammer-utils-0.3)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3))))
    (home-page "https://dioxuslabs.com")
    (synopsis
     "JS Interpreter for Dioxus - a concurrent renderer-agnostic Virtual DOM for interactive user experiences")
    (description
     "This package provides JS Interpreter for Dioxus - a concurrent renderer-agnostic Virtual DOM for
interactive user experiences.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-history-0.6
  (package
    (name "rust-dioxus-history")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-history" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02brkfy6wn4f4n4kds1j5pg8bn2m949i6wvjc1dz76662qkf5r2s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://dioxuslabs.com")
    (synopsis "History provider for dioxus")
    (description "This package provides History provider for dioxus.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lazy-js-bundle-0.6
  (package
    (name "rust-lazy-js-bundle")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lazy-js-bundle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qk7qasvi2r3klkdwfpb4wzspn77lr1c29aal53lk7cx7ci9d5g4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://dioxuslabs.com")
    (synopsis
     "codegen library to bundle TypeScript into JavaScript without requiring a bundler to be installed")
    (description
     "This package provides a codegen library to bundle @code{TypeScript} into
@code{JavaScript} without requiring a bundler to be installed.")
    (license (list license:expat license:asl2.0))))

(define-public rust-keyboard-types-0.7
  (package
    (name "rust-keyboard-types")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "keyboard-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12jjfk7dwa1cqp6wzw0xl1zzg3arsrnqy4afsynxn2csqfnxql5p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1))))
    (home-page "https://github.com/pyfisch/keyboard-types")
    (synopsis "Contains types to define keyboard related events")
    (description
     "This package contains types to define keyboard related events.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-html-internal-macro-0.6
  (package
    (name "rust-dioxus-html-internal-macro")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-html-internal-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xjas7v0bi16ssh802jps25z1havjlxlppzc37vck8l86ssqgfj3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-convert-case" ,rust-convert-case-0.6)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://dioxuslabs.com")
    (synopsis "HTML function macros for Dioxus")
    (description "This package provides HTML function macros for Dioxus.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-hooks-0.6
  (package
    (name "rust-dioxus-hooks")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-hooks" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l4n322aav8h4w5r4j24d2c34pvmn60h59ma031v5m6r40zjp3ll"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-dioxus-signals" ,rust-dioxus-signals-0.6)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-generational-box" ,rust-generational-box-0.6)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-warnings" ,rust-warnings-0.2))))
    (home-page "https://dioxuslabs.com")
    (synopsis "Basic useful hooks for Dioxus")
    (description "This package provides Basic useful hooks for Dioxus.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-html-0.6
  (package
    (name "rust-dioxus-html")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-html" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vbrvisdwjd2f61d4rsyw3mkwxbavgg94m09j1www87fdw7a9jar"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-dioxus-core-macro" ,rust-dioxus-core-macro-0.6)
                       ("rust-dioxus-core-types" ,rust-dioxus-core-types-0.6)
                       ("rust-dioxus-hooks" ,rust-dioxus-hooks-0.6)
                       ("rust-dioxus-html-internal-macro" ,rust-dioxus-html-internal-macro-0.6)
                       ("rust-dioxus-rsx" ,rust-dioxus-rsx-0.6)
                       ("rust-enumset" ,rust-enumset-1)
                       ("rust-euclid" ,rust-euclid-0.22)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-generational-box" ,rust-generational-box-0.6)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-keyboard-types" ,rust-keyboard-types-0.7)
                       ("rust-lazy-js-bundle" ,rust-lazy-js-bundle-0.6)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://dioxuslabs.com")
    (synopsis
     "HTML Element pack for Dioxus - a concurrent renderer-agnostic Virtual DOM for interactive user experiences")
    (description
     "This package provides HTML Element pack for Dioxus - a concurrent renderer-agnostic Virtual DOM for
interactive user experiences.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-document-0.6
  (package
    (name "rust-dioxus-document")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-document" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wmqg7dcimikzgrd1ndcdb34zvi2b1s7b8n0xqancav6s4a20al0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-dioxus-core-macro" ,rust-dioxus-core-macro-0.6)
                       ("rust-dioxus-core-types" ,rust-dioxus-core-types-0.6)
                       ("rust-dioxus-html" ,rust-dioxus-html-0.6)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-generational-box" ,rust-generational-box-0.6)
                       ("rust-lazy-js-bundle" ,rust-lazy-js-bundle-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://dioxuslabs.com")
    (synopsis "CLI Configuration for dioxus-cli")
    (description "This package provides CLI Configuration for dioxus-cli.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-signals-0.6
  (package
    (name "rust-dioxus-signals")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-signals" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b3j7nqlayr305c8az0l71qbdbmm42y5kvmqr1p3ih52ngdk5q0h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-generational-box" ,rust-generational-box-0.6)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-warnings" ,rust-warnings-0.2))))
    (home-page "https://dioxuslabs.com")
    (synopsis
     "Reactivie signals for Dioxus: Build fullstack web, desktop, and mobile apps with a single codebase")
    (description
     "This package provides Reactivie signals for Dioxus: Build fullstack web, desktop, and mobile apps with
a single codebase.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-devtools-types-0.6
  (package
    (name "rust-dioxus-devtools-types")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-devtools-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zzlxd13x31iix9bazs3583c40840zkxb71fqhxmm70c7jbk897n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://dioxuslabs.com")
    (synopsis "CLI Configuration for dioxus-cli")
    (description "This package provides CLI Configuration for dioxus-cli.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-devtools-0.6
  (package
    (name "rust-dioxus-devtools")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-devtools" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "103817hrmvy52y0m2nx2w02fyzhm8hh500vv30c14678y4076aki"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-dioxus-devtools-types" ,rust-dioxus-devtools-types-0.6)
                       ("rust-dioxus-signals" ,rust-dioxus-signals-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tungstenite" ,rust-tungstenite-0.23)
                       ("rust-warnings" ,rust-warnings-0.2))))
    (home-page "https://dioxuslabs.com/learn/0.4/migration/hot_reload")
    (synopsis "Hot reloading utilities for Dioxus")
    (description "This package provides Hot reloading utilities for Dioxus.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-desktop-0.6
  (package
    (name "rust-dioxus-desktop")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-desktop" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16h9qcglfa987zp077j86f95qgm5qx62r9byycyll2m1wyiwrc5m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-cocoa" ,rust-cocoa-0.25)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-dioxus-cli-config" ,rust-dioxus-cli-config-0.6)
                       ("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-dioxus-devtools" ,rust-dioxus-devtools-0.6)
                       ("rust-dioxus-document" ,rust-dioxus-document-0.6)
                       ("rust-dioxus-history" ,rust-dioxus-history-0.6)
                       ("rust-dioxus-hooks" ,rust-dioxus-hooks-0.6)
                       ("rust-dioxus-html" ,rust-dioxus-html-0.6)
                       ("rust-dioxus-interpreter-js" ,rust-dioxus-interpreter-js-0.6)
                       ("rust-dioxus-signals" ,rust-dioxus-signals-0.6)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-generational-box" ,rust-generational-box-0.6)
                       ("rust-global-hotkey" ,rust-global-hotkey-0.5)
                       ("rust-infer" ,rust-infer-0.11)
                       ("rust-jni" ,rust-jni-0.21)
                       ("rust-lazy-js-bundle" ,rust-lazy-js-bundle-0.6)
                       ("rust-muda" ,rust-muda-0.11)
                       ("rust-ndk" ,rust-ndk-0.9)
                       ("rust-ndk-context" ,rust-ndk-context-0.1)
                       ("rust-ndk-sys" ,rust-ndk-sys-0.6)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-objc-id" ,rust-objc-id-0.1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rfd" ,rust-rfd-0.14)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-tao" ,rust-tao-0.30)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.23)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tray-icon" ,rust-tray-icon-0.19)
                       ("rust-urlencoding" ,rust-urlencoding-2)
                       ("rust-webbrowser" ,rust-webbrowser-0.8)
                       ("rust-wry" ,rust-wry-0.45))))
    (home-page "https://dioxuslabs.com/learn/0.6/getting_started")
    (synopsis "WebView renderer for Dioxus")
    (description "This package provides @code{WebView} renderer for Dioxus.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-rsx-0.6
  (package
    (name "rust-dioxus-rsx")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-rsx" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18asni4yc7813si1nppv4gl2cxmxbg50zx5jj3msgd80b3h8id9y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-proc-macro2-diagnostics" ,rust-proc-macro2-diagnostics-0.10)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://dioxuslabs.com")
    (synopsis
     "Core functionality for Dioxus - a concurrent renderer-agnostic Virtual DOM for interactive user experiences")
    (description
     "This package provides Core functionality for Dioxus - a concurrent renderer-agnostic Virtual DOM for
interactive user experiences.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-core-macro-0.6
  (package
    (name "rust-dioxus-core-macro")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-core-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r00ry4f9vsy2a152hxd6f6sl60yn1vaa6rx1z8qrpxfm969ap0h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-convert-case" ,rust-convert-case-0.6)
                       ("rust-dioxus-rsx" ,rust-dioxus-rsx-0.6)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://dioxuslabs.com")
    (synopsis "Core macro for Dioxus Virtual DOM")
    (description "This package provides Core macro for Dioxus Virtual DOM.")
    (license (list license:expat license:asl2.0))))

(define-public rust-warnings-macro-0.2
  (package
    (name "rust-warnings-macro")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "warnings-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w85dc891dxbwmff36y0cw5ygh1z1mgbljfrcq1r4nz9n0fml6ar"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://dioxuslabs.com")
    (synopsis "macro for defining warnings")
    (description "This package provides a macro for defining warnings.")
    (license (list license:expat license:asl2.0))))

(define-public rust-warnings-0.2
  (package
    (name "rust-warnings")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "warnings" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bc06sdpacdzm0f3fn9m659wkxy6jmalcc4vgir6bawdhfc8kxk4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pin-project" ,rust-pin-project-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-warnings-macro" ,rust-warnings-macro-0.2))))
    (home-page "https://dioxuslabs.com")
    (synopsis
     "crate for defining debug only runtime warnings with a way to opt-out")
    (description
     "This package provides a crate for defining debug only runtime warnings with a
way to opt-out.")
    (license (list license:expat license:asl2.0))))

(define-public rust-longest-increasing-subsequence-0.1
  (package
    (name "rust-longest-increasing-subsequence")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "longest-increasing-subsequence" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11hxv1jya5m9nbs9jj7qh88i646sz9sn47xpzmb10mwhrp90vgdk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/fitzgen/longest-increasing-subsequence")
    (synopsis "Find a longest increasing subsequence of some input sequence")
    (description
     "This package provides Find a longest increasing subsequence of some input sequence.")
    (license (list license:expat license:asl2.0))))

(define-public rust-generational-box-0.6
  (package
    (name "rust-generational-box")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "generational-box" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ayqgva657wsvcrvzk8jm5vm51zydmsrb1n0hsm92span17wywx6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/DioxusLabs/dioxus/")
    (synopsis "box backed by a generational runtime")
    (description
     "This package provides a box backed by a generational runtime.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-core-types-0.6
  (package
    (name "rust-dioxus-core-types")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-core-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fzrv5m8jmgbm4rq8na491lh82ald5vjjghqmavlwms8zb62za4i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://dioxuslabs.com")
    (synopsis "CLI Configuration for dioxus-cli")
    (description "This package provides CLI Configuration for dioxus-cli.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-core-0.6
  (package
    (name "rust-dioxus-core")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13gzmlfp3rklfp2a9caffgkddxsvn6sqwbbfq4rp9r0rl58z80ww"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-const-format" ,rust-const-format-0.2)
                       ("rust-dioxus-core-types" ,rust-dioxus-core-types-0.6)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-generational-box" ,rust-generational-box-0.6)
                       ("rust-longest-increasing-subsequence" ,rust-longest-increasing-subsequence-0.1)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-slotmap" ,rust-slotmap-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-warnings" ,rust-warnings-0.2))))
    (home-page "https://dioxuslabs.com")
    (synopsis
     "Build fullstack web, desktop, and mobile apps with a single codebase")
    (description
     "This package provides Build fullstack web, desktop, and mobile apps with a single codebase.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-config-macro-0.6
  (package
    (name "rust-dioxus-config-macro")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-config-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jcbx4zdg90hhx358lf9150mfrch8rsscbh4l4s2vhxizf1gbjvm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1))))
    (home-page "https://dioxuslabs.com")
    (synopsis "Configuration macros for Dioxus")
    (description "This package provides Configuration macros for Dioxus.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-cli-config-0.6
  (package
    (name "rust-dioxus-cli-config")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus-cli-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11nfdcp33h6s3yiqxsgl5872b93kh0ai4j56v66hdnzzy546klfd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://dioxuslabs.com")
    (synopsis "CLI Configuration for dioxus-cli")
    (description "This package provides CLI Configuration for dioxus-cli.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dioxus-0.6
  (package
    (name "rust-dioxus")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dioxus" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "032qi88kmqpskn0fia5dryn4gs1x2ylfyb82hy7agw808l8lg8k0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-dioxus-cli-config" ,rust-dioxus-cli-config-0.6)
                       ("rust-dioxus-config-macro" ,rust-dioxus-config-macro-0.6)
                       ("rust-dioxus-core" ,rust-dioxus-core-0.6)
                       ("rust-dioxus-core-macro" ,rust-dioxus-core-macro-0.6)
                       ("rust-dioxus-desktop" ,rust-dioxus-desktop-0.6)
                       ("rust-dioxus-devtools" ,rust-dioxus-devtools-0.6)
                       ("rust-dioxus-document" ,rust-dioxus-document-0.6)
                       ("rust-dioxus-fullstack" ,rust-dioxus-fullstack-0.6)
                       ("rust-dioxus-history" ,rust-dioxus-history-0.6)
                       ("rust-dioxus-hooks" ,rust-dioxus-hooks-0.6)
                       ("rust-dioxus-html" ,rust-dioxus-html-0.6)
                       ("rust-dioxus-liveview" ,rust-dioxus-liveview-0.6)
                       ("rust-dioxus-logger" ,rust-dioxus-logger-0.6)
                       ("rust-dioxus-mobile" ,rust-dioxus-mobile-0.6)
                       ("rust-dioxus-router" ,rust-dioxus-router-0.6)
                       ("rust-dioxus-signals" ,rust-dioxus-signals-0.6)
                       ("rust-dioxus-ssr" ,rust-dioxus-ssr-0.6)
                       ("rust-dioxus-web" ,rust-dioxus-web-0.6)
                       ("rust-manganis" ,rust-manganis-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-warnings" ,rust-warnings-0.2))))
    (home-page "https://dioxuslabs.com")
    (synopsis
     "Build fullstack web, desktop, and mobile apps with a single codebase")
    (description
     "This package provides Build fullstack web, desktop, and mobile apps with a single codebase.")
    (license (list license:expat license:asl2.0))))

