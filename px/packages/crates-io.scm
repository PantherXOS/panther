(define-module (px packages crates-io)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-gtk)
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
                       ("rust-cocoa" ,rust-cocoa-0.17)
                       ("rust-core-foundation" ,rust-core-foundation-0.6)
                       ("rust-core-graphics" ,rust-core-graphics-0.16)
                       ("rust-gl-generator" ,rust-gl-generator-0.9)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-osmesa-sys" ,rust-osmesa-sys-0.1)
                       ("rust-shared-library" ,rust-shared-library-0.1)
                       ("rust-wayland-client" ,rust-wayland-client-0.20)
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