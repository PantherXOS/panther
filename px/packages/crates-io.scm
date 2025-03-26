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
     `(#:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
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