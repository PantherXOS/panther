(define-module (px packages log)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages tls)
  #:use-module (px packages crates-io))

(define-public remote_syslog2
  (package
    (name "remote_syslog2")
    (version "0.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/papertrail/remote_syslog2/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "09mrg6kxxrghhqg3ci8ryad9qixrdifiynffcxn01pgb0hkf3lg8"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f
       #:import-path "github.com/papertrail/remote_syslog2"))
    (home-page "https://www.papertrailapps.com/")
    (synopsis "Remote_syslog tails")
    (description
     "Remote_syslog tails one or more log files and sends syslog messages to a remote central syslog server. 
It generates packets itself, ignoring the system syslog daemon, so its configuration doesn't affect system-wide logging.")
    (license license:expat)))

(define-public loggily
  (package
    (name "loggily")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "-" version
                           ".crate"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z0008ydim1x9z7gbn87zrng8ykyh0hpasvzfxyx4r48a6mw98ra"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-postgres" ,rust-postgres-0.19)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-xmlrpc" ,rust-xmlrpc-0.15)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.8))))
    (inputs (list openssl))
    (home-page "https://pantherx.org")
    (synopsis "A simple logging library for Rust")
    (description "A simple logging library for Rust")
    (license license:expat)))
