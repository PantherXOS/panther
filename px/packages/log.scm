;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

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
  #:use-module (gnu packages crates-database)
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
