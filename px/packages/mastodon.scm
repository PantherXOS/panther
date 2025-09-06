;;; Package Repository for GNU Guix
;;; Copyright © 2021-2023 Fakhri Sajadi <f.sajadi@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages mastodon)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages curl))

(define-public mastodonpp
  (package
    (name "mastodonpp")
    (version "0.5.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://schlomp.space/tastytea/mastodonpp/archive/" version
             ".tar.gz"))
       (sha256
        (base32 "1vga22c85r86hidvfqysfj01d2y6w69m9rkmc1nsr8ffglcw83qy"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("curl" ,curl)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://schlomp.space/tastytea/mastodonpp")
    (synopsis "C++ library for working with Mastodon REST API")
    (description
     "Mastodonpp is a C++ wrapper for the Mastodon API.
You submit an API call and get the raw JSON that you can then transform into easy to use abstractions.")
    (license license:expat)))