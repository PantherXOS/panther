;;; Package Repository for GNU Guix
;;; Copyright © 2021-2023 Fakhri Sajadi <f.sajadi@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages mastodon)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages serialization)
  #:use-module (px packages common)
  #:use-module (px packages common)
  #:use-module (px packages networking))

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

(define-public px-mastodon-service
  (package
    (name "px-mastodon-service")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/px-mastodon-service_v"
             version ".tgz"))
       (sha256
        (base32 "0v7qmimrxqk4d3c4x3f7lmxf976bb099xyrqn1wg6ypsm6myf04d"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("mastodonpp" ,mastodonpp)
              ("rapidjson" ,rapidjson)
              ("yaml-cpp" ,yaml-cpp)
              ("openssl" ,openssl)
              ("curl" ,curl)
              ("nng" ,nng-1.5)
              ("zlib" ,zlib)
              ("capnproto" ,capnproto-0.9)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Mastodon Management Service")
    (description "A Mastodon API.")
    (license license:expat)))
