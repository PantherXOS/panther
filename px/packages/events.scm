;;; Central Event Management Service Packages Module for PantherX
;;; Reza Alizadeh Majd (r.majd@pantherx.org)

(define-module (px packages events)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages serialization)
  #:use-module (px packages common)
  #:use-module (px packages networking))

(define-public px-events-service
  (package
    (name "px-events-service")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/px-events-service_v"
                           version ".tgz"))
       (sha256
        (base32 "06030lzzpkw13q7ggr6iw4ywdra57cwc7kn870asd73n3gszyc6r"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("capnproto" ,capnproto-0.9)
              ("nng" ,nng-1.5)
              ("yaml-cpp" ,yaml-cpp)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Central Event Management Service")
    (description "This package provides background services to manage
event routing in PantherX")
    (license license:expat)))
