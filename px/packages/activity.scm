(define-module (px packages activity)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (px packages common)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages serialization))
  
 
(define-public px-org-remote-user-activity-service
  (package
    (name "px-org-remote-user-activity-service")
    (version "0.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
         "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256
          (base32
            "0zjry2gflrk9vpwhlyy4ik8c4gnh8sz4lay3dfrm789ym0yv5lxd"))))
    (build-system cmake-build-system)
    (arguments
      `(#:configure-flags '("../") #:tests? #f))
    (inputs `(
          ("yaml-cpp", yaml-cpp)
	  ("rapidjson", rapidjson)
	  ("nss-certs",nss-certs)
          ("openssl",openssl)
	  ("curl",curl)))
 ;   (native-inputs `(
;	      ("pkg-config" ,pkg-config)
 ;         ))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX PXCENTRAL Activity Service")
    (description "Report Activity Watch events to Central Management")
    (license license:expat)))


     
