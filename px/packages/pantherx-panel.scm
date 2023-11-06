;;; Settings Packages Module for PantherX
;;; Hamzeh Nasajpour (h.nasajpour@pantherx.org)

(define-module (px packages pantherx-panel)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages xorg)
  #:use-module (px packages common)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages web)
  #:use-module (px packages inspection)
  #:use-module (px packages desktop-tools)
  #:use-module (px packages networking))

(define-public pantherx-panel
  (package
    (inherit lxqt-panel)
    (name "pantherx-panel")
    (version "1.3.0-u1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/lxqt-panel_" version
                           ".tgz"))
       (sha256
        (base32 "149k9s47wvb9v49b4hzsz2g1yrisr03n4pbr9ignm5s2j5z8z9if"))))
    (build-system cmake-build-system)
    (inputs `(("zlib" ,zlib)
              ("nng" ,nng-1.5)
              ("rapidjson" ,rapidjson)
              ("capnproto" ,capnproto-0.9)
              ,@(package-inputs lxqt-panel)))
    (native-inputs `(("nng" ,nng-1.5)
                     ,@(package-native-inputs lxqt-panel)))
    (propagated-inputs `(("px-network-inspection" ,px-network-inspection)
                         ("px-recoll" ,px-recoll)
                         ,@(package-propagated-inputs lxqt-panel)))))
