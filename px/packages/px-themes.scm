;;; Theme Packages Module for PantherX
;;; Author: Hamzeh Nasajpour (h.nasajpour@pantherx.org)
;;;

(define-module (px packages px-themes)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web))

(define-public px-settings-service-plugin-theme-dark-bright
  (package
    (name "px-settings-service-plugin-theme-dark-bright")
    (version "0.2.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append 
       "https://source.pantherx.org/" name "_" version
       ".tgz"))
      (sha256
       (base32 "0svx2z0ws4ck5p1bzw57y4rasmjshna4wx2pqlmak621ghrrnfvn"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-screenshot-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "dark/theme.conf" 
                            (("dark.jpg") (string-append out "/share/px/themes/dark/dark.jpg")))
               (substitute* "bright/theme.conf" 
                            (("bright.jpg") (string-append out "/share/px/themes/bright/bright.jpg")))
               #t)))
         )))
    (synopsis "PantherX Dark and Bright theme")
    (home-page "https://git.pantherx.org/development/plugins/px-settings-service-plugin-theme-dark-bright")
    (description "PantherX Dark and Bright theme")
    (license license:gpl3+)))
