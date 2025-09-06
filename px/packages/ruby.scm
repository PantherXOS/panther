;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages ruby)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system ruby)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages ruby-xyz))

(define-public ruby-jekyll-polyglot
  (package
    (name "ruby-jekyll-polyglot")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "jekyll-polyglot" version))
       (sha256
        (base32 "1ly2mdaraihff4qj30002yrnh7gy2vl2m9m2ghzw2wggnmg9f088"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No Rakefile
    (propagated-inputs (list jekyll))
    (synopsis "Fast open source i18n plugin for Jekyll blogs.")
    (description "Fast open source i18n plugin for Jekyll blogs.")
    (home-page "https://polyglot.untra.io/")
    (license license:expat)))