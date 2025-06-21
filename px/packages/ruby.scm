;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

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
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "jekyll-polyglot" version))
       (sha256
        (base32 "0g7z5psi72pclkq6z2spqlyabw9jj11w1al9fmb2005ik8yn0ryr"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No Rakefile
    (propagated-inputs (list jekyll))
    (synopsis "Fast open source i18n plugin for Jekyll blogs.")
    (description "Fast open source i18n plugin for Jekyll blogs.")
    (home-page "https://polyglot.untra.io/")
    (license license:expat)))