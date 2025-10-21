;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages golang-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz))

(define-public go-github-com-rxwycdh-rxhash
  (package
    (name "go-github-com-rxwycdh-rxhash")
    (version "0.0.0-20230131062142-10b7a38b400d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rxwycdh/rxhash")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qw4kn5r0xjfy9mycv57f7lmlpksybzr2qcdr4713svrxakwmgyz"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/rxwycdh/rxhash"))
    (home-page "https://github.com/rxwycdh/rxhash")
    (synopsis "Create unique hash values for Go structs")
    (description
     "rxhash is a Go library for creating a unique hash value for struct in Go
with data consistency.")
    (license license:expat)))

(define-public go-github-com-sj14-astral
  (package
    (name "go-github-com-sj14-astral")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sj14/astral")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m4qirl3mrdpm1dw9lgfj6p7jsyy60kyhhzfkikxbf471wk5apba"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/sj14/astral/pkg/astral"
           #:unpack-path "github.com/sj14/astral"))
    (propagated-inputs
     (list go-github-com-stretchr-testify
           go-github-com-logrusorgru-aurora-v3))
    (home-page "https://github.com/sj14/astral")
    (synopsis "Astronomical calculations for sun and moon positions")
    (description
     "This package provides calculations for the position of the sun and moon.")
    (license license:asl2.0)))
