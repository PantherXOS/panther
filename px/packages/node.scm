(define-module (px packages node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages))

(define-public pnpm
  (package
    (name "pnpm")
    (version "8.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append 
          "https://github.com/pnpm/pnpm/releases/download/v"
          version "/pnpm-linuxstatic-"
          (match (or (%current-system) (%current-target-system))
            ("x86_64-linux" "x64")
            ("aarch64-linux" "arm64"))))
        (sha256 
          (base32 
            "0r7x1zamcznv3d4s0y410xg99qa3j8fh3gygqp5gaiikhwfln7kh"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
        (use-modules ((guix build utils)))
        (let* ((source (assoc-ref %build-inputs "source"))
               (bin (string-append %output "/bin"))
               (exe (string-append bin "/pnpm")))
          (mkdir-p bin)
          (copy-file source exe)
          (chmod exe #o755)))))
    (home-page "https://pnpm.io")
    (synopsis "Fast, disk space efficient package manager for nodejs")
    (description "PNPM uses a content-addressable filesystem to
store all files from all module directories on a disk")
    (license license:expat)))

(define-public pnpm-7
  (package
    (inherit pnpm)
    (name "pnpm")
    (version "7.32.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append 
          "https://github.com/pnpm/pnpm/releases/download/v"
          version "/pnpm-linuxstatic-"
          (match (or (%current-system) (%current-target-system))
            ("x86_64-linux" "x64")
            ("aarch64-linux" "arm64"))))
        (sha256 
          (base32 
            "1k3n24ink74ajs8nd0p6y6238vi41n5bvrk8clnydgwg11ax1zx0"))))))