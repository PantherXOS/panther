(define-module (px packages node)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages))

(define-public pnpm
  (package
    (name "pnpm")
    (version "10.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pnpm/pnpm/releases/download/v"
                           version "/pnpm-linuxstatic-"
                           (match (or (%current-system)
                                      (%current-target-system))
                             ("x86_64-linux" "x64")
                             ("aarch64-linux" "arm64"))))
       (sha256
        (base32 "1nl400limy4hqmq87mwbbfxazvj996lip0qrwx7hqcznsikj4gqn"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
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

(define-public pnpm-9
  (package
    (name "pnpm")
    (version "9.15.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pnpm/pnpm/releases/download/v"
                           version "/pnpm-linuxstatic-"
                           (match (or (%current-system)
                                      (%current-target-system))
                             ("x86_64-linux" "x64")
                             ("aarch64-linux" "arm64"))))
       (sha256
        (base32 "02nwfl15rhx0vjfm5za6grg1748kdlqkw6yfz21g4sgcv3sfiadb"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
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