;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages networking)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses)
                :prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages base)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match))

(define-public nebula
  (package
    (name "nebula")
    (version "1.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/slackhq/nebula/releases/download/v" version
             "/nebula-linux-amd64.tar.gz"))
       (sha256
        (base32 "1zvdhnkc06c3aaqbzzjjqlc72psg1jfga2qr9yb95giz0dqgzx9p"))))
    (build-system binary-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'adjust-paths
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((bin (string-append (assoc-ref outputs "out")
                                                "/bin")))
                        (mkdir-p bin)
                        (install-file "nebula" bin)
                        (install-file "nebula-cert" bin)))))))
    (inputs `(("expat" ,expat)))
    (home-page "https://github.com/slackhq/nebula")
    (synopsis
     "A scalable overlay networking tool with a focus on performance, simplicity and security")
    (description
     "Nebula is a scalable overlay networking tool with a focus on performance, simplicity
and security.It lets you seamlessly connect computers anywhere in the world. Nebula is portable,
and runs on Linux, OSX, Windows, iOS, and Android. It can be used to connect a small number of computers,
but is also able to connect tens of thousands of computers.")
    (license license:expat)))

(define-public v2ray
  (package
    (name "v2ray")
    (version "5.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/v2fly/v2ray-core/releases/download/v" version
             "/v2ray-linux-64.zip"))
       (sha256
        (base32 "127ka0cms2i0zi94845g1pps1nwab7lclr5qwhplk74q5qsnjg44"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("v2ray" "/bin/")
                        ("geoip.dat" "/bin/")
                        ("geosite.dat" "/bin/")
                        ("config.json" "/bin/"))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "unzip" source))))))
    (native-inputs (list unzip))
    (home-page "https://tricks.aseman.io")
    (synopsis
     " A platform for building proxies to bypass network restrictions.")
    (description
     "Project V is a set of network tools that help you to build your
own computer network. It secures your network connections and thus protects your
privacy. See our website for more information.")
    (license license:expat)))

(define-public xray-core
  (package
    (name "xray-core")
    (version "25.10.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/XTLS/Xray-core/releases/download/v" version
             "/Xray-linux-64.zip"))
       (file-name (string-append "Xray-linux-64-" version ".zip"))
       (sha256
        (base32 "0f29ixjyrgxd0r5sdsqmcwfgdvbjg1kv71bz7nv9y715q5has8nz"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("xray" "/bin/")
                        ("geoip.dat" "/bin/")
                        ("geosite.dat" "/bin/"))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "unzip" source))))))
    (native-inputs (list unzip))
    (home-page "https://tricks.aseman.io")
    (synopsis
     " A platform for building proxies to bypass network restrictions.")
    (description
     "Project V is a set of network tools that help you to build your
own computer network. It secures your network connections and thus protects your
privacy. See our website for more information.")
    (license license:expat)))

(define-public nng-1.5
  (package
    (name "nng")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nanomsg/nng")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sap0iny3z9lhmaiassv8jc399md1307y32xxx3mrr74jcpcrf59"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DNNG_ENABLE_COVERAGE=ON"
                               "-DNNG_ENABLE_TLS=ON" "-DBUILD_SHARED_LIBS=ON")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-failing-tests
                    (lambda _
                      ;; These tests require network access.
                      (substitute* "tests/CMakeLists.txt"
                        (("add_nng_test1\\(httpclient 60 NNG_SUPP_HTTP\\)")
                         "")
                        (("add_nng_test\\(multistress 60\\)")
                         "")
                        (("add_nng_test\\(tls 60\\)")
                         ""))
                      (substitute* "src/supplemental/websocket/CMakeLists.txt"
                        (("nng_test\\(wssfile_test\\)")
                         ""))
                      (substitute* "src/sp/transport/ws/CMakeLists.txt"
                        (("nng_test_if\\(WS_ON ws_test\\)")
                         ""))
                      (substitute* "src/sp/transport/tcp/CMakeLists.txt"
                        (("nng_test\\(tcp_test\\)")
                         ""))
                      (substitute* "src/platform/CMakeLists.txt"
                        (("nng_test\\(resolver_test\\)")
                         ""))
                      #t)))))
    (native-inputs `(("ksh" ,oksh)))
    (inputs `(("mbedtls" ,mbedtls)))
    (synopsis "Lightweight messaging library")
    (description
     "NNG project is a rewrite of the scalability protocols library
known as libnanomsg, and adds significant new capabilities, while retaining
compatibility with the original.  It is a lightweight, broker-less library,
offering a simple API to solve common recurring messaging problems, such as
publish/subscribe, RPC-style request/reply, or service discovery.")
    (home-page "https://nng.nanomsg.org/")
    (license license:expat)))

;;
;; Tailscale
;;
;; Derived from guix-tailscale by Brennan Vincent
;; https://github.com/umanwizard/guix-tailscale
;; Licensed under the Apache License, Version 2.0

(define-record-type* <go-git-reference>
  go-git-reference make-go-git-reference
  go-git-reference?
  (url go-git-reference-url)
  (commit go-git-reference-commit)
  (sha go-git-reference-sha256))

(define-record-type* <go-url-reference>
  go-url-reference make-go-url-reference
  go-url-reference?
  (url go-url-reference-url)
  (sha go-url-reference-sha))

(define* (go-fetch-vendored uri hash-algorithm hash-value name #:key system)
  (let ((src
         (match uri
           (($ <go-git-reference> url commit sha)
            (origin
              (method git-fetch)
              (uri (git-reference
                    (url url)
                    (commit commit)))
              (sha256 sha)))
           (($ <go-url-reference> url commit sha)
            (origin
              (method url-fetch)
              (uri url)
              (sha256 sha)))))
        (name (or name "go-git-checkout")))
    (gexp->derivation
     (string-append name "-vendored.tar.gz")
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))
           (let ((inputs (list
                          #+go-1.23
                          #+tar
                          #+bzip2
                          #+gzip)))
             (set-path-environment-variable "PATH" '("/bin") inputs))
           (mkdir "source")
           (chdir "source")
           (if (file-is-directory? #$src)
               (begin
                 (copy-recursively #$src "."
                                   #:keep-mtime? #t)
                 (for-each (lambda (f)
                             (false-if-exception (make-file-writable f)))
                           (find-files ".")))
               (begin
                 (cond
                  ((string-suffix? ".zip" #$src)
                   (invoke "unzip" #$src))
                  ((tarball? #$src)
                   (invoke "tar" "xvf" #$src))
                  (else
                   (let ((name (strip-store-file-name #$src))
                         (command (compressor #$src)))
                     (copy-file #$src name)
                     (when command
                       (invoke command "--decompress" name)))))))

           (setenv "GOCACHE" "/tmp/gc")
           (setenv "GOMODCACHE" "/tmp/gmc")
           (setenv "SSL_CERT_DIR" #+(file-append nss-certs "/etc/ssl/certs/"))

           (invoke "go" "mod" "vendor")

           (invoke "tar" "czvf" #$output
                   "--mtime=@0"
                   "--owner=root:0"
                   "--group=root:0"
                   "--sort=name"
                   "--hard-dereference"
                   ".")))
     #:hash hash-value
     #:hash-algo hash-algorithm)))

(define-public tailscale
  (let ((version "1.74.1"))
    (package
      (name "tailscale")
      (version version)
      (source (origin
                (method go-fetch-vendored)
                (uri (go-git-reference
                      (url "https://github.com/tailscale/tailscale")
                      (commit "v1.74.1")
                      (sha (base32 "0ncck013rzbrzcbpya1fq41jrgzxw22pps77l9kb7kx06as8bggb"))))
                (sha256
                 (base32
                  "19sv3q0hgb1h5v75c8hrkna4xgbgrs0ym2kvq16rbn9kr0hjjr1j"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "tailscale.com/cmd/tailscale"
         #:unpack-path "tailscale.com"
         #:install-source? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'check))
         #:go ,go-1.23))
      (home-page "https://tailscale.com")
      (synopsis "Tailscale VPN client")
      (description "Tailscale is a zero-config VPN based on WireGuard.
This package provides the Tailscale client command-line interface.")
      (license license:bsd-3))))

(define-public tailscaled
  (let ((import-path "tailscale.com/cmd/tailscaled"))
    (package
      (inherit tailscale)
      (name "tailscaled")
      (arguments
       (substitute-keyword-arguments (package-arguments tailscale)
         ((#:import-path _ #f)
          import-path)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (replace 'build
                (lambda _
                  (unsetenv "GO111MODULE")
                  (chdir "./src/tailscale.com")
                  (invoke "go" "build" "-o" "tailscaled"
                          #$import-path)
                  (chdir "../..")))
              (replace 'install
                (lambda _
                  (install-file "src/tailscale.com/tailscaled"
                                (string-append #$output "/bin"))))))))
      (synopsis "Tailscale VPN daemon")
      (description "Tailscale is a zero-config VPN based on WireGuard.
This package provides the Tailscale daemon (tailscaled) which manages
the VPN connection."))))