(define-module (px packages networking)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses)
                :prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages base)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml))

(define-public nebula
  (package
    (name "nebula")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/slackhq/nebula/releases/download/v" version
             "/nebula-linux-amd64.tar.gz"))
       (sha256
        (base32 "09rrxxrz0rppw2681w18czphp77jshw1niqkvgrxy0pq2yln7lr0"))))
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
    (version "5.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/v2fly/v2ray-core/releases/download/v" version
             "/v2ray-linux-64.zip"))
       (sha256
        (base32 "05mmzk6b2f9ivyb749b70s8l91q9n5h38zbzmzn3hzj0hf4pzhrc"))))
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
    (version "1.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/XTLS/Xray-core/releases/download/v" version
             "/Xray-linux-64.zip"))
       (file-name (string-append "Xray-linux-64-" version ".zip"))
       (sha256
        (base32 "0lrl6wj15wagyky5zy1ihh6ilvxf80yn7p83gdqp5m44zdplf8fn"))))
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
    (inputs `(("mbedtls" ,mbedtls-apache)))
    (synopsis "Lightweight messaging library")
    (description
     "NNG project is a rewrite of the scalability protocols library
known as libnanomsg, and adds significant new capabilities, while retaining
compatibility with the original.  It is a lightweight, broker-less library,
offering a simple API to solve common recurring messaging problems, such as
publish/subscribe, RPC-style request/reply, or service discovery.")
    (home-page "https://nng.nanomsg.org/")
    (license license:expat)))