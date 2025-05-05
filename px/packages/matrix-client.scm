(define-module (px packages matrix-client)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (px packages aidc)
  #:use-module (px packages gstreamer)
  #:use-module (px packages common)
  #:use-module (px packages library)
  #:use-module (px packages logging)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public qmtxclient
  (package
    (name "qmtxclient")
    (version "0.8.2-4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "07fc46a4nqs509i8biqb6ys74sgj0rdzwpvn7kfk6xf4gv172csn"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs (list boost
                  qtbase-5
                  qtwayland-5
                  json-modern-cxx
                  libevent
                  libolm
                  libsodium
                  openssl
                  spdlog-fmt
                  zlib))
    (native-inputs (list pkg-config))
    (home-page "https://git.pantherx.org/development/libraries/qmtxclient")
    (synopsis
     "Client API library for Matrix forked from mtxclient, replacing the network layer with Qt")
    (description
     "Client API library for Matrix forked from mtxclient, replacing the network layer with Qt.")
    (license license:expat)))

(define-public matrix-client-library
  (package
    (name "matrix-client-library")
    (version "0.1.43")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "1j9jywp29v5rjlpvf9lc223136yjlymazxs4xksl13xn80m8q8y4"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-source
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((pulseaudio (assoc-ref inputs "pulseaudio")))
                        (substitute* "src/voip/AudioDevices.cpp"
                          (("pactl")
                           (string-append pulseaudio "/bin/pactl")))
                        (substitute* "src/voip/AudioDeviceControl.cpp"
                          (("pacmd")
                           (string-append pulseaudio "/bin/pacmd"))) #t))))))
    (inputs `(("cmark" ,cmark)
              ("gst-plugins-base" ,gst-plugins-base)
              ("gst-plugins-bad" ,gst-plugins-bad)              ;sdp & webrtc for voip
              ("gst-plugins-good-qt" ,gst-plugins-good-qmlgl)   ;rtpmanager for voip
              ("json-modern-cxx" ,json-modern-cxx)
              ("libevent" ,libevent)
              ("libnice" ,libnice)                              ;for voip
              ("libolm" ,libolm)
              ("lmdb" ,lmdb)
              ("lmdbxx" ,lmdbxx)
              ("qmtxclient" ,qmtxclient)
              ("openssl" ,openssl)
              ("pulseaudio" ,pulseaudio)
              ("qtbase" ,qtbase-5)
              ("qtwayland-5" ,qtwayland-5)
              ("qtdeclarative" ,qtdeclarative-5)
              ("qtmultimedia" ,qtmultimedia-5)
              ("qtquickcontrols2" ,qtquickcontrols2-5)
              ("qtsvg" ,qtsvg-5)
              ("spdlog" ,spdlog-fmt)
              ("zlib" ,zlib)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page
     "https://git.pantherx.org/development/libraries/matrix-client-library")
    (synopsis "Client library for Matrix using Qt and C++17")
    (description
     "Provide a library for using the Matrix protocol that
feels more like a mainstream chat app and less like an IRC client.
Many matrix features are supported, including user registration, rooms, typing
notification, emojis, E2E encryption, and voip calls.")
    (license license:gpl3+)))

(define-public matrix-client-library-with-ciba
  (package
    (inherit matrix-client-library)
    (name "matrix-client-library-with-ciba")
    (arguments
     `(#:configure-flags '("-DCIBA_AUTHENTICATION=ON")
       ,@(package-arguments matrix-client-library)))
    (inputs `(("px-auth-library-cpp" ,px-auth-library-cpp)
              ,@(package-inputs matrix-client-library)))))

(define-public matrix-client-gui-library
  (package
    (name "matrix-client-gui-library")
    (version "0.1.55")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/matrix-client_"
                           version ".tgz"))
       (sha256
        (base32 "1xl0pxf9j9pqnn7rmq728pwrlwrh7r7xa5wh5fd21y06i9blb9bd"))))
    (arguments
     `(#:tests? #f ;no tests
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "MatrixClientLib.pro"
                        (("/usr")
                         (assoc-ref outputs "out")))
                      (invoke "qmake" "MatrixClientLib.pro"))))))
    (build-system qt-build-system)
    (inputs (list blurhash
                  json-modern-cxx
                  libevent
                  libolm
                  lmdb
                  lmdbxx
                  matrix-client-library-with-ciba
                  px-auth-library-cpp
                  qmtxclient
                  openssl
                  pulseaudio
                  qtbase-5
                  qtwayland-5
                  qtdeclarative-5
                  qtgraphicaleffects
                  qtlocation-5
                  qtmultimedia-5
                  qtquickcontrols-5
                  qtquickcontrols2-5
                  qtsvg-5
                  scodes
                  spdlog-fmt
                  xcb-util-wm
                  zlib
                  zxing-cpp-08978e2))
    (propagated-inputs (list libnice                  ;for voip
                             gst-plugins-good-qmlgl   ;rtpmanager for voip
                             gst-plugins-base
                             gst-plugins-bad))        ;sdp & webrtc for voip
    (native-inputs (list pkg-config qttools-5))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public matrix-client
  (package
    (inherit matrix-client-gui-library)
    (name "matrix-client")
    (arguments
     `(#:tests? #f ;no tests
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "MatrixClientApp.pro"
                        (("/usr")
                         (assoc-ref outputs "out")))
                      (substitute* "configurations/configurations.pri"
                        (("/usr")
                         (assoc-ref outputs "out")))
                      (invoke "qmake" "MatrixClientApp.pro"))))))
    (inputs `(("matrix-client-gui-library" ,matrix-client-gui-library)
              ,@(package-inputs matrix-client-gui-library)))))

(define-public matrix-client-call-auto-accept
  (package
    (inherit matrix-client-gui-library)
    (name "matrix-client-call-auto-accept")
    (version "0.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0n8zfvpf40p0bmv41csz3gq40nfcw8xwb5nv8nad9ixxx7c08jm0"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("matrix-client-gui-library" ,matrix-client-gui-library)
              ,@(package-inputs matrix-client-gui-library)))))
