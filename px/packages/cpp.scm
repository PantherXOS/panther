(define-module (px packages cpp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system qt)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gnome) ;; libsoup, json-glib
  #:use-module (gnu packages networking) ;; libnice
  #:use-module (gnu packages cpp) ;; cli11
  #:use-module (gnu packages compression) ;; xz
  #:use-module (px packages gstreamer) ;; gst-plugins-good-qmlgl
  #:use-module (ice-9 match))

(define-public cpp-qr-code-generator
  (package
    (name "qr-code-generator")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/nayuki/QR-Code-generator/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0q5f6iywzi8hzvpjnzd5rymkkqaz5pa77x7a5sa1qlkg7p9s9h1f"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       ;; The build scripts are are in cpp/.
       (modify-phases %standard-phases
         (add-after 'unpack 'pre-configure
           (lambda _
             (chdir "cpp")))
           ;; The source does not have a ./configure script.
           (delete 'configure)
           (replace 'install
           ;; The Makefile lacks an ‘install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib"))
                      (include (string-append out "/include")))
                    (mkdir-p lib)
                    (mkdir-p include)
                    (install-file "qrcodegen.hpp" include)
                    (install-file "qrcodegen.cpp" include)
                    (install-file "qrcodegen.o" lib)
                    (install-file "libqrcodegencpp.a" lib)
             #t))))))
    (home-page "https://github.com/nayuki/QR-Code-generator")
    (synopsis "High-quality QR Code generator library - C++ version")
    (description
     "This project aims to be the best, clearest QR Code generator library
in multiple languages.")
    (license license:expat)))

(define-public webrtc-cpp
  (package
    (name "webrtc-cpp")
    (version "0.1.4")
    (source
    (origin
      (method url-fetch)
      (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
      (sha256
       (base32 "0zgiabg9ahv1s050pcsw71v4qhszjjlp36pab17i77alfhh92idz"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f))
    (inputs (list qtbase-5
                  qtwayland-5
                  qtdeclarative-5
                  qtmultimedia-5
                  qtquickcontrols-5
                  qtquickcontrols2-5
                  qtwebsockets-5
                  gstreamer
                  gst-plugins-base
                  gst-plugins-good
                  gst-plugins-bad
                  gst-plugins-good-qmlgl
                  libnice
                  libsoup
                  json-glib
                  cli11
                  xz))
    (native-inputs (list pkg-config))
    (home-page "https://www.pantherx.org/")
    (synopsis "GStreamer WebRTC C++ library")
    (description "Ease integration of GStreamer WebRTC in C++/Qt applications.")
    (license license:expat)))