;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

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
    (version "0.1.5")
    (source
    (origin
      (method url-fetch)
      (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
      (sha256
       (base32 "0q8jw8hl67l24d4anf22npjglsmp88y4hza5y6vai42vinfydwz9"))))
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
    (home-page "https://f-a.nz/")
    (synopsis "GStreamer WebRTC C++ library")
    (description "Ease integration of GStreamer WebRTC in C++/Qt applications.")
    (license license:expat)))

(define-public webrtc-cpp-demo
  (package
    (inherit webrtc-cpp)
    (name "webrtc-cpp-demo")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure-for-demo
           (lambda _
             ;; Create a custom CMakeLists.txt that only builds the demo
             (call-with-output-file "CMakeLists.txt"
               (lambda (port)
                 (display "cmake_minimum_required(VERSION 3.14)
project(webrtc_demo)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# Find required packages (same as main library)
find_package(PkgConfig REQUIRED)

set(GSTREAMER_MODULES
    gstreamer-1.0
    gstreamer-sdp-1.0
    gstreamer-webrtc-1.0
    gstreamer-gl-1.0
    libsoup-3.0
    json-glib-1.0
)

pkg_check_modules(GSTREAMER REQUIRED IMPORTED_TARGET ${GSTREAMER_MODULES})
find_package(Qt5 COMPONENTS Core Gui Qml Quick REQUIRED)

# Find webrtclib
find_package(webrtclib REQUIRED)

# Build the demo
add_subdirectory(tests)

# Install the demo binary
install(TARGETS webrtc-cpp-demo
    RUNTIME DESTINATION bin
)
" port)))
             #t))
         (add-after 'configure-for-demo 'fix-tests-cmake
           (lambda _
             ;; Ensure the tests/CMakeLists.txt uses the installed webrtclib
             (substitute* "tests/CMakeLists.txt"
               (("webrtclib") "webrtclib::webrtclib"))
             #t))
         (add-after 'fix-tests-cmake 'fix-include-path
           (lambda _
             ;; Fix the include path in main.cpp to use the installed header location
             (substitute* "tests/main.cpp"
               (("#include <webrtc.hpp>") "#include <webrtclib/webrtc.hpp>"))
             #t)))))
    (inputs (list webrtc-cpp
                  qtbase-5
                  qtwayland-5
                  qtdeclarative-5
                  qtmultimedia-5
                  qtquickcontrols-5
                  qtquickcontrols2-5
                  qtwebsockets-5
                  libnice
                  libsoup
                  json-glib
                  cli11
                  xz))
    (propagated-inputs (list libnice
                             gstreamer
                             gst-plugins-base
                             gst-plugins-good
                             gst-plugins-bad
                             gst-plugins-good-qmlgl))
    (native-inputs (list pkg-config))
    (home-page "https://f-a.nz/")
    (synopsis "WebRTC C++ library demo application")
    (description "Demo application showcasing the webrtc-cpp library functionality.")
    (license license:expat)))