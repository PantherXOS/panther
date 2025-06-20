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
  #:use-module (gnu packages networking) ;; libnice
  #:use-module (gnu packages cpp) ;; cli11
  #:use-module (gnu packages compression) ;; xz
  #:use-module (gnu packages boost) ;; boost
  #:use-module (gnu packages web) ;; rapidjson, websocketpp
  #:use-module (gnu packages tls) ;; openssl
  #:use-module (gnu packages networking) ;; asio
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

(define-public cpp-webrtc
  (package
    (name "cpp-webrtc")
    (version "0.3.0")
    (source
    (origin
      (method url-fetch)
      (uri (string-append "https://source.pantherx.org/webrtc-cpp_v" version ".tgz"))
      (sha256
       (base32 "12k910nq20crkghvzb1pwil7iph2brqf94cfcc31px1nrzqv359w"))))
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
                  cli11
                  xz))
    (native-inputs (list pkg-config))
    (home-page "https://f-a.nz/")
    (synopsis "GStreamer WebRTC C++ library")
    (description "Ease integration of GStreamer WebRTC in C++/Qt applications.")
    (license license:expat)))

(define-public cpp-webrtc-demo
  (package
    (inherit cpp-webrtc)
    (name "cpp-webrtc-demo")
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
find_package(CLI11 REQUIRED)

set(GSTREAMER_MODULES
    gstreamer-1.0
    gstreamer-sdp-1.0
    gstreamer-webrtc-1.0
    gstreamer-gl-1.0
)

pkg_check_modules(GSTREAMER REQUIRED IMPORTED_TARGET ${GSTREAMER_MODULES})
find_package(Qt5 COMPONENTS Core Network WebSockets Gui Qml Quick REQUIRED)

# Find webrtclib
find_package(webrtclib REQUIRED)

# Build the demo
add_subdirectory(example)

# Install the demo binary
install(TARGETS webrtc-cpp-demo
    RUNTIME DESTINATION bin
)
" port)))
             #t))
         (add-after 'configure-for-demo 'fix-example-cmake
           (lambda _
             ;; Ensure the example/CMakeLists.txt uses the installed webrtclib
             (substitute* "example/CMakeLists.txt"
               (("webrtclib") "webrtclib::webrtclib"))
             #t))
         (add-after 'fix-example-cmake 'fix-include-path
           (lambda _
             ;; Fix the include path in main.cpp to use the installed header location
             (substitute* "example/main.cpp"
               (("#include <webrtc.hpp>") "#include <webrtclib/webrtc.hpp>")
               (("#include <logger.hpp>") "#include <webrtclib/logger.hpp>"))
             #t)))))
    (inputs (list cpp-webrtc
                  qtbase-5
                  qtwayland-5
                  qtdeclarative-5
                  qtmultimedia-5
                  qtquickcontrols-5
                  qtquickcontrols2-5
                  qtwebsockets-5
                  libnice
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

(define-public cpp-socketio-client
  (package
    (name "cpp-socketio-client")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/socketio/socket.io-client-cpp/archive/refs/tags/"
             version ".tar.gz"))
       (sha256
        (base32 "15wmj0jvj0kb1xvlpkrrja7sblx0hcd0qby4qwld062nirmx6kgm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DCMAKE_BUILD_TYPE=Release"
                           "-DCMAKE_INSTALL_INCLUDEDIR=include"
                           "-DCMAKE_INSTALL_LIBDIR=lib")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-cmake-config
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (cmake-dir (string-append lib "/cmake/sioclient")))
               (mkdir-p cmake-dir)
               ;; Create sioclientConfig.cmake
               (call-with-output-file (string-append cmake-dir "/sioclientConfig.cmake")
                 (lambda (port)
                   (display "# sioclient CMake configuration file
include(CMakeFindDependencyMacro)

# Find dependencies
find_dependency(Boost REQUIRED)
find_dependency(OpenSSL REQUIRED)

# Define the library targets
add_library(sioclient::sioclient STATIC IMPORTED)
set_target_properties(sioclient::sioclient PROPERTIES
    IMPORTED_LOCATION \"${CMAKE_CURRENT_LIST_DIR}/../../libsioclient.a\"
    INTERFACE_INCLUDE_DIRECTORIES \"${CMAKE_CURRENT_LIST_DIR}/../../include\"
    INTERFACE_COMPILE_DEFINITIONS \"ASIO_STANDALONE;BOOST_DATE_TIME_NO_LIB;BOOST_REGEX_NO_LIB;_WEBSOCKETPP_CPP11_FUNCTIONAL_;_WEBSOCKETPP_CPP11_STL_\"
)

add_library(sioclient::sioclient_tls STATIC IMPORTED)
set_target_properties(sioclient::sioclient_tls PROPERTIES
    IMPORTED_LOCATION \"${CMAKE_CURRENT_LIST_DIR}/../../libsioclient_tls.a\"
    INTERFACE_INCLUDE_DIRECTORIES \"${CMAKE_CURRENT_LIST_DIR}/../../include\"
    INTERFACE_COMPILE_DEFINITIONS \"ASIO_STANDALONE;BOOST_DATE_TIME_NO_LIB;BOOST_REGEX_NO_LIB;SIO_TLS;_WEBSOCKETPP_CPP11_FUNCTIONAL_;_WEBSOCKETPP_CPP11_STL_\"
    INTERFACE_LINK_LIBRARIES \"OpenSSL::SSL;OpenSSL::Crypto\"
)

# For compatibility, also provide the bare target names
add_library(sioclient ALIAS sioclient::sioclient)
add_library(sioclient_tls ALIAS sioclient::sioclient_tls)
" port)))
               ;; Create sioclientConfigVersion.cmake
               (call-with-output-file (string-append cmake-dir "/sioclientConfigVersion.cmake")
                 (lambda (port)
                   (display "set(PACKAGE_VERSION \"3.1.0\")

if(\"${PACKAGE_VERSION}\" VERSION_LESS \"${PACKAGE_FIND_VERSION}\")
    set(PACKAGE_VERSION_COMPATIBLE FALSE)
else()
    set(PACKAGE_VERSION_COMPATIBLE TRUE)
    if(\"${PACKAGE_VERSION}\" VERSION_EQUAL \"${PACKAGE_FIND_VERSION}\")
        set(PACKAGE_VERSION_EXACT TRUE)
    endif()
endif()
" port)))
               #t))))))
    (inputs (list boost
                  rapidjson
                  openssl
                  websocketpp
                  asio))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/socketio/socket.io-client-cpp")
    (synopsis "Socket.IO C++ client library")
    (description "A Socket.IO client library written in C++11 with support for 
modern Socket.IO server versions. Provides real-time bidirectional 
event-based communication.")
    (license license:expat)))