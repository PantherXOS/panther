;;; PantherX Desktop Packages and Tools
;;; Reza Alizadeh Majd (r.majd@pantherx.org)
;;;
;;; Note: This module is exported from (px packages desktop) to prevent
;;;       circular import problem.
;;;

(define-module (px packages desktop-tools)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages mate)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages search)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages serialization)
  #:use-module (px packages qt)
  #:use-module (px packages common)
  #:use-module (px packages library)
  #:use-module (px packages themes)
  #:use-module (srfi srfi-1))

(define-public albert-launcher
  (package
    (name "albert-launcher")
    (version "v0.16.1-0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/albertlauncher/albert")
             (commit "579d063b8e90fd854fd4738480c2d8dc833f908e")
             (recursive? #t)))
       (sha256
        (base32 "1cqh4nsvxwarxm7v0fyzabph3c2ff3ap0q2xi2h4c0s2snrk4qh4"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags '("-DBUILD_VIRTUALBOX=OFF"
                           "-DCMAKE_INSTALL_LIBDIR=libs")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-cmakelists
                    (lambda _
                      ;; path-main-cpp-add-plugin-dirs
                      (substitute* '("src/app/main.cpp")
                        (("QStringList dirs = \\{")
                         (string-append "QStringList dirs = {\"" %output
                                        "/libs\", ")))
                      ;; Adding X11Extras to target_link_libraries of widgetboxmodel plugin
                      (substitute* '("plugins/widgetboxmodel/CMakeLists.txt")
                        (("COMPONENTS Widgets")
                         "COMPONENTS Widgets X11Extras"))
                      (substitute* '("plugins/widgetboxmodel/CMakeLists.txt")
                        (("Qt5::Widgets")
                         "Qt5::Widgets Qt5::X11Extras X11"))
                      ;; Adding X11Extras to target_link_libraries of qmlboxmodel plugin
                      (substitute* '("plugins/qmlboxmodel/CMakeLists.txt")
                        (("COMPONENTS Widgets")
                         "COMPONENTS Widgets X11Extras"))
                      (substitute* '("plugins/qmlboxmodel/CMakeLists.txt")
                        (("Qt5::Widgets")
                         "Qt5::Widgets Qt5::X11Extras X11"))
                      #t)))))
    (native-inputs `(("libx11" ,libx11)
                     ("muparser" ,muparser)
                     ("pkg-config" ,pkg-config)
                     ("python3" ,python)
                     ("qtbase" ,qtbase-5)
                     ("qtcharts" ,qtcharts)
                     ("qtdeclarative" ,qtdeclarative-5)
                     ("qtsvg" ,qtsvg-5)
                     ("qtx11extras" ,qtx11extras)))
    (home-page "https://albertlauncher.github.io/")
    (synopsis "Albert is a unified and efficient access to your machine.")
    (description
     "Albert is a desktop agnostic launcher. Its goals are usability and beauty, performance and extensibility. It is written in C++ and based on the Qt framework.")
    (license license:gpl3+)))

(define-public qlipper
  (package
    (name "qlipper")
    (version "5.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pvanek/qlipper/archive/"
                           version ".tar.gz"))
       (sha256
        (base32 "0vbhiyn56qwlssavim02kp0y5rxj6gdffchyigkhpg8qza64afch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs (list qtbase-5 qttools-5))
    (home-page "https://github.com/pvanek/qlipper")
    (synopsis "Lightweight and cross-platform clipboard history applet.")
    (description "Lightweight and cross-platform clipboard history applet.")
    (license license:gpl2+)))

(define-public cpputilities
  (package
    (name "cpputilities")
    (version "5.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Martchus/cpp-utilities/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "18mbzw8w0a4l5r0w1fr200m4v8ww65r838618z19v1ymz6aahs7a"))))
    (build-system cmake-build-system)
    (inputs `(("gcc" ,gcc-11)))
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DCMAKE_VERBOSE_MAKEFILE=ON"
                           "-DBUILD_VIRTUALBOX=OFF"
                           "-DCMAKE_INSTALL_LIBDIR=libs")))
    (home-page "https://github.com/Martchus/cpp-utilities/")
    (synopsis "Useful C++ classes and routines")
    (description "Useful C++ classes and routines such as
argument parser, IO and conversion utilities.")
    (license license:gpl2+)))

(define-public fork-awesome
  (package
    (name "fork-awesome")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ForkAwesome/Fork-Awesome/archive/refs/tags/"
             version ".tar.gz"))
       (sha256
        (base32 "1cxxbyklk139cj7hw9jiq51cmmqgn74z8ysl9i0y017jj7qsbyr3"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("fonts" "share/")
                        ("src" "share/"))))
    (home-page "https://github.com/ForkAwesome/Fork-Awesome")
    (synopsis "A fork of the iconic font and CSS toolkit ")
    (description
     "Fork Awesome is a suite of 796 pictographic and
brand icons for easy, scalable vector graphics on websites and beyond.")
    (license license:expat)))

(define-public qtforkawesome
  (package
    (name "qtforkawesome")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Martchus/qtforkawesome/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0pj4w93vrsmgqlgihwz6s2jdazmfsdw3lxns7wk9908l4ilcqw9d"))))
    (build-system cmake-build-system)
    (native-inputs (list gcc-11
                         qtutilities
                         cpputilities
                         qtbase-5
                         qtquickcontrols-5
                         qtquickcontrols2-5
                         qtdeclarative-5
                         perl
                         perl-yaml
                         fork-awesome))
    (arguments
     `(#:tests? #f
       #:configure-flags ,#~(list (string-append "-DFORK_AWESOME_FONT_FILE="
                                   #$(this-package-native-input "fork-awesome")
                                   "/share/fonts/forkawesome-webfont.woff2")
                                  (string-append
                                   "-DFORK_AWESOME_ICON_DEFINITIONS="
                                   #$(this-package-native-input "fork-awesome")
                                   "/share/src/icons/icons.yml"))))
    (home-page "https://github.com/Martchus/qtforkawesome/")
    (synopsis
     "Useful C++ classes and routines such as argument parser, IO and conversion utilities.")
    (description
     "Useful C++ classes and routines such as argument parser, IO and conversion utilities.")
    (license license:gpl2+)))

(define-public qxkb
  (package
    (name "qxkb")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/thegala/qxkb/archive/refs/tags/" name "-"
             version ".tar.gz"))
       (sha256
        (base32 "1nprswfdnqmy6xs6pdkzy6c3xkzh79zifdvy4vpw4l41gnqrl94s"))))
    (build-system qt-build-system)
    (inputs (list libxkbfile qtbase-5 qtsvg-5 qtx11extras))
    (native-inputs (list qttools-5))
    (arguments
     (list
      #:tests? #f)) ;no upstream tests
    (home-page "https://github.com/thegala/qxkb")
    (synopsis "Keyboard layout switcher")
    (description "Keyboard layout switcher")
    (license license:gpl2+)))

(define-public syncthingtray
  (package
    (name "syncthingtray")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Martchus/syncthingtray/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0cyimd018bknvip6jxz83w0va05kgfcix53jryqdka665p4048ba"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda _
                      (invoke "make" "-j" "1") #t))
                  (add-after 'install 'wrap
                    ;; The program fails to find the QtWebEngineProcess program,
                    ;; so we set QTWEBENGINEPROCESS_PATH to help it.
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((bin (string-append (assoc-ref outputs "out")
                                                "/bin"))
                            (qtwebengineprocess (string-append (assoc-ref
                                                                inputs
                                                                "qtwebengine")
                                                 "/lib/qt5/libexec/QtWebEngineProcess")))
                        (for-each (lambda (program)
                                    (wrap-program program
                                      `("QTWEBENGINEPROCESS_PATH" =
                                        (,qtwebengineprocess))))
                                  (find-files bin ".*"))) #t)))))
    (native-inputs (list extra-cmake-modules qttools-5 gcc-11))
    (inputs `(("qtbase" ,qtbase-5)
              ("qtquickcontrols2" ,qtquickcontrols2-5)
              ("qtutilities" ,qtutilities)
              ("boost" ,boost)
              ("gcc" ,gcc-11)
              ("qtdeclarative" ,qtdeclarative-5)
              ("qtsvg" ,qtsvg-5)
              ("qtwebchannel-5" ,qtwebchannel-5)
              ("qtwebengine" ,qtwebengine-5)
              ("plasma-framework" ,plasma-framework)
              ("kwindowsystem" ,kwindowsystem-5)
              ("kio" ,kio-5)
              ("cppunit" ,cppunit)
              ("cpputilities" ,cpputilities)
              ("qtforkawesome" ,qtforkawesome)
              ("bash-minimal" ,bash-minimal)))
    (home-page "https://github.com/Martchus/syncthingtray")
    (synopsis "Qt-based tray application")
    (description "Qt-based tray application")
    (license license:gpl2+)))

(define-public px-terminal-launcher
  (package
    (name "px-terminal-launcher")
    (version "v0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0z7m63dr99sm287ibcvpb6wbln9slfdxpnxg9ksgi8k92b48z9j2"))))
    (build-system qt-build-system)
    (native-inputs (list qttools pkg-config extra-cmake-modules))
    (inputs (list capnproto
                  qtbase
                  qtsvg
                  qtcharts
                  networkmanager-qt))
    (propagated-inputs (list px-icons))
    (home-page "https://www.pantherx.dev")
    (synopsis "PantherX Terminal Launcher")
    (description "PantherX Terminal Launcher")
    (license license:expat)))