;;; Setup Packages Module for PantherX
;;; Hamzeh Nasajpour (h.nasajpour@pantherx.org)

(define-module (px packages setup)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xorg)
  #:use-module (px packages common))

(define-public px-first-login-welcome-screen
  (package
    (name "px-first-login-welcome-screen")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/"
                           name
                           "_" ;
                           version
                           ".tgz"))
       (sha256
        (base32 "1av540acbwpn7ccc790bifmndfx7kscx6y7y1nqln6cmmazvzfvn"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ;no tests
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-chpasswd-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((shadow (assoc-ref inputs "shadow")))
                        (substitute* "helper/px-first-login-password-helper.cpp"
                          (("chpasswd")
                           (string-append shadow "/sbin/chpasswd"))) #t))))))
    (inputs `(("qtbase" ,qtbase-5)
              ("qtlinguist" ,qttools-5)
              ("capnproto" ,capnproto-0.9)
              ("polkit-qt" ,polkit-qt)
              ("shadow" ,shadow)))
    (propagated-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Setup Assistant")
    (description
     "This package provides cli and gui applications for Setup PantherX Devices")
    (license license:gpl3)))

(define-public px-setup-assistant
  (package
    (name "px-setup-assistant")
    (version "v0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/px-setup-assistant_"
                           version ".tgz"))
       (sha256
        (base32 "0fkcldxrr6j3ig9b26ar7c11h9xqrrdb9cn0gj9fb63lhdv0zac5"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'set-executable
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (invoke "chmod" "755"
                                (string-append out "/bin/px-setup")) #t))))))
    (inputs `(("qtbase" ,qtbase-5)
              ("qtlinguist" ,qttools-5)
              ("rapidjson" ,rapidjson)
              ("yaml-cpp" ,yaml-cpp)))
    (propagated-inputs `(("tzdata" ,tzdata)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Setup Assistant")
    (description
     "This package provides cli and gui applications for Setup PantherX Devices")
    (license license:gpl3)))

(define-public px-install
  (package
    (name "px-install")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "19s5r30cs7ncqrxbzh8w0sr7hjldzv8jg9zchz2v83f2pip9r51c"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("python-requests" ,python-requests)
              ("python-tqdm" ,python-tqdm)
              ("python-pytz" ,python-pytz)
              ("python-qrcode" ,python-qrcode)
              ("python-py-cpuinfo" ,python-py-cpuinfo)
              ("python-urllib3" ,python-urllib3)
              ("python-psutil" ,python-psutil)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX OS Installer")
    (description "A command line driven installer with sane defaults.")
    (license license:gpl3)))

(define-public calamares
  (package
    (name "calamares")
    (version "3.2.61")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/calamares/"
                           name
                           "/releases/download/v"
                           version
                           "/"
                           name
                           "-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "1lhxf4rbii8ss86ny03an1nh08dbc9admf72kmxvmg9q0yvbk4bm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DINSTALL_CONFIG=ON")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-source
                    (lambda _
                      (let* ((out (assoc-ref %outputs "out"))
                             (zonefile (string-append (assoc-ref %build-inputs
                                                       "tzdata")
                                        "/share/zoneinfo/zone.tab"))
                             (xkb-file (string-append (assoc-ref %build-inputs
                                                       "xkeyboard-config")
                                        "/share/X11/xkb/rules/base.lst")))
                        ;; patch polkit rules path
                        (substitute* "CMakeLists.txt"
                          (("DESTINATION \"\\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                           "DESTINATION \"share/polkit-1/actions"))
                        ;; fix zone file path
                        (substitute* "src/libcalamares/locale/TimeZone.cpp"
                          (("/usr/share/zoneinfo/zone.tab")
                           zonefile))
                        ;; fix keyboard layout path
                        (substitute* "src/modules/keyboard/keyboardwidget/keyboardglobal.cpp"
                          (("/usr/share/X11/xkb/rules/base.lst")
                           xkb-file))
                        ;; settings.conf preparations
                        (call-with-output-file "settings.conf"
                          (lambda (port)
                            (format port "---
modules-search: [ local ]
sequence:
- show:
  - welcome
  - locale
  - keyboard
  - summary
- show:
  - finished
branding: default
prompt-install: false
dont-chroot: false
oem-setup: false
disable-cancel: false
disable-cancel-during-exec: false
hide-back-and-next-during-exec: false
quit-at-end: false
")))
                        #t))))))
    (native-inputs (list boost extra-cmake-modules pkg-config))
    (inputs (list kcrash
                  kcoreaddons
                  kdbusaddons
                  kparts
                  kservice
                  kwidgetsaddons
                  polkit-qt
                  python
                  python-jsonschema
                  python-pyyaml
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols-5
                  qtquickcontrols2-5
                  qtsvg-5
                  qttools-5
                  tzdata
                  xkeyboard-config
                  yaml-cpp))
    (propagated-inputs (list python))
    (home-page "https://calamares.io")
    (synopsis " Distribution-independent installer framework ")
    (description
     "Calamares is a distribution-independent system installer,
with an advanced partitioning feature for both manual and automated partitioning
operations. Calamares is designed to be customizable by distribution maintainers
without need for cumbersome patching, thanks to third party branding and external
modules support.")
    (license license:gpl3)))

(define-public px-install-gui
  (package
    (inherit calamares)
    (name "px-install-gui")
    (version "3.2.61-b1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1vdylrdkbvfq03i8rbzkamrjvg497j8zqlqwhvdkywqpazwgp8r9"))))
    (arguments
     (substitute-keyword-arguments (package-arguments calamares)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'patch-source 'setup-installer
              (lambda _
                (call-with-output-file "settings.conf"
                  (lambda (port)
                    (format port "---
modules-search: [ local ]
sequence:
- show:
  - welcome
  - locale
  - packagechooser
  - users
  - summary
- exec:
  - px-install
- show:
  - finished
branding: default
prompt-install: false
dont-chroot: false
oem-setup: false
disable-cancel: false
disable-cancel-during-exec: false
hide-back-and-next-during-exec: false
quit-at-end: false
")))))))))
    (propagated-inputs (list python
                             python-requests
                             python-tqdm
                             python-pytz
                             python-qrcode
                             python-py-cpuinfo
                             python-urllib3))))
