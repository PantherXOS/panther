;;; Settings Packages Module for PantherX
;;; Hamzeh Nasajpour <h.nasajpour@pantherx.org>
;;; Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Franz Geffke <franz@pantherx.org>

(define-module (px packages settings)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages qt)
  #:use-module (px packages accounts)
  #:use-module (px packages backup)
  #:use-module (px packages library)
  #:use-module (px packages common))

(define-public px-settings-ui
  (package
    (name "px-settings-ui")
    (version "v0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "04qd4vwjmz06idrbcfi4npwf442mvwvzyq1adkqzbd9qxc3dcnvc"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("qrencode" ,qrencode)
              ("yaml-cpp" ,yaml-cpp)
              ("capnproto" ,capnproto-0.9)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("qtbase" ,qtbase-5)
                     ("qtcharts" ,qtcharts)
                     ("libqtxdg" ,libqtxdg)
                     ("liblxqt" ,liblxqt)
                     ("python" ,python)
                     ("px-gui-library" ,px-gui-library)
                     ("pybind11" ,pybind11)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Settings GUI Application")
    (description "This package provides a QT-GUI to manage change
various settings around PantherX OS")
    (license license:gpl3)))

(define-public px-settings-service
  (package
    (name "px-settings-service")
    (version "v0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0r16z52mc01vr04a2qa2irmxxxf1dhi4nmsrhvldc788qc7nxclc"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("yaml-cpp" ,yaml-cpp)
              ("capnproto" ,capnproto-0.9)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("python" ,python)
                     ("pybind11" ,pybind11)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Settings Service")
    (description "This package provides background services to manage 
Configuration in PantherX")
    (license license:expat)))

(define-public px-settings-service-plugin-accounts
  (package
    (name "px-settings-service-plugin-accounts")
    (version "v0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "18w3zmf3vgb8vpa66g4ggph7r4h2yxw0vf6v3md83yz9hq51p74c"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'register-plugin
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (type "cpp")
                             (regpath (string-append out
                                       "/etc/px/settings/plugins"))
                             (regdata (string-append "plugin:\n"
                                                     "  name: "
                                                     ,name
                                                     "\n"
                                                     "  version: "
                                                     ,version
                                                     "\n"
                                                     "  type: "
                                                     type
                                                     "\n"
                                                     "  path: "
                                                     out
                                                     "/lib/lib"
                                                     ,name
                                                     ".so\n")))
                        (display regdata)
                        (mkdir-p regpath)
                        (with-output-to-file (string-append regpath "/"
                                                            ,name ".yaml")
                          (lambda _
                            (format #t regdata)))))))))
    (inputs `(("yaml-cpp" ,yaml-cpp)
              ("capnproto" ,capnproto-0.9)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Accounts Plugin For Settings service")
    (description
     "Accounts Plugin for Settings service, this plugin used for add/remove/edit accounts in PantherX.")
    (license license:expat)))

(define-public px-settings-service-plugin-cpp-test
  (package
    (name "px-settings-service-plugin-cpp-test")
    (version "v0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0dlg9wh5g86r4f0000wyd3fraqjqdj32v7ri75rzsg31n8kbfcsr"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'register-plugin
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (type "cpp")
                             (regpath (string-append out
                                       "/etc/px/settings/plugins"))
                             (regdata (string-append "plugin:\n"
                                                     "  name: "
                                                     ,name
                                                     "\n"
                                                     "  version: "
                                                     ,version
                                                     "\n"
                                                     "  type: "
                                                     type
                                                     "\n"
                                                     "  path: "
                                                     out
                                                     "/lib/lib"
                                                     ,name
                                                     ".so\n")))
                        (display regdata)
                        (mkdir-p regpath)
                        (with-output-to-file (string-append regpath "/"
                                                            ,name ".yaml")
                          (lambda _
                            (format #t regdata)))))))))
    (home-page "https://www.pantherx.org/")
    (synopsis "CPP Test Plugin For Settings service")
    (description "Test Plugin for Settings service, this plugin needs
		 to be installed in order to tests run properly.")
    (license license:expat)))

(define-public px-settings-service-plugin-python-test
  (package
    (name "px-settings-service-plugin-python-test")
    (version "v0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0cl6g5qcnf1ysvqjhnfcckahvpxl1rvjy5ld95vanvbmvys6pxqr"))))
    (build-system python-build-system)
    (inputs `(("python-pycapnp" ,python-pycapnp)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'register-plugin
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (type "python")
                             (regpath (string-append out
                                       "/etc/px/settings/plugins"))
                             (regdata (string-append "plugin:\n"
                                                     "  name: "
                                                     ,name
                                                     "\n"
                                                     "  version: "
                                                     ,version
                                                     "\n"
                                                     "  type: "
                                                     type
                                                     "\n"
                                                     "  path: "
                                                     out
                                                     "\n")))
                        (display regdata)
                        (mkdir-p regpath)
                        (with-output-to-file (string-append regpath "/"
                                                            ,name ".yaml")
                          (lambda _
                            (format #t regdata)))))))))
    (home-page "https://www.pantherx.org/")
    (synopsis "Python Test Plugin For Settings service")
    (description "Test Plugin for Settings service, this plugin needs
		 to be installed in order to tests run properly.")
    (license license:expat)))

(define-public px-settings-service-plugin-software
  (package
    (name "px-settings-service-plugin-software")
    (version "v0.0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0n518ip7psc6h0a2dqwhljychqyx0x9zss3l4hdisl7jfrzgzar1"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'register-plugin
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (type "python")
                             (regpath (string-append out
                                       "/etc/px/settings/plugins"))
                             (regdata (string-append "plugin:\n"
                                                     "  name: "
                                                     ,name
                                                     "\n"
                                                     "  version: "
                                                     ,version
                                                     "\n"
                                                     "  type: "
                                                     type
                                                     "\n"
                                                     "  path: "
                                                     out
                                                     "\n")))
                        (display regdata)
                        (mkdir-p regpath)
                        (with-output-to-file (string-append regpath "/"
                                                            ,name ".yaml")
                          (lambda _
                            (format #t regdata)))))))))
    (home-page "https://www.pantherx.org/")
    (synopsis "Software Plugin For Settings service")
    (description "Software/ update check preferences plugin.")
    (license license:expat)))

(define-public px-settings-service-plugin-theme
  (package
    (name "px-settings-service-plugin-theme")
    (version "0.0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "1xxw0r7idjrpnlyndrqdn6637a58zqbw8qv8k0yf3sq2yrb18k22"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-pyxdg" ,python-pyxdg)
                         ("python-configobj" ,python-configobj)
                         ("claws-mail-theme-breeze" ,claws-mail-theme-breeze)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'register-plugin
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (type "python")
                             (regpath (string-append out
                                       "/etc/px/settings/plugins"))
                             (regdata (string-append "plugin:\n"
                                                     "  name: "
                                                     ,name
                                                     "\n"
                                                     "  version: "
                                                     ,version
                                                     "\n"
                                                     "  type: "
                                                     type
                                                     "\n"
                                                     "  path: "
                                                     out
                                                     "\n")))
                        (display regdata)
                        (mkdir-p regpath)
                        (with-output-to-file (string-append regpath "/"
                                                            ,name ".yaml")
                          (lambda _
                            (format #t regdata)))))))))
    (home-page "https://www.pantherx.org/")
    (synopsis "Desktop Theme Plugin For Settings service")
    (description "Desktop Theme/Appereance plugin.")
    (license license:expat)))

(define-public px-settings-service-plugin-desktop-search
  (package
    (name "px-settings-service-plugin-desktop-search")
    (version "0.0.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "1mxq6avh826aln0x4x7i78i8zzsn84sak754wbjd7n3ba28qq1kr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'register-plugin
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (type "python")
                             (regpath (string-append out
                                       "/etc/px/settings/plugins"))
                             (regdata (string-append "plugin:\n"
                                                     "  name: "
                                                     ,name
                                                     "\n"
                                                     "  version: "
                                                     ,version
                                                     "\n"
                                                     "  type: "
                                                     type
                                                     "\n"
                                                     "  path: "
                                                     out
                                                     "\n")))
                        (display regdata)
                        (mkdir-p regpath)
                        (with-output-to-file (string-append regpath "/"
                                                            ,name ".yaml")
                          (lambda _
                            (format #t regdata)))))))))
    (home-page "https://www.pantherx.org/")
    (synopsis "Desktop Search Plugin For Settings service")
    (description "Desktop Search Settings plugin.")
    (license license:expat)))

(define-public px-settings-service-plugin-backup
  (package
    (name "px-settings-service-plugin-backup")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1nix2hn8acipdysyz9vgz2akd9j2hpjnilbdwgxvsw80bxxd8b68"))))
    (build-system python-build-system)
    (propagated-inputs `(("px-accounts-library-python" ,px-accounts-library-python)
                         ("px-backup" ,px-backup)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'register-plugin
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (type "python")
                             (regpath (string-append out
                                       "/etc/px/settings/plugins"))
                             (regdata (string-append "plugin:\n"
                                                     "  name: "
                                                     ,name
                                                     "\n"
                                                     "  version: "
                                                     ,version
                                                     "\n"
                                                     "  type: "
                                                     type
                                                     "\n"
                                                     "  path: "
                                                     out
                                                     "\n")))
                        (display regdata)
                        (mkdir-p regpath)
                        (with-output-to-file (string-append regpath "/"
                                                            ,name ".yaml")
                          (lambda _
                            (format #t regdata)))))))))
    (home-page "https://www.pantherx.org/")
    (synopsis "Backup Plugin For Settings service")
    (description "Backup Settings plugin.")
    (license license:expat)))

(define-public px-settings-service-plugin-maintenance
  (package
    (name "px-settings-service-plugin-maintenance")
    (version "v0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0rvnhv2lw3n9wchp61lfw2g1z31whizmd6n74cih2ncafcb0bdbf"))))
    (build-system python-build-system)
    (propagated-inputs `(("python-pyxdg" ,python-pyxdg)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'register-plugin
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (type "python")
                             (regpath (string-append out
                                       "/etc/px/settings/plugins"))
                             (regdata (string-append "plugin:\n"
                                                     "  name: "
                                                     ,name
                                                     "\n"
                                                     "  version: "
                                                     ,version
                                                     "\n"
                                                     "  type: "
                                                     type
                                                     "\n"
                                                     "  path: "
                                                     out
                                                     "\n")))
                        (display regdata)
                        (mkdir-p regpath)
                        (with-output-to-file (string-append regpath "/"
                                                            ,name ".yaml")
                          (lambda _
                            (format #t regdata)))))))))
    (home-page "https://www.pantherx.org/")
    (synopsis "Maintenance Plugin For Settings service")
    (description "Maintenance Plugin For Settings service.")
    (license license:expat)))
