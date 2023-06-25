;;; Online Accounts Packages Module for PantherX
;;; Author: Reza Alizadeh Majd (r.majd@pantherx.org)
;;; Author: Hossein Ghaffari (h.ghaffari@pantherx.org)

(define-module (px packages accounts)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages web)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (px packages common)
  #:use-module (px packages contacts-calendar)
  #:use-module (px packages networking)
  #:use-module (px packages etesync)
  #:use-module (px packages library)
  #:use-module (px packages python-xyz))


(define-public px-accounts-service
  (package
    (name "px-accounts-service")
    (version "0.4.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
          "https://source.pantherx.org/px-accounts-service_v" version ".tgz"))
        (sha256
         (base32 "0nw2ggm87mbjrw0xdmq301ygxgai3q4sbpinhk59rf8jhrds182d"))))
    (build-system cmake-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'unpack 'remove-tests
              (lambda _
                (substitute* "tests/CMakeLists.txt"
                  (("test_rpc_server.cpp") "")
                  (("test_plugin_system.cpp") "")
                  (("test_event_system.cpp") ""))
                #t))
            (add-after 'install 'register-plugins
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (define* (register-plugin plugin-name plugin-type #:optional (plugin-path ""))
                  (let* ((out     (assoc-ref outputs "out"))
                         (python  (assoc-ref inputs "python"))
                         (regpath (string-append out "/etc/px/accounts/plugins"))
                         (target  (cond
                                   ((string=? plugin-type "python")
                                    (string-append out "/lib/python"
                                                   ,(version-major+minor (package-version python))
                                                   "/site-packages/"))
                                   ((string=? plugin-type "cpp")
                                    (string-append out "/lib/lib" plugin-name ".so"))))
                         (data    (string-append "plugin:\n"
                                                 "  name: " plugin-name "\n"
                                                 "  version: " ,version "\n"
                                                 "  type: " plugin-type "\n"
                                                 "  path: " target "\n")))
                    (mkdir-p regpath)
                    (display data)
                    (with-output-to-file (string-append regpath "/" plugin-name ".yaml")
                      (lambda _ (format #t data)))
                    (if (string=? plugin-type "python")
                        (begin
                          (mkdir-p target)
                          (install-file plugin-path target)
                          (setenv "PYTHONPATH" (string-append target ":" (getenv "GUIX_PYTHONPATH")))))
                    #t))
                (register-plugin (string-append ,name "-plugin-python-test") "python"
                                 "src/px_accounts_service_plugin_python_test.py")
                (register-plugin (string-append ,name "-plugin-test-public-service") "python"
                                 "src/px_accounts_service_plugin_test_public_service.py")
                (register-plugin (string-append ,name "-plugin-cpp-test") "cpp")
                (register-plugin (string-append ,name "-plugin-cpp-custom") "cpp"))))))
    (inputs `(("yaml-cpp" ,yaml-cpp)
              ("capnproto" ,capnproto-0.9)))
    (native-inputs `(("catch2" ,catch2)
                     ("cli11" ,cli11)
                     ("pkg-config" ,pkg-config)
                     ("pybind11" ,pybind11-2.6.2)
                     ("nng" ,nng-1.5)
                     ("util-linux" ,util-linux "lib")))
    (propagated-inputs `(("python" ,python)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX (Online) AccountsService")
    (description "This package provides a background services to manage
Online Accounts in PantherX")
    (license license:expat)))


(define-public px-accounts-matrix-bridge
  (package
   (name "px-accounts-matrix-bridge")
   (version "0.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://source.pantherx.org/" name "_v" version ".tgz"))
     (sha256
      (base32 "1nmwdxns7isgr09v5mvhbr3jz60z6ia3ib84wi2q40ja2570k2g0"))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f))
   (native-inputs (list pkg-config))
   (inputs
    (list capnproto-0.9))
   (home-page "https://www.pantherx.org")
   (synopsis "Account integration for Matrix client library")
   (description "Add support for Online Accounts integration for Matrix client library")
   (license license:expat)))


(define-public px-accounts-service-plugin-common
  (package
    (name "px-accounts-service-plugin-common")
    (version "0.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "1kj84df95bmgy89k0mnxv8hy1l62b2xnyymkgz9rqs6882wk5lqx"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'sanity-check))))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Accounts Service Plugin Commons")
    (description "Python classes required for all
Accounts Service plugins.")
    (license license:expat)))


(define-public px-accounts-library-python
  (package
    (name "px-accounts-library-python")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri
         (string-append "https://source.pantherx.org/px-accounts-library-python_v"
                        version ".tgz"))
        (sha256
         (base32 "01i3s2wscjdsml59lls91w990rg61a0c0a0pppxd5jqkh6x98za7"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs 
      `(("python-pycapnp", python-pycapnp)
        ("python-pyyaml", python-pyyaml)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Client library and CLI tool for px-accounts-service")
    (description "Python library to work with online accounts service. plus a CLI tool")
    (license license:expat)))


(define-public px-accounts-service-providers-mail
  (package
    (name "px-accounts-service-providers-mail")
    (version "0.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256
          (base32 "1xr5k3xkdfdfbrl295drb66q03ajb1y1psxmk6ywj6470dwbbq09"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
       `(("px-accounts-service-plugin-imap" ,px-accounts-service-plugin-imap)
         ("px-accounts-service-plugin-smtp" ,px-accounts-service-plugin-smtp)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Official Providers package for Online Mail Accounts Service")
    (description "Collection of Official defined providers that Online Accounts Service
could load from store")
    (license license:expat)))


 (define-public px-accounts-service-plugin-imap
  (package
    (name "px-accounts-service-plugin-imap")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "0asdq32diniz2xa7q19qhhcp52cyzsa5g5l81yi7rz12vmd814sb"))))
    (build-system python-build-system)
    (arguments `(
      #:tests? #f,
      #:phases
        (modify-phases %standard-phases
          (add-after 'install 'register-plugin
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (type    "python")
                     (regpath (string-append out "/etc/px/accounts/plugins"))
                     (regdata (string-append "plugin:\n"
                                             "  name: " ,name "\n"
                                             "  version: " ,version "\n"
                                             "  type: " type "\n"
                                             "  path: " out "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata))))))
          (delete 'sanity-check))))
    (propagated-inputs
      `(("px-accounts-service-plugin-common", px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "IMAP Protocol Plugin For Online Accounts System")
    (description "Provides IMAP protocol support to Online Accounts System")
    (license license:expat)))



(define-public px-accounts-service-plugin-smtp
  (package
    (name "px-accounts-service-plugin-smtp")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://source.pantherx.org/px-accounts-service-plugin-smtp_v"
               version ".tgz"))
        (sha256
          (base32 "1x6ln81h8k6qqm83crdgbkkzcg77sg1wvpvy5qp7nnb0mdji34fv"))))
    (build-system python-build-system)
    (arguments `(
      #:tests? #f,
      #:phases
        (modify-phases %standard-phases
          (add-after 'install 'register-plugin
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (type    "python")
                     (regpath (string-append out "/etc/px/accounts/plugins"))
                     (regdata (string-append "plugin:\n"
                                             "  name: " ,name "\n"
                                             "  version: " ,version "\n"
                                             "  type: " type "\n"
                                             "  path: " out "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata))))))
          (delete 'sanity-check))))
    (propagated-inputs
      `(("px-accounts-service-plugin-common", px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "SMTP Protocol Plugin For Online Accounts System")
    (description "Provides SMTP protocol support to Online Accounts System")
    (license license:expat)))


(define-public px-oauth2-engine
  (package
    (name "px-oauth2-engine")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
                "https://source.pantherx.org/px-oauth2-engine_"
                version ".tgz"))
        (sha256
          (base32 "19wsr2vs8dy5b99pbv3a1amivz0m6yx1lf6bi1fz9kvsxcz3b63b"))))
  (build-system cmake-build-system)
  (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           ;; The program fails to find the QtWebEngineProcess program,
           ;; so we set QTWEBENGINEPROCESS_PATH to help it.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (qtwebengineprocess (string-append
                                        (assoc-ref inputs "qtwebengine")
                                        "/lib/qt5/libexec/QtWebEngineProcess")))
               (for-each (lambda (program)
                           (wrap-program program
                             `("QTWEBENGINEPROCESS_PATH" =
                               (,qtwebengineprocess))))
                         (find-files bin ".*")))
             #t)))))
  (inputs `(
                ("rapidjson" ,rapidjson)
                ("qt" ,qtbase-5)
                ("qtdeclarative" ,qtdeclarative-5)
                ("qtnetworkauth-5" ,qtnetworkauth-5)
                ("qtwebengine" ,qtwebengine-5)
                ("qtwebchannel-5" ,qtwebchannel-5)
                ("bash-minimal" ,bash-minimal)))
  (native-inputs `(
                ("curl" ,curl)
                ("pkg-config" ,pkg-config)))
  (home-page "https://www.pantherx.org/")
  (synopsis "Official Providers package for Online Accounts Service")
  (description "Collection of Official defined providers that Ocline Accounts Service
could load from store")
  (license license:expat)))


(define-public px-accounts-service-plugin-etherscan
  (package
    (name "px-accounts-service-plugin-etherscan")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "0jq81nrjslp4c2kz6vnrns13n65rx248ij28grrm7lkz4mbnb8y7"))))
    (build-system python-build-system)
    (arguments `(
      #:tests? #f,
      #:phases
        (modify-phases %standard-phases
          (add-after 'install 'register-plugin
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (type    "python")
                     (regpath (string-append out "/etc/px/accounts/plugins"))
                     (regdata (string-append "plugin:\n"
                                             "  name: " ,name "\n"
                                             "  version: " ,version "\n"
                                             "  type: " type "\n"
                                             "  path: " out "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata))))))
          (delete 'sanity-check))))
    (propagated-inputs
      `(("px-online-sources-library", px-online-sources-library)
        ("px-accounts-service-plugin-common", px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Etherscan provider plugin (Online) Accounts Service")
    (description "Provides Etherscan support to Online Accounts System")
    (license license:expat)))


(define-public px-accounts-service-plugin-blockio
  (package
    (name "px-accounts-service-plugin-blockio")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_" version ".tgz"))
        (sha256 (base32 "0rmx12122l1hi1smayyf7a19ss2rg53ck2db8y6y0g27zxnzwz9n"))))
    (build-system python-build-system)
    (arguments `(
      #:tests? #f,
      #:phases
        (modify-phases %standard-phases
          (add-after 'install 'register-plugin
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (type    "python")
                     (regpath (string-append out "/etc/px/accounts/plugins"))
                     (regdata (string-append "plugin:\n"
                                             "  name: " ,name "\n"
                                             "  version: " ,version "\n"
                                             "  type: " type "\n"
                                             "  path: " out "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata))))))
          (delete 'sanity-check))))
    (propagated-inputs
      `(("px-online-sources-library", px-online-sources-library)
        ("px-accounts-service-plugin-common", px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Bitcoin Plugin For Online Accounts System")
    (description "Provides bitcoin support to Online Accounts System")
    (license license:expat)))


(define-public px-accounts-service-plugin-cryptocurrency
  (package
    (name "px-accounts-service-plugin-cryptocurrency")
    (version "v0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
                "https://source.pantherx.org/px-online-accounts-plugin-cryptocurrency_"
                version ".tgz"))
        (sha256
         (base32 "1zqk5j52dpcg0lkyy7zk9n2d4ibg2wpbshdihafrx22d2d3y53zk"))))
    (build-system python-build-system)
    (arguments `(
      #:tests? #f,
      #:phases
        (modify-phases %standard-phases
          (add-after 'install 'register-plugin
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (type    "python")
                     (regpath (string-append out "/etc/px/accounts/plugins"))
                     (regdata (string-append "plugin:\n"
                                             "  name: " ,name "\n"
                                             "  version: " ,version "\n"
                                             "  type: " type "\n"
                                             "  path: " out "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata)))))))))
    (inputs `(
              ("python-pycapnp" ,python-pycapnp)))
    (propagated-inputs 
      `(("px-accounts-library-python", px-accounts-library-python)
        ("px-secret-library-python", px-secret-library-python)
        ("px-online-sources-library", px-online-sources-library)
        ("px-accounts-service-plugin-common", px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Cryptocurrency Plugin For Online Accounts Service")
    (description "Adds support for Ethereum and Bitcoin wallet addresses in Accounts Service.")
    (license license:expat)))


(define-public px-accounts-service-plugin-etesync
  (package
    (name "px-accounts-service-plugin-etesync")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "16zzqh9lb5gb1yn12v48akh5rfidssxjah9f3jg8hpzy0qaca64z"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'patch-version
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* (string-append "plugins/" ,name ".yaml")
                          (("PLUGIN_PATH") (assoc-ref outputs "out"))
                          (("PLUGIN_VERSION") ,version))))
          (delete 'sanity-check))))
    (propagated-inputs `(("etesync-dav" ,etesync-dav)
                         ("px-contacts-calendar" ,px-contacts-calendar)
                         ("python-etesync" ,python-etesync)))
    (home-page "https://www.pantherx.org/")
    (synopsis "EteSync Plugin for Online Accounts Service")
    (description "Support Etesync account as a CardDAV/CalDAV provider")
    (license license:expat)))


(define-public px-accounts-service-plugin-gitlab
  (package
    (name "px-accounts-service-plugin-gitlab")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "1j82f3m3y52kcamv2pnipi5gb4h2yxisshq4r3nkb5ar0i8hyrp2"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (type "python")
                       (regpath (string-append out "/etc/px/accounts/plugins"))
                       (regdata (string-append "plugin:"
                                               "\n  name: " ,name
                                               "\n  version: " ,version
                                               "\n  type: " type
                                               "\n  path: " out
                                               "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata))))))
            (delete 'sanity-check))))
    (propagated-inputs
      `(("px-online-sources-library", px-online-sources-library)
        ("px-accounts-service-plugin-common", px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "GitLab provider plugin for (Online) Accounts Service")
    (description "Adds support for GitLab accounts with access token to Accounts Service")
    (license license:expat)))

(define-public px-accounts-service-plugin-discourse
  (package
    (name "px-accounts-service-plugin-discourse")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
             "https://source.pantherx.org/" name "_" version ".tgz"))
        (sha256 (base32 "0pgw46k20ms5nzsx79xydk7synqs1k617gk432grl1wcsy5a1hii"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (type "python")
                       (regpath (string-append out "/etc/px/accounts/plugins"))
                       (regdata (string-append "plugin:"
                                               "\n  name: " ,name
                                               "\n  version: " ,version
                                               "\n  type: " type
                                               "\n  path: " out
                                               "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata))))))
            (delete 'sanity-check))))
    (propagated-inputs
    `(("px-online-sources-library", px-online-sources-library)
        ("px-accounts-service-plugin-common", px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Discourse provider plugin for (Online) Accounts Service")
    (description "Adds support for Discourse accounts with access token to Accounts Service")
    (license license:expat)))



(define-public px-accounts-service-plugin-github
  (package
    (name "px-accounts-service-plugin-github")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "17cav8xracpnlxyxgimsjbxwxbn7g5gpm7446a3lsj5lrqjyy87m"))))
    (build-system python-build-system)
    (arguments `(
      #:tests? #f,
      #:phases
        (modify-phases %standard-phases
          (add-after 'install 'register-plugin
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (type    "python")
                     (regpath (string-append out "/etc/px/accounts/plugins"))
                     (regdata (string-append "plugin:\n"
                                             "  name: " ,name "\n"
                                             "  version: " ,version "\n"
                                             "  type: " type "\n"
                                             "  path: " out "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata))))))
          (delete 'sanity-check))))
    (propagated-inputs
      `(("px-online-sources-library", px-online-sources-library)
        ("px-accounts-service-plugin-common", px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "GitHub provider plugin for Online Accounts Service")
    (description "Adds support for GitHub to Accounts Service.")
    (license license:expat)))


(define-public px-accounts-service-plugin-trojita
  (package
    (name "px-accounts-service-plugin-trojita")
    (version "0.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://source.pantherx.org/px-accounts-service-plugin-trojita_v"
               version ".tgz"))
        (sha256 (base32 "0k4bybh56ff0d303ik5vnd371fzr8wcm25l79rmfbxv3rd32dxm2"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (type "python")
                       (regpath (string-append out "/etc/px/accounts/plugins"))
                       (regdata (string-append "plugin:\n"
                                               "  name: " ,name "\n"
                                               "  version: " ,version "\n"
                                               "  type: " type "\n"
                                               "  path: " out "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file
                    (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata)))))))))
    (home-page "https://www.pantherx.org/")
    (synopsis "Trojita Plugin for Online Accounts Service")
    (description "Add or modify trojita account details using Online Accounts Service")
    (license license:expat)))

(define-public px-accounts-service-plugin-oauth2-github
  (package
    (name "px-accounts-service-plugin-oauth2-github")
    (version "v0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
                "https://source.pantherx.org/px-accounts-service-plugin-github-oauth_"
                version ".tgz"))
        (sha256
          (base32 "1znvcl0sy47zpn635h13lj5js1s0lbakk2x18bnmy1ygrqg2c309"))))
    (build-system cmake-build-system)
	(arguments
      `(#:tests? #f
	    #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "cpp")
                         (regpath (string-append out "/etc/px/accounts/plugins"))
                         (regdata (string-append "plugin:\n"
                                                 "  name: " ,name "\n"
                                                 "  version: " ,version "\n"
                                                 "  type: " type "\n"
                                                 "  path: " out "/lib/lib" ,name ".so\n")))
                      (display regdata)
                      (mkdir-p regpath)
                      (with-output-to-file (string-append regpath "/" ,name ".yaml")
                        (lambda _
                          (format #t regdata)))
                  )))
            )))
	(inputs `(	
				("rapidjson" ,rapidjson)))
    (propagated-inputs `(	
				("px-oauth2-engine" ,px-oauth2-engine)))
	(native-inputs `(
                ("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Github-OAuth2 Plugin For Online Accounts System")
    (description "Plugin to login, get github notifications and access to git account with OAUTH2 protocol.")
    (license license:expat)))

(define-public px-accounts-service-plugin-oauth2-mastodon
  (package
    (name "px-accounts-service-plugin-oauth2-mastodon")
    (version "v0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
                "https://source.pantherx.org/" name "_"
                version ".tgz"))
        (sha256
          (base32 "05pd5x0cksw6q5pzq03cymbxlycmbam5a0jy16pvx4p1m4kwwl0z"))))
    (build-system cmake-build-system)
	(arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "cpp")
                         (regpath (string-append out "/etc/px/accounts/plugins"))
                         (regdata (string-append "plugin:\n"
                                                 "  name: " ,name "\n"
                                                 "  version: " ,version "\n"
                                                 "  type: " type "\n"
                                                 "  path: " out "/lib/lib" ,name ".so\n")))
                      (display regdata)
                      (mkdir-p regpath)
                      (with-output-to-file (string-append regpath "/" ,name ".yaml")
                        (lambda _
                          (format #t regdata)))))))))
    (inputs `(("rapidjson" ,rapidjson)
              ("curl" ,curl)))
    (propagated-inputs `(("px-oauth2-engine" ,px-oauth2-engine)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Mastodon-OAuth2 protocol plugin for (Online) Accounts Service")
    (description "Plugin to login and access to mastodon account with OAUTH2 protocol.")
    (license license:expat)))

(define-public px-accounts-service-plugin-oauth2-google
  (package
    (name "px-accounts-service-plugin-oauth2-google")
    (version "v0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
                "https://source.pantherx.org/" name "_"
                version ".tgz"))
        (sha256
          (base32 "1h8v3p9yf8b5p207fs70lzkm9g4x1idqgd4alqd4661lpxphr140"))))
    (build-system cmake-build-system)
	(arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "cpp")
                         (regpath (string-append out "/etc/px/accounts/plugins"))
                         (regdata (string-append "plugin:\n"
                                                 "  name: " ,name "\n"
                                                 "  version: " ,version "\n"
                                                 "  type: " type "\n"
                                                 "  path: " out "/lib/lib" ,name ".so\n")))
                      (display regdata)
                      (mkdir-p regpath)
                      (with-output-to-file (string-append regpath "/" ,name ".yaml")
                        (lambda _
                          (format #t regdata)))))))))
    (inputs `(("rapidjson" ,rapidjson)))
    (propagated-inputs `(	
				("px-oauth2-engine" ,px-oauth2-engine)))
    (native-inputs `(
              ("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Google-OAuth2 protocol plugin for (Online) Accounts Service")
    (description "Plugin to login and access to Google account with OAUTH2 protocol.")
    (license license:expat)))


(define-public px-accounts-service-plugin-claws-mail
  (package
   (name "px-accounts-service-plugin-claws-mail")
   (version "0.2.9")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
     (sha256 (base32 "0fvq8sqiph6hkwcz8ya93lr0wd2faczi6xi6vjbqqmwmzlx35xxn"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-version
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* (string-append "plugins/" ,name ".yaml")
                         (("PLUGIN_PATH") (assoc-ref outputs "out"))
                         (("PLUGIN_VERSION") ,version))))
        (delete 'sanity-check))))
   (propagated-inputs
    `(("python-pbkdf2" ,python-pbkdf2)
      ("python-pycryptodomex" ,python-pycryptodomex)
      ("python-imap-tools" ,python-imap-tools)
      ("python-lxml" ,python-lxml)))
   (home-page "https://www.pantherx.org/")
   (synopsis "Claws Mail plugin for Online Accounts Service")
   (description "Support Claws Mail account for Online Accounts Service")
   (license license:expat)))

(define-public px-accounts-service-plugin-s3
  (package
    (name "px-accounts-service-plugin-s3")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "0zqaxfh2vks0a6wgh2qs9nf3dbprnvr8vyv3hq448wx5db9yrbyz"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                        (type "python")
                        (regpath (string-append out "/etc/px/accounts/plugins"))
                        (regdata (string-append "plugin:"
                                                "\n  name: " ,name
                                                "\n  version: " ,version
                                                "\n  type: " type
                                                "\n  path: " out
                                                "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata))))))
            (delete 'sanity-check))))
    (propagated-inputs
      `(("python-botocore" ,python-botocore)
        ("px-accounts-service-plugin-common", px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "S3 protocol support for (Online) Accounts Service")
    (description "Adds support for S3 (AWS/Wasabi) to Accounts Service")
    (license license:expat)))

(define-public px-accounts-service-plugin-cm
  (package
   (name "px-accounts-service-plugin-cm")
   (version "0.1.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
     (sha256 (base32 "11rs7jrx69798fr19byp9grqr4i3ym3hmwnffand36dh9niz8sz0"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-pyqt" ,python-pyqt)
      ("python-pyyaml" ,python-pyyaml)
      ("python-qrcode" ,python-qrcode)
      ("python-requests" ,python-requests)))
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'reguster-plugin
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "plugins/px-accounts-service-plugin-cm.yaml"
              (("PLUGIN_PATH") (assoc-ref outputs "out"))
              (("PLUGIN_VERSION") ,version)))))))
   (home-page "https://www.pantherx.org/")
    (synopsis "Central Management plugin for Online Accounts Service")
    (description "Add support for the Central Management to the
online accounts service")
    (license license:expat)))


(define-public px-accounts-service-plugin-carddav
  (package
    (name "px-accounts-service-plugin-carddav")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "0hgq3vwdcvswikf6ii4rhl6aykmb6rb2n7naa5m9s65zxsc1d7rk"))))
    (build-system python-build-system)
    (arguments `(
      #:tests? #f,
      #:phases
        (modify-phases %standard-phases
          (add-after 'install 'register-plugin
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (type    "python")
                     (regpath (string-append out "/etc/px/accounts/plugins"))
                     (regdata (string-append "plugin:\n"
                                             "  name: " ,name "\n"
                                             "  version: " ,version "\n"
                                             "  type: " type "\n"
                                             "  path: " out "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata))))))
          (delete 'sanity-check))))
    (propagated-inputs
      `(("python-requests" ,python-requests)
        ("px-accounts-service-plugin-common", px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "CardDAV protocol plugin (Online) Accounts Service")
    (description "Provides CardDAV support to Online Accounts System")
    (license license:expat)))

(define-public px-accounts-service-plugin-backup-local
  (package
    (name "px-accounts-service-plugin-backup-local")
    (version "0.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "0ycjd8gyd53s6hc7xvbf9bmswx53xa73pq2a4mhljavgcyldsakk"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                        (type "python")
                        (regpath (string-append out "/etc/px/accounts/plugins"))
                        (regdata (string-append "plugin:"
                                                "\n  name: " ,name
                                                "\n  version: " ,version
                                                "\n  type: " type
                                                "\n  path: " out
                                                "\n")))
                  (display regdata)
                  (mkdir-p regpath)
                  (with-output-to-file (string-append regpath "/" ,name ".yaml")
                    (lambda _ (format #t regdata))))))
            (delete 'sanity-check))))
    (propagated-inputs
      `(("px-accounts-service-plugin-common", px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Local backup account plugin for Online Accounts Service")
    (description "Adds support to save a password for
automatic, local backups with px-backup, to Online Accounts Service.")
    (license license:expat)))


(define-public px-accounts-service-plugin-activity-watch
  (package
   (name "px-accounts-service-plugin-activity-watch")
   (version "0.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
     (sha256 (base32 "1zgl625ay8g8xlwc7bgchcd7yx9y43kjxnpgxxc90kg737lvaa54"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'fix-path
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "plugins/px-accounts-service-plugin-activity-watch.yaml"
                 (("PLUGIN_PATH") (assoc-ref outputs "out"))
                 (("PLUGIN_VERSION") ,version))))
        (delete 'sanity-check))))
   (propagated-inputs
    `(("px-accounts-service" ,px-accounts-service)
      ("px-accounts-service-plugin-common" ,px-accounts-service-plugin-common)
      ("python-appdirs" ,python-appdirs)
      ("python-psutil" ,python-psutil)
      ("python-pyyaml" ,python-pyyaml)))
   (home-page "https://www.pantherx.org")
   (synopsis "ActivityWatch plugin for Online Accounts Service")
   (description "Support ActivityWatch for PantherX Online Accounts Service")
   (license license:expat)))


(define-public px-accounts-service-plugin-maestral
  (package
   (name "px-accounts-service-plugin-maestral")
   (version "0.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
     (sha256 (base32 "0fsivz2mxzpgjpgspr3pggj8wjanvnmipwpj2hq7c7s3wqzd499i"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'fix-path
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "plugins/px-accounts-service-plugin-maestral.yaml"
                         (("PLUGIN_PATH") (assoc-ref outputs "out"))
                         (("PLUGIN_VERSION") ,version))))
        (delete 'sanity-check))))

   (propagated-inputs
    `(("px-accounts-service" ,px-accounts-service)
      ("px-accounts-service-plugin-common" ,px-accounts-service-plugin-common)
      ("python-click" ,python-click-8)
      ("python-pyqt" ,python-pyqt)
      ("python-maestral" ,python-maestral)))
   (home-page "https://www.pantherx.org")
   (synopsis "Dropbox (Maestral) plugin for Online Accounts Service")
   (description "Support Dropbox (Maestral) plugin for PantherX Online Accounts Service")
   (license license:expat)))


(define-public px-accounts-service-plugin-matrix
  (package
    (name "px-accounts-service-plugin-matrix")
    (version "0.0.2")
    (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
     (sha256 (base32 "16f9fzv57p4zb047wxj4d8kw49h9k3m8wi22bi9914svd7nh46dv"))))
    (build-system python-build-system)
    (arguments 
    `(#:tests? #f
      #:phases
        (modify-phases %standard-phases
          (delete 'sanity-check)
          (add-after 'install 'register-plugin
            (lambda* (#:key inputs outputs #:allow-other-keys) 
              (let* ((out     (assoc-ref outputs "out"))
                     (regpath (string-append out "/etc/px/accounts/plugins")))
                (mkdir-p regpath)
                (call-with-output-file (string-append regpath "/" ,name ".yaml")
                  (lambda (port)
                    (format port (string-append "plugin:\n"
                                                "   name: ~a\n"
                                                "   version: ~a\n"
                                                "   type: python\n"
                                                "   path: ~a\n")
                                                ,name , version out)))))))))
    (propagated-inputs
      `(("python-requests" ,python-requests)
        ("px-accounts-service-plugin-common" ,px-accounts-service-plugin-common)))
    (home-page "https://www.pantherx.org")
    (synopsis "Matrix plugin for Online Accounts Service")
    (description "Matrix plugin for Online Accounts Service")
    (license license:expat)))
