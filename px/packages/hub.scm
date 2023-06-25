;;; Hub Packages Module for PantherX
;;; Author: Fakhri Sajadi (f.sajadi@pantherx.org)
;;;

(define-module (px packages hub)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages qt)
  #:use-module (px packages common)
  #:use-module (px packages networking)
  #:use-module (px packages dav)
  #:use-module (px packages databases)
  #:use-module (px packages library)
  #:use-module (px packages python-xyz)
  #:use-module (guix gexp))

(define-public px-hub-gui
  (package
    (name "px-hub-gui")
    (version "0.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
          "https://source.pantherx.org/" name "_" version ".tgz"))
        (sha256
          (base32
            "0dy2hlgqgjgifn2lyv5rjmvfl56wiwpqn9lszh3m6jdg9jjj31rl"))))
    (build-system cmake-build-system)
    (arguments
      `(
        #:tests? #f))
    (native-inputs `(
              ("pkg-config" ,pkg-config)
              ("qt" ,qtbase-5)
              ("yaml-cpp" ,yaml-cpp)
              ("zlib" ,zlib)
              ("capnproto" ,capnproto-0.9)
              ("px-gui-library" ,px-gui-library)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX HUB Desktop Application")
    (description "PantherX HUB Desktop Application")
    (license license:expat)))

(define-public px-hub-service
  (package
    (name "px-hub-service")
    (version "v0.4.12")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
          "https://source.pantherx.org/px-hub-service_" version ".tgz"))
        (sha256
          (base32
             "0wi2d5cq318jd4igc3b5mknwg93ip14x16lgkghawxwdwgkx5nhk"))))
    (build-system cmake-build-system)
    (arguments
      `(
        #:tests? #f))
    (inputs `(
              ("sqlite" ,sqlite)
              ("sqlitecpp" ,sqlitecpp)
              ("zlib" ,zlib)
              ("yaml-cpp" ,yaml-cpp)
              ("capnproto" ,capnproto-0.9)
              ("px-cal-card-dav-lib" ,px-cal-card-dav-lib)
              ("qtbase" ,qtbase-5)))
    (native-inputs `(
	      ("pkg-config" ,pkg-config)
	      ("nng" ,nng-1.5)
	      ("python" ,python)
	      ("pybind11" ,pybind11)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Hub Management Service")
    (description "This package provides background services to show status of accounts in PantherX")
    (license license:expat)))


(define-public px-hub-service-plugin-common
  (package
    (name "px-hub-service-plugin-common")
    (version "0.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "1vg6cjgav53s23xv4ymh11n2r7yg1wpp7pjajmwsg0mkcnv5ij2k"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'sanity-check))))
    (propagated-inputs
          `(("python-pycapnp" ,python-pycapnp)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Hub Service Plugin Commons")
    (description "Python classes required for all Hub Service plugins.")
    (license license:expat)))


(define-public px-hub-service-plugin-claws-mail
  (package
    (name "px-hub-service-plugin-claws-mail")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
          "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "18n9v8cnp9fdl29h1ms9n05b9rs3fsgnzkmpk7yh8cdbvgpam7ps"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "python")
                         (regpath (string-append out "/etc/px/hub/plugins"))
                         (regdata (string-append "plugin:\n"
                                                 "  name: " ,name "\n"
                                                 "  version: " ,version "\n"
                                                 "  type: " type "\n"
                                                 "  path: " out "\n")))
                      (display regdata)
                      (mkdir-p regpath)
                      (with-output-to-file (string-append regpath "/" ,name ".yaml")
                        (lambda _
                          (format #t regdata))))))
            (delete 'sanity-check))))
    (propagated-inputs
          `(("px-claws-mail-parser", px-claws-mail-parser)
            ("px-hub-service-plugin-common", px-hub-service-plugin-common)))
    (home-page "https://www.pantherx.org/")
    (synopsis "claws-mail plugin for HUB Service")
    (description "Claws-Mail plugin for HUB Service")
    (license license:expat)))


(define-public px-hub-service-plugin-github
  (package
    (name "px-hub-service-plugin-github")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "01jfdfkmag4lgr3wfh9fvhh9bvc0k81m41mh7akdafylbcqliqd2"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                        (type "python")
                        (regpath (string-append out "/etc/px/hub/plugins"))
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
          `(("px-hub-service-plugin-common", px-hub-service-plugin-common)
            ("px-online-sources-library", px-online-sources-library)))
    (home-page "https://www.pantherx.org/")
    (synopsis "GitHub OAUTH2 plugin for PantherX Hub Service")
    (description "Adds support to retrieve data from GitHub API.")
    (license license:expat)))


(define-public px-hub-service-plugin-gitlab
  (package
    (name "px-hub-service-plugin-gitlab")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append 
                  "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "0zajd6wr523hchslx47czgss1gw6m633ga5ybrxh6khhccfp38vj"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "python")
                         (regpath (string-append out "/etc/px/hub/plugins"))
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
          `(("px-hub-service-plugin-common", px-hub-service-plugin-common)
            ("px-online-sources-library", px-online-sources-library)))
    (home-page "https://www.pantherx.org/")
    (synopsis "GitLab plugin for PantherX Hub Service")
    (description "Adds support to retrieve data from GitLab API.")
    (license license:expat)))

(define-public px-hub-service-plugin-discourse
  (package
    (name "px-hub-service-plugin-discourse")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
                 "https://source.pantherx.org/" name "_" version ".tgz"))
        (sha256 (base32 "1hz9sqlc2ldnx0jh4sglprzqs0q57h1q6gj5wk3hynb0l0dfh4nb"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "python")
                         (regpath (string-append out "/etc/px/hub/plugins"))
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
          `(("px-hub-service-plugin-common", px-hub-service-plugin-common)
            ("px-online-sources-library", px-online-sources-library)))
    (home-page "https://www.pantherx.org/")
    (synopsis "GitLab plugin for PantherX Hub Service")
    (description "Adds support to retrieve data from GitLab API.")
    (license license:expat)))




(define-public px-hub-service-plugin-cpp-test
  (package
    (name "px-hub-service-plugin-cpp-test")
    (version "v0.0.8")
    (source
      (origin
	(method url-fetch)
	(uri (string-append
	       "https://source.pantherx.org/" name "_" version ".tgz"))
	(sha256
	  (base32
	    "0y1jpcbc75774ghv34sjsf2qx6b25fjg9h8q0lzyb4q41aaj9j8d"))))
    (build-system cmake-build-system)
    (arguments
      `(#:tests? #f
	  #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "cpp")
                         (regpath (string-append out "/etc/px/hub/plugins"))
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
			  ("zlib", zlib)
              ("yaml-cpp", yaml-cpp)))		
    (home-page "https://www.pantherx.org/")
    (synopsis "CPP Test Plugin For Hub service")
    (description "Test Plugin for Hub service, this plugin needs
		 to be installed in order to tests run properly.")
    (license license:expat)))
	
(define-public px-hub-service-plugin-python-single-service
  (package
    (name "px-hub-service-plugin-python-single-service")
    (version "v0.0.15")
    (source
      (origin
	(method url-fetch)
	(uri (string-append
	       "https://source.pantherx.org/" name "_"
	       version
	       ".tgz"))
	(sha256
	  (base32
	    "057ph59r79j0cnfv1qy9vpi7lm53zikf8kbbxf4vxpw5wqpswrzd"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "python")
                         (regpath (string-append out "/etc/px/hub/plugins"))
                         (regdata (string-append "plugin:\n"
                                                 "  name: " ,name "\n"
                                                 "  version: " ,version "\n"
                                                 "  type: " type "\n"
                                                 "  path: " out "\n")))
                      (display regdata)
                      (mkdir-p regpath)
                      (with-output-to-file (string-append regpath "/" ,name ".yaml")
                        (lambda _
                          (format #t regdata)))
                  )))
            )))
    (home-page "https://www.pantherx.org/")
    (synopsis "Test Plugin For Hub service")
    (description "Test Plugin (single service to multiple plugin mode) for hub service, this plugin needs
		 to be installed in order to tests run properly.")
    (license license:expat)))
    
    
(define-public px-hub-service-plugin-python-multi-service
  (package
    (name "px-hub-service-plugin-python-multi-service")
    (version "v0.0.6")
    (source
      (origin
	(method url-fetch)
	(uri (string-append
	       "https://source.pantherx.org/" name "_"
	       version
	       ".tgz"))
	(sha256
	  (base32
	    "1vzgf5f6s34v5942nv5ssb8w21l3r4g9hc3yv8iycvpj54ma182l"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "python")
                         (regpath (string-append out "/etc/px/hub/plugins"))
                         (regdata (string-append "plugin:\n"
                                                 "  name: " ,name "\n"
                                                 "  version: " ,version "\n"
                                                 "  type: " type "\n"
                                                 "  path: " out "\n")))
                      (display regdata)
                      (mkdir-p regpath)
                      (with-output-to-file (string-append regpath "/" ,name ".yaml")
                        (lambda _
                          (format #t regdata)))
                  )))
            )))
    (home-page "https://www.pantherx.org/")
    (synopsis "Test Plugin-1 For Hub service")
    (description "Test Plugin (multiple service to single plugin mode) for hub service, this plugin needs
		 to be installed in order to tests run properly.")
    (license license:expat)))
    

(define-public px-hub-service-plugin-data-service
  (package
    (name "px-hub-service-plugin-data-service")
    (version "v0.0.5")
    (source
      (origin
	(method url-fetch)
	(uri (string-append
	       "https://source.pantherx.org/px-hub-service-plugin-data-service_"
	       version
	       ".tgz"))
	(sha256
	  (base32
	    "0b17q21pqp45s79qwqfnjhrra2dg9xipgypdd5j8gwb1j0r0ifks"))))
    (build-system python-build-system)

    (arguments
      `(#:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "python")
                         (regpath (string-append out "/etc/px/hub/plugins"))
                         (regdata (string-append "plugin:\n"
                                                 "  name: " ,name "\n"
                                                 "  version: " ,version "\n"
                                                 "  type: " type "\n"
                                                 "  path: " out "\n")))
                      (display regdata)
                      (mkdir-p regpath)
                      (with-output-to-file (string-append regpath "/" ,name ".yaml")
                        (lambda _
                          (format #t regdata)))
                  )))
            )))

    (home-page "https://www.pantherx.org/")
    (synopsis "Data service Plugin For hub service")
    (description "Provides data support to hub service")
    (license license:expat)))
    
        
(define-public px-hub-service-plugin-mastodon
  (package
    (name "px-hub-service-plugin-mastodon")
    (version "v0.1.1")
    (source
      (origin
	(method url-fetch)
	(uri (string-append
	       "https://source.pantherx.org/" name "_" version ".tgz"))
	(sha256
	  (base32
	    "0gm5if2a73s30mp7n7nj6r4rnidnaahyrzi7mxqy8zsissagn94c"))))
    (build-system cmake-build-system)
    (arguments
      `(#:tests? #f
	  #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "cpp")
                         (regpath (string-append out "/etc/px/hub/plugins"))
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
              ("zlib", zlib)
              ("yaml-cpp", yaml-cpp)
              ("capnproto", capnproto-0.9)))
    (native-inputs `(
	      ("pkg-config", pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "CPP Test Plugin For Hub service")
    (description "Test Plugin for Hub service, this plugin needs
		 to be installed in order to tests run properly.")
    (license license:expat)))


