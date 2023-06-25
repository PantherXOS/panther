;;; Time Tracking  Packages Module for PantherX
;;; Author: Fakhri Sajadi (f.sajadi@pantherx.org)
;;;

(define-module (px packages time-tracking)
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
  #:use-module (px packages dav)
  #:use-module (px packages databases)
  #:use-module (px packages library)
  #:use-module (px packages python-xyz)
  #:use-module (px packages hub)
  #:use-module (guix gexp))


(define-public px-time-tracking
  (package
    (name "px-time-tracking")
    (version "0.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
          "https://source.pantherx.org/px-time-tracking_" version ".tgz"))
        (sha256
          (base32
             "0d34fp4vzxh191ilp19m4ms91zxr2pdsah0wcb98vsw8hi8wf71w"))))
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
              ("qtcharts" ,qtcharts)
              ("qtbase" ,qtbase-5)))
    (native-inputs `(
              ("pkg-config" ,pkg-config)
              ("python" ,python)
              ("px-gui-library" ,px-gui-library)
              ("pybind11" ,pybind11)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Time Tracking Service")
    (description "This package provides background services to tracking time spend on issues in PantherX")
    (license license:expat)))


(define-public px-time-tracking-plugin-gitlab
  (package
    (name "px-time-tracking-plugin-gitlab")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
                  "https://source.pantherx.org/" name "_" version ".tgz"))
        (sha256 (base32 "1qq9pdmyj0ar6l8m49n7n3bmzjfbxim7h3wjvg9znizz6lm2hw98"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
          (modify-phases %standard-phases
            (add-after 'install 'register-plugin
               (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out     (assoc-ref outputs "out"))
                         (type "python")
                         (regpath (string-append out "/etc/px/time-tracking/plugins"))
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
    (synopsis "GitLab plugin for PantherX Time Tracking Service")
    (description "Adds support to retrieve data from GitLab API.")
    (license license:expat)))
