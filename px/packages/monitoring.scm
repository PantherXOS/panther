;;; Fakhri Sajadi (f.sajadi@pantherx.org)
;;;

(define-module (px packages monitoring)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages linux)
  #:use-module (px packages common)
  #:use-module (px packages device)
  #:use-module (px packages library))

(define-public monit
  (package
    (name "monit")
    (version "5.25.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mmonit.com/monit/dist/monit-" version
                           ".tar.gz"))
       (sha256
        (base32 "0s8577ixcmx45b081yx6cw54iq7m5yzpq3ir616qc84xhg45h0n1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--without-pam" "--without-ssl")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-source
                    (lambda _
                      (invoke "ls" "-la"))))))
    (inputs `(("zlib" ,zlib)))

    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("pkg-config" ,pkg-config)))
    (home-page "https://mmonit.com")
    (synopsis "Pro-active Monitoring")
    (description
     "M/Monit can monitor and manage distributed computer systems, conduct automatic maintenance and repair and execute meaningful causal actions in error situations.")
    (license license:expat)))

(define-public px-org-remote-status-service
  (package
    (name "px-org-remote-status-service")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://source.pantherx.org/px-org-remote-status-service_v"
             version ".tgz"))
       (sha256
        (base32 "005n1vvsq25bm1axkhs1vvyzclgabqlkymzlsvjkci2bvv0iya89"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("sqlite" ,sqlite)
              ("curl" ,curl)
              ("restclient-cpp" ,restclient-cpp)
              ("zlib" ,zlib)
              ("yaml-cpp" ,yaml-cpp)
              ("qtbase" ,qtbase-5)
              ("capnproto" ,capnproto)
              ("px-auth-library-cpp" ,px-auth-library-cpp)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs `(("monit" ,monit)
                         ("sysstat" ,sysstat)
                         ("lshw" ,lshw)
                         ("coreutils" ,coreutils)
                         ("curl" ,curl)
                         ("util-linux+udev" ,util-linux+udev)
                         ("px-device-identity" ,px-device-identity)))

    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Remote Status Monitoring Service")
    (description "This package provides background services to show status ")
    (license license:expat)))

(define-public sentry
  (package
    (name "sentry")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/getsentry/sentry-native")
             (commit version)
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rd069j2jrqaj67l32p005jmlfah6blcxrca7h2kqgc8nv33pd6j"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (inputs `(("curl" ,curl)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/getsentry/sentry-native")
    (synopsis "Official Sentry SDK for C/C++")
    (description "The Sentry Native SDK is an error and crash reporting client
for native applications, optimized for C and C++.")
    (license license:expat)))
