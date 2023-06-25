(define-module (px packages email)
  #:use-module (guix build-system qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages search)
  #:use-module (gnu packages gnupg)
  #:use-module (guix git-download))


(define-public kasync
  (package
   (name "kasync")
   (version "0.3.0")
   (home-page "https://kube-project.com/")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/KDE/kasync")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0k5jrmyqa0y4vfvg6sjr3mf12p62b1f7l3ryvy3wg1app63rssaf"))))
   (build-system qt-build-system)
   (native-inputs
    (list extra-cmake-modules kdoctools pkg-config))
   (inputs
    (list
     qtbase-5))
   (arguments
    `(#:tests? #f))
   (synopsis "Library for composable asynchronous code
using a continuation based approach")
   (description "KAsync helps writing composable asynchronous code
using a continuation based approach.")
   (license license:gpl2+)))

(define-public kimap2
  (package
   (name "kimap2")
   (version "0.4.0")
   (home-page "https://kube-project.com/")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/KDE/kimap2")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "16ax75xq0lqzjf72xr7gci5vv55kf8r5iyr3s27y0j32fjr25l25"))))
   (build-system qt-build-system)
   (native-inputs
    (list extra-cmake-modules kdoctools pkg-config))
   (inputs
    (list
     qtbase-5
     kcoreaddons
     kcodecs
     kmime
     cyrus-sasl))
   (arguments
    `(#:tests? #f))
   (synopsis "This library provides a job-based API for interacting with an IMAP4rev1 server")
   (description "This library provides a job-based API for interacting with an IMAP4rev1 server.
It manages connections, encryption and parameter quoting and encoding,
but otherwise provides quite a low-level interface to the protocol.
This library does not implement an IMAP client; it merely makes it easier to do so.")
   (license license:gpl2+)))

(define-public kdav2
  (package
   (name "kdav2")
   (version "0.4.0")
   (home-page "https://kube-project.com/")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/KDE/kdav2")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1sy1kiwx8six711frpav7a3py7g66cv8ryy0my7rr7kdv8r82r7k"))))
   (build-system qt-build-system)
   (native-inputs
    (list extra-cmake-modules kdoctools pkg-config))
   (inputs
    (list
     qtbase-5
     qtxmlpatterns
     kcoreaddons))
   (arguments
    `(#:tests? #f))
   (synopsis "This is an DAV protocol implemention with KJobs")
   (description "This is an DAV protocol implemention with KJobs.
Calendars and todos are supported, using either GroupDAV
or CalDAV, and contacts are supported using GroupDAV or
CardDAV.")
   (license license:gpl2+)))