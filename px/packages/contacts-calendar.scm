(define-module (px packages contacts-calendar)
  #:use-module (guix licenses)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dav)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (px packages etesync)
  #:use-module (px packages common)
  #:use-module (px packages library)
  #:use-module (px packages python-xyz))

(define-public px-etesync-dav
  (package
    (inherit etesync-dav)
    (name "px-etesync-dav")
    (version "0.20.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/px-contacts-calendar-service_v" version ".tgz"))
       (sha256
        (base32 "1hllxmh6w6bj4d0sbzdxmvs3n8m0q1w2d812m1vr5v4pr8xj2w5q"))))))

(define-public px-contacts-calendar
  (package
    (name "px-contacts-calendar")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/px-contacts-calendar_v" version ".tgz"))
       (sha256
        (base32 "1mrp683izfnqp9pvgl44kwm9vd29fv8d515vd5r2ihn5sa39sfdz"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'sanity-check))))
    (inputs
      `(("python-cryptography" ,python-cryptography)
        ("python-etebase" ,python-etebase)
        ("python-lxml" ,python-lxml)
        ("python-requests" ,python-requests-2.23)
        ("python-pysocks" ,python-pysocks)
        ("python-urllib3" ,python-urllib3)
        ("python-vobject" ,python-vobject)
        ("python-peewee" ,python-peewee)
        ("python-pycapnp" ,python-pycapnp)
        ("python-pyyaml" ,python-pyyaml-v5.3.1)
        ("python-appdirs" ,python-appdirs)))
    (home-page "https://git.pantherx.org/development/applications/px-contacts-calendar")
    (synopsis "Sync and access PIM related data like contacts and calendar.")
    (description "Sync and access PIM related data like contacts and calendar.")
    (license license:expat)))

(define-public px-contacts
  (package
    (name "px-contacts")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
          "https://source.pantherx.org/" name "_" version ".tgz"))
        (sha256
          (base32
             "0vsnyi4m8r92qfrslv8060813fzr6svcghn37i3ib37wf3nv0rzr"))))
    (build-system cmake-build-system)
    (arguments
      `(
        #:tests? #f))
    (inputs `(
              ("capnproto" ,capnproto-0.9)
              ("px-gui-library" ,px-gui-library)
              ("qtbase" ,qtbase-5)))
    (native-inputs `(
	      ("pkg-config" ,pkg-config)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Contacts Management Application")
    (description "This package provides a gui application to manage you contacts.")
    (license license:expat)))
