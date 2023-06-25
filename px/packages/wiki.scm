;;; throttled PantherX
;;; Author: Hamzeh Nasajpour (h.nasajpour@pantherx.org)
;;;

(define-module (px packages wiki)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (px  packages library))
 
(define-public px-desktop-wiki
  (package
    (name "px-desktop-wiki")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
          "https://source.pantherx.org/" name "_" version ".tgz"))
        (sha256
          (base32
            "1xzr6b8f2478s81kngp8hd7qvvhx6nv6xja396sfhp78y04q5yw1"))))
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
    (propagated-inputs `(("px-wiki" ,px-wiki)))
    (native-inputs `(
              ("pkg-config" ,pkg-config)
              ("qt" ,qtbase-5)
              ("qtdeclarative" ,qtdeclarative-5)
              ("qtwebengine" ,qtwebengine-5)
              ("qtwebchannel-5" ,qtwebchannel-5)
              ("px-gui-library" ,px-gui-library)))
    (home-page "https://wiki.pantherx.org/")
    (synopsis "PantherX Wiki Desktop Application")
    (description "PantherX Wiki Desktop Application")
    (license license:expat)))
    
(define-public px-wiki
  (package
    (name "px-wiki")
    (version "0.0.15")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://source.pantherx.org/wiki.pantherx.org_v" version ".tgz"))
             (sha256
              (base32
               "05j1fk64yph8z2irw7vfr225xvgp8zhax3vm9awpfi3hy2hsd07x"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source   (assoc-ref %build-inputs "source"))
                          (tar      (assoc-ref %build-inputs "tar"))
                          (gzip     (assoc-ref %build-inputs "gzip"))
                          (wiki-dir (string-append %output "/share/px-wiki")))
                     (mkdir-p wiki-dir)
                     (setenv "PATH" (string-append gzip "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf" source "-C" wiki-dir)))))
    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)))
    (inputs `(("bash" ,bash)))
    (home-page "https://www.wiki.pantherx.org/")
    (synopsis "PantherX Wiki")
    (description
     "PantherX Wiki - Offline html files.")
    (license license:expat)))
