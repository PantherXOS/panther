;;; Theme Packages Module for PantherX
;;; Hamzeh Nasajpour (h.nasajpour@pantherx.org)
;;;

(define-module (px packages clawsmail)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression))

(define-public claws-mail-theme-breeze
  (package
    (name "claws-mail-theme-breeze")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.claws-mail.org/download.php?file=themes/png/claws-mail-theme_breeze.tar.gz"))
       (sha256
        (base32 "104ak4m3s7i4d44clpn4kcq4bhjz92ybmpiw83dpg2xwc9w8k2pf"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (tar (assoc-ref %build-inputs "tar"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (theme-dir (string-append %output
                                      "/share/claws-mail/themes")))
                     (mkdir-p theme-dir)
                     (setenv "PATH"
                             (string-append gzip "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf" source "-C"
                             theme-dir)))))
    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)))
    (synopsis "claws-mail breeze theme")
    (home-page "https://www.claws-mail.org/themes.php")
    (description "claws-mail breeze theme")
    (license license:gpl3+)))
