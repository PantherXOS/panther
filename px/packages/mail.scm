;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages mail)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages man)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages rust)
  #:use-module (px self))

(define-public pimsync
  (package
    (name "pimsync")
    (version "0.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~whynothugo/pimsync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12vqlz8sc4yh5h4vnhcab87xcjgfv468nrjrm914fnxl780m5isl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:rust ,rust-1.88
       #:phases
       (modify-phases %standard-phases
         (delete 'package)
         (add-after 'unpack 'set-version
           (lambda _
             (setenv "PIMSYNC_VERSION" ,version))))))
    (native-inputs
     (list git-minimal scdoc))
    (inputs
     (cons* sqlite (px-cargo-inputs 'pimsync)))
    (home-page "https://pimsync.whynothugo.nl/")
    (synopsis "Synchronize calendars and contacts")
    (description
     "pimsync is a tool for synchronizing calendars and contacts between
local storage and remote servers using the CalDAV and CardDAV protocols.")
    (license license:eupl1.1)))

(define-public himalaya
  (package
    (name "himalaya")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "himalaya" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j9j5qydnr98w5qd0xv56yvqj5gm259ps129q5scq9f93sb3ysz6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (px-cargo-inputs 'himalaya))
    (home-page "https://pimalaya.org/")
    (synopsis "CLI to manage emails")
    (description
     "Himalaya is a command-line interface for managing emails, providing a
modern and efficient way to interact with email accounts.  It supports IMAP,
Maildir, Notmuch, SMTP, and Sendmail backends, along with OAuth 2.0
authorization for various email providers including Gmail, Outlook, and iCloud.")
    (license license:expat)))

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