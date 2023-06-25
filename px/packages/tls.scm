(define-module (px packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages perl))


(define-public openssl-1.0
  (package
   (name "openssl")
   (version "1.0.2u")
   (source (origin
            (method url-fetch)
            (uri (list (string-append "https://www.openssl.org/source/openssl-"
                                      version ".tar.gz")
                       (string-append "ftp://ftp.openssl.org/source/"
                                      "openssl-" version ".tar.gz")
                       (string-append "ftp://ftp.openssl.org/source/old/"
                                      (string-trim-right version char-set:letter)
                                      "/openssl-" version ".tar.gz")))
            (patches (search-patches "openssl-runpath.patch"
                                     "openssl-c-rehash-in.patch"))
            (sha256
             (base32
              "05lxcs4hzyfqd5jn0d9p0fvqna62v2s4pc9qgmq0dpcknkzwdl7c"))))
   (build-system gnu-build-system)
   (outputs '("out"
              "doc"        ;1.5MiB of man3 pages
              "static"))   ;6MiB of .a files
   (native-inputs (list perl))
   (arguments
    `(#:parallel-tests? #f
      #:test-target "test"

      ;; Changes to OpenSSL sometimes cause Perl to "sneak in" to the closure,
      ;; so we explicitly disallow it here.
      #:disallowed-references ,(list (canonical-package perl))
      #:phases
      ,#~
      (modify-phases %standard-phases
         #$@(if (%current-target-system)
                #~((add-before
                    'configure 'set-cross-compile
                    (lambda* (#:key target #:allow-other-keys)
                      (setenv "CROSS_COMPILE" (string-append target "-"))
                      (setenv "CONFIGURE_TARGET_ARCH"
                              #$(target->openssl-target
                                 (%current-target-system))))))
                #~())
         ;; This test seems to be dependant on kernel features.
         ;; https://github.com/openssl/openssl/issues/12242
         #$@(if (or (target-arm?)
                    (target-riscv64?))
                #~((replace 'check
                     (lambda* (#:key tests? test-target #:allow-other-keys)
                       (when tests?
                         (invoke "make" "TESTS=-test_afalg" test-target)))))
                #~())
         (replace 'configure
           ;; Override this phase because OpenSSL 1.0 does not understand -rpath.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke #$@(if (%current-target-system)
                              #~("./Configure")
                              #~("./config"))
                       "shared"                 ;build shared libraries
                       "--libdir=lib"

                       ;; The default for this catch-all directory is
                       ;; PREFIX/ssl.  Change that to something more
                       ;; conventional.
                       (string-append "--openssldir=" out
                                      "/share/openssl-" #$version)

                       (string-append "--prefix=" out)
                       #$@(if (%current-target-system)
                              '((getenv "CONFIGURE_TARGET_ARCH"))
                              '())))))
         (add-after 'install 'move-static-libraries
           (lambda _
             ;; Move static libraries to the "static" output.
             (let* ((out    #$output)
                    (lib    (string-append out "/lib"))
                    (static #$output:static)
                    (slib   (string-append static "/lib")))
               (for-each (lambda (file)
                           (install-file file slib)
                           (delete-file file))
                         (find-files lib "\\.a$")))))
         (add-after 'install 'move-extra-documentation
           (lambda _
             ;; Move man pages and full HTML documentation to "doc".
             (let* ((out    #$output)
                    (man    (string-append out "/share/man"))
                    (html   (string-append out "/share/doc/openssl"))
                    (doc    #$output:doc)
                    (man-target (string-append doc "/share/man"))
                    (html-target (string-append doc "/share/doc/openssl")))
               (mkdir-p (dirname man-target))
               (mkdir-p (dirname html-target))
               (rename-file man man-target)
               (rename-file html html-target))))
         (add-after 'install 'remove-miscellany
           (lambda _
             ;; The 'misc' directory contains random undocumented shell and Perl
             ;; scripts.  Remove them to avoid retaining a reference on Perl.
             (delete-file-recursively (string-append #$output "/share/openssl-"
                                                     #$(package-version this-package)
                                                     "/misc"))))
         (add-before 'patch-source-shebangs 'patch-tests
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((bash (assoc-ref (or native-inputs inputs) "bash")))
               (substitute* (find-files "test" ".*")
                            (("/bin/sh")
                             (string-append bash "/bin/sh"))
                            (("/bin/rm")
                             "rm"))
               #t)))
         (add-before 'configure 'patch-Makefile.org
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The default MANDIR is some unusual place.  Fix that.
             (let ((out (assoc-ref outputs "out")))
               (patch-makefile-SHELL "Makefile.org")
               (substitute* "Makefile.org"
                            (("^MANDIR[[:blank:]]*=.*$")
                             (string-append "MANDIR = " out "/share/man\n")))
               #t)))
         (delete 'move-extra-documentation)
         (add-after 'install 'move-man3-pages
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move section 3 man pages to "doc".
             (let* ((out    (assoc-ref outputs "out"))
                    (man3   (string-append out "/share/man/man3"))
                    (doc    (assoc-ref outputs "doc"))
                    (target (string-append doc "/share/man/man3")))
               (mkdir-p target)
               (for-each (lambda (file)
                 (rename-file file
                              (string-append target "/"
                                             (basename file))))
                         (find-files man3))
               (delete-file-recursively man3)
               #t)))
         ;; XXX: Duplicate this phase to make sure 'version' evaluates
         ;; in the current scope and not the inherited one.
         (replace 'remove-miscellany
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The 'misc' directory contains random undocumented shell and Perl
             ;; scripts.  Remove them to avoid retaining a reference on Perl.
             (let ((out (assoc-ref outputs "out")))
               (delete-file-recursively (string-append out "/share/openssl-"
                                                       #$version "/misc"))
               #t)))
       )))
   (native-search-paths
    (list (search-path-specification
           (variable "SSL_CERT_DIR")
           (separator #f)              ;single entry
           (files '("etc/ssl/certs")))
          (search-path-specification
           (variable "SSL_CERT_FILE")
           (file-type 'regular)
           (separator #f)              ;single entry
           (files '("etc/ssl/certs/ca-certificates.crt")))))
   (synopsis "SSL/TLS implementation")
   (description
    "OpenSSL is an implementation of SSL/TLS.")
   (license license:openssl)
   (home-page "https://www.openssl.org/")))
