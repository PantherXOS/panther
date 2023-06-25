;;; Backup Packages Module for PantherX

(define-module (px packages backup)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages backup)
  #:use-module (px packages accounts)
  #:use-module (px packages library)
  #:use-module (px packages tarsnap))
 
 (define-public px-org-remote-backup-service
  (package
    (name "px-org-remote-backup-service")
    (version "v0.0.6")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://source.pantherx.org/px-org-remote-backup-service_" 
          version 
          ".tgz"))
             (sha256
              (base32
               "062p4p6jf8y8cw2pdxj50sw00sw9lfh404168ljhqqgzwhdxpp6p"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (tar    (assoc-ref %build-inputs "tar"))
                          (ls    (assoc-ref %build-inputs "source"))
                          (gzip     (assoc-ref %build-inputs "gzip"))
                          (bin-dir  (string-append %output "/bin"))
						  (backup-bin-file (string-append bin-dir "/" ,"px-org-remote-backup-create.sh"))
						  (restore-bin-file (string-append bin-dir "/" ,"px-org-remote-backup-restore.sh"))
                          (bash-bin (string-append (assoc-ref %build-inputs "bash")
                                         "/bin")))
                     (mkdir-p bin-dir)
					 (setenv "PATH" (string-append gzip "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf" source "-C" bin-dir)
				     (patch-shebang backup-bin-file (list bash-bin))
				     (patch-shebang restore-bin-file (list bash-bin))
   	     			 (chmod backup-bin-file #o555)
					 (chmod restore-bin-file #o555)))))
    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)
					 ))
	(inputs `(("bash" ,bash)))
	(propagated-inputs `(("tarsnap" ,tarsnap)))
	(home-page "https://www.pantherx.org/")
    (synopsis "PantherX Backup/Restore shell scripts")
    (description
     "This package provides two scripts for create backup and restore based on given user as parameter.")
    (license license:expat)))


(define-public px-backup
  (package
    (name "px-backup")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256 (base32 "0pzmq8kiw740nj1iswba5k7j0rqicrai27mbl2xiqf1k5r3406ra"))))
    (build-system python-build-system)
    (propagated-inputs 
      `(("restic", restic)
        ("python-psutil", python-psutil)
        ("python-appdirs", python-appdirs)
        ("python-pyyaml", python-pyyaml)
        ("px-secret-library-python", px-secret-library-python)
        ("px-accounts-library-python", px-accounts-library-python)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Client library and CLI tool for easy backup")
    (description "Python library that integrates with Accounts and Secrets
to provide a more automated backup experience. The CLI may be
accessed via: px-backup-cli")
    (license license:expat)))
