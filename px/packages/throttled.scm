;;; throttled PantherX
;;; Hamzeh Nasajpour (h.nasajpour@pantherx.org)
;;;

(define-module (px packages throttled)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public throttled
  (package
    (name "throttled")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/throttled_v" version
                           ".tgz"))
       (sha256
        (base32 "0xqdsrkn00vywqm8y7rv141ip7psajrq7g92c9ph4d134lfqclnp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (bash (assoc-ref %build-inputs "bash"))
                          (coreutils (assoc-ref %build-inputs "coreutils"))
                          (dbus (assoc-ref %build-inputs "dbus"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (python (assoc-ref %build-inputs "python"))
                          (python-dbus (assoc-ref %build-inputs "python-dbus"))
                          (python-pygo (assoc-ref %build-inputs
                                                  "python-pygobject"))
                          (pythonpath (string-append "/lib/python"
                                                     ,(version-major+minor (package-version
                                                                            python))
                                                     "/site-packages:"))
                          (tar (assoc-ref %build-inputs "tar"))
                          (bin-dir (string-append %output "/bin"))
                          (etc-dir (string-append %output "/etc/throttled"))
                          (python-dir (string-append %output "/python"))
                          (root-dir "throttled")
                          (bin-script (string-append root-dir
                                                     "/runit/lenovo_fix/run"))
                          (py-script (string-append root-dir "/lenovo_fix.py")))
                     (mkdir-p bin-dir)
                     (mkdir-p etc-dir)
                     (mkdir-p python-dir)
                     (setenv "PATH"
                             (string-append (string-append bash "/bin:")
                                            (string-append coreutils "/bin:")
                                            (string-append gzip "/bin:")
                                            (string-append tar "/bin:")))
                     (invoke "tar" "xvf" source)
                     (invoke "cp"
                             (string-append root-dir "/etc/lenovo_fix.conf")
                             (string-append etc-dir "/default.conf"))
                     (invoke "cp"
                             (string-append root-dir "/mmio.py") python-dir)
                     (substitute* py-script
                       (("/etc/throttled")
                        etc-dir))
                     (invoke "cp" py-script python-dir)
                     (substitute* bin-script
                       (("/opt/lenovo_fix/venv")
                        python))
                     (substitute* bin-script
                       (("/opt/lenovo_fix/lenovo_fix.py")
                        (string-append python-dir "/lenovo_fix.py" " $@")))
                     (invoke "mv" bin-script
                             (string-append bin-dir "/throttled"))
                     (wrap-program (string-append bin-dir "/throttled")
                       `("PYTHONPATH" ":" prefix
                         (,(string-append python-dbus pythonpath))))
                     (wrap-program (string-append bin-dir "/throttled")
                       `("PYTHONPATH" ":" prefix
                         (,(string-append python-pygo pythonpath))))

                     #t))))
    (native-inputs `(("coreutils" ,coreutils)
                     ("gzip" ,gzip)
                     ("python" ,python)
                     ("tar" ,tar)))
    (inputs `(("bash" ,bash)
              ("python-dbus" ,python-dbus)
              ("python-pygobject" ,python-pygobject)))
    (home-page "https://github.com/erpalma/throttled")
    (synopsis "Fix Intel CPU Throttling on Linux")
    (description
     "This tool was originally developed to fix Linux CPU throttling issues affecting Lenovo T480 / T480s / X1C6.")
    (license license:expat)))
