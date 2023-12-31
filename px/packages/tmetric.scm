(define-module (px packages tmetric)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages base)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg))

(define-public tmetric
  (package
    (name "tmetric")
    (version "20.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0ssw356b6a4z14vw74pdcplz326x6qwxpx6gnwgfg2s004w6raj8"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (atk (assoc-ref %build-inputs "atk"))
                          (bash (assoc-ref %build-inputs "bash"))
                          (cairo (assoc-ref %build-inputs "cairo"))
                          (coreutils (assoc-ref %build-inputs "coreutils"))
                          (gdk-pixbuf (assoc-ref %build-inputs "gdk-pixbuf"))
                          (glib (assoc-ref %build-inputs "glib"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (gtk (assoc-ref %build-inputs "gtk+"))
                          (pango (assoc-ref %build-inputs "pango"))
                          (patchelf (assoc-ref %build-inputs "patchelf"))
                          (tar (assoc-ref %build-inputs "tar"))
                          (xz (assoc-ref %build-inputs "xz"))
                          (libx11 (assoc-ref %build-inputs "libx11"))
                          (libxss (assoc-ref %build-inputs "libxscrnsaver"))
                          (ld-so (string-append (assoc-ref %build-inputs
                                                           "glibc")
                                                ,(glibc-dynamic-linker)))
                          (bin-dir (string-append %output "/bin")))
                     (mkdir-p bin-dir)
                     (setenv "PATH"
                             (string-append (string-append bash "/bin:")
                                            (string-append coreutils "/bin:")
                                            (string-append gzip "/bin:")
                                            (string-append patchelf "/bin:")
                                            (string-append tar "/bin:")
                                            (string-append xz "/bin:")))
                     (invoke "tar" "xvf" source)
                     (invoke "tar" "xvf" "tmetric/data.tar.xz")
                     (substitute* '("usr/share/applications/tmetricdesktop.desktop")
                       (("Exec=/usr/share/tmetricdesktop/TMetricDesktop")
                        (string-append "Exec=" %output "/bin/tmetric")))
                     (copy-recursively "usr" %output)
                     (invoke "patchelf" "--set-interpreter" ld-so
                             (string-append %output
                              "/share/tmetricdesktop/TMetricDesktop"))
                     (invoke "ln" "-s"
                             (string-append %output
                              "/share/tmetricdesktop/TMetricDesktop")
                             (string-append %output "/bin/tmetric"))
                     (wrap-program (string-append %output
                                    "/share/tmetricdesktop/TMetricDesktop")
                       `("LD_LIBRARY_PATH" ":" prefix
                         (,(string-append gdk-pixbuf "/lib"))))
                     (wrap-program (string-append %output
                                    "/share/tmetricdesktop/TMetricDesktop")
                       `("LD_LIBRARY_PATH" ":" prefix
                         (,(string-append pango "/lib"))))
                     (wrap-program (string-append %output
                                    "/share/tmetricdesktop/TMetricDesktop")
                       `("LD_LIBRARY_PATH" ":" prefix
                         (,(string-append glib "/lib"))))
                     (wrap-program (string-append %output
                                    "/share/tmetricdesktop/TMetricDesktop")
                       `("LD_LIBRARY_PATH" ":" prefix
                         (,(string-append cairo "/lib"))))
                     (wrap-program (string-append %output
                                    "/share/tmetricdesktop/TMetricDesktop")
                       `("LD_LIBRARY_PATH" ":" prefix
                         (,(string-append libx11 "/lib"))))
                     (wrap-program (string-append %output
                                    "/share/tmetricdesktop/TMetricDesktop")
                       `("LD_LIBRARY_PATH" ":" prefix
                         (,(string-append gtk "/lib"))))
                     (wrap-program (string-append %output
                                    "/share/tmetricdesktop/TMetricDesktop")
                       `("LD_LIBRARY_PATH" ":" prefix
                         (,(string-append libxss "/lib"))))
                     (wrap-program (string-append %output
                                    "/share/tmetricdesktop/TMetricDesktop")
                       `("PATH" ":" prefix
                         (,(string-append gtk "/bin"))))
                     (wrap-program (string-append %output
                                    "/share/tmetricdesktop/TMetricDesktop")
                       `("LD_LIBRARY_PATH" ":" prefix
                         (,(string-append atk "/lib"))))
                     #t))))
    (native-inputs `(("coreutils" ,coreutils)
                     ("cairo" ,cairo)
                     ("gcc-toolchain" ,gcc-toolchain)
                     ("glib" ,glib)
                     ("gdk-pixbuf" ,gdk-pixbuf)
                     ("gtk+" ,gtk+-2)
                     ("glibc" ,glibc)
                     ("libx11" ,libx11)
                     ("libxscrnsaver" ,libxscrnsaver)
                     ("gzip" ,gzip)
                     ("pango" ,pango)
                     ("patchelf" ,patchelf)
                     ("tar" ,tar)
                     ("xz" ,xz)))
    (inputs `(("bash" ,bash)
              ("zlib" ,zlib)))
    (home-page "https://tmetric.com/")
    (synopsis "Free Time Tracking Software & App")
    (description "TMetric is a simple and accurate work time-tracker
that sets you free from tedious time reporting.")
    (license license:expat)))
