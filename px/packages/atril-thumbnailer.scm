;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages atril-thumbnailer)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages mate))

(define-public atril-thumbnailer
  (package
    (inherit atril)
    (name "atril-thumbnailer")
    (version (package-version atril))
    (source
     (origin
       (inherit (package-source atril))
       (uri (string-append "mirror://mate/"
                           (version-major+minor version)
                           "/"
                           "atril-"
                           version
                           ".tar.xz"))))
    (arguments
     (substitute-keyword-arguments (package-arguments atril)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build
             (lambda _
               (invoke "make" "thumbnailer") #t))
           (replace 'install
             (lambda _
               (invoke "make" "install" "thumbnailer") #t))
           (add-after 'install 'remove-desktop-file
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (invoke "rm"
                         (string-append out
                                        "/share/applications/atril.desktop"))
                 #t)))))))))
