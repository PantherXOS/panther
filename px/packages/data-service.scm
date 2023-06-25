;;; Data-service Packages Module for PantherX
;;; Author: H.ghaffari (h.ghaffari@pantherx.org)
;;;

(define-module (px packages data-service)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (px packages common)
  #:use-module (px packages python-xyz)
  #:use-module (guix gexp)
)


(define-public px-data-service
  (package
    (name "px-data-service")
    (version "v0.0.10")
    (source
     (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/px-data-service_"
              version ".tgz"))
        (sha256
         (base32 "0rq1fi7ymizmcfzlycn31z2sz96zx3fw91h7gjkg707qys92dfcm"))))
    (build-system python-build-system)
    (propagated-inputs `(
              ("python-pycapnp" ,python-pycapnp)
              ("python-pynng" ,python-pynng)
              ("python-pygithub" ,python-pygithub)
              ("python-block-io" ,python-block-io)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-binary
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/px-data-service")))
               (wrap-program bin
                 `("PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH"))))
               #t)))
         (delete 'sanity-check)
         )))
    (home-page "https://www.pantherx.org")
    (synopsis "")
    (description "This service fetch data periodically and send event to px-hub-service to sync data ")
    (license license:expat)))
