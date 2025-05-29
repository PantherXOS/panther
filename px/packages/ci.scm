;;; Package Repository for GNU Guix
;;; Copyright © 2021-2023 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px packages ci)
  #:use-module (gnu packages bash)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public gitlab-runner
  (package
    (name "gitlab-runner")
    (version "v12.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://s3.amazonaws.com/gitlab-runner-downloads/"
             version "/binaries/gitlab-runner-linux-386"))
       (sha256
        (base32 "1lbri42l0bjz21gnq7prhi1g06mqz43qdgdzh5llq8vl49gfz0ap"))))
    (build-system trivial-build-system)
    (supported-systems '("x86_64-linux" "i686-linux")) ;; As long as we download binaries
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((src (assoc-ref %build-inputs "source"))
                          (bash (string-append (assoc-ref %build-inputs "bash")
                                               "/bin/sh"))
                          (out (assoc-ref %outputs "out"))
                          (target (string-append out "/bin/gitlab-runner")))
                     (setenv "PATH" bash)
                     (mkdir-p (string-append out "/bin"))
                     (copy-file src target)
                     (chmod target #o755) #t))))
    (home-page "https://docs.gitlab.com/runner/")
    (native-inputs `(("bash" ,bash)))
    (synopsis "GitLab Runner")
    (description "GitLab Runner is the open source project that is used 
to run your jobs and send the results back to GitLab.")
    (license license:expat)))
