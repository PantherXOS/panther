;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages music)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages music)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public strawberry
  (package
    (inherit (@ (gnu packages music) strawberry))
    (name "strawberry")
    (version "1.2.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/strawberrymusicplayer/strawberry")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sli0wm1l34ca8y6m3rfa604v1bbzbaiala13kzndcqqwnrsh66f"))
       (modules '((guix build utils)
                  (ice-9 regex)))
       (snippet
        '(begin
           (use-modules ((ice-9 regex)))
           (for-each
            (lambda (dir)
              ;; TODO: The following dependencies are still bundled:
              ;; - "singleapplication"
              ;; - "discord-rpc"
              (let ((bundled '("singleapplication" "discord-rpc")))
                (if (not
                     (string-match
                      (string-append ".?*(" (string-join bundled "|") ")")
                      dir))
                    (delete-file-recursively dir))))
            (find-files "3rdparty"
                        (lambda (file stat)
                          (string-match "^3rdparty/[^/]*$" file))
                        #:directories? #t))))))
    (inputs
     (modify-inputs (package-inputs (@ (gnu packages music) strawberry))
       (append kdsingleapplication
               (@ (gnu packages music) libgpod)
               rapidjson
               sparsehash)))))
