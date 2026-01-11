;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages gradle)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (nongnu packages gradle))

(define-public gradle-8
  (package
    (inherit gradle)
    (version "8.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://services.gradle.org/distributions/gradle-"
             version "-bin.zip"))
       (sha256
        (base32 "0c9mbwngm85wj4mydl49pn6iamzfpr3djaf2dsan0c292ci10wdx"))))))

(define-public gradle-7
  (package
    (inherit gradle)
    (version "7.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://services.gradle.org/distributions/gradle-"
             version "-bin.zip"))
       (sha256
        (base32 "1q1vcyldkyhn2v8xjfw90wdhbwgbsqrd4a9kzi471g03ydv9fgb7"))))))
