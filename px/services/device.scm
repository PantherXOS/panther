;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px services device)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)

  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages video)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)

  #:use-module (px packages device)
  #:use-module (px packages security-token)
  #:use-module (px packages tpm)

  #:export (px-device-identity-configuration
            px-device-identity-configuration?
            px-device-identity-service-type

            px-user-identity-configuration
            px-user-identity-configuration?
            px-user-identity-service-type

            px-device-runner-configuration
            px-device-runner-service-type

            px-file-upload-configuration
            px-file-upload-service-type))

;;
;; Device Identity Service
;;

(define-record-type* <px-device-identity-configuration>
                     px-device-identity-configuration
                     make-px-device-identity-configuration
  px-device-identity-configuration?
  (package
    px-device-identity-configuration-package
    (default px-device-identity-service)
    (docstring "The package to use for the device identity service"))
  (port px-device-identity-configuration-port
        (default 8000)
        (docstring "The port to listen on"))
  (config-dir px-device-identity-configuration-config-dir
              (default "/etc/px-device-identity")
              (docstring "The directory to store the configuration file"))
  (key-dir px-device-identity-configuration-key-dir
           (default "/root/.local/share/px-device-identity")
           (docstring "The directory to store the key files")))

(define px-device-identity-shepherd-service
  (match-lambda
    (($ <px-device-identity-configuration> package port config-dir key-dir)
     (list (shepherd-service (provision '(px-device-identity))
                             (documentation
                              "Run px-device-identity-service as a daemon")
                             (requirement '(networking user-processes))
                             (start #~(make-forkexec-constructor (list (string-append #$screen
                                                                        "/bin/screen")
                                                                  "-D"
                                                                  "-m"
                                                                  "-S"
                                                                  "identity-api"
                                                                  (string-append #$package
                                                                   "/bin/px-device-identity-service")
                                                                  "--port"
                                                                  (number->string #$port)
                                                                  "--config-dir"
                                                                  #$config-dir
                                                                  "--key-dir"
                                                                  #$key-dir)))
                             (stop #~(make-kill-destructor)))))))

(define px-device-identity-service-type
  (service-type (name 'px-device-identity)
                (description "PantherX device identity service")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   px-device-identity-shepherd-service)))
                (default-value (px-device-identity-configuration))))

;;
;; User Identity Service
;;

(define-record-type* <px-user-identity-configuration>
                     px-user-identity-configuration
                     make-px-user-identity-configuration
  px-user-identity-configuration?
  (package
    px-user-identity-configuration-package
    (default px-user-identity-service)))

(define (px-user-identity-shepherd-service config)
  (match config
    (($ <px-user-identity-configuration> package)
     (list (shepherd-service (provision '(px-user-identity))
                             (documentation
                              "Run px-user-identity-service as a shepherd daemon")
                             (requirement `(networking user-processes))
                             (start #~(make-forkexec-constructor (list (string-append #$screen
                                                                        "/bin/screen")
                                                                  "-D"
                                                                  "-m"
                                                                  "-S"
                                                                  "user-identity"
                                                                  (string-append #$package
                                                                   "/bin/px-user-identity-service"))
                                                                 #:environment-variables
                                                                 (cons*
                                                                  "HOME=/root"
                                                                  "XDG_DATA_HOME=/root/.local/share"
                                                                  "XDG_CONFIG_HOME=/root/.config"
                                                                  "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                                                                  "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                                                                  (default-environment-variables))))
                             (stop #~(make-kill-destructor)))))))

(define px-user-identity-service-type
  (service-type (name 'px-user-identity)
                (description "PantherX user identity service")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   px-user-identity-shepherd-service)))
                (default-value (px-user-identity-configuration))))

;;
;; Device Runner Service
;;

(define-record-type* <px-device-runner-configuration>
                     px-device-runner-configuration
                     make-px-device-runner-configuration
  px-device-runner-configuration?
  (schedule px-device-runner-configuration-schedule
            (default "*/5 * * * *")))

(define (px-device-runner-job config)
  #~(job #$(px-device-runner-configuration-schedule config)
         (string-append #$px-device-runner "/bin/px-device-runner")))

(define (px-device-runner-mcron-jobs config)
  (list (px-device-runner-job config)))

(define px-device-runner-service-type
  (service-type (name "px-device-runner")
                (extensions (list (service-extension mcron-service-type
                                   px-device-runner-mcron-jobs)))
                (description
                 "Service definition to run device runnner as a cronjob")
                (default-value (px-device-runner-configuration))))