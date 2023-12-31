(define-module (px services device)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages video)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)

  #:use-module (px packages device)
  #:use-module (px packages security-token)
  #:use-module (px packages tpm)

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)

  #:export (<px-device-identity-configuration>
            px-device-identity-configuration
            px-device-identity-configuration?
            px-device-identity-service-type

            <px-device-identity-configuration>
            px-user-identity-configuration
            px-user-identity-configuration?
            px-user-identity-service-type

            px-enterprise-channels-service

            px-device-runner-configuration
            px-device-runner-service-type

            px-file-upload-configuration
            px-file-upload-service-type

            btuart-configuration
            btuart-service-type

            bluetooth-client-manager-configuration
            bluetooth-client-manager-service-type))

;;
;; Device Identity API SERVICE
;;

(define-record-type* <px-device-identity-configuration>
                     px-device-identity-configuration
                     make-px-device-identity-configuration
  px-device-identity-configuration?
  (package
    px-device-identity-configuration-package
    (default px-device-identity-service)))

(define px-device-identity-shepherd-service
  (match-lambda
    (($ <px-device-identity-configuration> package)
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
                                                                   "/bin/px-device-identity-service"))))
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
;; Pantherx Enterprise Channels Service
;;

(define (px-enterprise-channels-service channels)
  (let ((channel-conf (call-with-output-string (lambda (port)
                                                 (pretty-print channels port)))))
    (simple-service 'enterprise-channels special-files-service-type
                    `(("/etc/guix/channels.scm" ,(computed-file "channels.scm"
                                                  (with-imported-modules '((guix
                                                                            build
                                                                            utils))
                                                                         #~(begin
                                                                             (use-modules
                                                                              (guix
                                                                               build
                                                                               utils))
                                                                             (let 
                                                                                  (
                                                                                   (base-dir
                                                                                    (dirname #$output)))
                                                                               
                                                                               
                                                                               (mkdir-p
                                                                                base-dir)
                                                                               (call-with-output-file #$output
                                                                                 (lambda 
                                                                                         (port)
                                                                                   
                                                                                   
                                                                                   (format
                                                                                    port
                                                                                    #$channel-conf))))))))))))

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

;;
;; File Upload Service
;;

(define-record-type* <px-file-upload-configuration>
                     px-file-upload-configuration
                     make-px-file-upload-configuration
  px-file-upload-configuration?
  (package
    px-file-upload-configuration-package
    (default px-file-upload-cli))
  (schedule px-file-upload-configuration-schedule
            (default "0 * * * *"))
  (types px-file-upload-configuration-types
         (default '()))
  (source px-file-upload-configuration-source)
  (endpoint px-file-upload-configuration-endpoint)
  (keys px-file-upload-configuration-keys
        (default '()))
  (parse? px-file-upload-configuration-parse?
          (default #f))
  (delete-on-success? px-file-upload-configuration-delete-on-success?
                      (default #f)))

(define (px-file-upload-configuration->config config)
  "Return configuration file for px-file-upload-cli"
  (match config
    (($ <px-file-upload-configuration>
        package
        schedule
        types
        source
        endpoint
        keys
        parse?
        delete-on-success?)
     (computed-file "file-upload.conf"
                    (with-imported-modules '((guix build utils))
                                           #~(begin
                                               (use-modules (guix build utils))
                                               (call-with-output-file #$output
                                                 (lambda (port)
                                                   (format port
                                                    "types = ~a
source = ~a
endpoint = ~a
keys = ~a
parse_file_name = ~a
delete_on_success = ~a"
                                                    #$(string-join types ",")
                                                    #$source
                                                    #$endpoint
                                                    #$(string-join keys "")
                                                    #$(if parse? "true"
                                                          "false")
                                                    #$(if delete-on-success?
                                                          "true" "false"))))))))))

(define (px-file-upload-mcron-jobs config)
  (let ((configpath (px-file-upload-configuration->config config))
        (schedule (px-file-upload-configuration-schedule config))
        (package
          (px-file-upload-configuration-package config)))
    (list #~(job #$schedule
                 (string-append #$package "/bin/px-file-upload-cli"
                                " --config "
                                #$configpath)))))

(define px-file-upload-service-type
  (service-type (name 'px-file-upload)
                (extensions (list (service-extension mcron-service-type
                                   px-file-upload-mcron-jobs)
                                  (service-extension profile-service-type
                                                     (lambda (config)
                                                       (list (px-file-upload-configuration-package
                                                              config)
                                                        px-device-identity
                                                        tpm2-tss-engine)))))
                (description
                 "Service definition to run file upload on intervals")))

;;
;; btuart-service-type
;;

(define-record-type* <btuart-configuration> btuart-configuration
                     make-btuart-configuration
  btuart-configuration?
  (package
    btuart-configuration-package
    (default bluez))
  (device btuart-configuration-device
          (default "/dev/ttyAMA0"))
  (protocol btuart-configuration-protocol
            (default "bcm"))
  (baudrate btuart-configuration-baudrate
            (default "3000000"))
  (flow-control? btuart-configuration-flow-control?
                 (default #t)))

(define btuart-shepherd-service
  (match-lambda
    (($ <btuart-configuration>
        package
        device
        protocol
        baudrate
        flow-control?)
     (list (shepherd-service (documentation
                              "attach serial lines as Bluetooth HCI interfaces")
                             (provision '(btuart))
                             (requirement '(udev))
                             (start #~(make-forkexec-constructor (list #$(file-append
                                                                          package
                                                                          "/bin/btattach")
                                                                       "-B"
                                                                       #$device
                                                                       "-P"
                                                                       #$protocol
                                                                       "-S"
                                                                       #$baudrate
                                                                       (when #$flow-control?
                                                                         "-N"))))
                             (one-shot? #t))))))

(define btuart-service-type
  (service-type (name 'btuart)
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   btuart-shepherd-service)))
                (default-value (btuart-configuration))
                (description "Attach serial lines as Bluetooth HCI interfaces")))

;;
;; bluetooth-client-manager-service
;;

(define-record-type* <bluetooth-client-manager-configuration>
                     bluetooth-client-manager-configuration
                     make-bluetooth-client-manager-configuration
  bluetooth-client-manager-configuration?
  (package
    bluetooth-client-manager-configuration-package
    (default bluetooth-client-manager-service))
  (debug? bluetooth-client-manager-configuration-debug?
          (default #f))
  (skip-approval? bluetooth-client-manager-configuration-skip-approval?
                  (default #f)))

(define bluetooth-client-manager-shepherd-service
  (match-lambda
    (($ <bluetooth-client-manager-configuration> package debug? skip-approval?)
     (list (shepherd-service (provision '(bluetooth-client-manager-service))
                             (documentation
                              "Run px-device-identity-service as a daemon")
                             (requirement '(networking user-processes
                                                       px-device-identity))
                             (start #~(make-forkexec-constructor (list (string-append #$package
                                                                        "/bin/bluetooth-client-manager-service")
                                                                       #$@(if
                                                                           debug?
                                                                           '("--verbose")
                                                                           '())
                                                                       #$@(if
                                                                           skip-approval?
                                                                           '("--skip-approval")
                                                                           '()))
                                       #:log-file
                                       "/var/log/bluetooth-client-manager-service.log"
                                       #:environment-variables (cons*
                                                                "HOME=/root"
                                                                "XDG_DATA_HOME=/root/.local/share"
                                                                "XDG_CONFIG_HOME=/root/.config"
                                                                "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                                                                "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                                                                (default-environment-variables))))
                             (stop #~(make-kill-destructor)))))))

(define bluetooth-client-manager-service-type
  (service-type (name 'bluetooth-client-manager-service)
                (description "PantherX Bluetooth Client Manager service")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   bluetooth-client-manager-shepherd-service)))
                (default-value (bluetooth-client-manager-configuration))))
