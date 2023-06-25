(define-module (px services bluetooth)
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

  #:export (btuart-configuration
            btuart-service-type

            bluetooth-client-manager-configuration
            bluetooth-client-manager-service-type))

;;
;; btuart-service-type
;;

(define-record-type* <btuart-configuration> 
  btuart-configuration make-btuart-configuration
  btuart-configuration?
  (package  btuart-configuration-package
            (default bluez))
  (device   btuart-configuration-device
            (default "/dev/ttyAMA0"))
  (protocol btuart-configuration-protocol
            (default "bcm"))
  (baudrate btuart-configuration-baudrate
            (default "3000000"))
  (flow-control? btuart-configuration-flow-control?
                 (default #t)))


(define btuart-shepherd-service
  (match-lambda
    (($ <btuart-configuration> package device protocol baudrate flow-control?)
     (list
      (shepherd-service
        (documentation "attach serial lines as Bluetooth HCI interfaces")
        (provision '(btuart))
        (requirement '(udev))
        (start #~(make-forkexec-constructor
                  (list #$(file-append package "/bin/btattach")
                        "-B" #$device "-P" #$protocol
                        "-S" #$baudrate
                        (when #$flow-control? "-N"))))
        (one-shot? #t))))))


(define btuart-service-type
  (service-type 
    (name 'btuart)
    (extensions (list (service-extension shepherd-root-service-type
                                         btuart-shepherd-service)))
    (default-value (btuart-configuration))
    (description "Attach serial lines as Bluetooth HCI interfaces")))

;;
;; bluetooth-client-manager-service
;;

(define-record-type* <bluetooth-client-manager-configuration>
  bluetooth-client-manager-configuration make-bluetooth-client-manager-configuration
  bluetooth-client-manager-configuration?
  (package        bluetooth-client-manager-configuration-package
                  (default bluetooth-client-manager-service))
  (debug?         bluetooth-client-manager-configuration-debug?
                  (default #f))
  (skip-approval? bluetooth-client-manager-configuration-skip-approval?
                  (default #f)))


(define bluetooth-client-manager-shepherd-service
  (match-lambda
    (($ <bluetooth-client-manager-configuration> package debug? skip-approval?)
     (list (shepherd-service
            (provision '(bluetooth-client-manager-service))
            (documentation "Run px-device-identity-service as a daemon")
            (requirement '(networking user-processes px-device-identity))
            (start #~(make-forkexec-constructor
                      (list (string-append #$package "/bin/bluetooth-client-manager-service")
                            #$@(if debug? '("--verbose") '())
                            #$@(if skip-approval? '("--skip-approval") '()))
                      #:log-file "/var/log/bluetooth-client-manager-service.log"
                      #:environment-variables
                      (cons* "HOME=/root"
                             "XDG_DATA_HOME=/root/.local/share"
                             "XDG_CONFIG_HOME=/root/.config"
                             "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                             "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                             (default-environment-variables))))
            (stop #~(make-kill-destructor)))))))


(define bluetooth-client-manager-service-type
  (service-type
   (name 'bluetooth-client-manager-service)
   (description "PantherX Bluetooth Client Manager service")
   (extensions (list (service-extension shepherd-root-service-type
                                        bluetooth-client-manager-shepherd-service)))
   (default-value (bluetooth-client-manager-configuration))))
