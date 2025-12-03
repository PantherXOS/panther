;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px services networking)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ntp)
  #:use-module (gnu packages screen)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (px packages networking)
  #:use-module (srfi srfi-1)

  #:export (chrony-service-configuration
            chrony-service-type
            nebula-configuration
            nebula-configuration-package
            nebula-configuration-provision
            nebula-configuration-config-path
            %default-nebula-configuration
            nebula-service-type
            tailscale-configuration
            tailscale-service-type))

;;
;; Chrony SERVICE
;;

(define-record-type* <chrony-service-configuration>
                     chrony-service-configuration
                     make-chrony-service-configuration
  chrony-service-configuration?
  (package
    chrony-service-configuration-package
    (default chrony))
  (user chrony-service-configuration-user
        (default "root"))
  (config chrony-service-configuration-config
          (default "server 0.pool.ntp.org iburst
server 1.pool.ntp.org iburst
server 2.pool.ntp.org iburst
server 3.pool.ntp.org iburst
driftfile /var/lib/chrony/drift
makestep 1.0 3
rtcsync
logdir /var/log/chrony")))

(define (chrony-service-config-file config)
  "Return the chorny configuration file corresponding to CONFIG."
  (computed-file "chrony.conf"
                 (with-imported-modules '((guix build utils))
                                        #~(begin
                                            (use-modules (guix build utils))
                                            (call-with-output-file #$output
                                              (lambda (port)
                                                (format port
                                                        #$config)))))))

(define chrony-shepherd-service
  (match-lambda
    (($ <chrony-service-configuration> package user config)
     (list (shepherd-service (provision '(chrony))
                             (documentation "Run chrony as a daemon")
                             (requirement '(networking))
                             (start #~(make-forkexec-constructor (list (string-append #$package
                                                                        "/sbin/chronyd")
                                                                       "-n"
                                                                       "-u"
                                                                       #$user
                                                                       "-f"
                                                                       #$(chrony-service-config-file
                                                                          config))))
                             (stop #~(make-kill-destructor)))))))

(define chrony-service-type
  (service-type (name "chrony")
                (description "Chrony service")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   chrony-shepherd-service)))
                (default-value (chrony-service-configuration))))

;;
;; Nebula SERVICE
;;

(define-record-type* <nebula-configuration> nebula-configuration
                     make-nebula-configuration
  nebula-configuration?
  (package
    nebula-configuration-package
    (default nebula))
  (provision nebula-configuration-provision)
  (config-path nebula-configuration-config-path))

(define nebula-profile-packages
  (lambda (configurations)
    (fold (lambda (config prv)
            (let ((pkg (nebula-configuration-package config)))
              (if (memq pkg prv) prv
                  (cons pkg prv))))
          '() configurations)))

(define (nebula-shepherd-service config)
  (match config
    (($ <nebula-configuration> package provision config-path)
     (let ((log-path (string-append "/var/log/"
                                    (symbol->string (car provision)) ".log")))
       (shepherd-service (provision provision)
                         (documentation
                          "Run configured instance of nebula on system start")
                         (requirement '(networking user-processes))
                         (start #~(make-forkexec-constructor (list (string-append #$package
                                                                    "/bin/nebula")
                                                                   "-config"
                                                                   #$config-path)
                                                             #:log-file #$log-path
                                                             #:environment-variables
                                                             (cons*
                                                              "HOME=/root"
                                                              "XDG_DATA_HOME=/root/.local/share"
                                                              "XDG_CONFIG_HOME=/root/.config"
                                                              "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                                                              "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                                                              (default-environment-variables))))
                         (stop #~(make-kill-destructor)))))))

(define (nebula-shepherd-services configurations)
  (map nebula-shepherd-service configurations))

(define %default-nebula-configuration
  (nebula-configuration (provision '(nebula))
                        (config-path "/etc/nebula/config.yml")))

(define %nebula-log-rotations
  (list "/var/log/nebula.log"))

(define nebula-service-type
  (service-type (name 'nebula)
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   nebula-shepherd-services)
                                  (service-extension profile-service-type
                                                     nebula-profile-packages)
                                  (service-extension log-rotation-service-type
                                                     (const
                                                      %nebula-log-rotations))))
                (default-value (list %default-nebula-configuration))
                (description
                 "Run configured instance of nebula on system start")))

;;
;; Tailscale SERVICE
;;

(define-public (tailscale-configuration) '())

(define (tailscale-shepherd-service config)
  (list (shepherd-service
         (documentation "Run the tailscale daemon")
         (provision '(tailscaled tailscale))
         (requirement '(user-processes))
         (start
          #~(lambda _
              (fork+exec-command (list #$(file-append tailscaled "/bin/tailscaled")))))
         (stop #~(make-kill-destructor)))))

(define-public tailscale-service-type
  (service-type
   (name 'tailscale)
   (extensions
    (list (service-extension shepherd-root-service-type tailscale-shepherd-service)))
   (default-value (tailscale-configuration))
   (description "Run and connect to tailscale")))
