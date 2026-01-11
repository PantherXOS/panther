;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px services ntp)
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
  #:use-module (srfi srfi-1)

  #:export (chrony-service-configuration chrony-service-type))

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
  (service-type (name 'chrony)
                (description "Chrony service")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   chrony-shepherd-service)))
                (default-value (chrony-service-configuration))))
