;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px services log)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages screen)
  #:use-module (gnu services shepherd)
  #:use-module (px packages log)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)

  #:export (remote-syslog-service-configuration 
            remote-syslog-service-type
            %rsyslog-default-config
            %rsyslog-default-config-file
            rsyslog-configuration
            rsyslog-service-type))

;;
;; remote-syslog SERVICE
;;

(define (script-builder destionation-host destionation-port hostname log-files
                        package)
  "Return the chorny configuration file corresponding to CONFIG."
  (computed-file "remote-syslog-script.sh"
                 (with-imported-modules '((guix build utils))
                                        #~(begin
                                            (use-modules (guix build utils))
                                            (call-with-output-file #$output
                                              (lambda (port)
                                                (format port
                                                 "#!~a~% exec ~a \"$@\"~%"
                                                 #+(file-append bash "/bin/sh")
                                                 (string-append #$package
                                                  "/bin/remote_syslog2"
                                                  " --no-detach"
                                                  " -d "
                                                  #$destionation-host
                                                  " -p "
                                                  #$destionation-port
                                                  " --pid-file=/var/run/remote_syslog.pid"
                                                  " --hostname "
                                                  #$hostname
                                                  " "
                                                  #$log-files))
                                                (chmod port #o555)))))))

(define-record-type* <remote-syslog-service-configuration>
                     remote-syslog-service-configuration
                     make-remote-syslog-service-configuration
  remote-syslog-service-configuration?
  (destionation-host remote-syslog-service-configuration-destionation-host
                     (default "logs.papertrailapp.com"))
  (destionation-port remote-syslog-service-configuration-destionation-port
                     (default "46169"))
  (hostname remote-syslog-service-configuration-host
            (default "$(hostname)"))
  (log-files remote-syslog-service-configuration-log-files
             (default "/var/log/messages"))
  (package
    remote-syslog-service-configuration-package
    (default remote_syslog2)))

(define remote-syslog-shepherd-service
  (match-lambda
    (($ <remote-syslog-service-configuration>
        destionation-host
        destionation-port
        hostname
        log-files
        package)
     (list (shepherd-service (provision '(remote-syslog))
                             (documentation "Run remote-syslog as a daemon")
                             (requirement '(networking syslogd))
                             (start #~(make-forkexec-constructor (list #$(script-builder
                                                                          destionation-host
                                                                          destionation-port
                                                                          hostname
                                                                          log-files
                                                                          package))))
                             (stop #~(make-kill-destructor)))))))

(define remote-syslog-service-type
  (service-type (name "remote-syslog")
                (description "Remote syslog service")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   remote-syslog-shepherd-service)))
                (default-value (remote-syslog-service-configuration))))

;;
;; rsyslog-service-type
;;

(define %rsyslog-default-config
  "## full conf created by rsyslog version 8.2204.1 at 2023-01-02 16:44:08 ##

$ModLoad imuxsock # provides support for local system logging (e.g. via logger command)
$ModLoad imklog   # provides kernel logging support (previously done by rklogd)
$ModLoad immark  # provides --MARK-- message capability

$ModLoad imudp
$UDPServerRun 514

$ModLoad imtcp
$InputTCPServerRun 514

$WorkDirectory /var/lib/rsyslog
$ActionFileDefaultTemplate RSYSLOG_TraditionalFileFormat

*.* /var/log/rsyslog
")

(define %rsyslog-default-config-file
  (plain-file "rsyslog.conf" %rsyslog-default-config))

(define-record-type* <rsyslog-configuration> rsyslog-configuration
                     make-rsyslog-cofiguration
  rsyslog-configuration?
  (package
    rsyslog-configuration-package
    (default rsyslog))
  (config-file rsyslog-configuration-config-file
               (default %rsyslog-default-config-file)))

(define (rsyslog-activation config)
  (with-imported-modules '((guix build utils))
                         #~(begin
                             (use-modules ((guix build utils)))
                             (let ((lib-dir "/var/lib/rsyslog"))
                               (mkdir-p lib-dir)))))

(define rsyslog-shepherd-service
  (match-lambda
    (($ <rsyslog-configuration> package config-file)
     (list (shepherd-service (provision '(rsyslogd))
                             (documentation "Rsyslog daemon service")
                             (requirement '(syslogd))
                             (start #~(make-forkexec-constructor (list #$(file-append
                                                                          package
                                                                          "/sbin/rsyslogd")
                                                                       "-n"
                                                                       "-f"
                                                                       #$config-file)))
                             (stop #~(make-kill-destructor)))))))

(define rsyslog-service-type
  (service-type (name 'rsyslog)
                (description "Run rsyslog daemon on machine")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   rsyslog-shepherd-service)
                                  (service-extension activation-service-type
                                                     rsyslog-activation)))
                (default-value (rsyslog-configuration))))
