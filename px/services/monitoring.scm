(define-module (px services monitoring)
  #:use-module (gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages screen)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (px packages device)
  #:use-module (ice-9 match)
  #:use-module (px packages monitoring)
  #:export (<px-remote-status-service-configuration>
            px-remote-status-service-configuration
            px-remote-status-service-configuration?
            px-remote-status-service-type))

;;
;; px-remote-status-service-type
;;

(define-record-type* <px-remote-status-service-configuration>
  px-remote-status-service-configuration make-px-remote-status-configuration
  px-remote-status-service-configuration?
  (package px-remote-status-service-configuration-package
           (default px-org-remote-status-service))
  (interval px-remote-status-service-configuration-interval
            (default 300))
  (jobs px-remote-status-service-configuration-jobs
        (default '())))

(define (px-remote-service-configuration->monitrc config)
  "Return monitrc file for config"
  (match config
    (($ <px-remote-status-service-configuration> jobs)
     (computed-file
      "monitrc"
      (with-imported-modules
       '((guix build utils))
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 match))
           (call-with-output-file #$output
             (lambda (port)
               (display "\
### Monit default configurations
## Check interval
set daemon  30              # check services at 30 seconds intervals
#   with start delay 240    # optional: delay the first check by 4-minutes (by
#                           # default Monit check immediately after Monit start)

## Set syslog logging
set log syslog

## Monit embedded HTTP interface
set httpd port 2812 and
    use address localhost  # only accept connection from localhost (drop if you use M/Monit)
    allow localhost        # allow localhost to connect to the server and
    allow admin:monit      # require user 'admin' with password 'monit'

### Monit job definitions\n" port)
               (for-each (lambda (job)
                           (display (string-append job "\n") port))
                         '#$(px-remote-status-service-configuration-jobs config))
               ))))))))

(define (px-remote-status-shepherd-service config)
  "Return <shepherd-service> running px-org-remote-status-service"
  (match config
    (($ <px-remote-status-service-configuration> package interval)
     (let ((monitrc (px-remote-service-configuration->monitrc config)))
       (list (shepherd-service
              (provision '(px-remote-status))
              (documentation "Run px-org-remote-status-service as a shepherd daemon")
              (requirement '(networking user-processes px-device-identity))
              (modules `((srfi srfi-1)
                         (srfi srfi-26)
                         ,@%default-modules))
              (start #~(make-forkexec-constructor
                        (list (string-append #$screen "/bin/screen")
                              "-D" "-m" "-S" "remote-status"
                              (string-append #$package "/bin/px-org-remote-status-service")
                              "-i" (number->string #$interval)
                              "-m" #$monitrc)
                        #:environment-variables
                        (cons* (string-append "PATH="
                                                 #$monit "/bin:"
                                                 #$network-manager "/bin:"
                                                 #$lshw  "/sbin:"
                                                 #$coreutils "/bin:"
                                                 #$sysstat  "/bin:"
                                                 #$curl     "/bin:"
                                                 #$util-linux+udev "/bin:" 
                                                 #$px-device-identity "/bin:"
                                                 "/run/current-system/profile/bin:"
                                                 (getenv "PATH"))
                               "HOME=/root"                               
                               "XDG_DATA_HOME=/root/.local/share"
                               "XDG_CONFIG_HOME=/root/.config"
                               "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                               "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                               (remove (cut string-prefix? "PATH=" <>)
                                            (environ)))))
              (stop #~(make-kill-destructor))))))))



(define px-remote-status-service-type
  (service-type
   (name 'px-remote-status)
   (description "PantherX remote status service")
   (extensions (list (service-extension shepherd-root-service-type
                                        px-remote-status-shepherd-service)))
   (default-value (px-remote-status-service-configuration))))

