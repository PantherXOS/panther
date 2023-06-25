(define-module (px services server)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages node)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (px-server-launcher-configuration
            px-server-launcher-service-type))


;;;
;;; PantherX Server Launcher Service
;;;

(define-record-type* <px-server-launcher-configuration>
  px-server-launcher-configuration make-px-server-launcher-configuration
  px-server-launcher-configuration?
  (user       px-server-launcher-configuration-user
              (default "panther"))
  (group      px-server-launcher-configuration-group
              (default "users"))
  (executable px-server-launcher-configuration-executable)
  (args       px-server-launcher-configuration-args
              (default '()))
  (cwd       px-server-launcher-configuration-cwd
              (default #f)))


(define (px-server-launcher->script config)
  (match config
    (($ <px-server-launcher-configuration> user group executable args cwd)
     (plain-file "px-server-launcher"
                 (string-append "#!/bin/sh\n\n"
                                "export PATH=$HOME/.local/bin:$PATH\n" ;; add user installed binaries to PATH
                                (if cwd (string-append "cd " cwd "\n") "")
                                "exec " executable " " (string-join args " ") "\n")))))


(define (px-server-launcher-shepherd-service config)
  (match config
    (($ <px-server-launcher-configuration> user group executable args cwd)
     (let* ((home-dir (if (eq? user "root")
                          "/root"
                          (string-append "/home/" user)))
            (script (px-server-launcher->script config)))
       (list (shepherd-service
              (provision '(px-server-launcher))
              (documentation "PantherX Server Application Launcher Service")
              (requirement '(networking user-processes))
              (one-shot? #t)
              (start #~(make-forkexec-constructor
                        (list (string-append #$bash "/bin/bash")
                              #$script)
                        #:user #$user
                        #:group #$group
                        #:environment-variables
                        (cons* (string-append "HOME=" #$home-dir)
                               (string-append "PATH=/run/current-system/profile/bin")
                               (string-append "XDG_DATA_HOME=" #$home-dir "/.local/share")
                               (string-append "XDG_CONFIG_HOME=" #$home-dir "/.config")
                               "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                               "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                               (default-environment-variables))
                        #:log-file "/var/log/px-server-launcher.log"))
              (stop #~(make-kill-destructor))))))))


(define px-server-launcher-service-type
  (service-type
   (name 'px-server-launcher)
   (description "PantherX Server Application Launcher Service")
   (extensions (list
                (service-extension shepherd-root-service-type
                                   px-server-launcher-shepherd-service)))))
