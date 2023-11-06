;;; Desktop service definitions for PantherX
;;;
;;; Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Franz Geffke <franz@pantherx.org>
;;;

(define-module (px services disk)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services cups)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system setuid)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (create-swap-space-service
            disk-init-configuration
            disk-init-service-type))

;;
;; Create swap-file service
;; primarily for ARM
;;

(define (create-swap-space-service size)
  (simple-service 'create-swap-space activation-service-type
                  (with-imported-modules '((guix build utils))
                                         #~(begin
                                             (use-modules (guix build utils))
                                             (let ((swapfile "/swapfile"))
                                               (when (not (file-exists?
                                                           swapfile))
                                                 (invoke #+(file-append
                                                            util-linux
                                                            "/bin/fallocate")
                                                         "-l"
                                                         #$size swapfile)
                                                 (chmod swapfile #o600)
                                                 (invoke #+(file-append
                                                            util-linux
                                                            "/sbin/mkswap")
                                                         swapfile)))))))

;;
;; Disk initiation service
;; primarily for ARM
;;

(define-record-type* <disk-init-configuration> disk-init-configuration
                     make-disk-init-configuration
  disk-init-configuration?
  (device disk-init-configuration-device)
  (index disk-init-configuration-index)
  (target disk-init-configuration-target)
  (swap-size disk-init-configuration-swap-size
             (default #f))
  (swap-path disk-init-configuration-swap-path
             (default "/swapfile")))

(define disk-init-activation
  (match-lambda
    (($ <disk-init-configuration>
        device
        index
        target
        swap-size
        swap-path)
     (with-imported-modules '((guix build utils))
                            #~(begin
                                (use-modules (guix build utils))
                                (let ((lock-file "/etc/disk-init.lock"))
                                  (when (not (file-exists? lock-file))
                                    ;; resize root partition
                                    ;; workaround to fix growpart execution
                                    (setenv "PATH"
                                            (string-append
                                             "/run/current-system/profile/bin:"
                                             (getenv "PATH")))
                                    (invoke #+(file-append cloud-utils
                                                           "/bin/growpart")
                                            #$device
                                            #$index)
                                    (invoke #+(file-append e2fsprogs
                                                           "/sbin/resize2fs")
                                            #$target)
                                    (invoke #+(file-append coreutils
                                                           "/bin/sync"))

                                    ;; create swap-file
                                    (when (and #$swap-size
                                               (not (file-exists? #$swap-path)))
                                      (invoke #+(file-append util-linux
                                                             "/bin/fallocate")
                                              "-l"
                                              #$swap-size
                                              #$swap-path)
                                      (chmod #$swap-path #o600)
                                      (invoke #+(file-append util-linux
                                                             "/sbin/mkswap")
                                              #$swap-path))

                                    (call-with-output-file lock-file
                                      (lambda (port)
                                        (display "disk image initiated\n" port))))))))))

(define disk-init-service-type
  (service-type (name 'disk-init)
                (extensions (list (service-extension activation-service-type
                                                     disk-init-activation)))
                (description
                 "Resize root partition on first boot and create swapfile")))
