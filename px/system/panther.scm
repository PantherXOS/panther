;;; Package Repository for GNU Guix
;;; Copyright © 2021-2023 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px system panther)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu packages package-management)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (srfi srfi-1)

  ;; bluetooth-service-type
  #:use-module (gnu services desktop)
  ;; pulseaudio-service-type
  #:use-module (gnu services sound)
  ;; gdm-service-type sddm-service-type
  #:use-module (gnu services xorg)
  #:use-module (gnu services sddm)

  ;; wpa-supplicant
  #:use-module (gnu packages admin)
  ;; blueman
  #:use-module (gnu packages networking)
  ;; libimobiledevice
  #:use-module (gnu packages libusb)
  ;; neovim
  #:use-module (gnu packages vim)
  ;; pam-u2f libu2f-host libu2f-server
  #:use-module (gnu packages security-token)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  ;; nitro-key-udev-rule
  #:use-module (px services security-token)
  
  #:export (%panther-base-services
            %panther-desktop-services
            %panther-desktop-services-minimal
            %panther-base-packages
            %panther-desktop-packages

            %panther-os
            %panther-desktop-os))

(define %gofranz-substitute-server-url
  "https://substitutes.guix.gofranz.com")

(define %nonguix-substitute-server-url
  "https://substitutes.nonguix.org")

(define %gofranz-substitute-server-key
  (plain-file "substitutes.guix.gofranz.com.pub"
   "(public-key
 (ecc
  (curve Ed25519)
  (q #0096373009D945F86C75DFE96FC2D21E2F82BA8264CB69180AA4F9D3C45BAA47#)
  )
 )
"))

(define %nonguix-substitute-server-key
  (plain-file "substitutes.nonguix.org.pub"
   "(public-key 
 (ecc 
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
  )
 )
"))

(define %pantherx-default-channels
  (append (list (channel
                  (name 'pantherx)
                  (branch "master")
                  (url "https://codeberg.org/gofranz/panther.git")
                  (introduction
                   (make-channel-introduction
                    "54b4056ac571611892c743b65f4c47dc298c49da"
                    (openpgp-fingerprint
                     "A36A D41E ECC7 A871 1003  5D24 524F EB1A 9D33 C9CB")))))
          %default-channels))

(define %panther-services-udev
  (list (simple-service 'custom-udev-rules udev-service-type
                        (list libu2f-host))

         ;; Adding plugdev group once should suffice
         (udev-rules-service 'nitro %nitro-key-udev-rule #:groups '("plugdev"))
         (udev-rules-service 'fido2 libfido2)
         (udev-rules-service 'yubikey yubikey-personalization)
         (udev-rules-service 'coinkite %coinkite-udev-rule)))

;;
;; BASE
;;
(define %panther-base-services
  (modify-services %base-services
    (guix-service-type config =>
      (guix-configuration
        (inherit config)
        (guix (guix-for-channels %pantherx-default-channels))
        (authorized-keys
        (cons* %gofranz-substitute-server-key
                %nonguix-substitute-server-key
                %default-authorized-guix-keys))
        (substitute-urls
        (cons* %gofranz-substitute-server-url
                %nonguix-substitute-server-url
                %default-substitute-urls))
        (channels %pantherx-default-channels)))))

(define %panther-base-packages
  (cons* wpa-supplicant
         libimobiledevice
         neovim
         %base-packages))

;;
;; DESKTOP
;;
(define %desktop-services-modified
  (modify-services %desktop-services
    (guix-service-type config =>
      (guix-configuration
        (inherit config)
        (guix (guix-for-channels %pantherx-default-channels))
        (authorized-keys
        (cons* %gofranz-substitute-server-key
                %nonguix-substitute-server-key
                %default-authorized-guix-keys))
        (substitute-urls
        (cons* %gofranz-substitute-server-url
                %nonguix-substitute-server-url
                %default-substitute-urls))
        (channels %pantherx-default-channels)))))

(define %panther-desktop-services
  (append %panther-services-udev
         %desktop-services-modified))

;; Remove display managers and other services for minimal setup.
;; Using 'remove' instead of 'modify-services' with 'delete' because
;; %desktop-services may contain either GDM or SDDM depending on architecture.
(define %panther-desktop-services-minimal
  (remove (lambda (service)
            (memq (service-kind service)
                  (list login-service-type
                        agetty-service-type
                        mingetty-service-type
                        gdm-service-type
                        sddm-service-type
                        pulseaudio-service-type
                        alsa-service-type)))
          %panther-desktop-services))

(define %panther-desktop-packages
  (cons* wpa-supplicant
         libimobiledevice
         neovim
         pam-u2f
         libu2f-host
         libu2f-server
         blueman
         %panther-base-packages))
  
;;
;; BASE OS
;;
(define %panther-os
  (operating-system
    (host-name "panther")
    (timezone "Europe/Berlin")
    (locale "en_US.UTF-8")
    (kernel linux)
    (firmware (list linux-firmware))

    (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets '("/dev/vda"))))
    
    (file-systems (cons (file-system
                        (device (file-system-label "my-root"))
                        (mount-point "/")
                        (type "ext4"))
                        %base-file-systems))

    (users (cons (user-account
                  (name "panther")
                  (comment "Default account")
                  (group "users")
                  (password (crypt "pantherx" "$6$abc"))
                  (supplementary-groups '("wheel" "audio" "video")))
                 %base-user-accounts))
                 
    (services %panther-base-services)
    (packages %panther-base-packages)))

;;
;; DESKTOP OS
;;
(define %panther-desktop-os
  (operating-system
    (inherit %panther-os)
    (services %panther-desktop-services)
    (packages %panther-desktop-packages)))
