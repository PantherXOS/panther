;;; Package Repository for GNU Guix
;;; Copyright © 2021-2023 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px system config)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system nss)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (px packages base)
  #:use-module (px services base)
  #:use-module (px system os)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (srfi srfi-1)

  #:export (px-core-os 
            px-desktop-os
            px-desktop-ee-os

            px-server-os
            px-server-ee-os

            %px-server-open-ports-common)

  ;; Re-export for convenience
  #:re-export (%px-core-services 
               %px-desktop-minmal-services
               %px-desktop-services
               %px-desktop-ee-services

               %px-server-services
               %px-server-ee-services

               %px-core-packages

               %px-desktop-minimal-packages
               %px-desktop-packages-gtk
               %px-desktop-packages-qt
               %px-desktop-packages
               %px-desktop-ee-packages

               %px-server-packages
               %px-server-ee-packages
               
               %pantherx-default-channels))

;;;
;;; PantherX Desktop OS defintions
;;;

(define %px-desktop-swap-devices
  (list (swap-space
          (target "/swapfile"))))

(define %px-server-open-ports-common
  '(("tcp" "ssh" "http" "https")))

;;;
;;; CORE
;;;

(define* (px-core-os os-config
                     #:key (kernel 'libre)
                     (templates '())
                     (open-ports #f)
                     (authorized-keys '()))
  "returns operating-system definition for px-core-os, based on config"
  (make-os os-config
           #:kernel kernel
           #:open-ports open-ports
           #:authorized-keys authorized-keys
           #:templates templates
           #:default-packages %px-core-packages
           #:default-services %px-core-services))

;;;
;;; DESKTOP
;;;

(define* (px-desktop-os os-config
                        #:key (kernel 'nonlibre)
                        (templates '())
                        (open-ports #f)
                        (authorized-keys '()))
  (make-os (operating-system
             (inherit os-config)
             (swap-devices (prepare-swap-devices os-config
                                                 %px-desktop-swap-devices)))
           #:kernel kernel
           #:open-ports open-ports
           #:authorized-keys authorized-keys
           #:templates templates
           #:default-packages %px-desktop-packages
           #:default-services %px-desktop-services))

(define* (px-desktop-ee-os os-config
                           #:key (kernel 'nonlibre)
                           (templates '())
                           (open-ports #f)
                           (authorized-keys '()))
  (make-os (operating-system
             (inherit os-config)
             (swap-devices (prepare-swap-devices os-config
                                                 %px-desktop-swap-devices)))
           #:kernel kernel
           #:open-ports open-ports
           #:authorized-keys authorized-keys
           #:templates templates
           #:default-packages %px-desktop-ee-packages
           #:default-services %px-desktop-ee-services))

;;;
;;; SERVER
;;;

(define* (px-server-os os-config
                       #:key (kernel 'libre)
                       (templates '())
                       (open-ports %px-server-open-ports-common)
                       (authorized-keys '()))
  (make-os os-config
           #:kernel kernel
           #:open-ports open-ports
           #:authorized-keys authorized-keys
           #:templates templates
           #:default-packages %px-server-packages
           #:default-services %px-server-services))

(define* (px-server-ee-os os-config
                          #:key (kernel 'libre)
                          (templates '())
                          (open-ports %px-server-open-ports-common)
                          (authorized-keys '()))
  (make-os os-config
           #:kernel kernel
           #:open-ports open-ports
           #:authorized-keys authorized-keys
           #:templates templates
           #:default-packages %px-server-ee-packages
           #:default-services %px-server-ee-services))
