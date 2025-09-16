;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px system os)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system nss)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  
  #:export (make-os 
            os-template
            os-template-service

            prepare-packages
            prepare-services
            prepare-swap-devices

            adjust-bootloader-theme
            
            %px-substitute-server-key
            %nonguix-substitute-server-key
            %px-substitute-server-url
            %nonguix-substitute-server-url

            %pantherx-default-channels

            apply-px-substitute-server))

;;;
;;; Templates
;;;

(define-record-type* <os-template-service> os-template-service
                     make-os-template-service
  os-template-service?
  (type os-template-service-type) ;type of modification required: 'add 'edit 'delete
  (kind os-template-service-kind) ;service-kind that needs to be matched
  (action os-template-service-action ;action to be applied on service
          (default #f)))

(define-record-type* <os-template> os-template make-os-template
  os-template?
  (title os-template-title)
  (firmwares os-template-firmwares ;list of firmwares to be installed
             (default '()))
  (packages os-template-packages ;list of <package> to be installed
            (default '()))
  (services os-template-services ;list of <os-template-service>
            (default '())))

(define (apply-template-firmwares initial-firmwares template-firmwares)
  (fold (lambda (firmware result)
          (if (memq firmware result) result
              (cons firmware result))) initial-firmwares template-firmwares))

(define (apply-template-package-imports initial-packages template-packages)
  (fold (lambda (pkg result)
          (if (memq pkg result) result
              (cons pkg result))) initial-packages template-packages))

(define (apply-template-service-modifications initial-services
                                              template-services)
  (fold (lambda (svc result)
          (match svc
            (($ <os-template-service> type kind action)
             (case type
               ((add)
                (cons (if action
                          (service kind action)
                          (service kind)) result))
               ((edit)
                (modify-services result
                  (kind config =>
                        (action config))))
               ((delete)
                (remove (lambda (s)
                          (eq? (service-kind s) kind)) result))
               (else result))))) initial-services template-services))

(define (apply-templates os-configuration os-templates)
  (fold (lambda (template result)
          (let ((target-firmwares (apply-template-firmwares (operating-system-firmware
                                                             os-configuration)
                                                            (os-template-firmwares
                                                             template)))
                (target-packages (apply-template-package-imports (operating-system-packages
                                                                  result)
                                                                 (os-template-packages
                                                                  template)))
                (target-services (apply-template-service-modifications (operating-system-user-services
                                                                        result)
                                                                       (os-template-services
                                                                        template))))
            (operating-system
              (inherit result)
              (firmware target-firmwares)
              (packages target-packages)
              (services
               target-services)))) os-configuration os-templates))

;;
;; Firewall customization
;;

(define (make-firewall-rules open-ports)
  (define (make-port-rules open-ports status)
    "Generate list of strings each is a port/service rule for nftables"
    (reduce-right append
                  '()
                  (map (match-lambda
                         ((protocol ports ...)
                          (map (lambda (port)
                                 (string-append "    "
                                                protocol
                                                " dport "
                                                port
                                                " "
                                                status)) ports))) open-ports)))
  (let ((port-rules (make-port-rules open-ports "accept")))
    (plain-file "nftables"
                (string-append "#PantherX firewall rules\n"
                 "table inet filter {\n"
                 "  chain input {\n"
                 "    type filter hook input priority 0; policy drop;
"
                 "    # early drop of invalid connections\n"
                 "    ct state invalid drop\n"
                 "    # allow established/related connections
"
                 "    ct state { established, related } accept
"
                 "    # allow from loopback\n"
                 "    iifname lo accept\n"
                 "    # allow icmp\n"
                 "    ip protocol icmp accept\n"
                 "    ip6 nexthdr icmpv6 accept\n"
                 (string-join port-rules "\n"
                              'suffix)
                 "    # reject everything else\n"
                 "    reject with icmpx type port-unreachable
"
                 "  }\n"
                 "  chain forward {\n"
                 "    type filter hook forward priority 0; policy drop;
"
                 "  }\n"
                 "  chain output {\n"
                 "    type filter hook output priority 0; policy accept;
"
                 "  }\n"
                 "}\n"))))

;;
;; OS customization
;;

(define %px-artwork-repository
  (let ((commit "ecfd456e814a59e3b6743bcda83eab5d5c12ae99"))
    (origin
      (method git-fetch)
      (uri (git-reference (url
                           "https://git.pantherx.org/development/desktop/px-artwork.git")
                          (commit commit)))
      (file-name (string-append "px-artwork-"
                                (string-take commit 7) "-checkout"))
      (sha256 (base32 "06i47c8qp239c9rgkcizk3jd8rld4qbx90s5gg1a1rw1x90p245z")))))

(define %px-grub-theme
  (grub-theme (image (file-append %px-artwork-repository
                                  "/grub/PantherX-4-3.svg"))))

(define %px-substitute-server-url
  "https://packages.pantherx.org")

(define %nonguix-substitute-server-url
  "https://substitutes.nonguix.org")

(define %px-substitute-server-key
  (plain-file "packages.pantherx.org.pub"
   "(public-key 
 (ecc 
  (curve Ed25519)
  (q #E8322D13EA02C09F06CB70FDA2ABBFD5E463F2AA34C18C692F5E25858F4E315D#)
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

(define (adjust-bootloader-theme config)
  (let* ((bootloader-config (operating-system-bootloader config))
         (bootloader (bootloader-configuration-bootloader bootloader-config)))
    (case (bootloader-name bootloader)
      ((grub grub-efi)
       (bootloader-configuration
         (inherit bootloader-config)
         (theme %px-grub-theme)))
      (else bootloader-config))))

(define (prepare-packages config default-packages)
  "Check if custom packages provided in system configuration file or not.
return @code{default-packages} if there was no modifications applied."
  (let ((package-list (operating-system-packages config)))
    (if (eq? package-list %base-packages) default-packages package-list)))

(define (prepare-services config default-services)
  "Check if custom services provided in system configuration file or not.
return @code{default-services} if there is no modification applied."
  (let ((service-list (operating-system-user-services config)))
    (if (eq? service-list %base-services) default-services service-list)))

(define (prepare-swap-devices config default-value)
  "Check if custom definition provided for swap-devices or not.
return @code{defaule-value} if there is no modification applied."
  (let ((devices (operating-system-swap-devices config)))
    (if (eq? devices
             '()) default-value devices)))

(define (prepare-kernel config kernel)
  (case kernel
    ((libre)
     linux-libre)
    ((nonlibre)
     linux)
    (else (operating-system-kernel config))))

(define (prepare-initrd config kernel)
  (case kernel
    ((libre)
     base-initrd)
    ((nonlibre)
     microcode-initrd)
    (else (operating-system-initrd config))))

(define (prepare-firmwares config kernel)
  (case kernel
    ((libre)
     %base-firmware)
    ((nonlibre)
     (list linux-firmware))
    (else (operating-system-firmware config))))

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

;;
;; OS config generation
;;

(define (apply-px-substitute-server guix-config)
  (let ((existing-urls (guix-configuration-substitute-urls guix-config))
        (existing-keys (guix-configuration-authorized-keys guix-config)))
    (guix-configuration (inherit guix-config)
                        (substitute-urls (append (list
                                                  %px-substitute-server-url
                                                  %nonguix-substitute-server-url)
                                                 existing-urls))
                        (authorized-keys (append (list
                                                  %px-substitute-server-key
                                                  %nonguix-substitute-server-key)
                                                 existing-keys))
                        (channels %pantherx-default-channels))))

(define* (make-os config
                  #:key (kernel 'libre)
                  (open-ports #f)
                  (authorized-keys '())
                  (templates '())
                  default-packages
                  default-services)
  "Create <operating-system> definition based on provided templates and default
packages and services"

  (define (apply-firewall-rules config)
    (nftables-configuration (inherit config)
                            (ruleset (if open-ports
                                         (make-firewall-rules open-ports)
                                         (nftables-configuration-ruleset
                                          config)))))

  (define (apply-authorized-keys config)
    (if (> (length authorized-keys) 0)
        (openssh-configuration (inherit config)
                               (authorized-keys authorized-keys)) config))

  (define (apply-swap-changes config)
    (let ((devices (operating-system-swap-devices config)))
      (if (eq? devices
               '())
          (list (swap-space
                  (target "/swapfile"))) devices)))

  (let ((target-kernel (prepare-kernel config kernel))
        (target-initrd (prepare-initrd config kernel))
        (target-firmwares (prepare-firmwares config kernel))
        (target-bootloader (adjust-bootloader-theme config))
        (target-packages (prepare-packages config default-packages))
        (target-services (prepare-services config default-services)))
    
    (apply-templates (operating-system
                       (inherit config)
                       (bootloader target-bootloader)
                       (kernel target-kernel)
                       (initrd target-initrd)
                       (firmware target-firmwares)
                       (swap-devices (apply-swap-changes config))

                       (packages target-packages)
                       (services
                        (fold (lambda (svc result)
                                (let ((type (service-kind svc))
                                      (value (service-value svc)))
                                  (cond
                                    ((eq? type guix-service-type)
                                     (cons (service guix-service-type
                                                    (apply-px-substitute-server
                                                     value)) result))
                                    ((eq? type nftables-service-type)
                                     (cons (service nftables-service-type
                                                    (apply-firewall-rules
                                                     value)) result))
                                    ((eq? type openssh-service-type)
                                     (cons (service openssh-service-type
                                                    (apply-authorized-keys
                                                     value)) result))
                                    (else (cons svc result)))))
                              '() target-services))
                       (name-service-switch %mdns-host-lookup-nss)) templates)))
