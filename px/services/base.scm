;;; PantherX System Configuration Module
;;; This module supports configuration modules for PantherX OS definitions
;;;
;;; Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Franz Geffke <franz@pantherx.org>
;;;

(define-module (px services base)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services cups)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services pm)
  #:use-module (gnu services sddm)
  #:use-module (gnu services sound)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (px packages desktop)
  #:use-module (px services desktop)
  #:use-module (px services device)
  #:use-module (px services security-token)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%px-core-services

            ;; for custom desktops (for ex. xfce)
            ;; without lxqt
            %px-desktop-services-base
            
            %px-desktop-services
            %px-desktop-ee-services

            ;; for custom servers (for ex. docker)
            ;; without nftables and dh
            %px-server-services-base
            
            %px-server-services
            ; %px-server-iptables-services
            %px-server-ee-services

            %px-core-arm-services
            %px-gui-arm-services            
            %px-desktop-arm-services)
  
  #:re-export (px-desktop-service-type))

;;;
;;; Utilities
;;;

(define (make-firewall-rules open-ports)

  (define (make-port-rules open-ports status)
    "Generate list of strings each is a port/service rule for nftables"
    (reduce-right append '()
                  (map (match-lambda
                         ((protocol ports ...)
                          (map (lambda (port)
                                 (string-append "    " protocol " dport " port " " status))
                               ports)))
                       open-ports)))

  (let ((port-rules (make-port-rules open-ports "accept")))
    (plain-file "nftables"
                (string-append "#PantherX firewall rules\n"
                               "table inet filter {\n"
                               "  chain input {\n"
                               "    type filter hook input priority 0; policy drop;\n"
                               "    # early drop of invalid connections\n"
                               "    ct state invalid drop\n"
                               "    # allow established/related connections\n"
                               "    ct state { established, related } accept\n"
                               "    # allow from loopback\n"
                               "    iifname lo accept\n"
                               "    # allow icmp\n"
                               "    ip protocol icmp accept\n"
                               "    ip6 nexthdr icmpv6 accept\n"
                               (string-join port-rules "\n" 'suffix)
                               "    # reject everything else\n"
                               "    reject with icmpx type port-unreachable\n"
                               "  }\n"
                               "  chain forward {\n"
                               "    type filter hook forward priority 0; policy drop;\n"
                               "  }\n"
                               "  chain output {\n"
                               "    type filter hook output priority 0; policy accept;\n"
                               "  }\n"
                               "}\n"))))

;;;
;;;
;;; CORE
;;;

(define %px-core-services
  (append
   ;; list of services that only required to be available in px-core-os,
   ;;  since they are available by default in upstream's %desktop-services
   (list (service dhcp-client-service-type)
         (service ntp-service-type))
   %base-services))

;;;
;;; DESKTOP
;;;

(define %px-desktop-services-base
  (append (list
           ;; Various udev rules incl. FIDO support
           (simple-service 'custom-udev-rules
                           udev-service-type
                           (list libu2f-host))

           (ledger-wallet-service)
           (nitro-key-service)

           ;; Power savings
           (service tlp-service-type)

           ;; Bluetooth service
           ;; (bluetooth-service #:auto-enable? #t)
           (service bluetooth-service-type
                    (bluetooth-configuration
                     (auto-enable? #t)))

           ;; Prevent overheating
           ;; TLP does not conflict with thermald.
           (service thermald-service-type)

           ; Display manager
           (service sddm-service-type
                    (sddm-configuration
                     (minimum-uid 1000)
                     (theme "px-sddm-theme")))

           ;; Printing
           (service cups-service-type
                    (cups-configuration
                     (web-interface? #t)
                     (browsing? #t)
                     (default-paper-size "a4")))

           ;; Keychain
           (service gnome-keyring-service-type
                    (gnome-keyring-configuration
                     (pam-services '(("passwd" . passwd)
                                     ("sddm" . login)))))

           ;; SSH is enabled by default but only with SSH key
           (service openssh-service-type
                    (openssh-configuration
                     (permit-root-login 'prohibit-password)))

           ;; Firewall
           (service nftables-service-type
                    (nftables-configuration
                     (ruleset (make-firewall-rules '()))))

           ;; Screensaver
           (service screen-locker-service-type
                    (screen-locker-configuration
                      (name "xlock")
                      (program (file-append xlockmore "/bin/xlock")))))

          (modify-services %desktop-services
                           ;; GDM is default on upstream, on x86_64
                           (delete gdm-service-type)
                           (delete screen-locker-service-type)
                           (dbus-root-service-type config => (dbus-configuration (inherit config)
                                                              (services (list blueman))))
                           (network-manager-service-type config =>
                                                         (network-manager-configuration
                                                          (inherit config)
                                                          (vpn-plugins (list network-manager-openvpn
                                                                             network-manager-openconnect)))))))

(define %px-desktop-services
  (append
         %px-desktop-services-base))

(define %px-desktop-ee-services
  (append (list (service px-device-identity-service-type)
                (service px-user-identity-service-type)
                ;; Desktop
                (service px-desktop-service-type))
          %px-desktop-services-base))

;;;
;;; SERVER
;;;

(define %px-server-services-base
  (append (list
           ;; OpenSSH is enabled by default but only with SSH key
           (service openssh-service-type
                    (openssh-configuration
                     (permit-root-login 'prohibit-password)))

           ;; Time service
           (service ntp-service-type))

          %base-services))

(define %px-server-services
  (append (list 
                ;; Firewall
                (service nftables-service-type)
                ;; DHCP
                (service dhcp-client-service-type))
          %px-server-services-base))

; (define %px-server-iptables-services
;   (append (list 
;                 ;; Firewall
;                 ;; nftables doesn't work well with Docker
;                 (service iptables-service-type))
;           %px-server-services-base))

(define %px-server-ee-services
  (append (list (service px-device-identity-service-type)
                ;; Firewall
                (service nftables-service-type)
                ;; DHCP
                (service dhcp-client-service-type))
          %px-server-services-base))

;;;
;;; ARM-SPECIFIC
;;;

(define %px-core-arm-services
  (cons*
    ;; networking
    (service wpa-supplicant-service-type)
    (service network-manager-service-type)
    (service modem-manager-service-type)
    (service usb-modeswitch-service-type)
    (service ntp-service-type)

    ;; remote access
    (service openssh-service-type
      (openssh-configuration
        (x11-forwarding? #t)
        (permit-root-login #t)))

    %base-services))

(define %px-gui-arm-services
  (cons*
    (service slim-service-type
      (slim-configuration
        (vt "vt7")
        (auto-login? #t)
        (auto-login-session (file-append openbox "/bin/openbox-session"))
        (default-user "default")))
    (service avahi-service-type)
    (service udisks-service-type)
    (service upower-service-type)
    (service accountsservice-service-type)
    (service polkit-service-type)
    (service elogind-service-type)
    (service dbus-root-service-type)
    polkit-wheel-service
    polkit-network-manager-service  ;; control network without sudo
    polkit-elogind-service          ;; reboot without sudo

    (service pulseaudio-service-type)
    (service alsa-service-type)
    %px-core-arm-services))

(define %px-desktop-arm-services
  (append
   (list (service dhcp-client-service-type)
         (service sddm-service-type
                  (sddm-configuration
                   (minimum-uid 1000)
                   (theme "px-sddm-theme")))
         (service px-desktop-service-type
                  (px-desktop-configuration
                   (lxqt lxqt-modified)
                   (default-packages '()))))
   (modify-services %desktop-services
                    (delete gdm-service-type)
                    (delete network-manager-service-type))))