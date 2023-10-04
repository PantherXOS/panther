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
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%px-core-services

            ;; for custom desktops (for ex. xfce)
            ;; without lxqt
            ; %px-desktop-services-base
            
            %px-desktop-services
            %px-desktop-ee-services
            
            %px-server-services
            %px-server-ee-services

            %px-core-arm-services
            %px-gui-arm-services            
            %px-desktop-arm-services)
  
  #:re-export (px-desktop-service-type))

;;;
;;;
;;; CORE
;;; px-core-os services
;;;

(define %px-core-services
  (append
   (list (service dhcp-client-service-type)
         (service ntp-service-type))
   %base-services))

;;;
;;;
;;; DESKTOP
;;; px-desktop-os services
;;; px-new-desktop services
;;;

(define %px-desktop-services
  (append
         %px-desktop-services-base))

;;;
;;;
;;; ENTERPRISE
;;; px-desktop-ee-o services
;;;

(define %px-desktop-ee-services
  (append (list (service px-device-identity-service-type)
                (service px-user-identity-service-type)
                ;; Desktop
                (service px-desktop-service-type))
          %px-desktop-services-base))

;;;
;;; SERVER
;;; px-server-os services
;;; px-server-ee-os services
;;;

(define %px-server-services
  (append (list
            ;; OpenSSH is enabled by default but only with SSH key
            (service openssh-service-type
                    (openssh-configuration
                     (permit-root-login 'prohibit-password)))

            ;; Time service
            (service ntp-service-type)
            ;; Firewall
            (service nftables-service-type)
            ;; DHCP
            (service dhcp-client-service-type))
          %base-services))

(define %px-server-ee-services
  (append (list (service px-device-identity-service-type))
          %px-server-services))

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
                    (delete network-manager-service-type))))