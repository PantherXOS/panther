;;; PantherX System Configuration Module
;;; This module supports configuration modules for PantherX OS definitions
;;;
;;; Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Franz Geffke <franz@pantherx.org>
;;;

(define-module (px packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu system)
  #:use-module (px packages activity)
  #:use-module (px packages desktop)
  #:use-module (px packages device)
  #:use-module (px packages multimedia)
  #:use-module (px packages package-management)
  #:use-module (px packages setup)
  #:use-module (px packages themes)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%px-core-packages

            %px-desktop-packages-gtk
            %px-desktop-packages-qt
            %px-desktop-packages
            %px-desktop-ee-packages
          
            %px-server-packages
            %px-server-ee-packages

            %px-core-arm-packages
            %px-gui-arm-packages
            %px-desktop-arm-packages))

;;;
;;; CORE
;;;

(define %px-core-packages
  (append (list px                                ;; User and system updates
                ;; px-setup-assistant             ;; System installation
                gnutls nss-certs                  ;; TLS library and NNS certificates
                libimobiledevice                  ;; iPhone tethering
                screen)
          %base-packages))

;;;
;;; DESKTOP
;;;

(define %px-desktop-packages-gtk
  (append
          %gtk-desktop-applications
          %common-desktop-applications
          %px-core-packages))

(define %px-desktop-packages-qt
  (append 
          %qt-desktop-applications
          %common-desktop-applications
          %px-core-packages))

(define %px-desktop-packages
  (append
          %px-desktop-packages-qt))

(define %px-desktop-ee-packages
  (append (list px-device-identity
                px-org-remote-user-activity-service)
          %px-desktop-packages))

;;;
;;; SERVER
;;;

(define %px-server-packages
  (append (list )
          %px-core-packages))

(define %px-server-ee-packages
  (append (list px-device-identity)
          %px-server-packages))

;;;
;;; ARM-SPECIFIC
;;;

(define %px-core-arm-packages
  (append (list cloud-utils           ;; disk utilities
                gnutls nss-certs curl ;; networking
                evtest                ;; hardware status
          %base-packages)))

(define %px-gui-arm-packages
  (append (list openbox              ;; window manager
                libnotify dunst dbus ;; desktop notifications
                xterm xdg-utils
                %px-core-arm-packages)))

(define %px-desktop-arm-packages
  (append (list px-sddm-theme)
          %base-packages))