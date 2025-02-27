(define-module (px packages base)
  #:use-module (gnu system)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (px packages desktop)
  #:use-module (px packages device)
  #:use-module (px packages package-management)
  #:use-module (px packages setup)
  #:use-module (px packages themes)
  #:export (%px-core-packages
            %px-desktop-minimal-packages
            %px-desktop-packages-gtk
            %px-desktop-packages-qt
            %px-desktop-packages
            %px-desktop-ee-packages
            %px-server-packages
            %px-server-ee-packages))

;;;
;;; CORE
;;;

(define %px-core-packages
  (append (list px
                gnutls
                libimobiledevice
                screen) 
          %base-packages))

;;;
;;; DESKTOP
;;;

;; Stuff for every desktop; QT / GTK on X / Wayland
(define %px-desktop-minimal-packages
  (append %minimal-desktop-applications
	  %px-core-packages))

;; GTK-specific
(define %px-desktop-packages-gtk
  (append %gtk-desktop-applications 
          %minimal-desktop-applications
          %common-desktop-applications
          %px-core-packages))

;; QT-specific
(define %px-desktop-packages-qt
  (append %qt-desktop-applications
          %minimal-desktop-applications
          %common-desktop-applications
          %px-core-packages))

;; Default
(define %px-desktop-packages
  (append %px-desktop-packages-qt))

(define %px-desktop-ee-packages
  (append (list px-device-identity)
          %px-desktop-packages))

;;;
;;; SERVER
;;;

(define %px-server-packages
  (append (list) 
	  %px-core-packages))

(define %px-server-ee-packages
  (append (list px-device-identity) 
	  %px-server-packages))
