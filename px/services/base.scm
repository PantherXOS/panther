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

            %px-desktop-core-services
            %px-desktop-services
            %px-desktop-ee-services

            %px-server-services
            %px-server-ee-services))

;;;
;;; CORE
;;; px-core-os services
;;;

(define %px-core-services
  (append (list
	   (service dhcp-client-service-type)
           (service ntp-service-type))
          %base-services))

;;;
;;; DESKTOP
;;; px-desktop-os services
;;; px-desktop-ee-os services
;;;

(define %px-desktop-core-services
  (append %desktop-services-assembly-plain))

(define %px-desktop-services
  (append %desktop-services-assembly))

(define %px-desktop-ee-services
  ;; TODO: Does not include default desktop
  (append (list
	   (service px-device-identity-service-type)
           (service px-user-identity-service-type))
	  %desktop-services-assembly))

;;;
;;; SERVER
;;; px-server-os services
;;; px-server-ee-os services
;;;

(define %px-server-services
  (append (list
           (service openssh-service-type
                    (openssh-configuration (permit-root-login 'prohibit-password)))
	   
           (service ntp-service-type)
           (service nftables-service-type)
           (service dhcp-client-service-type)) 
          %base-services))

(define %px-server-ee-services
  (append (list
	   (service px-device-identity-service-type)) 
	  %px-server-services))
