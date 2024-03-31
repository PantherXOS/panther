;;; PantherX System Configuration Module
;;; This module supports configuration modules for PantherX OS definitions
;;;
;;; Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Franz Geffke <franz@pantherx.org>
;;;

(define-module (px services base)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages networking)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)

  #:use-module (px services desktop)
  #:use-module (px services device)
  #:use-module (px services security-token)

  #:export (%px-core-services

            %px-desktop-minmal-services
            %px-desktop-services
            %px-desktop-ee-services

            %px-server-services
            %px-server-ee-services))

;;;
;;; CORE
;;;

(define %px-core-services
  (append (list (service dhcp-client-service-type)
                (service ntp-service-type)) 
          %base-services))

;;;
;;; DESKTOP
;;;

(define %px-desktop-minmal-services
  (append %px-desktop-base-minimal-services))

(define %px-desktop-services
  (append %px-desktop-base-services))

(define %px-desktop-ee-services
  ;; TODO: Does not include default desktop
  (append (list (service px-device-identity-service-type)
                (service px-user-identity-service-type))
          %px-desktop-base-services))

;;;
;;; SERVER
;;;

(define %px-server-services
  (append (list (service openssh-service-type
                         (openssh-configuration (permit-root-login 'prohibit-password)))
                (service nftables-service-type)) 
          %px-core-services))

(define %px-server-ee-services
  (append (list (service px-device-identity-service-type)) 
          %px-server-services))
