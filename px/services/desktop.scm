;;; Desktop service definitions for PantherX
;;;
;;; Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Franz Geffke <franz@pantherx.org>
;;;

(define-module (px services desktop)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services cups)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services sddm)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (gnu system setuid)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (px packages desktop)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (px-desktop-configuration
            px-desktop-configuration?
            px-desktop-service-type

            polkit-network-manager-service
            polkit-elogind-service

            create-swap-space-service))

;;
;; PantherX desktop service type
;;

(define-record-type* <px-desktop-configuration> px-desktop-configuration
  make-px-desktop-configuration
  px-desktop-configuration?
  (lxqt px-config-package
        (default lxqt-modified))
  (default-packages px-config-default-packages
                    (default (list px-desktop-defaults))))

(define (px-desktop-polkit-settings config)
  "Return the list of LXQt dependencies that provide polkit actions and
rules."
  (list lxqt-admin))

(define px-desktop-profile-packages
  (lambda (config)
    (append 
      (list (px-config-package config))
      (px-config-default-packages config))))


(define px-desktop-service-type
  (service-type
   (name 'px-desktop)
   (extensions
    (list (service-extension polkit-service-type
                             px-desktop-polkit-settings)
          (service-extension profile-service-type
                             px-desktop-profile-packages)))
   (default-value (px-desktop-configuration))
   (description "Run LXQt desktop environment on PantherX.")))

;;
;; allow netdev group to control network manger
;;

(define polkit-network-manager
  (file-union
   "polkit-nm"
   `(("share/polkit-1/rules.d/50-org.freedesktop.NetworkManager.rules"
      ,(plain-file
        "nm.rules"
        "polkit.addRule(function(action, subject) {
  if (action.id.indexOf(\"org.freedesktop.NetworkManager.\") == 0 && subject.isInGroup(\"netdev\")) {
    return polkit.Result.YES;
  }
});
")))))


;; primarily for ARM

(define polkit-network-manager-service
  (simple-service 'polkit-nm polkit-service-type (list polkit-network-manager)))

;;
;; Allow users group to perform reboot/poweroff
;; primarily for ARM
;;

(define polkit-loginctl
  (file-union
   "polkit-loginctl"
   `(("share/polkit-1/rules.d/10-enable-session-power.rules"
      ,(plain-file
        "login.rules"
        "polkit.addRule(function(action, subject) {
  if ( (action.id == \"org.freedesktop.login1.reboot\" ||
        action.id == \"org.freedesktop.login1.reboot-multiple-sessions\" ||
        action.id == \"org.freedesktop.login1.power-off\" ||
        action.id == \"org.freedesktop.login1.power-off-multiple-sessions\")
        && subject.isInGroup(\"users\") ) {
    return polkit.Result.YES;
  }
});
")))))

(define polkit-elogind-service
  (simple-service 'polkit-login polkit-service-type (list polkit-loginctl)))