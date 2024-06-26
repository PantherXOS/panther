;;; Desktop service definitions for PantherX
;;; Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Franz Geffke <franz@pantherx.org>

(define-module (px services desktop)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages nfs)
  #:use-module (gnu services)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services cups)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages security-token)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services sddm)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu services sound)
  #:use-module (gnu services pm)
  #:use-module (gnu system)
  #:use-module (gnu system setuid)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (px packages desktop)
  #:use-module (px services security-token)
  #:use-module (srfi srfi-1)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:export (px-desktop-configuration
            px-desktop-configuration?

            polkit-network-manager-service
            polkit-elogind-service

            create-swap-space-service

            %px-desktop-base-services
            %px-desktop-base-minimal-services
		        %desktop-services-assembly-plain))

;;
;; allow netdev group to control network manger
;;

(define polkit-network-manager
  (file-union "polkit-nm"
              `(("share/polkit-1/rules.d/50-org.freedesktop.NetworkManager.rules" ,
                 (plain-file "nm.rules"
                  "polkit.addRule(function(action, subject) {
  if (action.id.indexOf(\"org.freedesktop.NetworkManager.\") == 0 && subject.isInGroup(\"netdev\")) {
    return polkit.Result.YES;
  }
});
")))))

;; primarily for ARM

(define polkit-network-manager-service
  (simple-service 'polkit-nm polkit-service-type
                  (list polkit-network-manager)))

;;
;; Allow users group to perform reboot/poweroff
;; primarily for ARM
;;

(define polkit-loginctl
  (file-union "polkit-loginctl"
              `(("share/polkit-1/rules.d/10-enable-session-power.rules" ,(plain-file
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
  (simple-service 'polkit-login polkit-service-type
                  (list polkit-loginctl)))

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

;; Generic Desktop for Qt, GTP
(define* (desktop-services-for-system #:optional (system (or (%current-target-system)
                                                             (%current-system))))

  ;;
  ;; GUIX DEFAULT
  ;;

  ;; List of services typically useful for a "desktop" use case.
  (cons* (service screen-locker-service-type
                  (screen-locker-configuration (name "xlock")
                                               (program (file-append xlockmore
                                                         "/bin/xlock"))))

         ;; Add udev rules for MTP devices so that non-root users can access them.
         (simple-service 'mtp udev-service-type (list libmtp))
         ;; Add udev rules for scanners.
         (service sane-service-type)
         ;; Add polkit rules, so that non-root users in the wheel group can
         ;; perform administrative tasks (similar to "sudo").
         polkit-wheel-service

         ;; Allow desktop users to also mount NTFS and NFS file systems
         ;; without root.
         (simple-service 'mount-setuid-helpers setuid-program-service-type
                         (map (lambda (program)
                                (setuid-program
                                  (program program)))
                              (list (file-append nfs-utils "/sbin/mount.nfs")
                                    (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))

         ;; This is a volatile read-write file system mounted at /var/lib/gdm,
         ;; to avoid GDM stale cache and permission issues.
         gdm-file-system-service

         ;; The global fontconfig cache directory can sometimes contain
         ;; stale entries, possibly referencing fonts that have been GC'd,
         ;; so mount it read-only.
         fontconfig-file-system-service

         ;; NetworkManager and its applet.
         (service network-manager-service-type)
         (service wpa-supplicant-service-type) ;needed by NetworkManager
         (simple-service 'network-manager-applet profile-service-type
                         (list network-manager-applet))
         (service modem-manager-service-type)
         (service usb-modeswitch-service-type)

         ;; The D-Bus clique.
         (service avahi-service-type)
         (service udisks-service-type)
         (service upower-service-type)
         (service accountsservice-service-type)
         (service cups-pk-helper-service-type)
         (service colord-service-type)
         (service geoclue-service-type)
         (service polkit-service-type)
         (service elogind-service-type)
         (service dbus-root-service-type)

         (service ntp-service-type)

         (service x11-socket-directory-service-type)

         (service pulseaudio-service-type)
         (service alsa-service-type)

         ;;
         ;; PANTHERX SPECIFIC
         ;;

         (simple-service 'custom-udev-rules udev-service-type
                         (list libu2f-host))

         ;; Adding plugdev group once should suffice
         (udev-rules-service 'nitro %nitro-key-udev-rule #:groups '("plugdev"))
         ;; Using the rules from libfido2 package
         (udev-rules-service 'fido2 libfido2)
         (udev-rules-service 'yubikey yubikey-personalization)
         (udev-rules-service 'coinkite %coinkite-udev-rule)

         ;; Power savings
         (service tlp-service-type)

         ;; Bluetooth service
         ;; (bluetooth-service #:auto-enable? #t)
         (service bluetooth-service-type
                  (bluetooth-configuration (auto-enable? #t)))

         ;; Prevent overheating
         ;; TLP does not conflict with thermald.
         (service thermald-service-type)

         ;; Display manager
         (service sddm-service-type
                  (sddm-configuration (minimum-uid 1000)
                                      (theme "px-sddm-theme")))

         ;; Printing
         (service cups-service-type
                  (cups-configuration (web-interface? #t)
                                      (browsing? #t)
                                      (default-paper-size "a4")))

         ;; Keychain
         (service gnome-keyring-service-type
                  (gnome-keyring-configuration (pam-services '(("passwd" . passwd)
                                                               ("sddm" . login)))))

         ;; SSH is enabled by default but only with SSH key
         (service openssh-service-type
                  (openssh-configuration (permit-root-login 'prohibit-password)))

         ;; Firewall
         (service nftables-service-type
                  (nftables-configuration (ruleset (make-firewall-rules '()))))

         %base-services))

(define-syntax %px-desktop-base-services
  (identifier-syntax (desktop-services-for-system)))


(define %px-desktop-base-minimal-services
  (modify-services
    %px-desktop-base-services
    (delete login-service-type)
    (delete agetty-service-type)
    (delete mingetty-service-type)
    (delete pulseaudio-service-type)
    (delete alsa-service-type)
    (delete sddm-service-type)
    (delete gnome-keyring-service-type)
    (delete openssh-service-type)))