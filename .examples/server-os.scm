;; PantherX OS Server Configuration

(use-modules (gnu)
             (gnu system)
             (px system config)
             (px services device))

(define %ssh-public-key
  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP7gcLZzs2JiEx2kWCc8lTHOC0Gqpgcudv0QVJ4QydPg franz")

(px-server-os
 (operating-system
  (host-name "px-base")
  (timezone "Europe/Berlin")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/vda"))))
  
  (file-systems (cons (file-system
                       (device (file-system-label "my-root"))
                       (mount-point "/")
                       (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "panther")
                (comment "panther's account")
                (group "users")
                ;; Set the default password to 'pantherx'
                ;; Important: Change with 'passwd panther' after first login
                (password (crypt "pantherx" "$6$abc"))
                (supplementary-groups '("wheel"
                                        "audio" "video"))
                (home-directory "/home/panther"))
               %base-user-accounts))

  (services (cons*
    (service px-device-identity-service-type
              (px-device-identity-configuration
                (port 8000)
                (config-dir "/etc/px-device-identity")
                (key-dir "/etc/px-device-identity/keys")))
   %px-server-services)))

 #:open-ports '(("tcp" "ssh"))
 #:authorized-keys `(("root" ,(plain-file "panther.pub" %ssh-public-key))))
