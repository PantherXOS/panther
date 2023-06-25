(define-module (px system raspberry)
  #:use-module (gnu bootloader)
  #:use-module (gnu image)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (gnu system nss)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix platforms arm)
  #:use-module (nongnu packages linux)
  #:use-module (px bootloader u-boot)
  #:use-module (px system os)
  #:use-module (px hardware raspberrypi)
  #:use-module (px packages base)
  #:use-module (px packages bootloaders)
  #:use-module (px packages linux)
  #:use-module (px services base)
  #:use-module (px services device)
  #:use-module (px system config)
  #:use-module (srfi srfi-26)
  #:export (%raspberrypi-config-params
            %raspberrypi-cmdline-params
            raspberrypi-gui-os
            raspberrypi-gui-image

            %reterminal-config-params
            %reterminal-cmdline-params
            reterminal-image-type

            %reterminal-core-packages
            %reterminal-core-services
            reterminal-core-os
            reterminal-core-image

            %reterminal-gui-packages
            %reterminal-gui-services
            reterminal-gui-os))


(define %raspberrypi-config-params
  (list "enable_uart=1"
        "uart_2ndstage=1"
        "arm_64bit=1"
        "kernel=u-boot.bin"))


(define %raspberrypi-cmdline-params
  (list "root=LABEL=RASPIROOT rw rootwait"
        "console=serial0,115200 console=tty1 console=ttyAMA0,115200"
        "selinux=0 plymouth.enable=0 smsc95xx.turbo_mode=N"
        "dwc_otg.lpm_enable=0 kgdboc=serial0,115200"))


(define (make-raspberrypi-boot-partition config cmdline)
  (partition
   (size (* 128 (expt 2 20)))
   (label "BOOT")
   (file-system "fat32")
   (flags '())
   (initializer
    (gexp (lambda* (root #:key #:allow-other-keys)
            (use-modules (guix build utils))
            (mkdir-p root)
            (copy-recursively #$(file-append u-boot-rpi-arm64 "/libexec/u-boot.bin" )
                              (string-append root "/u-boot.bin"))
            (copy-recursively #$(file-append raspberrypi-firmware "/" ) root)
            (copy-recursively #$(file-append seeed-reterminal-dtoverlays "/" )
                              (string-append root "/overlays"))
            (copy-recursively #$(plain-file "config.txt"
                                            (string-join config "\n"))
                              (string-append root "/config.txt"))
            (copy-recursively #$(plain-file "cmdline.txt"
                                            (string-join cmdline " "))
                              (string-append root "/cmdline.txt"))
            )))))

(define %raspberrypi-boot-partition
  (make-raspberrypi-boot-partition %raspberrypi-config-params
                                   %raspberrypi-cmdline-params))


(define %raspberrypi-root-partition
  (partition
   (size 'guess)
   (label "RASPIROOT")
   (file-system "ext4")
   (flags '(boot))
   (initializer (gexp initialize-root-partition))))


(define raspberrypi-image-type
  (image-type
    (name 'raspberrypi-raw)
    (constructor (cut image-with-os
                      (image-without-os
                        (format 'disk-image)
                        (partitions (list %raspberrypi-boot-partition
                                          %raspberrypi-root-partition)))
                      <>))))


(define raspberrypi-gui-os
  (operating-system
    (inherit px-gui-arm-os)
    
    (bootloader (bootloader-configuration
                  (bootloader  u-boot-rpi-arm64-bootloader)
		              (targets '("/dev/vda"))
                  (device-tree-support? #f)))
    (kernel linux-raspberry-5.15)
    (kernel-arguments (cons* "cgroup_enable=memory"
                             %default-kernel-arguments))
    (initrd-modules '())
    (firmware (list raspberrypi-firmware
                    brcm80211-firmware
                    bluez-firmware))
    (file-systems (cons* (file-system
                          (device (file-system-label "BOOT"))
                          (mount-point "/boot/firmware")
                          (type "vfat"))
                         (file-system
                          (device (file-system-label "RASPIROOT"))
                          (mount-point "/")
                          (type "ext4"))
                         %base-file-systems))
    (services (cons* (service btuart-service-type)
                     %px-gui-arm-services))))


(define raspberrypi-gui-image
  (image
   (inherit
    (os+platform->image raspberrypi-gui-os aarch64-linux
                        #:type raspberrypi-image-type))
   (partition-table-type 'mbr)
   (name 'raspberrypi-gui-image)))

;;
;; SEEED reTerminal core configurations
;;

(define %reterminal-config-params
  (cons* "dtoverlay=dwc2,dr_mode=host"
         "dtparam=ant2"
         "disable_splash=1"
         "ignore_lcd=1"
         "dtoverlay=vc4-kms-v3d-pi4"
         "dtoverlay=i2c3,pins_4_5"
         "gpio=13=pu"
         "dtoverlay=reTerminal,tp_rotate=0"
         "dtoverlay=reTerminal-bridge"
         "dtoverlay=reTerminal,key0=0x043,key1=0x044,key2=0x057,key3=0x058"
         "vt.global_cursor_default=0"  
         %raspberrypi-config-params))

(define %reterminal-cmdline-params
  %raspberrypi-cmdline-params)

(define %reterminal-boot-partition
  (make-raspberrypi-boot-partition %reterminal-config-params
                                   %reterminal-cmdline-params))


(define reterminal-image-type
  (image-type
    (name 'reterminal-image-raw)
    (constructor (cut image-with-os
                      (image-without-os
                        (format 'disk-image)
                        (partitions (list %reterminal-boot-partition
                                          %raspberrypi-root-partition)))
                      <>))))

(define %reterminal-core-packages
  %px-core-arm-packages)


(define %reterminal-core-services
  %px-core-arm-services)


(define* (reterminal-core-os #:key (open-ports %px-server-open-ports-common)
                                   (authorized-keys '()))
  (make-os
    (operating-system
      (host-name "reterminal-core")
      (timezone "Europe/Berlin")
      (locale "en_US.utf8")

      (bootloader (bootloader-configuration
                    (bootloader u-boot-rpi-arm64-bootloader)
                    (targets '("/dev/vda"))
                    (device-tree-support? #f)))
      (initrd-modules '())
      (kernel linux-raspberry-5.15)
      (kernel-loadable-modules %reterminal-kernel-modules)
      (kernel-arguments (cons* "cgroup_enable=memory"
                               %default-kernel-arguments))
      (file-systems (cons* (file-system
                            (device (file-system-label "BOOT"))
                            (mount-point "/boot/firmware")
                            (type "vfat"))
                           (file-system
                            (device (file-system-label "RASPIBOOT"))
                            (mount-point "/")
                            (type "ext4"))
                           %base-file-systems))
      (users (cons (user-account
                    (name "panther")
                    (comment "panther's account")
                    (group "users")
                    (password (crypt "pantherx" "$6$abc"))
                    (supplementary-groups '("wheel"
                                            "audio" "video"))
                    (home-directory "/home/panther"))
               %base-user-accounts))
      (name-service-switch %mdns-host-lookup-nss))
    #:kernel 'custom
    #:open-ports open-ports
    #:authorized-keys authorized-keys
    #:templates (list %raspberry-pi-4-template
                      %seeed-reterminal-template)
    #:default-packages %reterminal-core-packages
    #:default-services %reterminal-core-services)
    )

(define reterminal-core-image
  (image
   (inherit
    (os+platform->image (reterminal-core-os)
                        aarch64-linux
                        #:type reterminal-image-type))
   (partition-table-type 'mbr)
   (name 'reterminal-core-image)))

;;
;; SEEED reTerminal GUI configuration
;;


(define %reterminal-gui-packages
  (cons* seeed-reterminal-dtoverlays
         %px-gui-arm-packages))

(define %reterminal-gui-services
  %px-gui-arm-services)

(define reterminal-gui-os
  (operating-system
    (inherit raspberrypi-gui-os)
    (kernel-loadable-modules %reterminal-kernel-modules)
    (packages %reterminal-gui-packages)
    (services %reterminal-gui-services)))
