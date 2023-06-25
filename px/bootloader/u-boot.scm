(define-module (px bootloader u-boot)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (guix gexp)
  #:use-module (px packages bootloaders)
  #:export (u-boot-rpi-arm64-bootloader))


(define install-rpi-arm64-u-boot
  #~(lambda (bootloader root-index image)
      #t))


(define u-boot-rpi-arm64-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-rpi-arm64)
   (disk-image-installer install-rpi-arm64-u-boot)))
