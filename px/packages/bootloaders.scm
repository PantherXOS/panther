;;; Bootloader definitions for PantherX OS
;;; Reza Alizadeh Majd <r.majd@pantherx.org>
;;;

(define-module (px packages bootloaders)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages firmware)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; #:export (u-boot-khadas-edge-rk3399-bootloader)
  )

;;; required items:
;;;   1. bootloader package
;;;   2. bootloader definition
;;;

; (define-public u-boot-khadas-edge-rk3399
;   (let ((base (make-u-boot-package "khadas-edge-rk3399" "aarch64-linux-gnu")))
;     (package
;       (inherit base)
;       (arguments
;         (substitute-keyword-arguments (package-arguments base)
;           ((#:phases phases)
;            `(modify-phases ,phases
;               (add-after 'unpack 'set-environment
;                 (lambda* (#:key inputs #:allow-other-keys)
;                   ;; Reference:
;                   ;; https://github.com/krescue/khadas-uboot/blob/2a2c7b5058b8cf26b4aa315d463f2c32c32d1abb/packages/arm-trusted-firmware/package.mk#L23
;                   (setenv "BL31" (string-append (assoc-ref inputs "firmware")
;                                                 "/bl31.elf"))
;                   #t))
;               ;; Phases do not succeed on bl31 ELF.
;               (delete 'strip)
;               (delete 'validate-runpath)))))
;       (native-inputs
;        `(("firmware" ,arm-trusted-firmware-rk3399)
;         ,@(package-native-inputs base))))))

; (define install-khadas-edge-rk3399-u-boot
;   #~(lambda (bootloader root-index image)
;       (let ((idb (string-append bootloader "/libexec/idbloader.img"))
;             (u-boot (string-append bootloader "/libexec/u-boot.itb")))
;         ;; (write-file-on-device file size device offset)
;         ;; TODO: we might need to update offset
;         (write-file-on-device idb (stat:size (stat idb))
;                               image (* 64 512))
;         (write-file-on-device u-boot (stat:size (stat u-boot))
;                               image (* 16384 512)))))

; (define u-boot-khadas-edge-rk3399-bootloader
;   (bootloader
;     (inherit u-boot-bootloader)
;     (package u-boot-khadas-edge-rk3399)
;     (disk-image-installer install-khadas-edge-rk3399-u-boot)))

(define-public u-boot-rpi-arm64
  (make-u-boot-package "rpi_arm64" "aarch64-linux-gnu"))
