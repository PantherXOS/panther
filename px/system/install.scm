;;; PantherX disk image configuration file
;;;
;;; Author: Reza Alizadeh Majd <r.majd@PantherX.org>
;;; Version: 1.0.0
;;; Time-stamp: <2022-02-09 21:33:50 reza>

;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system disk-image path/to/px-install.scm
;; Using Guix time-machine
;; $ guix time-machine --channels=/path/to/channels.scm -- system disk-image /path/to/px-install.scm

(define-module (px system install)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (nongnu packages linux)
  #:use-module (px packages setup)
  #:export (installation-os-nonfree))

(define px-installation-os
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (firmware (list linux-firmware))

    (packages (cons* px-install
                     (operating-system-packages installation-os)))))

px-installation-os
