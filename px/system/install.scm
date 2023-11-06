;; PantherX disk image configuration file
;; Reza Alizadeh Majd <r.majd@PantherX.org>
;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system disk-image path/to/px-install.scm
;; Using Guix time-machine
;; $ guix time-machine --channels=/path/to/channels.scm -- system disk-image -t iso9660 /path/to/px/system/install.scm

(define-module (px system install)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages libusb)
  #:use-module (nongnu packages linux)
  #:use-module (px packages setup)
  #:use-module (guix gexp)
  #:export (installation-os-nonfree))

(define %issue
  ;; Greeting.
  "
\x1b[1;37mThis is a PantherX OS installation image.\x1b[0m
\x1b[1;37mVisit wiki.pantherx.org for more information.\x1b[0m

\x1b[1;33mStart with: px-install run\x1b[0m

")

(define px-installation-os
  (operating-system
    (inherit installation-os)
    (host-name "panther")
    (locale "en_US.utf8")
    (kernel linux)
    (firmware (list linux-firmware))

    (issue %issue)

    (packages (cons* px-install
                     ;; Wi-Fi connection via CLI
                     wpa-supplicant
                     ;; iPhone USB tethering
                     libimobiledevice
                     (operating-system-packages installation-os)))))

px-installation-os
