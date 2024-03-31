(define-module (px hardware raspberrypi)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu system)
  #:use-module (nongnu packages linux)
  #:use-module (px system os)
  #:use-module (px packages linux)
  #:use-module (px services bluetooth)
  #:export (%raspberry-pi-4-template
            %seeed-reterminal-template))

(define %raspberry-pi-4-template
  (os-template
    (title "RaspberryPi 4 hardware")
    (packages (list cloud-utils
                    evtest))
    (services (list (os-template-service
                      (type 'add)
                      (kind btuart-service-type))))
    (firmwares (list raspberrypi-firmware
                     brcm80211-firmware
                     bluez-firmware))))


(define %seeed-reterminal-template
  (os-template
    (title "SEEED reTerminal hardware")
    (packages (list seeed-reterminal-dtoverlays))))
