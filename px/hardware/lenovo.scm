(define-module (px hardware lenovo)
  #:use-module (gnu services sddm)
  #:use-module (gnu services xorg)
  #:use-module (nongnu packages linux)
  #:use-module (px system os)
  #:use-module (px packages throttled)
  #:use-module (px system config)
  #:export (%lenovo-thinkpad-t450-template))

(define %lenovo-thinkpad-t450-template
  (os-template
   (title "Lenovo ThinkPad T450 hardware")
   (firmwares (list iwlwifi-firmware))
   (packages (list throttled))
   (services (list
              (os-template-service
               (type 'edit)
               (kind sddm-service-type)
               (action (lambda (config)
                         (sddm-configuration
                          (inherit config)
                          (xorg-configuration
                           (xorg-configuration
                            (extra-config
                             `( "Section \"InputClass\"\n"
                                "   Identifier \"touchpad\"\n"
                                "   Driver \"libinput\"\n"
                                "   MatchIsTouchpad \"on\"\n"
                                "   Option \"Tapping\" \"on\"\n"
                                "EndSection\n"
                                "\n")))))
                         )))))))
