;;; module for security tokens and hardware wallet related services
;;; Reza Alizadeh Majd (r.majd@pantherx.org)

(define-module (px services security-token)
  #:use-module (gnu services base)
  #:export (ledger-wallet-service nitro-key-service yubikey-service coinkite-service))

;;;
;;; Ledger hardware wallet definitions
;;; udev-rules from: https://github.com/LedgerHQ/udev-rules/blob/master/20-hw1.rules
;;;

(define (ledger-udev-rule-record title vendor-id product-id tags)
  (string-append "# "
                 title
                 "\n"
                 "SUBSYSTEMS==\"usb\", "
                 "ATTRS{idVendor}==\""
                 vendor-id
                 "\", "
                 "ATTRS{idProduct}==\""
                 product-id
                 "\", "
                 (string-join (map (lambda (tag)
                                     (string-append "TAG+=\"" tag "\"")) tags)
                              ", ")
                 "\n"))

(define %ledger-udev-rule
  (udev-rule "20-ledger.rules"
             (string-append (ledger-udev-rule-record "HW.1 / Nano" "2581"
                                                     "1b7c|2b7c|3b7c|4b7c"
                                                     '("uaccess" "udev-acl"))
                            (ledger-udev-rule-record "Blue" "2c97"
                             "0000|0000|0001|0002|0003|0004|0005|0006|0007|0008|0009|000a|000b|000c|000d|000e|000f|0010|0011|0012|0013|0014|0015|0016|0017|0018|0019|001a|001b|001c|001d|001e|001f"
                             '("uaccess" "udev-acl"))
                            (ledger-udev-rule-record "Nano S" "2c97"
                             "0001|1000|1001|1002|1003|1004|1005|1006|1007|1008|1009|100a|100b|100c|100d|100e|100f|1010|1011|1012|1013|1014|1015|1016|1017|1018|1019|101a|101b|101c|101d|101e|101f"
                             '("uaccess" "udev-acl"))

                            (ledger-udev-rule-record "Aramis" "2c97"
                             "0002|2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|200a|200b|200c|200d|200e|200f|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|201a|201b|201c|201d|201e|201f"
                             '("uaccess" "udev-acl"))
                            (ledger-udev-rule-record "HW2" "2c97"
                             "0003|3000|3001|3002|3003|3004|3005|3006|3007|3008|3009|300a|300b|300c|300d|300e|300f|3010|3011|3012|3013|3014|3015|3016|3017|3018|3019|301a|301b|301c|301d|301e|301f"
                             '("uaccess" "udev-acl"))
                            (ledger-udev-rule-record "Nano X" "2c97"
                             "0004|4000|4001|4002|4003|4004|4005|4006|4007|4008|4009|400a|400b|400c|400d|400e|400f|4010|4011|4012|4013|4014|4015|4016|4017|4018|4019|401a|401b|401c|401d|401e|401f"
                             '("uaccess" "udev-acl"))
                            (ledger-udev-rule-record "Ledger Test" "2c97"
                             "0005|5000|5001|5002|5003|5004|5005|5006|5007|5008|5009|500a|500b|500c|500d|500e|500f|5010|5011|5012|5013|5014|5015|5016|5017|5018|5019|501a|501b|501c|501d|501e|501f"
                             '("uaccess" "udev-acl")))))

(define (ledger-wallet-service)
  (udev-rules-service 'ledger %ledger-udev-rule))

;;;
;;; Nitrokey definitions
;;; udev-rules from: https://raw.githubusercontent.com/Nitrokey/libnitrokey/master/data/41-nitrokey.rules
;;;

(define %nitro-key-udev-rule
  (udev-rule "41-nitrokey.rules"
   "ACTION!=\"add|change\", GOTO=\"u2f_end\"
# Nitrokey U2F
KERNEL==\"hidraw*\", SUBSYSTEM==\"hidraw\", ATTRS{idVendor}==\"2581\", ATTRS{idProduct}==\"f1d0\", TAG+=\"uaccess\"
# Nitrokey FIDO U2F
KERNEL==\"hidraw*\", SUBSYSTEM==\"hidraw\", ATTRS{idVendor}==\"20a0\", ATTRS{idProduct}==\"4287\", TAG+=\"uaccess\"
# Nitrokey FIDO2
KERNEL==\"hidraw*\", SUBSYSTEM==\"hidraw\", ATTRS{idVendor}==\"20a0\", ATTRS{idProduct}==\"42b1\", TAG+=\"uaccess\"
# Nitrokey 3 NFC
KERNEL==\"hidraw*\", SUBSYSTEM==\"hidraw\", ATTRS{idVendor}==\"20a0\", ATTRS{idProduct}==\"42b3\", TAG+=\"uaccess\"

LABEL=\"u2f_end\"


SUBSYSTEM!=\"usb\", GOTO=\"gnupg_rules_end\"
ACTION!=\"add\", GOTO=\"gnupg_rules_end\"

# USB SmartCard Readers
## Crypto Stick 1.2
ATTR{idVendor}==\"20a0\", ATTR{idProduct}==\"4107\", ENV{ID_SMARTCARD_READER}=\"1\", ENV{ID_SMARTCARD_READER_DRIVER}=\"gnupg\", TAG+=\"uaccess\"
## Nitrokey Pro
ATTR{idVendor}==\"20a0\", ATTR{idProduct}==\"4108\", ENV{ID_SMARTCARD_READER}=\"1\", ENV{ID_SMARTCARD_READER_DRIVER}=\"gnupg\", TAG+=\"uaccess\"
## Nitrokey Pro Bootloader
ATTRS{idVendor}==\"20a0\", ATTRS{idProduct}==\"42b4\", TAG+=\"uaccess\"
## Nitrokey Storage
ATTR{idVendor}==\"20a0\", ATTR{idProduct}==\"4109\", ENV{ID_SMARTCARD_READER}=\"1\", ENV{ID_SMARTCARD_READER_DRIVER}=\"gnupg\", TAG+=\"uaccess\"
## Nitrokey Start
ATTR{idVendor}==\"20a0\", ATTR{idProduct}==\"4211\", ENV{ID_SMARTCARD_READER}=\"1\", ENV{ID_SMARTCARD_READER_DRIVER}=\"gnupg\", TAG+=\"uaccess\"
## Nitrokey HSM
ATTR{idVendor}==\"20a0\", ATTR{idProduct}==\"4230\", ENV{ID_SMARTCARD_READER}=\"1\", ENV{ID_SMARTCARD_READER_DRIVER}=\"gnupg\", TAG+=\"uaccess\"

LABEL=\"gnupg_rules_end\"


# Nitrokey Storage dev Entry
KERNEL==\"sd?1\", ATTRS{idVendor}==\"20a0\", ATTRS{idProduct}==\"4109\", SYMLINK+=\"nitrospace\""))

(define (nitro-key-service)
  (udev-rules-service 'nitro %nitro-key-udev-rule #:groups '("plugdev")))

;;;
;;; YubiKey definitions
;;; https://wiki.archlinux.org/title/YubiKey#YubiKey_not_acting_as_HID_device
;;; DO NOTE: This rule is very similar to 1st of Nitrokey
;;;

(define %yubikey-udev-rule
  (udev-rule "10-security-key.rules"
   "KERNEL==\"hidraw*\", SUBSYSTEM==\"hidraw\", MODE=\"0664\", GROUP=\"plugdev\", ATTRS{idVendor}==\"2581\", ATTRS{idProduct}==\"f1d0\""))

(define (yubikey-service)
  (udev-rules-service 'yubikey %yubikey-udev-rule #:groups '("plugdev")))

(define %coinkite-udev-rule
  (udev-rule "51-coinkite.rules"
   "SUBSYSTEM==\"usb\", MODE=\"0666\", GROUP=\"plugdev\", ATTRS{idVendor}==\"d13e\", ATTRS{idProduct}==\"cc10\"
KERNEL==\"hidraw*\", SUBSYSTEM==\"hidraw\", MODE=\"0666\", GROUP=\"plugdev\", ATTRS{idVendor}==\"d13e\", ATTRS{idProduct}==\"cc10\""))

(define (coinkite-service)
  (udev-rules-service 'coinkite %coinkite-udev-rule #:groups '("plugdev")))