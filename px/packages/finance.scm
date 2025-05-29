;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <m@f-a.nz>

(define-module (px packages finance)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages serialization)
  #:use-module (px packages python-xyz))

(define-public electrum-cc
  (package
    (inherit electrum)
    (name "electrum-cc")
    (inputs
     (list libsecp256k1
           python-aiohttp
           python-aiohttp-socks
           python-aiorpcx
           python-attrs
           python-bitstring
           python-btchip-python
           python-certifi
           python-cryptography
           python-dnspython
           python-hidapi
           python-ledgerblue
           python-protobuf
           python-pyqt
           python-qdarkstyle
           python-qrcode
           zbar
           python-ckcc-protocol
           python-cbor
           python-pyaes))))