;;; Desktop related packages Module for PantherX
;;; Reza Alizadeh Majd <r.majd@pantherc.org>
;;; Franz Geffke <franz@pantherx.org>

(define-module (px packages desktop)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu system)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bittorrent)
  #:use-module ((gnu packages compression)
                #:prefix compression:)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jami)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages kde-utils)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages lxde)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages music)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages xdisorg)

  #:use-module (px packages accounts)
  #:use-module (px packages atril-thumbnailer)
  #:use-module (px packages backup)
  #:use-module (px packages common)
  #:use-module (px packages desktop-tools)
  #:use-module (px packages document)
  #:use-module (px packages hub)
  #:use-module (px packages library)
  #:use-module (px packages matrix-client)
  #:use-module (px packages package-management)
  #:use-module (px packages settings)
  #:use-module (px packages setup)
  #:use-module (px packages images)
  #:use-module (px packages themes)
  #:use-module (px packages device)

  #:use-module (nongnu packages compression)
  #:use-module (nongnu packages mozilla)
  #:use-module (srfi srfi-1)
  #:export (%minimal-desktop-applications
            %common-desktop-applications 
            %gtk-desktop-applications
            %qt-desktop-applications))

;;
;; PantherX OS Desktop default Applications and Services
;;

;; Stuff for every desktop; QT / GTK on X / Wayland
(define %minimal-desktop-applications
  (list aspell
        aspell-dict-en

        font-liberation
        font-adobe-source-sans-pro
        font-adobe-source-code-pro
        font-wqy-zenhei
        font-ibm-plex
        font-vazir
        font-openmoji

        gvfs

        pam-u2f
        libu2f-host
        libu2f-server

        compression:zip
        compression:unzip
        unrar

        curl
        neofetch
        wget

        blueman))


(define %common-desktop-applications
  (list libreoffice
  
        px-sddm-theme
        xcursor-themes
        gnome-themes-standard
        sddm-darkine-theme
        paper-icon-theme
        breeze-gtk

        ;; Keychain
        gcr
        gnome-keyring

        ;; Printing
        print-manager

        network-manager-applet
  ))

;; GTK-specific
(define %gtk-desktop-applications
  (list syncthing-gtk

        ;; PGP
        seahorse))

;; QT-specific
(define %qt-desktop-applications
  (list syncthingtray
        speedcrunch

        qimgv
        ;; strawberry
        mpv

        featherpad
        qpdfview

        lxqt-archiver
        flameshot
        pinentry-qt
        lxmenu-data

        albert-launcher
        kleopatra

        ;; Clipboard manager
        copyq))
