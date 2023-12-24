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
  #:use-module (nongnu packages compression)
  #:use-module (nongnu packages mozilla)
  #:use-module (px packages accounts)
  #:use-module (px packages atril-thumbnailer)
  #:use-module (px packages backup)
  #:use-module (px packages common)
  #:use-module (px packages contacts-calendar)
  #:use-module (px packages desktop-tools)
  #:use-module (px packages document)
  #:use-module (px packages hub)
  #:use-module (px packages kde-frameworks)
  #:use-module (px packages library)
  #:use-module (px packages matrix-client)
  #:use-module (px packages package-management)
  #:use-module (px packages px-themes)
  #:use-module (px packages settings)
  #:use-module (px packages setup)
  #:use-module (px packages software)
  #:use-module (px packages images)
  #:use-module (px packages themes)
  #:use-module (px packages backup)
  #:use-module (px packages user-services)
  #:use-module (px packages wiki)
  #:use-module (px packages device)
  #:use-module (px packages time-tracking)
  #:use-module (srfi srfi-1)
  #:export (%common-desktop-applications 
            %gtk-desktop-applications
            %qt-desktop-applications))


(define-public px-terminal-launcher
  (package
    (name "px-terminal-launcher")
    (version "v0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_" version
                           ".tgz"))
       (sha256
        (base32 "0fqlzbipnddn9cd0amjn9py7qzwnk5ir674lshpf7rsnv7fasalw"))))
    (build-system qt-build-system)
    (native-inputs (list qttools-5 pkg-config extra-cmake-modules))
    (inputs (list capnproto-0.9
                  qtbase-5
                  qtsvg-5
                  qtcharts
                  px-auth-library-cpp
                  networkmanager-qt))
    (propagated-inputs (list px-icons))
    (home-page "https://www.pantherx.dev")
    (synopsis "PantherX Terminal Launcher")
    (description "PantherX Terminal Launcher")
    (license license:expat)))

;;
;; PantherX OS Desktop default Applications and Services
;;

(define %common-desktop-applications
  (list ;; Default applications and so on...
        ;; px-contacts
        ;; px-backup
        ;; px-hub-gui
        ;; px-software
        ;; px-software-assets-meta

        ;; Browser
        firefox

        ;; Connectivity
        ;; qbittorrent

        ;; Office
        libreoffice
        aspell
        aspell-dict-en
        aspell-dict-de
        aspell-dict-uk

        ;; Look and Feel
        px-sddm-theme ;; Login theme
        xcursor-themes
        gnome-themes-standard
        font-liberation
        font-adobe-source-sans-pro
        font-adobe-source-code-pro
        font-cns11643-swjz ;?
        font-wqy-zenhei ;?
        font-ibm-plex
        font-vazir
        font-openmoji

        ;; WIP
        ;; lxqt-arc-dark-theme
        
        ;; Utils
        ;; Userspace virtual file system for GIO
        gvfs
        print-manager

        ;; U2F
        pam-u2f
        libu2f-host
        libu2f-server

        ;; Compression
        compression:zip
        compression:unzip
        unrar

        ;; Command line utils
        curl
        neofetch
        wget
        xrandr

        ;; Secrets
        ;; Displaying certificates and accessing key stores
        gcr
        gnome-keyring
        ;; seahorse
        
        ;; Bluetooth
        blueman))

(define %gtk-desktop-applications
  (list

        ;; Excludes syncthingtray
        ;; Does not work on Gnome wayland
        ;; px-user-services-gtk
        syncthing-gtk

        ;; PGP
        seahorse))

(define %qt-desktop-applications
  (list
   ;; Primarily for LXQt
	paper-icon-theme
	sddm-darkine-theme
    breeze-gtk
   

   ;; Includes syncthingtray (QT)
   ;; px-user-services

   ;; Default applications and so on...
   ;; px-first-login-welcome-screen
   ;; px-desktop-wiki
   lxqt-archiver
   ;; px-settings-ui

   ;; Office
   speedcrunch

   ;; Multimedia
   qimgv
   strawberry
   mpv

   ;; Connectivity
   ;; This package contains a systray applet for NetworkManager
   ;; Does not work on Gnome wayland
   network-manager-applet
   featherpad
   qpdfview

   ;; Utils
   flameshot
   pinentry-qt
   lxmenu-data

   albert-launcher

   ;; PGP
   kleopatra

   ;; Clipboard manager
   copyq))

   ;; Account Service Plugins
   ;; px-accounts-service-plugin-etesync  ;; TODO: uncomment whenever we had a working package for `px-contact-calendar`
   ;; px-accounts-service-plugin-activity-watch
   ;; px-accounts-service-plugin-claws-mail
   ;; px-accounts-service-plugin-github
   ;; px-accounts-service-plugin-gitlab
   ;; px-accounts-service-plugin-oauth2-github
   ;; px-accounts-service-plugin-oauth2-mastodon
   ;; px-accounts-service-plugin-oauth2-google
   ;; px-accounts-service-providers-mail
   ;; px-accounts-service-plugin-imap
   ;; px-accounts-service-plugin-maestral
   ;; px-accounts-service-plugin-smtp
   ;; px-accounts-service-plugin-carddav
   ;; px-accounts-service-plugin-s3
   ;; px-accounts-service-plugin-backup-local
   ;; px-accounts-service-plugin-etherscan
   ;; px-accounts-service-plugin-blockio
   ;; px-accounts-service-plugin-cryptocurrency
   ;; px-accounts-service-plugin-discourse
   
   ;; Hub Service Plugins
   ;; px-hub-service-plugin-claws-mail
   ;; px-hub-service-plugin-github
   ;; px-hub-service-plugin-gitlab
   ;; px-hub-service-plugin-discourse
   ;; px-hub-service-plugin-mastodon
   
   ;; Time Tracking Plugins
   ;; px-time-tracking-plugin-gitlab
   
   ;; Settings Service Plugins
   ;; px-settings-service-plugin-accounts
   ;; px-settings-service-plugin-backup
   ;; px-settings-service-plugin-desktop-search
   ;; px-settings-service-plugin-maintenance
   ;; px-settings-service-plugin-software
   ;; px-settings-service-plugin-theme
   ;; px-settings-service-plugin-theme-dark-bright))

; (define %pantherx-desktop-i3
;   (list i3-wm i3lock i3lock-fancy i3status
;         dmenu i3blocks))
