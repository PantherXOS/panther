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

;; Stuff for every desktop; QT / GTK on X / Wayland
(define %common-desktop-applications
  (list libreoffice
        aspell
        aspell-dict-en
        aspell-dict-de
        aspell-dict-uk

        font-liberation
        font-adobe-source-sans-pro
        font-adobe-source-code-pro
        font-cns11643-swjz ;?
        font-wqy-zenhei ;?
        font-ibm-plex
        font-vazir
        font-openmoji

        gvfs
        print-manager

        pam-u2f
        libu2f-host
        libu2f-server

        compression:zip
        compression:unzip
        unrar

        curl
        neofetch
        wget

        gcr
        gnome-keyring

        blueman))

;; GTK-specific
(define %gtk-desktop-applications
  (list syncthing-gtk

        ;; Look and feel
        px-sddm-theme
        xcursor-themes
        gnome-themes-standard

        ;; PGP
        seahorse))

;; QT-specific
(define %qt-desktop-applications
  (list px-sddm-theme
        xcursor-themes
        gnome-themes-standard
        paper-icon-theme
        sddm-darkine-theme
        breeze-gtk

        speedcrunch

        qimgv
        strawberry
        mpv

        network-manager-applet
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
