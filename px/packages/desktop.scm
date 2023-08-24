;;; Desktop related packages Module for PantherX
;;;
;;; Reza Alizadeh Majd <r.majd@pantherc.org>
;;; Franz Geffke <franz@pantherx.org>
;;;

(define-module (px packages desktop)
  #:use-module ((guix licenses) #:prefix license:)
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
  #:use-module ((gnu packages compression) #:prefix compression:)
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
  #:use-module (gnu packages xdisorg) ;; copyq
  #:use-module (nongnu packages compression)
  #:use-module (nongnu packages mozilla)
  #:use-module (px packages accounts)
  #:use-module (px packages atril-thumbnailer)
  #:use-module (px packages backup) ;; px-backup
  #:use-module (px packages common) ;; capnproto
  #:use-module (px packages contacts-calendar) ;; px-contacts
  #:use-module (px packages desktop-tools) ;; px-about
  #:use-module (px packages document)
  #:use-module (px packages hub)
  #:use-module (px packages kde-frameworks)
  #:use-module (px packages library)
  #:use-module (px packages lxqt-dev)
  #:use-module (px packages matrix-client)
  #:use-module (px packages multimedia)
  #:use-module (px packages package-management)
  #:use-module (px packages pantherx-panel)
  #:use-module (px packages px-themes)
  #:use-module (px packages settings)
  #:use-module (px packages setup)
  #:use-module (px packages software)
  #:use-module (px packages images)
  #:use-module (px packages themes)
  #:use-module (px packages backup)
  #:use-module (px packages user-services)
  #:use-module (px packages wiki)
  #:use-module (px packages device) ;; px-remote-access
  #:use-module (px packages time-tracking)
  #:use-module (srfi srfi-1)
  #:export (%common-desktop-applications
            %gtk-desktop-applications
            %qt-desktop-applications
            
            lxqt-modified
            ;; TODO: This should probably go into px/services/desktop
            px-desktop-defaults))

;; Currently only lxqt-modified
(define-public openbox-modified
  (package
   (inherit openbox)
   (name "openbox-modified")
   (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'force-reconfigure
                 ;; This is made necessary by the openbox-python3 patch.
                 (lambda _
                   (delete-file "configure")))
                (add-after 'unpack 'patch-config-file
                 (lambda _
                   (substitute* "data/rc.xml"
                                (("Clearlooks") "Arc-Dark")))))))))

;; Currently only lxqt-modified
(define-public px-file-manager
  (package
   (inherit pcmanfm-qt)
   (name "px-file-manager")
   (source
    (origin
     (inherit (package-source pcmanfm-qt))
     (patches (search-patches "px-file-manager-0001-update-config.patch"))))
   (arguments
    (substitute-keyword-arguments (package-arguments pcmanfm-qt)
        ((#:phases phases)
          #~(modify-phases #$phases
            (add-before 'configure 'patch-settings.conf.in
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((wallpaper (search-input-file inputs
                                  "share/lxqt/wallpapers/pantherx.jpg")))
                          (substitute* "config/pcmanfm-qt/lxqt/settings.conf.in"
                            (("Wallpaper=.*")
                              (string-append "Wallpaper=" wallpaper "\n")))
                          (substitute* (find-files "pcmanfm/translations" "\\.desktop.yaml")  
                              (("PCManFM-Qt File Manager") "File Manager"))
                          (substitute* '("config/pcmanfm-qt/lxqt/settings.conf.in")
                             (("WallpaperMode=stretch") "WallpaperMode=zoom")
                             ;; Patch FONT
                             (("Font=\"Sans Serif,10,-1,5,50,0,0,0,0,0\"") "Font=\"IBM Plex Sans,10,-1,5,50,0,0,0,0,0,Regular\"")
                             ;; Patch DEFAULT APPLICATIONS
                             (("TerminalDirCommand=xterm") "TerminalDirCommand=qterminal")
                             (("TerminalExecCommand=xterm") "TerminalExecCommand=qterminal")
                             ;; Patch TUMBNAILS
                             (("MaxThumbnailFileSize=4096") "MaxThumbnailFileSize=30720"))
                          (substitute* '("config/CMakeLists.txt")
			                       (("\\$\\{CMAKE_INSTALL_DATADIR\\}") "etc/xdg"))
                          )))))))
   (inputs
     (list libfm-qt qtbase-5 qtx11extras px-lxqt-themes))
   (propagated-inputs
    `(("atril-thumbnailer" ,atril-thumbnailer)
      ("ffmpegthumbnailer" ,ffmpegthumbnailer)
      ("freetype" ,freetype)
      ("libgsf" ,libgsf)
      ("tumbler" ,tumbler)))))

(define-public px-terminal-launcher
  (package
    (name "px-terminal-launcher")
    (version "v0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_" version ".tgz"))
        (sha256 (base32 "14b3kn9invpawynn3nxgwvyr1l8k796v3jjcq8rzjmbfc48qpxi6"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "mainwindow.cpp"
               (("matrix-call-auto-accept-load-params.py") 
                (string-append (assoc-ref outputs "out") "/bin/matrix-call-auto-accept-load-params.py")))
             (substitute* "scripts/matrix-call-auto-accept-load-params.py"
               (("matrix-client-call-auto-accept") 
                (string-append (assoc-ref inputs "matrix-client-call-auto-accept") 
                  "/bin/matrix-client-call-auto-accept"))))))))
    (native-inputs
      (list qttools-5
            pkg-config
            extra-cmake-modules)
      )
    (inputs
     (list capnproto-0.9
           qtbase-5
           qtcharts
           px-auth-library-cpp
           matrix-client-call-auto-accept
           networkmanager-qt))
    (propagated-inputs (list px-icons))
    (home-page "https://www.pantherx.dev")
    (synopsis "PantherX Terminal Launcher")
    (description "PantherX Terminal Launcher")
    (license license:expat)))

(define-public lxqt-modified
  (package
   (inherit lxqt)
   (name "lxqt-modified")
   (propagated-inputs
    `(
      ;; Apply Arc-Dark to openbox:
      ("openbox-modified" ,openbox-modified)
      ;; Apply modified menu and others to lxqt-panel:
      ;; TODO: FIX and restore
      ;; ("pantherx-panel" ,pantherx-panel)
      ;; ("libqtxdg" ,libqtxdg)
      ;; Apply default wallpaper and so on to pcmanfm-qt:
      ("px-file-manager" ,px-file-manager)
      ;; Rename QTerminal to Terminal:
      ("px-terminal" ,px-terminal)
      ("px-icons" ,px-icons)
      ("px-lxqt-themes" ,px-lxqt-themes)
      ;; "lxqt-panel"
      ,@(fold alist-delete (package-propagated-inputs lxqt)
              '("lximage-qt" "pcmanfm-qt" "qterminal"
                "lxqt-themes"
                "breeze-icons"))))))

;;
;; Desktop Configuration
;;

;; This goes straight into px/services/desktop
(define-public px-desktop-defaults
  (package
   (name "px-desktop-defaults")
   (version "0.0.47")
   (source 
    (origin
     (method url-fetch)
     (uri (string-append
           "https://source.pantherx.org/px-desktop-defaults_"
           version ".tgz"))
     (sha256 (base32 "1y9wp2d35nrf72bkiv39k17paa6arxp2hpz2102mymj61zb96fvk"))))
   (build-system trivial-build-system)
   (arguments `(
		#:modules ((guix build utils))
			  #:builder 
			  (begin
			    (use-modules (guix build utils))
			    (mkdir %output)
			    (setenv "PATH" (string-append
              (assoc-ref %build-inputs "coreutils") "/bin" ":"
					    (assoc-ref %build-inputs "tar") "/bin" ":"
					    (assoc-ref %build-inputs "gzip") "/bin"))
			    (invoke "tar" "zxvf" (assoc-ref %build-inputs "source"))
					; (chdir (string-append (string-capitalize ,name) "-" ,version))
					; (display (string-append ,name "_" ,version))
					; (chdir (string-append ,name "_" ,version))
			    (let ((source       (assoc-ref %build-inputs "source"))
                (albert       (assoc-ref %build-inputs "albert-launcher"))
                (copyq        (assoc-ref %build-inputs "copyq"))
                (px-first-login-welcome-screen
                              (assoc-ref %build-inputs "px-first-login-welcome-screen"))
                (out          (assoc-ref %outputs "out")))
			      (chdir ,name)
			      (substitute* '("etc/xdg/autostart/lxqt-copyq-autostart.desktop")
					   (("Exec=copyq") (string-append "Exec=" copyq "/bin/copyq")))
			      (substitute* '("etc/xdg/autostart/albert.desktop")
					   (("Exec=albert") (string-append "Exec=" albert "/bin/albert")))
            (substitute* '("etc/xdg/autostart/px-first-login-welcome-screen.desktop")
					   (("Exec=px-first-login-welcome-screen") 
                (string-append "Exec=" px-first-login-welcome-screen "/bin/px-first-login-welcome-screen")))
			      (copy-recursively "." %output)
            (chmod (string-append %output "/etc/px-desktop/scripts/lxqt-switch-desktop.sh" ) #o755)
			      #t))))
   (native-inputs
    `(("coreutils" ,coreutils)
      ("tar" ,tar)
      ("gzip" ,compression:gzip)))
   (propagated-inputs
    `(("albert-launcher" ,albert-launcher)
      ("px-widget-style" ,px-widget-style)
      ("px-icons" ,px-icons)
      ("px-first-login-welcome-screen" ,px-first-login-welcome-screen)
      ("px-openbox-theme" ,px-openbox-theme)
      ("copyq" ,copyq)))
   (home-page "https://www.pantherx.org/")
   (synopsis "PantherX Default Configuration Package")
   (description "Default Configurations for PantherX Desktop")
   (license license:expat)))

;;
;; PantherX OS Desktop default Applications and Services
;;

(define %common-desktop-applications
  (list ; px-backup
        ; px-contacts
        px-first-login-welcome-screen
        ;; Default applications and so on...
        px-desktop-wiki
        px-software
        ; px-hub-gui
        px-software-assets-meta
        
        ;; Browser
        firefox

        ;; Connectivity
        qbittorrent

        ;; Office
        libreoffice
        speedcrunch
        aspell
        aspell-dict-en
        aspell-dict-de ;; :)
        aspell-dict-uk

        ;; Look and Feel
        paper-icon-theme
        sddm-darkine-theme
        px-sddm-theme
        xcursor-themes
        gnome-themes-standard
        font-liberation
        font-adobe-source-sans-pro
        font-adobe-source-code-pro
        breeze-gtk
        font-cns11643-swjz ;; ?
        font-wqy-zenhei ;; ?
        font-ibm-plex
        font-vazir
        font-openmoji
        
        ;; WIP
        ;; lxqt-arc-dark-theme

        ;; Multimedia
        px-image-viewer
        px-music-player
        px-video-player

        ;; Utils
        albert-launcher
        ;; Userspace virtual file system for GIO
        gvfs
        lxmenu-data
        flameshot
        pinentry-qt
        print-manager
        ;; CLipboard manager
        copyq
        ;; PGP
        kleopatra

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

        px-user-services

        ;; Bluetooth
        blueman

        ;; Account Service Plugins
	      ;; px-accounts-service-plugin-etesync  ;; TODO: uncomment whenever we had a working package for `px-contact-calendar`
	      ; px-accounts-service-plugin-activity-watch
        ; px-accounts-service-plugin-claws-mail
        ; px-accounts-service-plugin-github
        ; px-accounts-service-plugin-gitlab
        ; px-accounts-service-plugin-oauth2-github
        ; px-accounts-service-plugin-oauth2-mastodon
        ; px-accounts-service-plugin-oauth2-google
        ; px-accounts-service-providers-mail
        ; px-accounts-service-plugin-imap
        ; px-accounts-service-plugin-maestral
        ; px-accounts-service-plugin-smtp
        ; px-accounts-service-plugin-carddav
        ; px-accounts-service-plugin-s3
        ; px-accounts-service-plugin-backup-local
        ; px-accounts-service-plugin-etherscan
        ; px-accounts-service-plugin-blockio
        ; px-accounts-service-plugin-cryptocurrency
        ; px-accounts-service-plugin-discourse

        ;; Hub Service Plugins
        ;; px-hub-service-plugin-claws-mail
        ; px-hub-service-plugin-github
        ; px-hub-service-plugin-gitlab
        ; px-hub-service-plugin-discourse
        ; px-hub-service-plugin-mastodon

        ;; Time Tracking Plugins
        ; px-time-tracking-plugin-gitlab

        ;; Settings Service Plugins
        ; px-settings-service-plugin-accounts
        px-settings-service-plugin-backup
        px-settings-service-plugin-desktop-search
        px-settings-service-plugin-maintenance
        px-settings-service-plugin-software
        px-settings-service-plugin-theme
        px-settings-service-plugin-theme-dark-bright))

(define %gtk-desktop-applications
  (list ))

(define %qt-desktop-applications
  (list
      px-about
      px-file-archiver
      px-settings-ui
      px-network-manager-applet
      featherpad
      qpdfview))

; (define %pantherx-desktop-i3
;   (list i3-wm i3lock i3lock-fancy i3status
;         dmenu i3blocks))