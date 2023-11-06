(define-module (px packages lxqt-dev)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages lxde)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (px packages themes)
  #:use-module (srfi srfi-1))

(define-public lxqt-config-dev
  (let ((rep_name "lxqt-config")
        (commit "5365be813f9c43e8fa6ed6a42f30744cf043980a")
        (revision "0"))
    (package
      (inherit lxqt-config)
      (name "lxqt-config-dev")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url (string-append "https://github.com/lxqt/" rep_name ".git"))
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wx186nq7hihywm7dn3w4p31zyplg3kr11aav4maivv14dw9k17k"))))
      (inputs `(("eudev" ,eudev)
                ("kwindowsystem" ,kwindowsystem)
                ("libkscreen" ,libkscreen)
                ("liblxqt" ,liblxqt)
                ("libqtxdg" ,libqtxdg)
                ("libxcursor" ,libxcursor)
                ("libxi" ,libxi)
                ("perl" ,perl)
                ("qtbase" ,qtbase-5)
                ("qtsvg" ,qtsvg-5)
                ("qtx11extras" ,qtx11extras)
                ("solid" ,solid)
                ("xf86-input-libinput" ,xf86-input-libinput)
                ("xkeyboard-config" ,xkeyboard-config)
                ("zlib" ,zlib)))
      (native-inputs (list pkg-config lxqt-build-tools qttools-5))
      (propagated-inputs `(;; temporary workaround for lxqt-config-monitor segmentation fault
                           
                           ;; more in https://git.pantherx.org/development/guix-pantherx/-/issues/67
                           ("libkscreen" ,libkscreen)
                           ("setxkbmap" ,setxkbmap)))
      (arguments
       '(#:tests? #f ;no tests
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'patch-source
                      (lambda _
                        (substitute* '("src/translations/lxqt-config.desktop.yaml")
                          (("LXQt Configuration Center")
                           "All Settings"))
                        (substitute* '("src/lxqt-config.menu")
                          (("<Name>System settings</Name>")
                           (string-append "<Name>PantherX settings</Name>"
                            "
	<Directory>lxqt-settings-pantherx.directory</Directory>"
                            "\n\t<Include>"
                            "\n\t\t<And>"
                            "\n\t\t\t<Category>Settings</Category>"
                            "\n\t\t\t<Category>PantherX</Category>"
                            "\n\t\t</And>"
                            "\n\t</Include>"
                            "\n</Menu>"
                            "\n\n<Menu>"
                            "\n\t<Name>System settings</Name>"))) #t))
                    (add-after 'unpack 'set-xkeyboard-config-file-name
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; Set the file name to xkeyboard-config.
                        (let ((xkb (assoc-ref inputs "xkeyboard-config")))
                          (substitute* "lxqt-config-input/keyboardlayoutconfig.h"
                            (("/usr/share/X11/xkb/rules/base.lst")
                             (string-append xkb
                                            "/share/X11/xkb/rules/base.lst")))
                          #t)))))))))
