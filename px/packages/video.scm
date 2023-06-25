
(define-module (px packages video)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system waf)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))


(define-public mpv-0.34
  (package
    (name "mpv")
    (version "0.34.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mpv-player/mpv")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "12qxwm1ww5vhjddl8yvj1xa0n1fi9z3lmzwhaiday2v59ca0qgsk"))))
    (build-system waf-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-file-names
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "player/lua/ytdl_hook.lua"
		(("\"yt-dlp\",")
                 (string-append
                  "\"" (search-input-file inputs "bin/yt-dlp") "\",")))))
          (add-before 'configure 'build-reproducibly
            (lambda _
              ;; Somewhere in the build system library dependencies are enumerated
              ;; and passed as linker flags, but the order in which they are added
              ;; varies.  See <https://github.com/mpv-player/mpv/issues/7855>.
              ;; Set PYTHONHASHSEED as a workaround for deterministic results.
              (setenv "PYTHONHASHSEED" "1")))
          (add-before 'configure 'set-up-waf
            (lambda* (#:key inputs #:allow-other-keys)
              (copy-file (search-input-file inputs "bin/waf") "waf")
              (setenv "CC" #$(cc-for-target)))))
      #:configure-flags
      #~(list "--enable-libmpv-shared"
              "--enable-cdda"
              "--enable-dvdnav"
              "--disable-build-date")
      ;; No check function defined.
      #:tests? #f))
    (native-inputs
     (list perl ; for zsh completion file
           pkg-config python-docutils))
    ;; Missing features: libguess, V4L2.
    (inputs
     (list alsa-lib
           enca
           ffmpeg-5
           jack-1
           ladspa
           lcms
           libass
           libbluray
           libcaca
           libbs2b
           libcdio-paranoia
           libdvdread
           libdvdnav
           libjpeg-turbo
           libva
           libvdpau
           libx11
           libxext
           libxkbcommon
           libxinerama
           libxrandr
           libxscrnsaver
           libxv
           ;; XXX: lua > 5.2 is not currently supported; see
           ;; waftools/checks/custom.py
           lua-5.2
           mesa
           mpg123
           pulseaudio
           python-waf
           rsound
           shaderc
           vulkan-headers
           vulkan-loader
           wayland
           wayland-protocols
           yt-dlp
           zlib))
    (home-page "https://mpv.io/")
    (synopsis "Audio and video player")
    (description "MPV is a general-purpose audio and video player.  It is a
fork of mplayer2 and MPlayer.  It shares some features with the former
projects while introducing many more.")
    (license license:gpl2+)))

