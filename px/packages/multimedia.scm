(define-module (px packages multimedia)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages music)
  #:use-module (gnu packages video))

(define-public px-music-player
  (package
    (inherit strawberry)
    (name "px-music-player")
    (arguments
     (substitute-keyword-arguments (package-arguments strawberry)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-app-name
             (lambda _
               (substitute* '("dist/unix/org.strawberrymusicplayer.strawberry.desktop")
                 (("Name=Strawberry\n")
                  "Name=Music\n")) #t))))))))

(define-public px-video-player
  (package
    (inherit mpv)
    (name "px-video-player")
    (arguments
     (substitute-keyword-arguments (package-arguments mpv)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "etc/mpv.desktop"
                 (("mpv Media Player")
                  "Px Video Player"))))))))))

