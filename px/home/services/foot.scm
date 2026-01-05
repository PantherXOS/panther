;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>
;;;
;;; Inspired by foot service implementations from:
;;; - https://git.sr.ht/~whereiseveryone/guixrus (guixrus/home/services/foot.scm)
;;; - https://git.sr.ht/~plattfot/plt (plt/home/foot.scm)

(define-module (px home services foot)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages terminals)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:export (home-foot-server-configuration
            home-foot-server-configuration?
            home-foot-server-service-type))

;;;
;;; Foot Server - Wayland terminal emulator server
;;;

(define-maybe/no-serialization string)

(define-configuration/no-serialization home-foot-server-configuration
  (foot
   (package foot)
   "The foot package to use.")

  (config-file
   (maybe-string)
   "Path to foot configuration file.  When unspecified, foot will use its
default configuration from @file{~/.config/foot/foot.ini} if it exists.")

  (hold?
   (boolean #f)
   "Remain open after child process exits."))

(define (home-foot-server-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Run foot terminal emulator in server mode.")
    (provision '(foot-server))
    (requirement '())
    (modules '((shepherd support)))      ;for '%user-log-dir'
    (start #~(make-forkexec-constructor
              (list #$(file-append (home-foot-server-configuration-foot config)
                                  "/bin/foot")
                    "--server"
                    #$@(if (home-foot-server-configuration-hold? config)
                           '("--hold")
                           '())
                    #$@(if (maybe-value-set?
                            (home-foot-server-configuration-config-file config))
                           (list "--config"
                                 (home-foot-server-configuration-config-file config))
                           '()))
              #:log-file
              (string-append %user-log-dir "/foot-server.log")))
    (stop #~(make-kill-destructor)))))

(define (home-foot-server-profile config)
  (list (home-foot-server-configuration-foot config)))

(define home-foot-server-service-type
  (service-type
   (name 'home-foot-server)
   (extensions
    (list (service-extension home-shepherd-service-type
                            home-foot-server-shepherd-service)
          (service-extension home-profile-service-type
                            home-foot-server-profile)))
   (default-value (home-foot-server-configuration))
   (description
    "Run foot terminal emulator in server mode.  The foot server runs as a
background daemon that can be connected to using @command{footclient}.  This
reduces startup time and memory usage when opening multiple terminal instances.

The server listens on a Unix socket at @env{$XDG_RUNTIME_DIR/foot-$WAYLAND_DISPLAY.sock}.
New terminal windows can be spawned using @command{footclient}.")))
