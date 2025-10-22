;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px home services darkman)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:autoload   (gnu packages glib) (dbus)
  #:use-module (gnu home services desktop)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (px packages tools)
  #:export (home-darkman-configuration
            home-darkman-configuration?
            home-darkman-service-type))

;;;
;;; Darkman - Dark/Light mode transition framework
;;;

(define (inexact-number? n)
  (and (number? n) (inexact? n)))

(define-maybe inexact-number)

(define (serialize-yaml-boolean field value)
  (string-append (symbol->string field) ": "
                 (if value "true" "false") "\n"))

(define (serialize-yaml-number field value)
  (string-append (symbol->string field) ": "
                 (number->string value) "\n"))

(define (serialize-yaml-string field value)
  (string-append (symbol->string field) ": "
                 value "\n"))

(define-configuration/no-serialization home-darkman-configuration
  (darkman
   (package darkman)
   "The darkman package to use.")

  (latitude
   maybe-inexact-number
   "Geographic latitude for sunrise/sunset calculations.  When unspecified,
geoclue2 will be used if @code{use-geoclue?} is enabled.")

  (longitude
   maybe-inexact-number
   "Geographic longitude for sunrise/sunset calculations.  When unspecified,
geoclue2 will be used if @code{use-geoclue?} is enabled.")

  (use-geoclue?
   (boolean #t)
   "Whether to use geoclue2 for automatic location detection.  When enabled,
the system geoclue service must be running.")

  (dbus-server?
   (boolean #t)
   "Whether to expose the current mode via darkman's D-Bus API.")

  (portal?
   (boolean #t)
   "Whether to expose the current mode via the XDG settings portal.")

  (extra-config
   (string "")
   "Extra configuration content to append to the YAML configuration file."))

(define (home-darkman-config-file config)
  "Generate the darkman configuration file in YAML format."
  (mixed-text-file
   "darkman-config.yaml"
   (if (maybe-value-set? (home-darkman-configuration-latitude config))
       (serialize-yaml-number 'lat
                             (home-darkman-configuration-latitude config))
       "")
   (if (maybe-value-set? (home-darkman-configuration-longitude config))
       (serialize-yaml-number 'lng
                             (home-darkman-configuration-longitude config))
       "")
   (serialize-yaml-boolean 'usegeoclue
                          (home-darkman-configuration-use-geoclue? config))
   (serialize-yaml-boolean 'dbusserver
                          (home-darkman-configuration-dbus-server? config))
   (serialize-yaml-boolean 'portal
                          (home-darkman-configuration-portal? config))
   (home-darkman-configuration-extra-config config)))

(define (home-darkman-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Run darkman daemon for dark/light mode transitions.")
    (provision '(darkman))
    (requirement '(dbus))
    (modules '((shepherd support)))      ;for '%user-log-dir'
    (start #~(make-forkexec-constructor
              (list #$(file-append (home-darkman-configuration-darkman config)
                                  "/bin/darkman")
                    "run")
              #:log-file
              (string-append %user-log-dir "/darkman.log")))
    (stop #~(make-kill-destructor)))))

(define (home-darkman-xdg-configuration-files config)
  (list `("darkman/config.yaml"
          ,(home-darkman-config-file config))))

(define (home-darkman-profile config)
  (list (home-darkman-configuration-darkman config)))

(define home-darkman-service-type
  (service-type
   (name 'home-darkman)
   (extensions
    (list (service-extension home-shepherd-service-type
                            home-darkman-shepherd-service)
          (service-extension home-xdg-configuration-files-service-type
                            home-darkman-xdg-configuration-files)
          (service-extension home-profile-service-type
                            home-darkman-profile)
          ;; Ensure D-Bus is available
          (service-extension home-dbus-service-type
                            (const #t))))
   (default-value (home-darkman-configuration))
   (description
    "Run darkman, a framework for managing dark mode and light mode transitions.
Darkman runs as a background service and automatically switches between dark and
light themes based on the time of day, using geoclue2 to determine sunrise and
sunset times (or manual coordinates if specified).

Scripts can be placed in @file{~/.local/share/dark-mode.d/} and
@file{~/.local/share/light-mode.d/} (or @env{$XDG_DATA_HOME}) to execute
custom actions when switching themes.  Scripts must have the executable bit set.")))
