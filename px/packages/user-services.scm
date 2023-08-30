(define-module (px packages user-services)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages sync)
  #:use-module (px packages accounts)
  #:use-module (px packages contacts-calendar)
  #:use-module (px packages desktop-tools)
  #:use-module (px packages events)
  #:use-module (px packages hub)
  #:use-module (px packages mastodon)
  #:use-module (px packages secret)
  #:use-module (px packages time-tracking)
  #:use-module (px packages settings)
  #:use-module ((guix licenses) #:prefix license:))


(define-public px-user-services
  (package
    (name "px-user-services")
    (version "2.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256
          (base32 "0mrqdgcm7z20inf6qrq8agliidn13327gihjassqdlcr9k7fkq1y"))))
    (build-system trivial-build-system)
    (arguments
      `(#:modules ((guix build utils))
        #:builder 
        (begin
          (use-modules (guix build utils))
	        (let* ((out (assoc-ref %outputs "out"))
	               (src (assoc-ref %build-inputs "source")))
	          (setenv "PATH"
		          (string-append
		            (assoc-ref %build-inputs "tar") "/bin" ":"
		            (assoc-ref %build-inputs "gzip") "/bin" ":"))
            (invoke "tar" "xvf" src)
            (chdir ,name)
	          (mkdir-p (string-append out "/etc/xdg/autostart"))
	          (copy-recursively "etc/xdg/autostart"
			                        (string-append out "/etc/xdg/autostart"))
	          (mkdir-p (string-append out "/etc/px/services"))
	          (copy-recursively "etc/px/services"
			                        (string-append out "/etc/px/services"))
	          (substitute* (string-append out "/etc/xdg/autostart/user-services.desktop")
		          (("Exec=/etc/px/services/start.sh")
			         (string-append "Exec=" out "/etc/px/services/start.sh")))
            #t))))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (propagated-inputs
      `(
        ; ("px-secret-service"    ,px-secret-service)
        ; ("px-events-service"    ,px-events-service)
        ; ("px-accounts-service"  ,px-accounts-service)
        ; ("px-contacts-calendar" ,px-contacts-calendar)
        ; ("px-settings-service"  ,px-settings-service)
        ; ("px-mastodon-service"  ,px-mastodon-service)
        ; ("px-hub-service"       ,px-hub-service)
        ; ("px-time-tracking"     ,px-time-tracking)
        ("mcron" ,mcron)
      	("syncthing" ,syncthing)
	      ("syncthingtray" ,syncthingtray)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX user services execution package")
    (description "Run user-level services for PantherX desktop")
    (license license:expat)))

(define-public px-user-services-gtk
  (package
    (name "px-user-services")
    (version "2.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://source.pantherx.org/" name "_v" version ".tgz"))
        (sha256
          (base32 "0mrqdgcm7z20inf6qrq8agliidn13327gihjassqdlcr9k7fkq1y"))))
    (build-system trivial-build-system)
    (arguments
      `(#:modules ((guix build utils))
        #:builder 
        (begin
          (use-modules (guix build utils))
	        (let* ((out (assoc-ref %outputs "out"))
	               (src (assoc-ref %build-inputs "source")))
	          (setenv "PATH"
		          (string-append
		            (assoc-ref %build-inputs "tar") "/bin" ":"
		            (assoc-ref %build-inputs "gzip") "/bin" ":"))
            (invoke "tar" "xvf" src)
            (chdir ,name)
	          (mkdir-p (string-append out "/etc/xdg/autostart"))
	          (copy-recursively "etc/xdg/autostart"
			                        (string-append out "/etc/xdg/autostart"))
	          (mkdir-p (string-append out "/etc/px/services"))
	          (copy-recursively "etc/px/services"
			                        (string-append out "/etc/px/services"))
	          (substitute* (string-append out "/etc/xdg/autostart/user-services.desktop")
		          (("Exec=/etc/px/services/start.sh")
			         (string-append "Exec=" out "/etc/px/services/start.sh")))
            #t))))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (propagated-inputs
      `(
        ; ("px-secret-service"    ,px-secret-service)
        ; ("px-events-service"    ,px-events-service)
        ; ("px-accounts-service"  ,px-accounts-service)
        ; ("px-contacts-calendar" ,px-contacts-calendar)
        ; ("px-settings-service"  ,px-settings-service)
        ; ("px-mastodon-service"  ,px-mastodon-service)
        ; ("px-hub-service"       ,px-hub-service)
        ; ("px-time-tracking"     ,px-time-tracking)
        ("mcron" ,mcron)
      	("syncthing" ,syncthing)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX user services execution package")
    (description "Run user-level services for PantherX desktop")
    (license license:expat)))