;;; Netwrok Inspection tool packages. 
;;; Copyright © 2020 Sina Mahmoodi Khorandi <s.mahmoodi@pantherx.org> 

(define-module (px packages inspection)
			   #:use-module ((guix licenses) #:prefix license:)
			   #:use-module (guix download)
			   #:use-module (guix packages)
			   #:use-module (guix build-system cmake)
			   #:use-module (gnu packages glib)
			   #:use-module (gnu packages web)
			   #:use-module (gnu packages linux)
			   #:use-module (gnu packages curl)
			   #:use-module (gnu packages networking)
			   #:use-module (guix utils)
			   #:use-module (gnu packages pkg-config))


(define-public px-network-inspection
			   (package
				 (name "px-network-inspection")
				 (version "0.0.15")
				 (source
				   (origin
					 (method url-fetch)
					 (uri (string-append
							"https://source.pantherx.org/px-network-inspection_" 
							version 
							".tgz"))
					 (sha256
					   (base32
						 "0fzcy9f61qz0zi8chb6y86qfsjyn510fkxkxcw66rgxji76b1qx1"))))
				 (build-system cmake-build-system)
				 (arguments
				   `(
					 #:tests? #f))
				 ;        #:phases
				 ;          (modify-phases %standard-phases
				 ;            (add-after 'unpack 'fix-source
				 ;              (lambda _ 
				 ;                (chdir "../")
				 ;					    )))))
				 (inputs `(
						   ("glib" ,glib)
						   ("curl" ,curl)
						   ("fping" ,fping)
						   ("json-c" ,json-c)
						   ("libnl" ,libnl)))
				 (native-inputs `(
								  ("pkg-config" ,pkg-config)
								  ))
				;(propagated-inputs `(
				;      		   ("fping" ,fping)))

				 (home-page "https://www.pantherx.org/")
				 (synopsis "PantherX Netwrok Inspection")
				 (description "This package provides network inspection utility tool")
				 (license license:expat)))
