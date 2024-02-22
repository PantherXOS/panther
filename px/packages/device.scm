(define-module (px packages device)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages qt)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages web)
  #:use-module (px packages crates-io)
  #:use-module (px packages common)
  #:use-module (px packages kde-frameworks)
  #:use-module (px packages python-xyz)
  #:use-module (px packages tpm)
  #:use-module (px packages library))

(define-public px-device-identity
  (package
   (name "px-device-identity")
   (version "0.11.1")
   (source
    (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1q595j2aipga92i5dxq680l3bz412dzd44zm3sbf8j79nkh2b8bv"))))
   (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases
	%standard-phases
	(add-after 'install 'wrap-for-openssl-tss2-conf
		   (lambda* (#:key outputs #:allow-other-keys)
		     (let ((out (assoc-ref outputs "out"))
			   (openssl (assoc-ref %build-inputs "openssl"))
			   (tpm2-tss (assoc-ref %build-inputs "tpm2-tss"))
			   (tpm2-tss-engine (assoc-ref %build-inputs
						       "tpm2-tss-engine")))
		       (wrap-program (string-append out "/bin/px-device-identity")
				     `("OPENSSL_CONF" ":" prefix
				       (,(string-append tpm2-tss-engine
							"/etc/openssl-tss2.conf"))))
		       (wrap-program (string-append out "/bin/px-device-identity")
				     `("PATH" ":" prefix
				       (,(string-append tpm2-tss-engine "/bin/"))))
		       (wrap-program (string-append out "/bin/px-device-identity")
				     `("PATH" ":" prefix
				       (,(string-append openssl "/bin/"))))
		       (wrap-program (string-append out "/bin/px-device-identity")
				     `("TPM2TSSENGINE_TCTI" ":" prefix
				       (,(string-append tpm2-tss
							"/lib/libtss2-tcti-device.so:/dev/tpm0"))))
		       (wrap-program (string-append out "/bin/px-device-identity")
				     `("TPM2TOOLS_TCTI" ":" prefix
				       (,(string-append tpm2-tss
							"/lib/libtss2-tcti-device.so:/dev/tpm0"))))
		       #t)))
        (delete 'sanity-check))))
    (inputs `(("openssl" ,openssl-1.1)
              ("tpm2-tss" ,tpm2-tss-openssl-1.1)
              ("tpm2-tss-engine" ,tpm2-tss-engine)
              ("bash-minimal" ,bash-minimal)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs `(("python-requests" ,python-requests)
                         ("python-authlib-0.14.3" ,python-authlib-0.14.3)
                         ("python-pycryptodomex" ,python-pycryptodomex)
                         ("python-pyyaml" ,python-pyyaml)
                         ("python-shortuuid" ,python-shortuuid-v1)
                         ("python-appdirs" ,python-appdirs)
                         ("python-psutil" ,python-psutil)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Device Identity Manager")
    (description
     "Initiates device identity based on RSA or ECC key pair and optionally registers with Central Management.")
    (license license:expat)))

(define-public px-device-identity-service
  (package
   (name "px-device-identity-service")
   (version "0.11.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version
                         ".tgz"))
     (sha256
      (base32 "1is0dcdhm3faf4ib4wpnq77497zynfz4r7lr7d9mfbsgndaxcnbi"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases 
      (modify-phases
       %standard-phases
       (add-after 'install 'wrap-for-openssl-tss2-conf
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (openssl (assoc-ref %build-inputs "openssl"))
			  (tpm2-tss (assoc-ref %build-inputs "tpm2-tss"))
			  (tpm2-tss-engine (assoc-ref %build-inputs "tpm2-tss-engine")))
		      (wrap-program (string-append out "/bin/px-device-identity-service")
				    `("OPENSSL_CONF" ":" prefix
				      (,(string-append tpm2-tss-engine
						       "/etc/openssl-tss2.conf")))
				    `("PATH" ":" prefix
				      (,(string-append tpm2-tss-engine "/bin/") 
				       ,(string-append openssl "/bin/")))
				    `("TPM2TSSENGINE_TCTI" ":" prefix
				      (,(string-append tpm2-tss
						       "/lib/libtss2-tcti-device.so:/dev/tpm0")))
				    `("TPM2TOOLS_TCTI" ":" prefix
				      (,(string-append tpm2-tss
						       "/lib/libtss2-tcti-device.so:/dev/tpm0"))))
		      #t)))
       (delete 'sanity-check))))
   (inputs `(("openssl" ,openssl-1.1)
             ("tpm2-tss" ,tpm2-tss-openssl-1.1)
             ("tpm2-tss-engine" ,tpm2-tss-engine)))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (propagated-inputs `(("px-device-identity" ,px-device-identity)
                        ("python-waitress" ,python-waitress)
                        ("python-requests" ,python-requests)
                        ("python-flask" ,python-flask)
                        ("python-werkzeug" ,python-werkzeug)
                        ("python-pyyaml" ,python-pyyaml)))
   (home-page "https://www.pantherx.org/")
   (synopsis "PantherX Device Identity Service")
   (description "Makes device signing capabilities available
to other applications, without root priviliges.")
   (license license:expat)))

(define-public px-device-runner
  (package
   (name "px-device-runner")
   (version "0.0.14")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version
                         ".tgz"))
     (sha256
      (base32 "0sdyz81z8l0q99r09fymw2v6r9ylc7rkxxdwkbnm6lgch5pib36r"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases 
      (modify-phases
       %standard-phases
       (add-after 'install 'wrap-for-openssl-tss2-conf
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (openssl (assoc-ref %build-inputs "openssl"))
			  (tpm2-tss (assoc-ref %build-inputs "tpm2-tss"))
			  (tpm2-tss-engine (assoc-ref %build-inputs "tpm2-tss-engine")))
		      (wrap-program (string-append out "/bin/px-device-runner")
				    `("OPENSSL_CONF" ":" prefix
				      (,(string-append tpm2-tss-engine
						       "/etc/openssl-tss2.conf")))
				    `("PATH" ":" prefix
				      (,(string-append tpm2-tss-engine "/bin/") 
				       ,(string-append openssl "/bin/")))
				    `("TPM2TSSENGINE_TCTI" ":" prefix
				      (,(string-append tpm2-tss
						       "/lib/libtss2-tcti-device.so:/dev/tpm0")))
				    `("TPM2TOOLS_TCTI" ":" prefix
				      (,(string-append tpm2-tss
						       "/lib/libtss2-tcti-device.so:/dev/tpm0"))))
		      #t)))
       (delete 'sanity-check))))
   (inputs `(("openssl" ,openssl-1.1)
             ("tpm2-tss" ,tpm2-tss-openssl-1.1)
             ("tpm2-tss-engine" ,tpm2-tss-engine)))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (propagated-inputs `(("px-device-identity" ,px-device-identity)
                        ("python-requests" ,python-requests)))
   (home-page "https://www.pantherx.org/")
   (synopsis "PantherX Device Runner")
   (description
    "Downloads administrative jobs from Central Management to run on local device.")
   (license license:expat)))

(define-public px-user-identity-service
  (package
   (name "px-user-identity-service")
   (version "0.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version
                         ".tgz"))
     (sha256
      (base32 "0vhb5f4klvbdf802b3i4mli3926ny4pxcnbhif8mn56dnj8lgf84"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases 
      (modify-phases
       %standard-phases
       (add-after 'install 'wrap-for-openssl-tss2-conf
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (openssl (assoc-ref %build-inputs "openssl"))
			  (tpm2-tss (assoc-ref %build-inputs "tpm2-tss"))
			  (tpm2-tss-engine (assoc-ref %build-inputs "tpm2-tss-engine")))
		      (wrap-program (string-append out "/bin/px-user-identity-service")
				    `("OPENSSL_CONF" ":" prefix
				      (,(string-append tpm2-tss-engine
						       "/etc/openssl-tss2.conf")))
				    `("PATH" ":" prefix
				      (,(string-append tpm2-tss-engine "/bin/") 
				       ,(string-append openssl "/bin/")))
				    `("TPM2TSSENGINE_TCTI" ":" prefix
				      (,(string-append tpm2-tss
						       "/lib/libtss2-tcti-device.so:/dev/tpm0")))
				    `("TPM2TOOLS_TCTI" ":" prefix
				      (,(string-append tpm2-tss
						       "/lib/libtss2-tcti-device.so:/dev/tpm0"))))
		      #t)))
       (delete 'sanity-check))))
   (inputs `(("openssl" ,openssl-1.1)
             ("tpm2-tss" ,tpm2-tss-openssl-1.1)
             ("tpm2-tss-engine" ,tpm2-tss-engine)))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (propagated-inputs `(("px-device-identity" ,px-device-identity)
                        ("python-waitress" ,python-waitress)
                        ("python-requests" ,python-requests)
                        ("python-flask" ,python-flask)
                        ("python-werkzeug" ,python-werkzeug)))
   (home-page "https://www.pantherx.org/")
   (synopsis "PantherX User Identity Service REST API")
   (description
    "User Identity API to support QR and BC login with device signature.")
   (license license:expat)))

(define-public px-file-upload-cli
  (package
    (name "px-file-upload-cli")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "-" version
                           ".crate"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10p0zxf576v28hgzbby45q9xlj3bd0314h9zaikallc1l30qb2l9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-2)
                       ("rust-configparser" ,rust-configparser-2)
                       ("rust-fern" ,rust-fern-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.8)
                       ("rust-syslog" ,rust-syslog-4)
                       ;; TODO: restore back to upstream version once the following patch is merged upstream:
                       ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=60174
                       ("rust-tokio" ,rust-tokio-1-patched)
                       ("rust-tokio-util" ,rust-tokio-util-0.6)
                       ("rust-uuid" ,rust-uuid-0.8))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap-for-openssl-tss2-conf
                    (lambda* (#:key outputs inputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (openssl (assoc-ref inputs "openssl"))
                            (tpm2-tss (assoc-ref inputs "tpm2-tss"))
                            (tpm2-tss-engine (assoc-ref inputs
                                                        "tpm2-tss-engine")))
                        (wrap-program (string-append out
                                       "/bin/px-file-upload-cli")
                          `("OPENSSL_CONF" ":" prefix
                            (,(string-append tpm2-tss-engine
                                             "/etc/openssl-tss2.conf")))
                          `("PATH" ":" prefix
                            (,(string-append tpm2-tss-engine "/bin/") ,(string-append
                                                                        openssl
                                                                        "/bin/")))
                          `("TPM2TSSENGINE_TCTI" ":" prefix
                            (,(string-append tpm2-tss
                               "/lib/libtss2-tcti-device.so:/dev/tpm0")))
                          `("TPM2TOOLS_TCTI" ":" prefix
                            (,(string-append tpm2-tss
                               "/lib/libtss2-tcti-device.so:/dev/tpm0")))) #t))))))
    (inputs `(("openssl" ,openssl-1.1)
              ("tpm2-tss" ,tpm2-tss-openssl-1.1)
              ("tpm2-tss-engine" ,tpm2-tss-engine)
              ("bash-minimal" ,bash-minimal)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs `(("px-device-identity" ,px-device-identity)))
    (home-page "https://pantherx.org")
    (synopsis "PantherX Uploader Utility")
    (description
     "Uploader cli application which reads a
configuration file from commandline args and upload results to the server")
    (license license:expat)))

(define-public px-device-backup
  (package
   (name "px-device-backup")
   (version "0.0.5")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version
                         ".tgz"))
     (sha256
      (base32 "1wcqvwwcv5x98haj956gmwgv977h41pwh42qvhp6z0v3sfn21cby"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases 
      (modify-phases
       %standard-phases
       (add-after 'install 'wrap-for-openssl-tss2-conf
		  (lambda* (#:key outputs #:allow-other-keys)
		    (let ((out (assoc-ref outputs "out"))
			  (openssl (assoc-ref %build-inputs "openssl"))
			  (tpm2-tss (assoc-ref %build-inputs "tpm2-tss"))
			  (tpm2-tss-engine (assoc-ref %build-inputs "tpm2-tss-engine")))
		      (wrap-program (string-append out "/bin/px-device-backup")
				    `("OPENSSL_CONF" ":" prefix
				      (,(string-append tpm2-tss-engine
						       "/etc/openssl-tss2.conf")))
				    `("PATH" ":" prefix
				      (,(string-append tpm2-tss-engine "/bin/") 
				       ,(string-append openssl "/bin/")))
				    `("TPM2TSSENGINE_TCTI" ":" prefix
				      (,(string-append tpm2-tss
						       "/lib/libtss2-tcti-device.so:/dev/tpm0")))
				    `("TPM2TOOLS_TCTI" ":" prefix
				      (,(string-append tpm2-tss
						       "/lib/libtss2-tcti-device.so:/dev/tpm0"))))
		      #t)))
       (delete 'sanity-check))))
   (inputs `(("openssl" ,openssl-1.1)
             ("tpm2-tss" ,tpm2-tss-openssl-1.1)
             ("tpm2-tss-engine" ,tpm2-tss-engine)))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (propagated-inputs `(("px-device-identity" ,px-device-identity)
                        ("python-requests" ,python-requests)
                        ("python-boto3" ,python-boto3)))
   (home-page "https://www.pantherx.org/")
   (synopsis "PantherX Device Backup")
   (description
    "Pulls device backup config from Central Management and runs the backup.")
   (license license:expat)))

(define-public bluetooth-client-manager-service
  (package
    (name "bluetooth-client-manager-service")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1rryll9pic2k5gqdhyrxiff20dgs4xs8j2213vy5l1p4ahinlwrf"))))
    (build-system cmake-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("bluez-qt" ,bluez-qt)
              ("capnproto" ,capnproto-0.9)
              ("openssl" ,openssl)
              ("qtbase" ,qtbase-5)
              ("util-linux" ,util-linux "lib")
              ("yaml-cpp" ,yaml-cpp)))
    (arguments
     `(#:tests? #f))
    (home-page "https://pantherx.org")
    (synopsis "Bluetooth Client Manager Service")
    (description
     "Background service for Bluetooth device discovery, data retrieval and submission")
    (license license:expat)))

(define-public px-python-shared
  (package
    (name "px-python-shared")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1aj7d4r3d2w7v9igyfqiyay2zp2d5scsz0n0hkd363prbb35qhzk"))))
    (build-system python-build-system)
    (inputs `(("python-requests" ,python-requests)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs `(("px-device-identity" ,px-device-identity)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Stuff that's shared across px-projects.")
    (description
     "Stuff that's shared across px-python-projects.")
    (license license:expat)))

(define-public sysinfo-daemon
  (package
    (name "sysinfo-daemon")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "0cngbrwanmc65fmnglrkl36sqq6wljsr301fg65msiq3a9zbnkjk"))))
    (build-system python-build-system)
    (inputs `(("python-requests" ,python-requests)
              ("python-py-cpuinfo" ,python-py-cpuinfo)
              ("px-python-shared" ,px-python-shared)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs `(("px-device-identity" ,px-device-identity)))
    (home-page "https://www.pantherx.org/")
    (synopsis "Collect and submit detailed system info")
    (description
     "Submit detailed system information")
    (license license:expat)))
