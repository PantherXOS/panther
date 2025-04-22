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
  #:use-module (gnu packages crates-web)
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
  #:use-module (gnu packages time)
  #:use-module (px packages crates-io)
  #:use-module (px packages common)
  #:use-module (px packages python-xyz)
  #:use-module (px packages tpm)
  #:use-module (px packages library))

(define-public px-device-identity
  (package
   (name "px-device-identity")
   (version "0.13.0")
   (source
    (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "_v" version
                           ".tgz"))
       (sha256
        (base32 "1gd9s04brlw3qd9b633j9r68blbbrp038j7b2c7y9c2gx1fq10yq"))))
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
                         ("python-pycryptodomex" ,python-pycryptodomex)
                         ("python-pyyaml" ,python-pyyaml)
                         ("python-shortuuid" ,python-shortuuid)
                         ("python-appdirs" ,python-appdirs)
                         ("python-psutil" ,python-psutil)
                         ("python-joserfc", python-joserfc)))
    (home-page "https://www.pantherx.org/")
    (synopsis "PantherX Device Identity Manager")
    (description
     "Initiates device identity based on RSA or ECC key pair and optionally registers with Central Management.")
    (license license:expat)))

(define-public px-device-identity-service
  (package
   (name "px-device-identity-service")
   (version "0.11.6")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version
                         ".tgz"))
     (sha256
      (base32 "02cs4rb4xrvrszzdwr38kqi0ha5vma5z1yi4zc16nd9q7v2s2rvy"))))
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
   (version "0.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version
                         ".tgz"))
     (sha256
      (base32 "1i11210hpj6d6d09xrdwfq5vrnvm26bm97k80a8yz9mil9dxnqak"))))
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
                        ("python-requests" ,python-requests)
                        ("px-python-shared" ,px-python-shared)
                        ("python-appdirs" ,python-appdirs)
                        ("python-dateutil" ,python-dateutil)
                        ("python-sentry-sdk" ,python-sentry-sdk-2)))
   (home-page "https://www.pantherx.org/")
   (synopsis "PantherX Device Runner")
   (description
    "Downloads administrative jobs from Central Management to run on local device.")
   (license license:expat)))

(define-public px-user-identity-service
  (package
   (name "px-user-identity-service")
   (version "0.2.5")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version
                         ".tgz"))
     (sha256
      (base32 "0vx6rgz5znz4av9gg1mxvi8r3c0a7xfb1xl8465072m91panylfr"))))
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
    (version "0.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://source.pantherx.org/" name "-" version
                           ".crate"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xzl7nx5sd43wjrf6sbrrmsbmys54dp1sds9qdniqx55j62jsifs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-2)
                       ("rust-configparser" ,rust-configparser-3)
                       ("rust-fern" ,rust-fern-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.8)
                       ("rust-syslog" ,rust-syslog-4)
                       ("rust-tokio" ,rust-tokio-1)
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

(define-public sysinfo-daemon
  (package
   (name "sysinfo-daemon")
   (version "0.0.10")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version
                         ".tgz"))
     (sha256
      (base32 "0m4zyqxv2a6fk01x7f88wvp3w26wk17i9qgxh3il1q9089z000av"))))
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
		      (wrap-program (string-append out "/bin/sysinfo-daemon")
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
   (propagated-inputs (list px-device-identity
                           px-python-shared
                           python-py-cpuinfo
                           python-requests
                           python-psutil
                           procps))
   (home-page "https://www.pantherx.org/")
   (synopsis "Collect and submit detailed system info")
   (description
    "Submit detailed system information")
   (license license:expat)))

(define-public bcms
  (package
   (name "bcms")
   (version "0.0.16")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://source.pantherx.org/" name "_v" version
                         ".tgz"))
     (sha256
      (base32 "1wd8b4q7rr2k3l4x8m0c040xa3q7p2gpqf48malfh3yba8k0h6l6"))))
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
		      (wrap-program (string-append out "/bin/bcms")
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
                        ("python-pycapnp" ,python-pycapnp)
                        ("python-requests" ,python-requests)
                        ("python-bleak" ,python-bleak)
                        ("python-dbus" ,python-dbus)
                        ("px-python-shared" ,px-python-shared)
                        ("python-sentry-sdk" ,python-sentry-sdk-2)))
   (home-page "https://www.pantherx.org/")
   (synopsis "Collect and submit detailed system info")
   (description
    "Submit detailed system information")
   (license license:expat)))
