;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages vpn)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix utils)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages vpn))

(define-public go-github-com-mdlayher-socket
  (package
    (name "go-github-com-mdlayher-socket")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/socket")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bq6sphsffjqqk2v9wy8qkv5yf0r6d72pklapgy3znqlnpgvnqab"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/mdlayher/socket"
           #:tests? #f))  ; Tests require network access
    (propagated-inputs
     (list go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-sys))
    (home-page "https://github.com/mdlayher/socket")
    (synopsis "Low-level network socket operations for Go")
    (description
     "Package socket provides a low-level network connection type which
integrates with Go's runtime network poller to provide asynchronous I/O
and deadline support.")
    (license license:expat)))

(define-public go-github-com-mdlayher-genetlink
  (package
    (name "go-github-com-mdlayher-genetlink")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/genetlink")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vppn8071nh8pnbyq9769j1zcgq76iadd5fry90xkmfq429if356"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/mdlayher/genetlink"
           #:tests? #f))  ; Tests require root/network
    (propagated-inputs
     (list go-github-com-mdlayher-netlink
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://github.com/mdlayher/genetlink")
    (synopsis "Generic netlink interactions for Go")
    (description
     "Package genetlink implements generic netlink interactions and data types.")
    (license license:expat)))

(define-public go-golang-zx2c4-com-wireguard-wgctrl
  (package
    (name "go-golang-zx2c4-com-wireguard-wgctrl")
    (version "0.0.0-20241231184526-a9ab2273dd10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/WireGuard/wgctrl-go")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q1vn73g1lk6pw824p87wmr0gxhpqz4yy9i8hb21lfwgb551knam"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "golang.zx2c4.com/wireguard/wgctrl"
           #:unpack-path "golang.zx2c4.com/wireguard/wgctrl"
           #:tests? #f))  ; Tests require WireGuard kernel module
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-mdlayher-genetlink
           go-github-com-mdlayher-netlink
           go-github-com-mdlayher-socket
           go-golang-org-x-crypto
           go-golang-org-x-sys
           go-golang-zx2c4-com-wireguard))
    (home-page "https://github.com/WireGuard/wgctrl-go")
    (synopsis "WireGuard device configuration in Go")
    (description
     "Package wgctrl enables control of WireGuard devices on multiple platforms.")
    (license license:expat)))

;; Newer wifi package required by IVPN (Guix has 0.3.0 which lacks required APIs)
(define-public go-github-com-mdlayher-wifi-next
  (package
    (name "go-github-com-mdlayher-wifi")
    (version "0.5.1-0.20250704183335-1b2199ae492f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/wifi")
             (commit "1b2199ae492f")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gj6iiyg73fmvzfcld8q6vzyvcdzb12k8y63r3v7fhxnrvz1fa7x"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/mdlayher/wifi"
           #:tests? #f))  ; Tests require network/root
    (propagated-inputs
     (list go-github-com-mdlayher-genetlink
           go-github-com-mdlayher-netlink
           go-golang-org-x-sys))
    (home-page "https://github.com/mdlayher/wifi")
    (synopsis "Wi-Fi interface operations for Go")
    (description
     "Package wifi provides access to IEEE 802.11 WiFi device operations.")
    (license license:expat)))

(define-public go-github-com-josharian-native
  (package
    (name "go-github-com-josharian-native")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/josharian/native")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wa4yzc3r06qjklqjf4n30zx9v660w8hmxkmybzwk03fmlv2rcyj"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/josharian/native"))
    (home-page "https://github.com/josharian/native")
    (synopsis "Native byte order detection for Go")
    (description
     "Package native provides easy access to native byte order.")
    (license license:expat)))

(define-public ivpn
  (package
    (name "ivpn")
    (version "3.14.34")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ivpn/desktop-app")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k60m7ag9ixifry5lxqdbpp9wn50fj0z12csqvbrk12scbk8dpj3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/ivpn/desktop-app/daemon"
      #:unpack-path "github.com/ivpn/desktop-app"
      #:install-source? #f
      #:tests? #f  ; Tests require external binaries and network
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key import-path build-flags #:allow-other-keys)
              (let ((ldflags (string-append
                              "-s -w"
                              " -X github.com/ivpn/desktop-app/daemon/version._version="
                              #$version)))
                ;; Build the daemon
                (apply invoke "go" "build" "-v" "-x"
                       (string-append "-ldflags=" ldflags)
                       "-o" "bin/ivpn-service"
                       import-path
                       build-flags)
                ;; Build the CLI
                (apply invoke "go" "build" "-v" "-x"
                       (string-append "-ldflags=" ldflags)
                       "-o" "bin/ivpn"
                       "github.com/ivpn/desktop-app/cli"
                       build-flags))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (sbin (string-append out "/sbin")))
                (mkdir-p bin)
                (mkdir-p sbin)
                (install-file "bin/ivpn-service" sbin)
                (install-file "bin/ivpn" bin)))))))
    (inputs
     (list openvpn
           wireguard-tools))
    (propagated-inputs
     (list go-github-com-fsnotify-fsnotify
           go-github-com-google-uuid
           go-github-com-stretchr-testify
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-zx2c4-com-wireguard
           go-golang-zx2c4-com-wireguard-wgctrl
           go-github-com-mdlayher-wifi-next
           go-github-com-mdlayher-netlink
           go-github-com-mdlayher-genetlink
           go-github-com-mdlayher-socket
           go-github-com-josharian-native
           go-github-com-olekukonko-tablewriter))
    (home-page "https://www.ivpn.net/")
    (synopsis "IVPN client daemon and CLI")
    (description
     "IVPN is a privacy-focused VPN service.  This package provides the daemon
service and command-line interface for connecting to IVPN servers using
OpenVPN or WireGuard protocols.  Features include kill-switch, multi-hop
connections, and custom DNS settings.")
    (license license:gpl3+)))
