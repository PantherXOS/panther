;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages golang-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix utils)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages vpn))

(define-public go-github-com-rxwycdh-rxhash
  (package
    (name "go-github-com-rxwycdh-rxhash")
    (version "0.0.0-20230131062142-10b7a38b400d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rxwycdh/rxhash")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qw4kn5r0xjfy9mycv57f7lmlpksybzr2qcdr4713svrxakwmgyz"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/rxwycdh/rxhash"))
    (home-page "https://github.com/rxwycdh/rxhash")
    (synopsis "Create unique hash values for Go structs")
    (description
     "rxhash is a Go library for creating a unique hash value for struct in Go
with data consistency.")
    (license license:expat)))

(define-public go-github-com-sj14-astral
  (package
    (name "go-github-com-sj14-astral")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sj14/astral")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m4qirl3mrdpm1dw9lgfj6p7jsyy60kyhhzfkikxbf471wk5apba"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/sj14/astral/pkg/astral"
           #:unpack-path "github.com/sj14/astral"))
    (propagated-inputs
     (list go-github-com-stretchr-testify
           go-github-com-logrusorgru-aurora-v3))
    (home-page "https://github.com/sj14/astral")
    (synopsis "Astronomical calculations for sun and moon positions")
    (description
     "This package provides calculations for the position of the sun and moon.")
    (license license:asl2.0)))

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

