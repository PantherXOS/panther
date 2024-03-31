## System Configuration Templates

### Desktop

- `px-desktop-os`
- `px-desktop-ee-os`

#### Services

Internally:

- `%px-desktop-base-minimal-services` is great for custom-desktops, and excludes
  - `sddm-service-type`
  - `gnome-keyring-service-type`
  - `openssh-service-type`
- `%px-desktop-base-services` is similiar to guix `%desktop-services`

Stable:

- `%px-desktop-minmal-services` (`%px-desktop-base-minimal-services`)
  - Ideal for Sway
- `%px-desktop-services` (`%px-desktop-base-services`)
  - Ideal for Gnome, LXQt, KDE, etc.
- `%px-desktop-ee-services` (`%px-desktop-base-services`)

### Server

- `px-server-os`
- `px-server-ee-os`

#### Services

- `%px-server-services` is %base-services, with:
  - `openssh-service-type` (permit root login; disable password auth)
  - `nftables-service-type` (firewall)
  - `ntp-service-type` (time sync)
  - `dhcp-client-service-type` (dhcp)
- `%px-server-ee-services`

By default, the following ports are open:

- 22 (SSH)
- 80 (HTTP)
- 443 (HTTPS)

## Services

### Device Identity Service

Options:

- `port` - The port the service will listen on.
- `config-dir` - The directory where the configuration files are stored.
- `key-dir` - The directory where the keys are stored.

```scheme
(service px-device-identity-service-type
          (px-device-identity-configuration
            (port 8000)
            (config-dir "/etc/px-device-identity")
            (key-dir "/root/.local/share/px-device-identity")))
```

### User Identity Service

```scheme
(service px-user-identity-service-type)
```