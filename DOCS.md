
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