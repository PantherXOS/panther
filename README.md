# Guix Channel: PantherX OS Packages

This repository contains GUIX package defintions maintained primarily by [Franz Geffke](https://gofranz.com).

## Channel Definition

```scheme
(cons* (channel
        (name 'pantherx)
        (url "https://codeberg.org/gofranz/panther.git")
        ;; Enable signature verification
        (introduction
         (make-channel-introduction
          "54b4056ac571611892c743b65f4c47dc298c49da"
          (openpgp-fingerprint
           "A36A D41E ECC7 A871 1003  5D24 524F EB1A 9D33 C9CB"))))
       %default-channels)
```

## Time Travel

When things break because of upstream changes, this will allow you to run a future guix commit, to fix and test the channel without updating the whole system.

Create a channels file that includes only the guix channel:

```scheme
(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix.git")
        (branch "master")
        ;; Specify commit
        (commit "dc1a77267f03e37b331c8597b066c5ee52a75445")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
```

Spawn a shell for a clean environment:

```bash
guix shell --container --nesting --network openssl nss-certs coreutils guix
```

And build the target package:

```bash
guix time-machine --channels=default-channel.scm -- build -L panther pimsync
```