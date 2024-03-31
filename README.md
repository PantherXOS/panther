# Guix Channel: PantherX OS Packages

This repository contains package defintions for PantherX OS.

This is a fork of our original package repository [guix-pantherx](https://git.pantherx.org/development/guix-pantherx) to resolve authentication issues (many of the commits weren't signed) and reduce the size which had blown up to 700+ MB.

Refer to `./DOCS.md` for more information on configuration options.

## Authentication

```scheme
(cons* (channel
        (name 'pantherx)
        (url "https://channels.pantherx.org/git/panther.git")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "54b4056ac571611892c743b65f4c47dc298c49da"
          (openpgp-fingerprint
           "A36A D41E ECC7 A871 1003  5D24 524F EB1A 9D33 C9CB"))))
       %default-channels)
```

## Package for Guix

- [A packaging tutorial for Guix](https://www.gnu.org/software/guix/blog/2018/a-packaging-tutorial-for-guix/)
- [Pjotr’s hacking guide to GNU Guix](https://github.com/pjotrp/guix-notes/blob/master/HACKING.org)
- [Creating package variants with GNU Guix and Guile Scheme](https://guix.mdc-berlin.de/documentation.html#sec-7)

Tip: GNU Guix respects the `GUIX_PACKAGE_PATH` environment variable and will prefer packages specified in the directories listed in this variable over those that come with GNU Guix.

```bash
$ export GUIX_PACKAGE_PATH=/root/panther
$ guix package -i packagename
```

Format and lint the file before commit:

```bash
guix style --whole-file px/packages/tools.scm
```

### Test changes

```bash
./pre-inst-env guix system vm .examples/server-os.scm
```

### Patches

For the time being, all patches must remain in `./` to be found by guix, due to:

```scheme
;; gnu/packages.scm
(define %patch-path
  ;; Define it after '%package-module-path' so that '%load-path' contains user
  ;; directories, allowing patches in $GUIX_PACKAGE_PATH to be found.
  (make-parameter
   (map (lambda (directory)
          (if (string=? directory %distro-root-directory)
              (string-append directory "/gnu/packages/patches")
              directory))
        %load-path)))
```

## Troubleshooting

When you are upgrading from a old installation, you might face issues where `%default-channels` points to previous guix and nongnu (nonguix) channels.

Here's what your channels file probably looked like:

```scheme
(list (channel
        (name 'guix)
        (url "https://channels.pantherx.org/git/pantherx.git")
        (branch "rolling-nonlibre"))
      (channel
        (name 'nongnu)
        (url "https://channels.pantherx.org/git/nongnu.git")
        (branch "rolling"))
      (channel
        (name 'pantherx)
        (url "https://channels.pantherx.org/git/pantherx-extra.git")
        (branch "master")))
```

This is what you tried:

```scheme
(cons* (channel
        (name 'pantherx)
        (branch "master")
        (url "https://channels.pantherx.org/git/panther.git")
         (introduction
          (make-channel-introduction
           "54b4056ac571611892c743b65f4c47dc298c49da"
           (openpgp-fingerprint
            "A36A D41E ECC7 A871 1003  5D24 524F EB1A 9D33 C9CB"))))
       %default-channels)
```

This is a potential issue:

```bash
zcat /var/log/guix/drvs/k7/dgx55ivy7bfgm2z2qvk0njnjs2x5dh-nongnu.drv.gz
(repl-version 0 1 1)
(exception unbound-variable (value #f) (value "Unbound variable: ~S") (value (linux-libre-6.3)) (value #f))
```

This is how you can temporarily overwrite all channels:

```scheme
(list (channel
       (name 'guix)
       (branch "master")
       (url "https://git.savannah.gnu.org/git/guix.git")
        (introduction
         (make-channel-introduction
          "9edb3f66fd807b096b48283debdcddccfea34bad"
          (openpgp-fingerprint
           "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
       (channel
        (name 'nonguix)
        (url "https://channels.pantherx.org/git/nonguix.git")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       (channel
        (name 'pantherx)
        (branch "master")
        (url "https://channels.pantherx.org/git/panther.git")
         (introduction
          (make-channel-introduction
           "54b4056ac571611892c743b65f4c47dc298c49da"
           (openpgp-fingerprint
            "A36A D41E ECC7 A871 1003  5D24 524F EB1A 9D33 C9CB")))))
```

After pull and reconfigure succeeds, you may restore the default.
