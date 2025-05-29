# Guix Channel: PantherX OS Packages

This repository contains package defintions for PantherX OS.

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
./pre-inst-env guix system vm ../panther-examples/server-os.scm
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