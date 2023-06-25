# Guix Channel: PantherX OS Packages

This repository contains package defintions for PantherX OS.
## Branches

- `master` Development
- `rolling` Live branch for most users
- `production` Live branch for enterprice users

## Authentication

```scheme
(cons* (channel
        (name 'pantherx)
        (url "https://channels.pantherx.org/git/pantherx.git")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          ""
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

### Automatically Publish Source Code

1. Creates archive of your repository
2. Uploads to S3 with the name `repository_tag.tgz`
3. Available via `https://s3.eu-central-1.amazonaws.com/source-git-pantherx-org/repository_tag.tgz`

Example: `px_accounts_service_0.0.1.tgz`

#### Set-Up CI

1. Settings > General > Permissions; Enable **Pipelines**
2. Settings > Repository > Protected Tags; Add `*` and select **Maintainers**

#### Prepare your Project

Add a new file `.gitlab-ci.yml` with the following contents:

```yaml
# GitLab CI Automation for PantherX Packages
# Author: Franz Geffke <franz@pantherx.org>
# Version: 0.0.8

# GitLab provided templates
include:
  - template: SAST.gitlab-ci.yml
  - template: Code-Quality.gitlab-ci.yml
  - template: License-Scanning.gitlab-ci.yml

# Git submodules
variables:
  GIT_SUBMODULE_STRATEGY: recursive

stages:
  - test
  - deploy

# Pack and upload to S3
deploy:
  stage: deploy
  image: alpine:latest
  script:
    - cd ../
    - apk add py-pip
    - pip install --user awscli
    - export PATH="$PATH:/root/.local/bin"
    - export AWS_ACCESS_KEY_ID=$(echo "$AWS_ACCESS_KEY_ID")
    - export AWS_SECRET_ACCESS_KEY=$(echo "$AWS_SECRET_ACCESS_KEY")
    - export AWS_DEFAULT_REGION=eu-central-1
    - export PACKAGE_NAME="$(echo $CI_PROJECT_NAME)_$(echo $CI_COMMIT_TAG).tgz"
    - tar --exclude="$CI_PROJECT_NAME/.git" -zcvf "$PACKAGE_NAME" "$CI_PROJECT_NAME"
    - aws s3 cp "$PACKAGE_NAME" s3://source-git-pantherx-org/
  only:
    - tags
  tags: [source]
```
      
**Note**: If you're using Git submodules, please follow [Using Git submodules with GitLab CI](https://docs.gitlab.com/ee/ci/git_submodules.html) to prepare your repository and modify the `upload` stage in the `.gitlab-ci.yml` according to your requirements.

#### Trigger a build

1. Repository > Tags; Create a new Tag
2. CI / CD > Pipleines; Watch the progress

Assuming that your repository name is `px_accounts_service` and you create a Tag 0.0.4, source code should be available at `https://s3.eu-central-1.amazonaws.com/source-git-pantherx-org/px_accounts_service_0.0.4.tgz` within a couple of minutes.

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