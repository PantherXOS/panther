;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px self)
  #:use-module (guix build-system cargo)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix utils))

(define-public (px-cargo-inputs name)
  (cargo-inputs name #:module '(px packages rust-crates)))