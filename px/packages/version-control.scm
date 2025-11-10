;;; Package Repository for GNU Guix
;;; Copyright © 2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages version-control)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (gnu packages base))

(define-public gh
  (package
    (name "gh")
    (version "2.83.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cli/cli/releases/download/v"
                           version "/gh_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "0vv7y5lrm3my9wjlzfl1xaf7fi2066rjc4anvwd7arzw83dnrkx5"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("bin/gh" "bin/")
         ("share/man" "share/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'generate-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash-comp (string-append out "/etc/bash_completion.d"))
                    (zsh-comp (string-append out "/share/zsh/site-functions"))
                    (fish-comp (string-append out "/share/fish/vendor_completions.d"))
                    (gh (string-append out "/bin/gh")))
               (mkdir-p bash-comp)
               (mkdir-p zsh-comp)
               (mkdir-p fish-comp)
               ;; Generate shell completions
               (with-output-to-file (string-append bash-comp "/gh")
                 (lambda () (invoke gh "completion" "-s" "bash")))
               (with-output-to-file (string-append zsh-comp "/_gh")
                 (lambda () (invoke gh "completion" "-s" "zsh")))
               (with-output-to-file (string-append fish-comp "/gh.fish")
                 (lambda () (invoke gh "completion" "-s" "fish")))
               #t))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://cli.github.com")
    (synopsis "GitHub command-line tool")
    (description
     "gh is GitHub on the command line.  It brings pull requests, issues, and
other GitHub concepts to the terminal next to where you are already working
with git and your code.")
    (license license:expat)))
