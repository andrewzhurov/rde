(define-module (rde features bun)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde predicates)

  #:use-module (rde packages)
  #:use-module (rde packages rust)
  #:use-module (rde packages emacs-xyz)

  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages rust-crates)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages graphviz)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (guix gexp)

  #:export (feature-bun))

(define* (feature-bun)
  "Setup and configure Bun."
  ;; (ensure-pred file-like? rust)

  (define (get-home-services config)
    (define emacs-f-name 'rust-foreign)

    ;; export BUN_INSTALL="$HOME/.bun"
    ;; export PATH="$BUN_INSTALL/bin:$PATH"

    (list
     (simple-service
      'add-env-vars
      home-environment-variables-service-type
      `(("BUN_INSTALL" . "${HOME}/.bun")
        ("PATH" . "${PATH}:${BUN_INSTALL}/bin:abra")))))

  (feature
   (name 'bun)
   (values `((bun . #t)))
   (home-services-getter get-home-services)))
