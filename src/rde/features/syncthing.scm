(define-module (rde features syncthing)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde predicates)

  #:use-module (rde packages)
  #:use-module (rde packages emacs-xyz)

  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages syncthing)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (guix gexp)

  #:export (feature-syncthing))

(define* (feature-syncthing)
  "Setup and configure syncthing."

  (define (get-home-services config)
    (list
     (simple-service
      'rust-foreign-add-rustup-bin
      home-environment-variables-service-type
      `(("PATH" . "${PATH}:${HOME}/.cargo/bin/")))

     (simple-service
      'syncthing-add-packages
      home-profile-service-type
      (list syncthing syncthing-gtk))))

  (feature
   (name 'syncthing)
   (values `((syncthing . #t)))
   (home-services-getter get-home-services)))
