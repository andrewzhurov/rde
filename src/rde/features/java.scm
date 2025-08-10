(define-module (rde features java)
  #:use-module (rde features)
  #:use-module (rde features emacs)

  #:use-module (rde packages)
  #:use-module (rde packages emacs-xyz)
  #:use-module (rde packages java)

  #:use-module (gnu packages base)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (guix gexp)

  #:export (feature-jdecomp))

;;  TODO doesn't compile and I don't wont to debug it without a proper repl
(define* (feature-jdecomp)
  "Setup and configure jdecomp."

  (define (get-home-services config)
    (define emacs-f-name 'jdecomp)

    (list
     (simple-service
      'jdecomp-add-packages
      home-profile-service-type
      (list cfr))

     ;; https://github.com/DogLooksGood/meomacs/blob/master/programming.org#fix-rust-syntax-highlighting
     (rde-elisp-configuration-service
      emacs-f-name
      config
      (let ((cfr-jar (file-append (@ (gnu packages java) cfr) "cfr.jar")))
        `((require 'jdeconf)
          (with-eval-after-load 'jdeconf
            (customize-set-variable 'jdecomp-decompiler-paths
                                    '((cfr . ,#~#$cfr-jar)))
            (jdecomp-mode 1))
          ))

      #:summary ""
      #:commentary ""
      #:keywords '(convenience java)
      #:elisp-packages
      (list emacs-jdecomp))))

  (feature
   (name 'jdecomp)
   (values `((jdecomp . #t)))
   (home-services-getter get-home-services)))
