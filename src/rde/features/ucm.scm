(define-module (rde features ucm)
  #:export (feature-ucm))

(define* (feature-ucm) ;; based on feature-emacs-org-roam-ui
  "Configure Unisonweb Code Manager."

  (define f-name 'ucm)

  (define (get-home-services config)
    (list
     ))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
