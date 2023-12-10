(define-module (rde features bluetooth)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde packages)

  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (guix utils)
  #:use-module (guix packages)

  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)
  #:use-module (gnu services desktop)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 pretty-print)

  #:export (feature-bluetooth))

(define* (feature-bluetooth)
  "Initiates bluetooth for notes repo."

  (define (get-home-services config)
    (list
     ;;(bluetooth-service #:auto-enable? #t)
     ))

  (define (get-system-services config)
    (list
     (bluetooth-service #:auto-enable? #t)
     ))

  (feature
   (name 'bluetooth)
   (values (append
            `((bluetooth . #t))))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
