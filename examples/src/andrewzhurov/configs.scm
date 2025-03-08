(define-module (andrewzhurov configs)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (andrewzhurov users user1)
  #:use-module (andrewzhurov hosts t450))

;; (define* (use-nested-configuration-modules
;;           #:key
;;           (users-subdirectory "/users")
;;           (hosts-subdirectory "/hosts"))
;;   (use-modules (guix discovery)
;;                (guix modules))

;;   (define current-module-file
;;     (search-path %load-path
;;                  (module-name->file-name (module-name (current-module)))))

;;   (define current-module-directory
;;     (dirname (and=> current-module-file canonicalize-path)))

;;   (define src-directory
;;     (dirname current-module-directory))

;;   (define current-module-subdirectory
;;     (string-drop current-module-directory (1+ (string-length src-directory))))

;;   (define users-modules
;;     (scheme-modules
;;      src-directory
;;      (string-append current-module-subdirectory users-subdirectory)))

;;   (define hosts-modules
;;     (scheme-modules
;;      src-directory
;;      (string-append current-module-subdirectory hosts-subdirectory)))

;;   (map (lambda (x) (module-use! (current-module) x)) hosts-modules)
;;   (map (lambda (x) (module-use! (current-module) x)) users-modules))

;; (use-nested-configuration-modules)


;;; t450

(define-public t450-config
  (rde-config
   (features
    (append
     %t450-features
     %user1-features))))

(define-public t450-os
  (rde-config-operating-system t450-config))

(define-public t450-he
  (rde-config-home-environment t450-config))


;;; Dispatcher, which helps to return various values based on environment
;;; variable value.

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("t450-home" t450-he)
      ("t450-system" t450-os)
      ("live-system" live-os)
      (_ t450-he))))

(dispatcher)
