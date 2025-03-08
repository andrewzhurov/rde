(define-module (rde features hypothesis)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde packages)

  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home services shepherd)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (guix utils)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 pretty-print)

  #:export (feature-gitwatch))

(define (add-gitwatch-shepherd-service config)
  (list (shepherd-service
         (documentation "Gitwatch service.") ;; TODO add how to use it
         (provision '(gitwatch))
         (start #~(make-forkexec-constructor
                   (list #$(file-append
                            gitwatch
			    "/bin/gitwatch")
                         "-b" "main"
                         "-r" "origin"
                         "/home/user1/notes/org-roam/")
                   #:log-file (string-append
			       (or (getenv "XDG_LOG_HOME")
				   (format #f "~a/.local/var/log"
					   (getenv "HOME")))
			       "/gitwatch.log")))
         (stop #~(make-kill-destructor)))))

(define home-gitwatch-service-type
  (service-type (name 'home-gitwatch)
                (extensions
                 (list (service-extension
			home-shepherd-service-type
			add-gitwatch-shepherd-service)
                       ))
		;; (compose identity)
		;; (extend home-emacs-extensions)
                ;; (default-value (home-emacs-configuration))
                (description "Sets up gitwatch.")
                ))

(define* (feature-gitwatch)
  "Initiates gitwatch for notes repo."

  (define (get-home-services config)
    (list
     (service
      home-gitwatch-service-type
      #f)))

  (feature
   (name 'gitwatch)
   (values (append
            `((gitwatch . #t))))
   (home-services-getter get-home-services)))
