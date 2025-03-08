(define-module (rde features arei)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)

  #:use-module (rde packages)
  #:use-module (rde packages emacs-xyz)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages base)
  #:use-module (guix gexp)

  #:export (feature-emacs-arei))

;; https://github.com/jpe90/emacs-clj-deps-new

(define* (feature-emacs-arei)
  "Setup and configure environment for emacs-arei."

  (define (get-home-services config)
    (define emacs-f-name 'arei)
    (list
     (simple-service
      'emacs-arei-add-packages
      home-profile-service-type
      (list guile-next guile-ares-rs))

     ;; https://github.com/DogLooksGood/meomacs/blob/master/programming.org#fix-rust-syntax-highlighting
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defun arei-mode-keybindings-hook ()
          "Arei keybindings that repeat those of geiser and cider"
          (define-key arei-mode-map "C-x C-e" 'arei-evaluate-last-sexp)
          (define-key arei-mode-map "C-c C-b" 'arei-evaluate-buffer)
          (define-key arei-mode-map "C-c C-r" 'arei-evaluate-region)
          (define-key arei-mode-map "C-c C-c" 'arei-interrupt-evaluation)
          ;; Not confident in these keybindings
          (define-key arei-mode-map "C-c C-x C-e" 'arei-evaluate-sexp)
          (define-key arei-mode-map "TAB" 'arei-complete-at-point))

        (setq geiser-mode-auto-p nil)

        (with-eval-after-load
            'arei
          (add-hook 'arei-mode 'arei-enable-keybindings-hook)))

      #:summary ""
      #:commentary ""
      #:keywords '(convenience emacs-arei)
      #:elisp-packages
      (list emacs-arei
            ;; emacs-smartparens
            ))))

  (feature
   (name 'emacs-arei)
   (values `((emacs-arei . #t)))
   (home-services-getter get-home-services)))
