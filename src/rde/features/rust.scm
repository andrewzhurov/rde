(define-module (rde features rust)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (gnu packages java)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-rust))

;; https://github.com/jpe90/emacs-clj-deps-new

(define* (feature-rust
          #:key
          ;; (rust     (@ (rde packages rust) rust))
          (rust-lsp (@ (rde packages rust) rust-lsp)))
  "Setup and configure environment for Rust."
  ;; (ensure-pred file-like? rust)
  (ensure-pred file-like? rust-lsp)

  (define (get-home-services config)
    (define emacs-f-name 'rust)
    (define rust-lsp-binary
      (if (any-package? rust-lsp)
          (file-append rust-lsp "/bin/rust-lsp")
          rust-lsp))
    (list
     (unless (get-value 'openjdk config)
         (simple-service
          'rust-add-packages
          home-profile-service-type
          (list
           ;; for go-to-definition
           ;; MAYBE: Add as a dependency to cider?
           ;; (@ (gnu packages compression) unzip)
           ;; rust-tools
           ;; jdk
           )))

     ;; https://github.com/DogLooksGood/meomacs/blob/master/programming.org#fix-rust-syntax-highlighting
     (when (get-value 'emacs config)
       (rde-elisp-configuration-service
        emacs-f-name
        config
        `((defun rde--rust-disable-eglot-parts-in-favor-of-cider ()
            (setq-local eglot-stay-out-of '(eldoc flymake)))
          (add-hook 'rust-mode-hook
                    'rde--rust-disable-eglot-parts-in-favor-of-cider)

          (with-eval-after-load
           'consult-imenu
           (add-to-list
            'consult-imenu-config
            ;; https://github.com/rust-lsp/rust-lsp/blob/13487d1eb0d06596565f76b8f6d76d00b7e9f03b/cli/integration-test/integration/initialize_test.clj#L62
            '(rust-mode
              :toplevel "Variable"
              :types ((?f "Function"  font-lock-function-name-face)
                      (?m "Macro"     font-lock-function-name-face)
                      (?M "Method"    font-lock-function-name-face)
                      (?e "Event"     font-lock-function-name-face)
                      (?n "Namespace" font-lock-constant-face)
                      (?k "Keyword"   font-lock-keyword-face)
                      (?c "Class"     font-lock-type-face)
                      (?t "Type"      font-lock-type-face)
                      (?v "Variable"  font-lock-variable-name-face)))))
          (with-eval-after-load
           'eglot
           (add-to-list
            'eglot-server-programs
            '(((rust-mode :language-id "rust")
               (rustc-mode :language-id "rust")
               (rustscript-mode :language-id "rustscript"))
              . (,rust-lsp-binary))))
          (with-eval-after-load
           'rust-mode
           (customize-set-variable 'rust-align-forms-automatically t)))
        #:summary "\
Rust(Script) code style, CIDER, LSP, imenu and other tweaks"
        #:commentary "\
Configure eglot, imenu, CIDER, flymake and other packages.
"
        #:keywords '(convenience rust)
        #:elisp-packages
        (list emacs-rust-mode
              (get-value 'emacs-eglot config emacs-eglot)
              emacs-flymake-kondor emacs-html-to-hiccup)))))

  (feature
   (name 'rust)
   (values `((rust . #t)))
   (home-services-getter get-home-services)))
