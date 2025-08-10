(define-module (rde features rust)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)

  #:use-module (rde packages)
  #:use-module (rde packages rust)
  #:use-module (rde packages emacs-xyz)

  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages graphviz)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (guix gexp)

  #:export (feature-rust feature-rust-foreign))

;; https://github.com/jpe90/emacs-clj-deps-new

(define* (feature-rust
          #:key
          (rust          (@ (gnu packages rust) rust))
          (rust-analyzer (@ (gnu packages rust) rust-analyzer)))
  "Setup and configure environment for Rust."
  ;; (ensure-pred file-like? rust)

  (define (get-home-services config)
    (define emacs-f-name 'rust)

    (list
     (simple-service
      'rust-add-packages
      home-profile-service-type
      (list rust
            wasm-pack
            cargo-watch
            gcc-toolchain ;; for librt.so.1, required by cargo run (rustc)
            graphviz-minimal ;; for eglot-x viz
            (@ (gnu packages tree-sitter) tree-sitter-rust)
            (list rust "tools")
            (list rust "cargo")))

     ;; https://github.com/DogLooksGood/meomacs/blob/master/programming.org#fix-rust-syntax-highlighting
     (rde-elisp-configuration-service
      emacs-f-name
      config
      (let ((rust-analyzer (file-append rust "/bin/rust-analyzer"))
            (rust-clippy   (file-append rust "/bin/cargo-clippy"))
            (rust-rustfmt  (file-append rust "/bin/rustfmt"))
            (rust-cargo    (file-append rust "/bin/cargo")))
        `((require 'eglot)
           ;; Setup eglot-x
           ;; https://github.com/nemethf/eglot-x?tab=readme-ov-file#protocol-extensions-for-eglot
          (require 'eglot-x)
          (with-eval-after-load 'eglot-x
            (eglot-x-setup))
          (require 'flymake-clippy)
          (require 'smartparens)

          (with-eval-after-load 'consult-imenu
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

          ;; (with-eval-after-load
          ;;     'eglot
          ;;   (add-to-list
          ;;    'eglot-server-programs
          ;;    '(((rust-mode :language-id "rust")
          ;;       (rustc-mode :language-id "rust")
          ;;       (rustscript-mode :language-id "rustscript"))
          ;;      . ;; (,rust-lsp-binary)
          ;;      #$(file-append rust:tools "/bin/rust-analyzer")
          ;;      ;; "rust-analyzer" ;; maybe try with gexp / craft path to bin file
          ;;      )))

          (with-eval-after-load 'rust-mode
            (customize-set-variable 'rust-align-forms-automatically t)

            ;; https://github.com/rust-lang/rust-mode?tab=readme-ov-file#tree-sitter
            (setq rust-mode-treesitter-derive t))

          (defun rde--set-consult-imenu-cache-to-nil ()
            (setq consult-imenu--cache nil))

          (with-eval-after-load 'eglot
            (customize-set-variable 'eglot-extend-to-xref t)
            (customize-set-value 'eglot-connect-timeout 600)

            ;; https://rust-analyzer.github.io/manual.html#eglot
            (add-to-list 'eglot-server-programs
                         '((rust-mode
                            rustc-mode
                            rustscript-mode
                            rust-ts-mode) .
                            (;; "rust-analyzer"
                             ;; ,#~(string-append #$rust:tools "")
                             ,#~#$rust-analyzer:tools
                             :initializationOptions (:check (:command
                                                             "clippy"
                                                             ;; ,#~#$rust-clippy:tools
                                                             ;; $~$#rust-clippy:tools
                                                             ;; ,#~(string-append #$rust:tools "/bin/cargo-clippy")
                                                             )))))

            (setq rust-rustfmt-bin    ,#~#$rust-rustfmt:tools
                  rust-cargo-bin      ,#~#$rust-cargo:cargo
                  rust-format-on-save t
                  rust-rustfmt-switches '("--edition" "2024"))

            (add-hook 'eglot-managed-mode-hook 'rde--set-consult-imenu-cache-to-nil)
            (add-hook 'rust-mode-hook       'eglot-ensure) ;; source: https://rust-analyzer.github.io/manual.html#eglot
            (add-hook 'rustc-mode-hook      'eglot-ensure) ;; also: https://github.com/joaotavora/eglot#connecting-automatically
            (add-hook 'rustscript-mode-hook 'eglot-ensure)
            (add-hook 'rust-ts-mode         'eglot-ensure))


          ;; Clippy flymake
          ;; post's
          ;; flymake-clippy's https://github.com/emacsmirror/flymake-clippy?tab=readme-ov-file#complete-eglot--rust-mode--use-package-example
          (defun rde--clippy-flymake-manually-activate-flymake ()
            "Shim for working around eglot's tendency to suppress flymake backends."
            (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
            (flymake-mode 1))

          (defun rde--eglot-dont-supress-flymake ()
            (setq-local eglot-stay-out-of '(eldoc flymake)))

          (with-eval-after-load 'flymake-clippy
            (add-hook 'eglot-managed-mode-hook 'rde--clpppy-flymake-manually-activate-flymake nil t)
            (add-hook 'rust-mode-hook 'rde--eglot-dont-supress-flymake)
            (add-hook 'rust-mode-hook 'rde--clippy-flymake-manually-activate-flymake)
            (add-hook 'rust-mode-hook 'flymake-clippy-setup-backend)
            ;; TODO for other rust*-mode
            )

          ;; (require 'eglot)

          ;; (eval-when-compile
          ;;  (require 'flymake-clippy))


          ;; blog posts's https://unwoundstack.com/blog/emacs-as-a-rust-ide.html
          ;; (defun sp1ff/rust/mode-hook ()
          ;;   "My rust-mode hook"
          ;;   (column-number-mode)
          ;;   (display-line-numbers-mode)
          ;;   (hs-minor-mode)
          ;;   (smartparens-mode)
          ;;   (define-key rust-mode-map "\C-ca" 'eglot-code-actions)
          ;;   (define-key rust-mode-map (kbd "C-<right>")   'sp-forward-slurp-sexp)
          ;;   (define-key rust-mode-map (kbd "C-<left>")    'sp-forward-barf-sexp)
          ;;   (define-key rust-mode-map (kbd "C-M-<right>") 'sp-backward-slurp-sexp)
          ;;   (define-key rust-mode-map (kbd "C-M-<left>")  'sp-backward-barf-sexp)
          ;;   (define-key rust-mode-map "\C-c>" 'hs-show-all)
          ;;   (define-key rust-mode-map "\C-c<" 'hs-hide-all)
          ;;   (define-key rust-mode-map "\C-c;" 'hs-toggle-hiding)
          ;;   (define-key rust-mode-map "\C-c'" 'hs-hide-level)
          ;;   (setq indent-tabs-mode nil
          ;;         tab-width 4
          ;;         c-basic-offset 4
          ;;         fill-column 100))

          ;; (add-hook 'rust-mode 'sp1ff/rust/mode-hook)

          ;; Rustic
          ;;
          ;; (with-eval-after-load
          ;;     'rustic ;; not tested, is it 'rustic-mode'?
          ;;   (setq rustic-analyzer-command '("~/gits/abcdw/rde/examples/andrewzhurov/config/rust/docker-lsp-server/rust-analyzer.sh")) ;; found in https://github.com/brotzeit/rustic#server
          ;;   (setq rustic-lsp-server 'rust-analyzer)
          ;;   (setq rustic-lsp-client 'eglot) ;; found in https://github.com/brotzeit/rustic#client

          ;;   ;; cargo bin path to rustic, as shown how to here https://github.com/brotzeit/rustic#cargo
          ;;   ;; does it work?
          ;;   (setq rustic-cargo-bin "~/gits/abcdw/rde/examples/andrewzhurov/config/rust/docker-lsp-server/cargo.sh"))


          ;; already enabled by smartparens feature
          ;; (use-package smartparens :ensure t
          ;;              :config (require 'smartparens-rust))
          ))

      #:summary ""
      #:commentary ""
      #:keywords '(convenience rust)
      #:elisp-packages
      (list emacs-rust-mode
            ;; emacs-rustic
            emacs-eglot-x
            emacs-flymake-clippy
            emacs-smartparens
            ;; emacs-jsonrpc ;; https://www.reddit.com/r/emacs/comments/1d0g29e/comment/l5v2eln/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
            ))))

  (feature
   (name 'rust)
   (values `((rust . #t)))
   (home-services-getter get-home-services)))

(define* (feature-rust-foreign)
  "Setup and configure environment for Rust."
  ;; (ensure-pred file-like? rust)

  (define (get-home-services config)
    (define emacs-f-name 'rust-foreign)

    (list
     (simple-service
      'rust-foreign-add-rustup-bin
      home-environment-variables-service-type
      `(("PATH" . "${PATH}:${HOME}/.cargo/bin/")))

     (simple-service
      'rust-add-packages
      home-profile-service-type
      (list graphviz-minimal ;; for eglot-x viz
            (@ (gnu packages tree-sitter) tree-sitter-rust)))

     ;; https://github.com/DogLooksGood/meomacs/blob/master/programming.org#fix-rust-syntax-highlighting
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'eglot)
        ;; Setup eglot-x
        ;; https://github.com/nemethf/eglot-x?tab=readme-ov-file#protocol-extensions-for-eglot
        (require 'eglot-x)
        (with-eval-after-load 'eglot-x
          (eglot-x-setup))
        (require 'flymake-clippy)
        (require 'smartparens)
        (require 'smartparens-config)

        (with-eval-after-load 'consult-imenu
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

        ;; (with-eval-after-load
        ;;     'eglot
        ;;   (add-to-list
        ;;    'eglot-server-programs
        ;;    '(((rust-mode :language-id "rust")
        ;;       (rustc-mode :language-id "rust")
        ;;       (rustscript-mode :language-id "rustscript"))
        ;;      . ;; (,rust-lsp-binary)
        ;;      #$(file-append rust:tools "/bin/rust-analyzer")
        ;;      ;; "rust-analyzer" ;; maybe try with gexp / craft path to bin file
        ;;      )))

        (with-eval-after-load 'rust-mode
          (customize-set-variable 'rust-align-forms-automatically t)

          ;; https://github.com/rust-lang/rust-mode?tab=readme-ov-file#tree-sitter
          (setq rust-mode-treesitter-derive t))

        (defun rde--set-consult-imenu-cache-to-nil ()
          (setq consult-imenu--cache nil))

        (with-eval-after-load 'eglot
          (customize-set-variable 'eglot-extend-to-xref t)
          (customize-set-value 'eglot-connect-timeout 600)

          ;; https://rust-analyzer.github.io/manual.html#eglot
          (add-to-list 'eglot-server-programs
                       '((rust-mode
                          rustc-mode
                          rustscript-mode
                          rust-ts-mode) .
                          ("~/.cargo/bin/rust-analyzer"
                           ;; ,#~(string-append #$rust:tools "")
                           :initializationOptions (:check (:command
                                                           "clippy"
                                                           ;; ,#~#$rust-clippy:tools
                                                           ;; $~$#rust-clippy:tools
                                                           ;; ,#~(string-append #$rust:tools "/bin/cargo-clippy")
                                                           )))))

          (setq rust-rustfmt-bin    "~/.cargo/bin/rustfmt"
                rust-cargo-bin      "~/.cargo/bin/cargo"
                rust-format-on-save t
                rust-rustfmt-switches '("--edition" "2024"))

          (add-hook 'eglot-managed-mode-hook 'rde--set-consult-imenu-cache-to-nil)
          (add-hook 'rust-mode-hook       'eglot-ensure) ;; source: https://rust-analyzer.github.io/manual.html#eglot
          (add-hook 'rustc-mode-hook      'eglot-ensure) ;; also: https://github.com/joaotavora/eglot#connecting-automatically
          (add-hook 'rustscript-mode-hook 'eglot-ensure)
          (add-hook 'rust-ts-mode         'eglot-ensure))


        ;; Clippy flymake
        ;; post's
        ;; flymake-clippy's https://github.com/emacsmirror/flymake-clippy?tab=readme-ov-file#complete-eglot--rust-mode--use-package-example
        (defun rde--clippy-flymake-manually-activate-flymake ()
          "Shim for working around eglot's tendency to suppress flymake backends."
          (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
          (flymake-mode 1))

        (defun rde--eglot-dont-supress-flymake ()
          (setq-local eglot-stay-out-of '(eldoc flymake)))

        (with-eval-after-load 'flymake-clippy
          (add-hook 'eglot-managed-mode-hook 'rde--clpppy-flymake-manually-activate-flymake nil t)
          (add-hook 'rust-mode-hook 'rde--eglot-dont-supress-flymake)
          (add-hook 'rust-mode-hook 'rde--clippy-flymake-manually-activate-flymake)
          (add-hook 'rust-mode-hook 'flymake-clippy-setup-backend)
          ;; TODO for other rust*-mode
          )

        ;; (require 'eglot)

        ;; (eval-when-compile
        ;;  (require 'flymake-clippy))


        ;; blog posts's https://unwoundstack.com/blog/emacs-as-a-rust-ide.html
        ;; (defun sp1ff/rust/mode-hook ()
        ;;   "My rust-mode hook"
        ;;   (column-number-mode)
        ;;   (display-line-numbers-mode)
        ;;   (hs-minor-mode)
        ;;   (smartparens-mode)
        ;;   (define-key rust-mode-map "\C-ca" 'eglot-code-actions)
        ;;   (define-key rust-mode-map (kbd "C-<right>")   'sp-forward-slurp-sexp)
        ;;   (define-key rust-mode-map (kbd "C-<left>")    'sp-forward-barf-sexp)
        ;;   (define-key rust-mode-map (kbd "C-M-<right>") 'sp-backward-slurp-sexp)
        ;;   (define-key rust-mode-map (kbd "C-M-<left>")  'sp-backward-barf-sexp)
        ;;   (define-key rust-mode-map "\C-c>" 'hs-show-all)
        ;;   (define-key rust-mode-map "\C-c<" 'hs-hide-all)
        ;;   (define-key rust-mode-map "\C-c;" 'hs-toggle-hiding)
        ;;   (define-key rust-mode-map "\C-c'" 'hs-hide-level)
        ;;   (setq indent-tabs-mode nil
        ;;         tab-width 4
        ;;         c-basic-offset 4
        ;;         fill-column 100))

        ;; (add-hook 'rust-mode 'sp1ff/rust/mode-hook)

        ;; Rustic
        ;;
        ;; (with-eval-after-load
        ;;     'rustic ;; not tested, is it 'rustic-mode'?
        ;;   (setq rustic-analyzer-command '("~/gits/abcdw/rde/examples/andrewzhurov/config/rust/docker-lsp-server/rust-analyzer.sh")) ;; found in https://github.com/brotzeit/rustic#server
        ;;   (setq rustic-lsp-server 'rust-analyzer)
        ;;   (setq rustic-lsp-client 'eglot) ;; found in https://github.com/brotzeit/rustic#client

        ;;   ;; cargo bin path to rustic, as shown how to here https://github.com/brotzeit/rustic#cargo
        ;;   ;; does it work?
        ;;   (setq rustic-cargo-bin "~/gits/abcdw/rde/examples/andrewzhurov/config/rust/docker-lsp-server/cargo.sh"))


        ;; already enabled by smartparens feature
        ;; (use-package smartparens :ensure t
        ;;              :config (require 'smartparens-rust))
        )

      #:summary ""
      #:commentary ""
      #:keywords '(convenience rust)
      #:elisp-packages
      (list emacs-rust-mode
            ;; emacs-rustic
            emacs-eglot-x
            emacs-flymake-clippy
            emacs-smartparens
            ;; emacs-jsonrpc ;; https://www.reddit.com/r/emacs/comments/1d0g29e/comment/l5v2eln/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
            ))))

  (feature
   (name 'rust-foreign)
   (values `((rust-foreign . #t)))
   (home-services-getter get-home-services)))
