;; andrewzhurov's paredit setup
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t) ;; required for barf and slurp to work
(enable-paredit-mode)
(global-set-key (kbd "M-l") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "M-h") 'paredit-forward-barf-sexp)
(disable-paredit-mode) ;; it gives an error on sp C-M-n

(with-eval-after-load
  'consult
  (setq consult-async-min-input 2))

(with-eval-after-load
  'dired
  (defun dired-subtree-cycle-recursive (&optional max-depth)
    (interactive "P")
    (save-excursion
      (cond
       ;; if not expanded show subtrees up to max-depth
       ((not (dired-subtree--is-expanded-p))
        (dired-subtree--insert-recursive 1 (if (integerp max-depth) max-depth nil)))

       ;; if expanded hide subtree
       ((dired-subtree--is-expanded-p)
        (dired-next-line 1)
        (dired-subtree-remove)))))
  (let ((map dired-mode-map))
    (define-key map (kbd "TAB") 'dired-subtree-toggle) ;; I don't like 'dired-subtree-cycle due to its complex behaviour, feels better decomplect it onto two different shortcuts
    (define-key map (kbd "M-TAB") 'dired-subtree-cycle-recursive)))


;; setup clj-kondo via flycheck, as described here: https://github.com/borkdude/flycheck-clj-kondo
(require 'flycheck-clj-kondo)
(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
;; (dolist (checkers '((clj-kondo-clj . clojure-joker)
;;                     (clj-kondo-cljs . clojurescript-joker)
;;                     (clj-kondo-cljc . clojure-joker)
;;                     (clj-kondo-edn . edn-joker)))
;;   (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))

(with-eval-after-load
    'clojure-mode
  (add-hook 'clojure-mode 'flycheck-mode)
  (add-hook 'clojurec-mode 'flycheck-mode))

(with-eval-after-load
    'clojurescript-mode
  (add-hook 'clojurescript-mode 'flycheck-mode))
(global-flycheck-mode)
(with-eval-after-load
    'flycheck
  ;; based on: https://emacs.stackexchange.com/a/21207
  (setq flycheck-javascript-eslint-executable "/home/user1/gits/andrewzhurov/comunica/node_modules/eslint/bin/eslint.js"
        ;; "npx eslint" ;; isn't considered to be an executable by eslint
        ))


(with-eval-after-load
    'minimap
  (setq minimap-width-fraction 0.05)
  (setq minimap-update-delay 0)
  (setq minimap-highlight-line nil)
  (setq minimap-hide-fringes t)
  (defface minimap-font-face
    '((default :family "TrueType" :height 1))
    "Face used for text in minimap buffer, notably the font family and height.
This height should be really small.  You probably want to use a
TrueType font for this.  After changing this, you should
recreate the minimap to avoid problems with recentering."
    :group 'minimap))

;; this been commented
;; lsp setup, so it can be modded by clojure-lsp
;; registering that var, since it's expected by clojure-lsp setup
;; this var declaration has been found at https://emacs-lsp.github.io/lsp-mode/page/adding-new-language/
;; (defvar lsp-language-id-configuration
;;   '(
;;     ))
;; if you are adding the support for your language server in separate repo use
;; (add-to-list 'lsp-language-id-configuration '(python-mode . "python"))

;; this hasn't been commented
;; clojure-lsp setup from https://clojure-lsp.io/clients/#emacs
;; (setenv "PATH" (concat
;;                 "/usr/local/bin" ;; for 'clojure'
;;                 path-separator
;;                 (getenv "PATH")))

;; ;; as found in https://emacs-lsp.github.io/lsp-mode/page/adding-new-language/#registering-server
;; (setq lsp-language-id-configuration '((clojure-mode . "clojure")
;;                                       (clojurescript-mode . "clojure")
;;                                       (clojurec-mode . "clojure")
;;                                       (clojurex-mode . "clojure")))

;; (setq lsp-clojure-server-command '("/home/user1/gits/clojure-lsp/clojure-lsp/clojure-lsp")) ;; hardcoded


(with-eval-after-load
    'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda () (setq consult-imenu--cache nil)))
  (add-to-list 'eglot-server-programs '(rustic-mode . ("~/gits/abcdw/rde/rde/examples/abcdw/config/rust/docker-lsp-server/rust-analyzer.sh")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("~/gits/abcdw/rde/rde/examples/abcdw/config/rust/docker-lsp-server/rust-analyzer.sh")))
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1))) ;; found in https://github.com/brotzeit/rustic#eglot
  (add-hook 'clojure-mode-hook 'eglot-ensure) ;; source: https://hyp.is/Bc_xhpTUEe2jgWN8emZ1jw/joaotavora.github.io/eglot/
  (add-hook 'clojurescript-mode-hook 'eglot-ensure)
  (add-hook 'clojurec-mode-hook 'eglot-ensure)
  (customize-set-variable 'eglot-extend-to-xref t)
  (customize-set-value 'eglot-connect-timeout 600))



;; configure rust with rustic
(add-hook 'rustic-mode 'eglot-ensure) ;; connect automatically, found in https://github.com/joaotavora/eglot#connecting-automatically
(add-hook 'rust-mode 'eglot-ensure)
(with-eval-after-load
    'rustic ;; not tested, is it 'rustic-mode'?
  (setq rustic-analyzer-command '("~/gits/abcdw/rde/rde/examples/abcdw/config/rust/docker-lsp-server/rust-analyzer.sh")) ;; found in https://github.com/brotzeit/rustic#server
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-lsp-client 'eglot) ;; found in https://github.com/brotzeit/rustic#client

  ;; cargo bin path to rustic, as shown how to here https://github.com/brotzeit/rustic#cargo
  ;; does it work?
  (setq rustic-cargo-bin "~/gits/abcdw/rde/rde/examples/abcdw/config/rust/docker-lsp-server/cargo.sh"))


;; Daitaas indents, supplied by DR
;; (with-eval-after-load 'clojure-mode
;;   (define-clojure-indent
;;    ;; Fulcro
;;    (>defn :defn)
;;    (defmutation [1 :form :form [1]])
;;    ;; (pc/defmutation [2 :form :form [1]])

;;    ;; Fulcro-spec
;;    (specification [1])
;;    (component [1])
;;    (behavior [1])
;;    (when-mocking '(0))
;;    (assertions [0])

;;    (thrown-with-data? [1])
;;    (not-thrown-with-data? [1])

;;    ;; Datomic
;;    (not-join 1)

;;    ;; JRA
;;    (system/let [1])
;;    (clet [1])
;;    (sp/collected? 1)
;;    (sp/cond-path :defn)
;;    (sp/if-path :defn)
;;    (sp/recursive-path :defn)
;;    (load-marker-utils/capture-load-marker-states 1)

;;    (swap!-> [1])

;;    (comment :defn)

;;    (m/search 1)

;;    ;; compojure
;;    (context 2)
;;    (POST 2)
;;    (GET 2)
;;    (PUT 2)))


;; based on
;; source - https://orgmode.org/worg/org-hacks.html
;; source - https://stackoverflow.com/a/17438212
(defun my-org-insert-from-clipboard ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  ;; TODO: assert for Wayland
  (let* ((hash (string-trim-right (shell-command-to-string "wl-paste | sha256sum | sed 's/[- ]//g'"))) ;; TODO: get values from guix store
         (store-path "~/notes/org-roam/store/sha256/") ;; TODO: parameterize from guix env var
         (store-item-path (concat store-path hash)))
    (shell-command (concat "mkdir -p " store-path))
    (shell-command (concat "wl-paste > " store-item-path))
    (insert (concat "[[" store-item-path "]]")))
  ;; (org-display-inline-images)
  )
