;; (require 'rde-core)
;; (require 'rde-variables)
;; (require 'rde-faces)
;; (require 'rde-org-roam)
;; (require 'rde-modus-themes)

;; (require 'orderless)
;; (setq completion-styles '(orderless))

(setq org-adapt-indentation nil)
(setq org-edit-src-content-indentation 0)

(setq user-full-name "Andrei Zhurau")
(setq user-mail-address "zhurov.andrew@gmail.com")
(setq message-auto-save-directory "~/.cache/mail-drafts")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d2db4af7153c5d44cb7a67318891e2692b8bf5ddd70f47ee7a1b2d03ad25fcd9" "a10ca93d065921865932b9d7afae98362ce3c347f43cb0266d025d70bec57af1" default))
 '(safe-local-variable-values
   '((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (eval let
	   ((root-dir-unexpanded
	     (locate-dominating-file default-directory ".dir-locals.el")))
	   (when root-dir-unexpanded
	     (let*
		 ((root-dir
		   (expand-file-name root-dir-unexpanded))
		  (root-dir*
		   (directory-file-name root-dir)))
	       (unless
		   (boundp 'geiser-guile-load-path)
		 (defvar geiser-guile-load-path 'nil))
	       (make-local-variable 'geiser-guile-load-path)
	       (require 'cl-lib)
	       (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal))))
     (eval setq-local guix-directory
	   (locate-dominating-file default-directory ".dir-locals.el"))))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)

;(define-key key-translation-map [?\C-x] [?\C-u])
;(define-key key-translation-map [?\C-u] [?\C-x])
;(define-key key-translation-map [?\M-x] [?\M-u])
;(define-key key-translation-map [?\M-u] [?\M-x])

(require 'evil)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(global-set-key (kbd "M-l") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "M-h") 'paredit-forward-barf-sexp)


;; (use-package aggressive-indent-mode
;;   :defer t
;;   :ensure t)

;; (use-package clojure-mode
;;   :defer t
;;   :ensure t
;;   :hook ((clojure-mode-hook . paredit-mode)
;; 	 (clojure-mode-hook . smartparens-strict-mode)
;; 	 (clojure-mode-hook . aggressive-indent-mode)))
