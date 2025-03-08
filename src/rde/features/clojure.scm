;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2022, 2023, 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde features clojure)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde packages clojure)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages java)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde packages emacs-xyz)
  #:use-module (guix gexp)

  #:export (feature-clojure))

;; https://practical.li/spacemacs/ :: some emacs clojure tips

;; (define-public clojure-tools
;;   (package
;;     (name "clojure-tools")
;;     (version "1.11.3.1463")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (string-append "https://download.clojure.org/install/clojure-tools-"
;;                            version
;;                            ".tar.gz"))
;;        (sha256 (base32 "1q0z71ifdxwvyy9gvq8mx8jbygf8cszrlhb3h22walfamnisbhwk"))
;;        ;; Remove AOT compiled JAR.  The other JAR only contains uncompiled
;;        ;; Clojure source code.
;;        (snippet
;;         `(delete-file ,(string-append "clojure-tools-" version ".jar")))))
;;     (build-system copy-build-system)
;;     (arguments
;;      `(#:install-plan
;;        '(("deps.edn" "lib/clojure/")
;;          ("example-deps.edn" "lib/clojure/")
;;          ("tools.edn" "lib/clojure/")
;;          ("exec.jar" "lib/clojure/libexec/")
;;          ("clojure" "bin/")
;;          ("clj" "bin/"))
;;        #:modules ((guix build copy-build-system)
;;                   (guix build utils)
;;                   (srfi srfi-1)
;;                   (ice-9 match))
;;        #:phases
;;        (modify-phases %standard-phases
;;          (add-after 'unpack 'fix-paths
;;            (lambda* (#:key outputs #:allow-other-keys)
;;              (substitute* "clojure"
;;                (("PREFIX") (string-append (assoc-ref outputs "out") "/lib/clojure")))
;;              (substitute* "clj"
;;                (("BINDIR") (string-append (assoc-ref outputs "out") "/bin"))
;;                (("rlwrap") (which "rlwrap")))))
;;          (add-after 'fix-paths 'copy-tools-deps-alpha-jar
;;            (lambda* (#:key inputs outputs #:allow-other-keys)
;;              (substitute* "clojure"
;;                (("\\$install_dir/libexec/clojure-tools-\\$version\\.jar")
;;                 (string-join
;;                  (append-map (match-lambda
;;                                ((label . dir)
;;                                 (find-files dir "\\.jar$")))
;;                              inputs)
;;                  ":"))))))))
;;     (inputs (list rlwrap
;;                   clojure
;;                   clojure-tools-deps
;;                   java-commons-logging-minimal))
;;     (home-page "https://clojure.org/releases/tools")
;;     (synopsis "CLI tools for the Clojure programming language")
;;     (description "The Clojure command line tools can be used to start a
;; Clojure repl, use Clojure and Java libraries, and start Clojure programs.")
;;     (license license:epl1.0)))

(define* (feature-clojure
          #:key
          (clojure-tools clojure-tools)
          (clojure-lsp (@ (rde packages clojure) clojure-lsp))
          (eglot-stay-out-of '(flymake eldoc))
          (jdk (list jbr21 "jdk"))
          (clj-deps-new-key "J")
          (leiningen #f))
  "Setup and configure an environment for Clojure.
If you want Leiningen support, make sure to pass in the LEININGEN package."
  (ensure-pred file-like? clojure-tools)
  (ensure-pred maybe-file-like? clojure-lsp)
  ;; (ensure-pred file-like? jdk)
  (ensure-pred list? eglot-stay-out-of)
  (ensure-pred string? clj-deps-new-key)
  (ensure-pred maybe-file-like? leiningen)

  (define (get-home-services config)
    (define emacs-f-name 'clojure)
    (define clojure-lsp-binary
      (if (file-like? clojure-lsp)
          (file-append clojure-lsp "/bin/clojure-lsp")
          clojure-lsp))

    (append
      (list
       (simple-service
        'add-clojure-packages
        home-profile-service-type
        (append
         (if leiningen
             (list leiningen)
           '())
         (list
          ;; for go-to-definition
          ;; MAYBE: Add as a dependency to cider?
          (@ (gnu packages compression) unzip)
          clojure-tools ;; broken as of 2024-07-14
          jdk))))
      (if leiningen
          (list
           (simple-service
            'add-leiningen-xdg-home-envs
            home-environment-variables-service-type
            '(("LEIN_HOME" . "$XDG_DATA_HOME/lein"))))
          '())
      ;; https://github.com/DogLooksGood/meomacs/blob/master/programming.org#fix-clojure-syntax-highlighting
      (if (get-value 'emacs config #f)
          (list
           (rde-elisp-configuration-service
            emacs-f-name
            config
            `((defun rde--clojure-disable-eglot-parts-in-favor-of-cider ()
                (setq-local eglot-stay-out-of ',eglot-stay-out-of))
              (add-hook 'clojure-mode-hook
                        'rde--clojure-disable-eglot-parts-in-favor-of-cider)

              ;; TODO: Check if this hack is still needed
              (with-eval-after-load 'consult-imenu
                (add-to-list
                 'consult-imenu-config
                 ;; https://github.com/clojure-lsp/clojure-lsp/blob/13487d1eb0d06596565f76b8f6d76d00b7e9f03b/cli/integration-test/integration/initialize_test.clj#L62
                 '(clojure-mode
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

              (with-eval-after-load 'orderless
                (defun rde-orderless-clojure (component)
                  "Match ns and symbol separately by changing `/' to `.*'."
                  (orderless--separated-by
                      '(zero-or-more nonl)
                    (split-string component "/")))

                (defun rde--setup-clojure-orderless-matching-style ()
                  (make-local-variable 'orderless-matching-styles)
                  (add-hook 'orderless-matching-styles
                            'rde-orderless-clojure 0 t))

                (add-hook 'cider-mode-hook
                          'rde--setup-clojure-orderless-matching-style))

              ,@(if clojure-lsp-binary
                    `((with-eval-after-load 'eglot
                        (add-to-list
                         'eglot-server-programs
                         '(((clojure-mode :language-id "clojure")
                            (clojurec-mode :language-id "clojure")
                            (clojurescript-mode :language-id "clojurescript"))
                           . (,clojure-lsp-binary)))))
                    '())

              ;; MAYBE: Move it to configure-rde-emacs?  It's very generic and
              ;; can be useful in many other situations.
              (add-hook 'after-init-hook 'jarchive-setup)

              (with-eval-after-load 'cider
                (setq cider-allow-jack-in-without-project t)
                (setq cider-offer-to-open-cljs-app-in-browser nil))

              (with-eval-after-load 'cider-mode
                ;; Make cider-completion work together with orderless and eglot
                ;; https://github.com/clojure-emacs/cider/issues/3019
                ;; https://github.com/oantolin/orderless/issues/89
                (require 'cider-completion)
                (defun rde--cider-complete-at-point ()
                  "Complete the symbol at point."
                  (require 'thingatpt)
                  (when (and (cider-connected-p)
                             (not (cider-in-string-p)))
                    (let* ((bounds (or (bounds-of-thing-at-point 'symbol)
                                       ;; It may be too expensive to calculate it every time
                                       (cons (point) (point))))
                           (beg (car bounds))
                           (end (cdr bounds))
                           (completion
                            (append
                             (cider-complete
                              ;; Use only namespace as a prefix for nrepl completions,
                              ;; the rest will be filtered with orderless
                              (replace-regexp-in-string
                               "/.*" "/" (buffer-substring beg end)))
                             (get-text-property (point) 'cider-locals))))
                      (list beg end (completion-table-dynamic
                                     (lambda (_) completion))
                            :annotation-function 'cider-annotate-symbol))))

                (advice-add 'cider-complete-at-point
                            :override 'rde--cider-complete-at-point)

                ,@(if (get-value 'clojure-lsp config #f)
                      `((setq cider-use-xref nil))  ;; eglot will handle it
                      '())
                (setq cider-auto-select-error-buffer nil)
                (setq cider-inspector-auto-select-buffer nil)
                (setq cider-auto-select-test-report-buffer nil)
                (setq cider-print-options '(("right-margin" 70) ("length" 50)))
                (setq cider-doc-auto-select-buffer nil))

              (with-eval-after-load 'cider-repl
                (define-key cider-repl-mode-map (kbd "C-M-q") 'indent-sexp)
                (setq cider-repl-pop-to-buffer-on-connect 'display-only)
                ,@(if (get-value 'emacs-advanced-user? config #f)
                      '((setq cider-repl-display-help-banner nil))
                      '()))

              (with-eval-after-load 'rde-keymaps
                (define-key rde-app-map (kbd ,clj-deps-new-key) 'clj-deps-new))

              ,@(if (get-value 'emacs-org config #f)
                    '((with-eval-after-load 'org
                        (add-to-list 'org-structure-template-alist
                                     '("clj" . "src clojure")))
                      (with-eval-after-load 'ob-core
                        (require 'ob-clojure)
                        (require 'ob-java)
                        (setq org-babel-default-header-args:clojure
                              '((:results . "scalar")
                                (:session . ""))))
                      (with-eval-after-load 'ob-clojure
                        (setq org-babel-clojure-backend 'cider)))
                    '())

              (with-eval-after-load 'clojure-mode
                (setq clojure-align-forms-automatically t)
                (add-hook 'clojure-mode-hook 'eglot-ensure)

                (require 'flymake-kondor)
                (with-eval-after-load 'flymake-kondor
                  (add-hook 'clojure-mode-hook       'flymake-kondor-setup)
                  (add-hook 'clojure-mode-hook       (lambda () (flymake-mode t)))))
              (require 'clojure-mode))
            #:summary "\
Clojure(Script) code style, CIDER, LSP, imenu and other tweaks"
            #:commentary "\
Configure eglot, imenu, CIDER, flymake and other packages.
"
            #:keywords '(convenience clojure)
            #:elisp-packages
            (list emacs-cider emacs-clojure-mode
                  emacs-jarchive emacs-html-to-hiccup
                  emacs-clj-deps-new
                  emacs-flymake-kondor)))
          '())))

  (feature
   (name 'clojure)
   (values `((clojure . #t)
             (clojure-lsp . ,clojure-lsp)))
   (home-services-getter get-home-services)))
