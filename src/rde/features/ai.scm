(define-module (rde features ai)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde predicates)

  #:use-module (rde packages)
  #:use-module (rde packages rust)
  #:use-module (rde packages emacs-xyz)

  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages rust-crates)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages graphviz)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix ui)

  #:use-module (srfi srfi-1)

  #:export (feature-ai))


;; gptel backend constructors, keyed by the name accepted in BACKENDS.
;; Deliberately no :key here -- see `backend-form'.
(define %ai-backends
  '((sambanova
     . (gptel-make-openai "Sambanova"
         :host "api.sambanova.ai"
         :endpoint "/v1/chat/completions"
         :stream t
         :models '(DeepSeek-R1)))
    (aiml
     . (gptel-make-openai "AI/ML API"
         :host "api.aimlapi.com"
         :endpoint "/v1/chat/completions"
         :stream t
         :models '(deepseek-chat gemini-pro gpt-4o)))
    (deepseek
     . (gptel-make-deepseek "DeepSeek"
         :stream t))))

(define (backend-form name)
  ;; The key is looked up via auth-source when a request is made, so it
  ;; never ends up in the store.  An explicit :key is required: without it
  ;; gptel sends no auth header at all.
  (append (assq-ref %ai-backends name)
          '(:key (function gptel-api-key-from-auth-source))))

(define* (feature-ai #:key (backends '()) (default-backend #f))
  "Configure gptel with the GitHub Copilot backend plus the API BACKENDS, a
list of symbols out of @code{sambanova}, @code{aiml} and @code{deepseek}.
DEFAULT-BACKEND, when set, must be one of BACKENDS and becomes
@code{gptel-backend}.

API keys are not part of the configuration.  gptel looks them up in
auth-source when a request is made, using the backend host with login
@code{apikey}, e.g. in @file{~/.authinfo.gpg}:

@example
machine api.deepseek.com login apikey password sk-...
machine api.sambanova.ai login apikey password ...
machine api.aimlapi.com  login apikey password ...
@end example

or, with @code{feature-password-store}, pass entries such as
@file{api.deepseek.com/apikey}."
  (ensure-pred list? backends)
  (ensure-pred maybe-symbol? default-backend)

  (let ((unknown (remove (lambda (b) (assq b %ai-backends)) backends)))
    (unless (null? unknown)
      (raise (formatted-message
              (G_ "feature-ai: unknown backends ~a, known ones are ~a")
              unknown (map car %ai-backends)))))
  (when (and default-backend (not (memq default-backend backends)))
    (raise (formatted-message
            (G_ "feature-ai: default-backend ~a is not among backends ~a")
            default-backend backends)))

  (define (get-home-services config)
    (define emacs-f-name 'ai)

    (list
     (simple-service
      'ai-add-packages
      home-profile-service-type
      (list))

     ;; https://github.com/DogLooksGood/meomacs/blob/master/programming.org#fix-rust-syntax-highlighting
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'gptel
          (setq gptel-model 'gpt-4o)

          ;; enable back-ends
          (gptel-make-gh-copilot "Copilot")

          ,@(map (lambda (name)
                   (if (eq? name default-backend)
                       `(setq gptel-backend ,(backend-form name))
                       (backend-form name)))
                 backends)))
      #:summary ""
      #:commentary ""
      #:keywords '(convenience ai)
      #:elisp-packages
      (list emacs-gptel
            ;; emacs-gptel-quick
            ;; emacs-gptel-prompts
            ))))

  (feature
   (name 'ai)
   (values `((ai . #t)))
   (home-services-getter get-home-services)))
