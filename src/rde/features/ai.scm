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

  #:use-module (guix gexp)

  #:export (feature-ai))


(define* (feature-ai #:key aiml-api-key sambanova-api-key deepseek-api-key)
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

          ,@(if sambanova-api-key
                `((gptel-make-openai "Sambanova" ;Any name you want
                                     :host "api.sambanova.ai"
                                     :endpoint "/v1/chat/completions"
                                     :stream t               ;for streaming responses
                                     :key ,sambanova-api-key ;can be a function that returns the key
                                     :models '(DeepSeek-R1)))
                `())

          ,@(if aiml-api-key
                `((gptel-make-openai "AI/ML API"
                                     :host "api.aimlapi.com"
                                     :endpoint "/v1/chat/completions"
                                     :stream t
                                     :key ,aiml-api-key
                                     :models '(deepseek-chat gemini-pro gpt-4o)))
                `())

          ,@(if deepseek-api-key
                `((setq gptel-backend
                        ;; default backend
                        (gptel-make-deepseek "DeepSeek"
                                             :stream t
                                             :key ,deepseek-api-key)))
                `())
          ))
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
