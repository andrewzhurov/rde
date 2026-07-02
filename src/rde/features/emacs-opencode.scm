;;; rde --- Reproducible development environment.
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; OpenCode pairing feature for Emacs.
;;;
;;; feature-emacs-opencode wires opencode.el (the Emacs pairing client)
;;; into an RDE Emacs configuration: installs the elisp package, requires
;;; it, sets the server URL, and binds the two commands.
;;;
;;; opencode.el is a thin remote control — it captures editor context
;;; (project, file, point, selection) and hands a contextual message to a
;;; selected OpenCode agent.  The rich conversation stays in the web UI.
;;;
;;; The elisp lives next to this file as opencode.el and is vendored into
;;; rde, so the feature is self-contained (no external load paths or
;;; checkouts required).  Usage in keeper.scm:
;;;
;;;   (feature-emacs-opencode
;;;    #:server-url "http://localhost:4098")

(define-module (rde features emacs-opencode)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (feature-emacs-opencode
            emacs-opencode-from-source))


;;; ---------------------------------------------------------------------------
;;; emacs-opencode package — built from the vendored elisp.

(define* (emacs-opencode-from-source source #:key (version "0.1.0"))
  "Package opencode.el from SOURCE (a file-like pointing at opencode.el).
Pure Emacs Lisp, built-in deps only (url.el, json.el, project, vc)."
  (package
    (name "emacs-opencode")
    (version version)
    (source source)
    (build-system emacs-build-system)
    (home-page "https://github.com/sst/opencode")
    (synopsis "Pair with an OpenCode agent from Emacs")
    (description
     "A thin remote control for consulting an OpenCode agent while navigating
a project in Emacs.  Captures editor context (project, file, point, selection)
and sends a contextual message to a selected agent; the rich conversation
stays in the OpenCode web UI.")
    (license license:expat)))


;;; ---------------------------------------------------------------------------
;;; feature-emacs-opencode

(define* (feature-emacs-opencode
          #:key
          (opencode-el-source (local-file "./opencode.el"))
          (server-url "http://localhost:4098")
          (select-agent-key "C-c o a")
          (message-key "C-c o m"))
  "Configure the OpenCode Emacs pairing client.

OPENCODE-EL-SOURCE is a file-like containing opencode.el; it defaults to
the copy vendored next to this feature.  SERVER-URL is the OpenCode
daemon base URL (the daily-driver dev instance serves on
http://localhost:4098).  SELECT-AGENT-KEY and MESSAGE-KEY are global
keybindings for the two commands."
  (ensure-pred file-like? opencode-el-source)
  (ensure-pred string? server-url)
  (ensure-pred string? select-agent-key)
  (ensure-pred string? message-key)

  (define emacs-opencode (emacs-opencode-from-source opencode-el-source))

  (define emacs-f-name 'opencode)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'opencode)
        (setq opencode-server-url ,server-url)
        (define-key global-map (kbd ,select-agent-key) 'opencode-select-agent)
        (define-key global-map (kbd ,message-key) 'opencode-message)
        ;; The agent's file edits flow back automatically.
        (global-auto-revert-mode 1))
      #:summary "Pair with an OpenCode agent from Emacs"
      #:commentary "\
Selects a running OpenCode agent for the current project and sends it
contextual messages (region/point + file).  Feedback is viewed in the
OpenCode web UI."
      #:keywords '(tools convenience)
      #:elisp-packages (list emacs-opencode))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
