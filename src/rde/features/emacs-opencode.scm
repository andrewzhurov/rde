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
;;;    #:server-url "http://localhost:4098"
;;;    #:server-instances
;;;    '(("isolated" . "http://localhost:4097")))

(define-module (rde features emacs-opencode)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
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

(define* (emacs-opencode-from-source source #:key (version "0.4.0"))
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
          (server-instances '())
          (select-agent-key "C-c o a")
          (message-key "C-c o m")
          (compose-key "C-c o c")
          (which-agent-key "C-c o w"))
  "Configure the OpenCode Emacs pairing client.

OPENCODE-EL-SOURCE is a file-like containing opencode.el; it defaults to
the copy vendored next to this feature.  SERVER-URL is the default OpenCode
daemon base URL (the daily-driver dev instance serves on
http://localhost:4098).  SERVER-INSTANCES is an alist of named additional
daemon base URLs considered by prefix selection.  Candidate daemons must
share Emacs's filesystem path namespace.  SELECT-AGENT-KEY, MESSAGE-KEY,
COMPOSE-KEY and WHICH-AGENT-KEY are global keybindings for the commands.

SELECT-AGENT-KEY runs `opencode-select-agent': plain it picks among the
default instance's current-project agents; with a prefix arg (C-u) it first
offers the running configured instances (when multiple respond), then a project
and one of its agents.  The pick is remembered against the buffer's project
root and as a global sticky default (used from buffers whose root has no
selection of its own).
MESSAGE-KEY runs `opencode-message' (one-shot send); COMPOSE-KEY runs
`opencode-compose' (accumulate several takes in a buffer, send on
C-c C-c).  Magit and Majutsu regions capture exact selected diff text with
old/new source lines; repeated compose captures deduplicate unchanged project
and revision headers.  WHICH-AGENT-KEY runs `opencode-which-agent'."
  (define (http-server-url? value)
    (and (string? value)
         (not (string-any char-whitespace? value))
         (or (string-prefix? "http://" value)
             (string-prefix? "https://" value))
         (> (string-length value)
            (if (string-prefix? "https://" value) 8 7))))

  (ensure-pred file-like? opencode-el-source)
  (ensure-pred http-server-url? server-url)
  (ensure-pred
   (lambda (instances)
     (and (list? instances)
          (every (lambda (instance)
                   (and (pair? instance)
                        (string? (car instance))
                        (string-any (lambda (char)
                                      (not (char-whitespace? char)))
                                    (car instance))
                        (http-server-url? (cdr instance))))
                 instances)))
   server-instances)
  (ensure-pred string? select-agent-key)
  (ensure-pred string? message-key)
  (ensure-pred string? compose-key)
  (ensure-pred string? which-agent-key)

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
        (setq opencode-server-instances ',server-instances)
        (define-key global-map (kbd ,select-agent-key) 'opencode-select-agent)
        (define-key global-map (kbd ,message-key) 'opencode-message)
        (define-key global-map (kbd ,compose-key) 'opencode-compose)
        (define-key global-map (kbd ,which-agent-key) 'opencode-which-agent)
        ;; The agent's file edits flow back automatically.
        (global-auto-revert-mode 1))
      #:summary "Pair with an OpenCode agent from Emacs"
      #:commentary "\
Selects a running OpenCode agent — of the default instance and current
project, or (with a prefix arg) of any configured running instance and
project — and sends it contextual messages (region/point + file), one-shot
or by accumulating several takes in a compose buffer.  Feedback is viewed in
the OpenCode web UI."
      #:keywords '(tools convenience)
      #:elisp-packages (list emacs-opencode))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
