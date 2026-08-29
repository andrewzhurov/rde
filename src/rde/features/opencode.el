;;; opencode.el --- Pair with an OpenCode agent from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrew Zhurov

;; Author: Andrew Zhurov
;; Maintainer: Andrew Zhurov
;; URL: https://github.com/sst/opencode
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience

;; This file is part of OpenCode.

;;; Commentary:

;; A thin remote control for pairing with an OpenCode agent while you
;; navigate a project in Emacs.  It does NOT render the conversation —
;; the rich timeline lives in the OpenCode web UI, which you keep open
;; alongside Emacs.  Emacs's job is the one thing the web UI cannot do:
;; capture WHERE you are (project, file, point, selection) and hand a
;; contextual message to the agent you picked.
;;
;; Commands:
;;
;;   `opencode-select-agent'  Pick which running agent this buffer's
;;                            messages go to.  Plain: lists the agents of
;;                            the workspace the current directory belongs
;;                            to.  With a prefix arg (C-u): first pick ANY
;;                            running configured instance (when more than
;;                            one is available), then a project, then one
;;                            of its agents — so you can drive an agent from
;;                            another daemon or project without changing the
;;                            global default.
;;                            Create and manage agents in the web UI; this
;;                            only selects among the ones already there.
;;
;;   `opencode-message'       The one-shot gesture.  Saves the current
;;                            buffer (so the agent reads your latest),
;;                            captures editor context (active region with
;;                            line numbers, else point's line + file),
;;                            prompts for a message, and sends it to the
;;                            selected agent.  Watch the reply in the web UI.
;;
;;   `opencode-compose'       Same capture, different tempo: append the
;;                            context to a persistent compose buffer for the
;;                            selected agent instead of sending now.  Call it
;;                            repeatedly — from anywhere — to collect several
;;                            takes across a module; the buffer content is
;;                            exactly what will be sent.  C-c C-c sends the
;;                            whole buffer (then kills it); C-c C-k discards.
;;                            Magit/Majutsu selections include semantic old/new
;;                            source lines.  Repeated selections deduplicate
;;                            unchanged project and revision headers.
;;                            With a prefix arg (C-u), first pick the target
;;                            agent — any instance/project — as
;;                            `opencode-select-agent' does.
;;
;;   `opencode-which-agent'   Report which agent this buffer currently
;;                            messages, and how that was resolved.
;;
;; Selection model (two layers, checked in order):
;;
;;   A. Per-project-root.  Each project root (git worktree top level)
;;      remembers its own selected agent.  Buffers under a root share it.
;;
;;   B. Global sticky default.  The most recent pick — from anywhere — is
;;      also remembered globally and used as the fallback when the current
;;      root has no selection of its own.  This is what makes "I'm focused
;;      on ONE agent while editing across many directories" ergonomic.
;;
;; The chosen agent may belong to a DIFFERENT project than the buffer you
;; message from.  When it does, the context preamble quotes the file by
;; its absolute path (the agent locates files by absolute path via its
;; tools); for a same-project agent the path stays pretty and relative.
;;
;; The agent's file changes flow back into Emacs via
;; `global-auto-revert-mode' (enable it).  A buffer with unsaved edits to
;; a file the agent also rewrote is a genuine conflict: auto-revert
;; refuses to clobber it and warns — resolve it yourself.
;;
;; The mapping is an editor-local preference; it is not stored on the
;; server.  Talks to running daemons (e.g. `make -C dev daily-web' on
;; http://localhost:4098).  `opencode-server-url' is the default instance;
;; `opencode-server-instances' names additional instances offered by the
;; prefix selection flow.  Candidate daemons must share Emacs's filesystem
;; path namespace so captured file paths mean the same thing to the agent.

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)
(require 'subr-x)
(require 'project)
(require 'vc)
(require 'eieio)

(declare-function magit-section-at "magit-section" (&optional position))
(declare-function magit-rev-parse "magit-git" (&rest args))
(declare-function magit-rev-verify "magit-git" (rev))

(defgroup opencode nil
  "Pair with an OpenCode agent from Emacs."
  :group 'tools
  :prefix "opencode-")

(defcustom opencode-server-url "http://localhost:4098"
  "Base URL of the default OpenCode server to pair with.
The daily-driver dev instance serves its API on port 4098; the
isolated instance on 4097.  No trailing slash."
  :type 'string
  :group 'opencode)

(defcustom opencode-server-instances
  nil
  "Named additional OpenCode instances considered by prefix selection.
Each entry is (NAME . BASE-URL).  `opencode-server-url' is always added
as the first, \"default\" candidate, so it need not be repeated here.
Candidate daemons must share Emacs's filesystem path namespace."
  :type '(alist :key-type string :value-type string)
  :group 'opencode)

(defcustom opencode-instance-probe-timeout 3
  "Seconds to wait for each configured instance during discovery.
This short timeout applies only to prefix-selection discovery; ordinary
requests continue to use `opencode-request-timeout'."
  :type 'number
  :group 'opencode)

(defcustom opencode-request-timeout 10
  "Seconds to wait for an OpenCode HTTP request before giving up."
  :type 'integer
  :group 'opencode)

(defcustom opencode-include-line-numbers t
  "When non-nil, prefix quoted region/line context with line numbers."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-diff-line-markers
  '((removed . "-") (added . "+") (context . " "))
  "Markers used for removed, added, and preserved diff lines."
  :type '(alist :key-type (choice (const removed)
                                  (const added)
                                  (const context))
                :value-type string)
  :group 'opencode)

;;; A selection is a plist describing a chosen agent:
;;;
;;;   (:server-url     STRING  instance API base — part of target identity)
;;;    :instance-label STRING  human name for the instance)
;;;    :log-id         NUMBER  agent logID — unique only within an instance)
;;;    :created        NUMBER  immutable agent creation timestamp)
;;;    :project-id     STRING  project identity reported with the agent)
;;;    :workspace-id   STRING  selected workspace identity, when available)
;;;    :label          STRING  "<project> / <agent title>" for echoes/menus)
;;;    :directory      STRING) the agent's project directory, or nil — used to
;;;                            decide whether a buffer's file is same-project
;;;                            (pretty relative path) or cross-project (absolute).

(defvar opencode-agent-alist nil
  "Alist mapping a project root (string) to a selection plist.
Layer A of the selection model: which agent each project's buffers
currently message.  Populated by `opencode-select-agent'.")

(defvar opencode-agent-current nil
  "The most recently selected agent, as a selection plist, or nil.
Layer B of the selection model: the global sticky default, used as the
fallback when the current buffer's project root has no selection of its
own.  Populated by `opencode-select-agent'.")

(defvar opencode--instance-discovery-failures nil
  "Diagnostics for candidates omitted by the latest instance discovery.")

;;; ---------------------------------------------------------------------------
;;; Project root

(defun opencode--root ()
  "Return the canonical root directory of the current buffer's project.
Prefers the VC (git) top level — for a git worktree this is the
worktree's own root, which is exactly what the OpenCode server uses
as a project directory.  Falls back to `default-directory'."
  (let ((root (or (vc-root-dir)
                  (when-let ((proj (project-current nil)))
                    (project-root proj))
                  default-directory)))
    ;; `directory-file-name' strips the trailing slash so the alist key and
    ;; the server `directory` query are stable regardless of whether the root
    ;; came from `vc-root-dir' (slash-terminated) or elsewhere.
    (directory-file-name (file-truename (expand-file-name root)))))

;;; ---------------------------------------------------------------------------
;;; HTTP (url.el, no external deps)

(define-error 'opencode-error "OpenCode request failed")

(defun opencode--normalize-server-url (server-url)
  "Return SERVER-URL without trailing slashes."
  (replace-regexp-in-string "/+\\'" "" server-url))

(defun opencode--valid-server-url-p (server-url)
  "Return non-nil when SERVER-URL is a non-empty HTTP(S) base URL."
  (and (stringp server-url)
       (string-match-p "\\`https?://[^[:space:]]+\\'"
                       (opencode--normalize-server-url server-url))))

(defun opencode--url (server-url path)
  "Join PATH onto SERVER-URL."
  (concat (opencode--normalize-server-url server-url) path))

(defun opencode--parse-json-buffer ()
  "Parse the JSON body of the current `url-retrieve' response buffer.
Point is moved past the headers first.  Returns a parsed object using
alist objects and vector arrays, or signals `opencode-error'."
  (goto-char (point-min))
  (unless (re-search-forward "\n\n" nil t)
    (signal 'opencode-error (list "malformed HTTP response (no header/body split)")))
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (condition-case err
        (json-read)
      (error (signal 'opencode-error
                     (list (format "could not parse JSON: %s"
                                   (error-message-string err))))))))

(defun opencode--http-status ()
  "Return the integer HTTP status code of the current response buffer."
  (when (boundp 'url-http-response-status)
    url-http-response-status))

(defun opencode--get (server-url path &optional timeout)
  "GET PATH from SERVER-URL and return the parsed JSON body.
Synchronous.  Signals `opencode-error' on transport failure, timeout,
or a non-2xx status."
  (let* ((url-request-method "GET")
         (full-url (opencode--url server-url path))
         (buf (condition-case err
                  (url-retrieve-synchronously
                   full-url t nil (or timeout opencode-request-timeout))
                (error
                 (signal 'opencode-error
                         (list (format "GET %s: %s" full-url
                                       (error-message-string err))))))))
    (unless buf
      (signal 'opencode-error
              (list (format "GET %s: no response (is the server running?)"
                            full-url))))
    (unwind-protect
        (with-current-buffer buf
          (let ((status (opencode--http-status)))
            (when (and status (>= status 400))
              (signal 'opencode-error
                      (list (format "GET %s → HTTP %d" full-url status))))
            (opencode--parse-json-buffer)))
      (kill-buffer buf))))

(defun opencode--post-json (server-url path payload)
  "POST PAYLOAD (a Lisp object, JSON-encoded) to PATH on SERVER-URL.
Synchronous.  Returns the parsed JSON body when present, otherwise t.
Signals `opencode-error' on transport failure, timeout, or non-2xx."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (full-url (opencode--url server-url path))
         (buf (condition-case err
                  (url-retrieve-synchronously
                   full-url t nil opencode-request-timeout)
                (error
                 (signal 'opencode-error
                         (list (format "POST %s: %s" full-url
                                       (error-message-string err))))))))
    (unless buf
      (signal 'opencode-error
              (list (format "POST %s: no response (is the server running?)"
                            full-url))))
    (unwind-protect
        (with-current-buffer buf
          (let ((status (opencode--http-status)))
            (when (and status (>= status 400))
              (signal 'opencode-error
                      (list (format "POST %s → HTTP %d" full-url status))))
            ;; Body may be empty ({ ok: true } or 204) — tolerate both.
            (goto-char (point-min))
            (if (re-search-forward "\n\n" nil t)
                (if (looking-at-p "[ \t\n]*\\'") t (opencode--parse-json-buffer))
              t)))
      (kill-buffer buf))))

;;; ---------------------------------------------------------------------------
;;; Server reads

(defun opencode--fetch-logs (server-url directory)
  "Fetch from SERVER-URL the agent logs for DIRECTORY's workspace.
Returns the parsed { project, logs } object."
  (opencode--get
   server-url (concat "/agent/logs?directory=" (url-hexify-string directory))))

(defun opencode--fetch-workspaces (server-url &optional timeout)
  "Fetch SERVER-URL's full workspace catalog as a list of alists.
Newest-first.  Each entry has keys like `projectID', `workspaceID',
`directory', `worktree', `name', `projectName', `branch'."
  (let ((response (opencode--get server-url "/project/workspaces" timeout)))
    (unless (vectorp response)
      (signal 'opencode-error
              (list (format "GET %s/project/workspaces: expected a JSON array"
                            server-url))))
    (append response nil)))

(defun opencode--logs-vector (response)
  "Extract the logs vector from a /agent/logs RESPONSE."
  (or (alist-get 'logs response) []))

(defun opencode--project-null-p (response)
  "Non-nil when RESPONSE reports the directory is not an opened project."
  (let ((project (assq 'project response)))
    ;; project key present and its value is JSON null (nil in alist parse).
    (and project (null (cdr project)))))

(defun opencode--project-directory (response)
  "Return the project directory reported by a /agent/logs RESPONSE, or nil."
  (let ((project (alist-get 'project response)))
    (and project (alist-get 'directory project))))

(defun opencode--workspace-matches-response-p (workspace response)
  "Return non-nil when WORKSPACE is the project resolved in RESPONSE.
The directory-based agent endpoint does not return a workspaceID, so this
checks every identity fact it does expose: projectID, directory and worktree."
  (let ((project (alist-get 'project response)))
    (and project
         (equal (alist-get 'projectID workspace)
                (alist-get 'projectID project))
         (equal (alist-get 'directory workspace)
                (alist-get 'directory project))
         (equal (alist-get 'worktree workspace)
                (alist-get 'worktree project)))))

;;; ---------------------------------------------------------------------------
;;; Selection: pick an agent, remember it (layers A + B)

(defun opencode--project-label (project-directory)
  "A short human label for PROJECT-DIRECTORY (its basename), or \"?\"."
  (if (and project-directory (not (string-empty-p project-directory)))
      (file-name-nondirectory (directory-file-name project-directory))
    "?"))

(defun opencode--agent-selections
    (server-url instance-label query-directory
                &optional project-label expected-workspace workspace-catalog)
  "Return agent selections for QUERY-DIRECTORY on SERVER-URL.
INSTANCE-LABEL names the server.  PROJECT-LABEL, when given, names the
project in menu/echo text; otherwise it is derived from the resolved project
directory.  EXPECTED-WORKSPACE, when present, is the catalog identity being
resolved and must match the directory lookup; WORKSPACE-CATALOG is the catalog
it came from.  Signals `user-error' when the directory is not an open project,
resolves ambiguously, or changed identity."
  (unless (opencode--valid-server-url-p server-url)
    (user-error "OpenCode: invalid server URL: %S" server-url))
  (let ((response (opencode--fetch-logs server-url query-directory)))
    (when (opencode--project-null-p response)
      (user-error
       "OpenCode: %s is not an open project — open it in the web UI (%s) first"
       query-directory server-url))
    (let* ((project (alist-get 'project response))
           (matching-workspaces
            (seq-filter
             (lambda (candidate)
               (opencode--workspace-matches-response-p candidate response))
             (or workspace-catalog
                 (opencode--fetch-workspaces server-url))))
           (workspace (and (null (cdr matching-workspaces))
                           (car matching-workspaces)))
           (project-directory (opencode--project-directory response))
           (label (or project-label (opencode--project-label project-directory)))
           (logs (append (opencode--logs-vector response) nil)))
      (unless workspace
        (user-error
         "OpenCode: workspace lookup is stale or ambiguous on %s; select again"
         server-url))
      (when (and expected-workspace
                 (not (equal (alist-get 'workspaceID expected-workspace)
                             (alist-get 'workspaceID workspace))))
        (user-error
         "OpenCode: workspace identity changed on %s; select it again"
         server-url))
      (mapcar
       (lambda (log)
         (list :server-url (opencode--normalize-server-url server-url)
               :instance-label instance-label
               :log-id (alist-get 'logID log)
               :created (alist-get 'created log)
               :project-id (alist-get 'projectID project)
               :workspace-id (alist-get 'workspaceID workspace)
               :label (format "%s / %s"
                              label (alist-get 'title log "(untitled)"))
               :directory project-directory
               :worktree (alist-get 'worktree project)))
       logs))))

(defun opencode--pick-selection (server-url selections &optional project-label)
  "Prompt for one of SELECTIONS from SERVER-URL and return it.
PROJECT-LABEL, when given, names the already-selected project."
  (when (null selections)
    (user-error
     "OpenCode: no agents on %s yet — create one in the web UI first"
     server-url))
  (let ((choices
         (mapcar
          (lambda (selection)
            (cons (format "%s — %s (#%s)"
                          (plist-get selection :label)
                          (abbreviate-file-name
                           (plist-get selection :directory))
                          (plist-get selection :log-id))
                  selection))
          selections)))
    (cdr (assoc (completing-read (if project-label
                                     (format "Agent in %s on %s: "
                                             project-label server-url)
                                   (format "Agent on %s: " server-url))
                                 choices nil t)
                choices))))

(defun opencode--pick-agent
    (server-url instance-label query-directory
                &optional project-label expected-workspace workspace-catalog)
  "Pick an agent of QUERY-DIRECTORY's workspace on SERVER-URL."
  (opencode--pick-selection
   server-url
   (opencode--agent-selections
    server-url instance-label query-directory project-label
    expected-workspace workspace-catalog)))

(defun opencode--pick-project (server-url projects)
  "Prompt for one of SERVER-URL's PROJECTS and return its catalog row."
  (let ((choices
         (mapcar
          (lambda (project)
            (let* ((directory (alist-get 'directory project))
                   (label (or (alist-get 'projectName project)
                              (alist-get 'name project)
                              (opencode--project-label directory))))
              (cons (format "%s — %s"
                            label (abbreviate-file-name directory))
                    project)))
          projects)))
    (cdr (assoc (completing-read (format "Project on %s: " server-url)
                                 choices nil t)
                choices))))

(defun opencode--pick-instance-project-agent (instance)
  "Pick a project from INSTANCE, then one of that project's agents."
  (let ((server-url (plist-get instance :server-url))
        (instance-label (plist-get instance :name))
        (projects (plist-get instance :workspaces)))
    (let* ((project (opencode--pick-project server-url projects))
           (label (or (alist-get 'projectName project)
                      (alist-get 'name project)
                      (opencode--project-label
                       (alist-get 'directory project)))))
      (opencode--pick-selection
       server-url
       (opencode--agent-selections
        server-url instance-label (alist-get 'directory project)
        label project projects)
       label))))

(defun opencode--instance-candidates ()
  "Return normalized configured instance plists, deduplicated by URL.
The default instance is first."
  (let ((seen nil)
        (result nil))
    (dolist (entry (cons (cons "default" opencode-server-url)
                         opencode-server-instances))
      (if (not (and (consp entry) (stringp (car entry))
                    (opencode--valid-server-url-p (cdr entry))))
          (push (list :name (if (and (consp entry) (stringp (car entry)))
                                (car entry)
                              "invalid")
                      :server-url (and (consp entry) (cdr entry))
                      :configuration-error "expected a named HTTP(S) URL")
                result)
        (let ((name (car entry))
              (url (opencode--normalize-server-url (cdr entry))))
          (if (string-empty-p (string-trim name))
              (push (list :name "invalid"
                          :server-url url
                          :configuration-error "instance name is empty")
                    result)
            (unless (member url seen)
              (push url seen)
              (push (list :name name :server-url url) result))))))
    (nreverse result)))

(defun opencode--discover-instances ()
  "Return configured instances that expose a valid workspace catalog.
Each returned plist includes the already-fetched `:workspaces'.  Candidate
failures are isolated; if every candidate fails, signal one diagnostic
`user-error' listing all attempted instances."
  (let ((available nil)
        (failures nil))
    (dolist (candidate (opencode--instance-candidates))
      (let ((name (plist-get candidate :name))
            (server-url (plist-get candidate :server-url)))
        (if-let ((configuration-error
                  (plist-get candidate :configuration-error)))
            (push (format "%s (%s): %s" name server-url configuration-error)
                  failures)
          (condition-case err
              (let ((workspaces
                     (seq-filter
                      (lambda (workspace)
                        (let ((directory (alist-get 'directory workspace)))
                          (and directory (not (string-empty-p directory)))))
                      (opencode--fetch-workspaces
                       server-url opencode-instance-probe-timeout))))
                (if workspaces
                    (push (append candidate (list :workspaces workspaces))
                          available)
                  (push (format "%s (%s): no open workspaces" name server-url)
                        failures)))
            (error
             (push (format "%s (%s): %s" name server-url
                           (error-message-string err))
                   failures))))))
    (setq opencode--instance-discovery-failures (nreverse failures))
    (unless available
      (user-error "OpenCode: no configured instance is usable: %s"
                  (string-join opencode--instance-discovery-failures "; ")))
    (nreverse available)))

(defun opencode--pick-instance ()
  "Choose an available configured OpenCode instance.
When exactly one candidate is available, return it without prompting."
  (let ((instances (opencode--discover-instances)))
    (when opencode--instance-discovery-failures
      (message "OpenCode: unavailable instances: %s"
               (string-join opencode--instance-discovery-failures "; ")))
    (if (null (cdr instances))
        (car instances)
      (let ((choices
             (mapcar
              (lambda (instance)
                (cons (format "%s — %s"
                              (plist-get instance :name)
                              (plist-get instance :server-url))
                      instance))
              instances)))
        (cdr (assoc (completing-read
                     (if opencode--instance-discovery-failures
                         (format "OpenCode instance (%d unavailable): "
                                 (length opencode--instance-discovery-failures))
                       "OpenCode instance: ")
                     choices nil t)
                    choices))))))

(defun opencode--remember (root selection)
  "Store SELECTION against project ROOT (layer A) and as sticky default (layer B)."
  (setf (alist-get root opencode-agent-alist nil nil #'string=) selection)
  (setq opencode-agent-current selection)
  selection)

(defun opencode--configured-server-url-p (server-url)
  "Return non-nil when SERVER-URL is in the effective instance config."
  (and (opencode--valid-server-url-p server-url)
       (seq-some
        (lambda (candidate)
          (and (not (plist-get candidate :configuration-error))
               (equal server-url (plist-get candidate :server-url))))
        (opencode--instance-candidates))))

(defun opencode--selection-valid-p (selection)
  "Return non-nil when SELECTION has a complete configured target identity."
  (and selection
       (opencode--configured-server-url-p (plist-get selection :server-url))
       (numberp (plist-get selection :log-id))
       (numberp (plist-get selection :created))
       (stringp (plist-get selection :project-id))
       (stringp (plist-get selection :workspace-id))
       (stringp (plist-get selection :directory))
       (stringp (plist-get selection :worktree))))

(defun opencode--choose-agent (cross-project)
  "Interactively choose an agent and remember it (layers A + B).
With CROSS-PROJECT non-nil, first choose among the available configured
instances when necessary, then a project and one of its agents; otherwise pick
among the default instance's current-project agents.  Refetches each call, so
agents created in the web UI appear immediately.  Returns the selection plist."
  (let* ((root (opencode--root))
         (selection
          (if cross-project
              (opencode--pick-instance-project-agent
               (opencode--pick-instance))
            (opencode--pick-agent
             (if (opencode--valid-server-url-p opencode-server-url)
                 (opencode--normalize-server-url opencode-server-url)
               (user-error "OpenCode: invalid default server URL: %S"
                           opencode-server-url))
             "default" root))))
    (opencode--remember root selection)
    selection))

;;;###autoload
(defun opencode-select-agent (&optional cross-project)
  "Select which OpenCode agent this buffer's messages go to.
Without a prefix argument, lists the agents of the workspace the current
directory belongs to.  With a prefix argument (\\[universal-argument]),
CROSS-PROJECT is non-nil: pick a running configured instance when more
than one is available, then a project and one of its agents.  Use that to
message an agent from another daemon or project.

The pick is remembered against this project root AND as the global sticky
default (used from buffers whose root has no selection of its own)."
  (interactive "P")
  (let ((selection (opencode--choose-agent cross-project)))
    (message "OpenCode: %s → %s (logID %s @ %s — %s)"
             (abbreviate-file-name (opencode--root))
             (plist-get selection :label)
             (plist-get selection :log-id)
             (plist-get selection :instance-label)
             (plist-get selection :server-url))
    selection))

(defun opencode--selection-for-root ()
  "Return the selection plist in effect for the current root, or nil.
Layer A (this root's own pick) wins; otherwise layer B (the global
sticky default)."
  (let* ((root (opencode--root))
         (own (alist-get root opencode-agent-alist nil nil #'string=)))
    (unless (opencode--selection-valid-p own)
      (setq opencode-agent-alist
            (assoc-delete-all root opencode-agent-alist #'string=))
      (setq own nil))
    (unless (opencode--selection-valid-p opencode-agent-current)
      (setq opencode-agent-current nil))
    (or own opencode-agent-current)))

(defun opencode--agent-selection (&optional prompt-if-missing)
  "Return the selection plist in effect for the current buffer.
When none is selected and PROMPT-IF-MISSING is non-nil, run
`opencode-select-agent' to pick one; otherwise signal `user-error'."
  (let ((selection
         (or (opencode--selection-for-root)
             (when prompt-if-missing (opencode-select-agent))
             (user-error
              "OpenCode: no agent selected (M-x opencode-select-agent)"))))
    selection))

;;;###autoload
(defun opencode-which-agent ()
  "Report which OpenCode agent this buffer currently messages."
  (interactive)
  (let* ((root (opencode--root))
         (own-before (alist-get root opencode-agent-alist nil nil #'string=))
         (selection (opencode--selection-for-root))
         (own (and selection (eq selection own-before))))
    (if (not selection)
        (message "OpenCode: no agent selected for %s (M-x opencode-select-agent)"
                 (abbreviate-file-name root))
      (message "OpenCode: %s → %s (logID %s @ %s — %s)%s"
               (abbreviate-file-name root)
               (plist-get selection :label)
               (plist-get selection :log-id)
               (or (plist-get selection :instance-label) "?")
               (or (plist-get selection :server-url) "reselect required")
               (if own "" " [global default]")))))

;;; ---------------------------------------------------------------------------
;;; Context capture + message

(defun opencode--context-file (selection)
  "Return the label for the current buffer's file, given SELECTION.
When the file lives under the selected agent's project directory
(same-project), the path is relative to that directory (pretty).
Otherwise — a cross-project message, or no file — the absolute truename
is used, since the agent locates files by absolute path via its tools."
  (if (not buffer-file-name)
      (buffer-name)
    (let* ((truename (file-truename buffer-file-name))
           (agent-dir (plist-get selection :directory)))
      (if (and agent-dir
               (not (string-empty-p agent-dir))
               (string-prefix-p (file-name-as-directory agent-dir) truename))
          (file-relative-name truename agent-dir)
        truename))))

(defun opencode--quote-lines (start end)
  "Return the text between START and END as a string.
When `opencode-include-line-numbers' is non-nil, prefix each line with
its buffer line number."
  (let ((text (buffer-substring-no-properties start end)))
    (if (not opencode-include-line-numbers)
        text
      (let ((line (line-number-at-pos start))
            (lines (split-string text "\n")))
        (mapconcat
         (lambda (l) (prog1 (format "%d: %s" line l) (setq line (1+ line))))
         lines "\n")))))

(defun opencode--generic-context-preamble (selection)
  "Build a generic markdown preamble for SELECTION."
  (let ((file (opencode--context-file selection)))
    (if (use-region-p)
        (let* ((start (region-beginning))
               (end (region-end))
               (l1 (line-number-at-pos start))
               (l2 (line-number-at-pos (if (and (> end start)
                                                (= (char-before end) ?\n))
                                           (1- end) end)))
               (lang (if (and (boundp 'major-mode) major-mode)
                         (string-remove-suffix "-mode" (symbol-name major-mode))
                       "")))
          (format "Context: `%s` lines %d-%d:\n\n```%s\n%s\n```\n\n"
                  file l1 l2 lang (opencode--quote-lines start end)))
      (format "Context: `%s` line %d.\n\n"
              file (line-number-at-pos (point))))))

(defun opencode--bound-value (symbol)
  "Return SYMBOL's value when bound, otherwise nil."
  (and (boundp symbol) (symbol-value symbol)))

(defun opencode--section-slot (section slot)
  "Return SECTION's SLOT value when that slot exists."
  (and section (slot-exists-p section slot) (slot-value section slot)))

(defun opencode--section-ancestor (section types)
  "Return SECTION or its nearest ancestor whose type is in TYPES."
  (while (and section
              (not (memq (opencode--section-slot section 'type) types)))
    (setq section (opencode--section-slot section 'parent)))
  section)

(defun opencode--section-at (position)
  "Return the Magit section at POSITION, if available."
  (when (fboundp 'magit-section-at)
    (or (magit-section-at position)
        (and (> position (point-min))
             (magit-section-at (1- position))))))

(defun opencode--short-id (id)
  "Return a short display form of revision ID."
  (if (> (length id) 8) (substring id 0 8) id))

(defun opencode--git-empty-tree-hash (root)
  "Return Git's empty-tree hash for the repository at ROOT."
  (let ((default-directory root))
    (or (and (fboundp 'magit-git-string)
             (ignore-errors
               (magit-git-string "hash-object" "-t" "tree" "/dev/null")))
        (user-error "OpenCode: cannot resolve Git's empty-tree identity"))))

(defun opencode--revision-side (vcs root expression)
  "Return VCS revision-side data for EXPRESSION at ROOT."
  (pcase vcs
    ('git
     (if (member expression '("{index}" "{worktree}"))
         (list :expression expression :identity expression :display expression)
       (let ((identity
              (and expression (fboundp 'magit-rev-verify)
                   (let ((default-directory root))
                     (ignore-errors (magit-rev-verify expression))))))
         (list :expression expression
               :identity (or identity expression)
               :display (if identity (opencode--short-id identity) expression)))))
    ('jj
     (let* ((lines
              (and expression (fboundp 'majutsu-jj-lines)
                   (let ((default-directory root))
                     (ignore-errors
                       (majutsu-jj-lines
                        "log" "--no-graph" "-r" expression
                        "-T" "commit_id ++ \"\\t\" ++ change_id ++ \"\\n\"")))))
            (entries
             (mapcar
              (lambda (line)
                (pcase-let ((`(,commit ,change)
                             (split-string line "\t")))
                  (list commit change)))
              lines))
            (single (and (= (length entries) 1) (car entries)))
            (commit (car single))
            (change (cadr single)))
       (list :expression expression
             :identity (if entries (cons 'resolved entries) expression)
             :display
             (cond
              (single (opencode--short-id commit))
              (entries
               (format "merge(%s)"
                       (string-join
                        (mapcar (lambda (entry)
                                  (opencode--short-id (car entry)))
                                entries)
                        ", ")))
              (t expression))
             :entries entries
             :change (and change (not (string-empty-p change)) change))))))

(defun opencode--revision-pair (vcs root from to raw)
  "Return a complete revision-pair context for VCS FROM, TO, and RAW."
  (let* ((from-side (opencode--revision-side vcs root from))
         (to-side (opencode--revision-side vcs root to))
         (change (plist-get to-side :change))
         (suffix (if (eq vcs 'jj)
                     (concat " [jj]"
                             (if change
                                 (format "; change %s" (opencode--short-id change))
                               ""))
                   " [git]")))
    (list :key (list vcs raw
                     (plist-get from-side :identity)
                     (plist-get to-side :identity))
          :header (format "Diff: %s -> %s%s"
                          (plist-get from-side :display)
                          (plist-get to-side :display)
                          suffix))))

(defun opencode--diff-project-root (vcs)
  "Return the current diff's canonical project root for VCS."
  (let ((root
         (pcase vcs
           ('git (and (fboundp 'magit-toplevel)
                      (ignore-errors (magit-toplevel))))
           ('jj (and (fboundp 'majutsu-toplevel)
                     (ignore-errors (majutsu-toplevel)))))))
    (directory-file-name
     (file-truename (expand-file-name (or root default-directory))))))

(defun opencode--magit-revision-context (root hunk)
  "Return Magit revision display and identity data for ROOT and HUNK."
  (let* ((commit-section (opencode--section-ancestor hunk '(commit)))
         (stash-mode (derived-mode-p 'magit-stash-mode))
         (buffer-revision (opencode--bound-value 'magit-buffer-revision))
         (section-revision (opencode--section-slot commit-section 'value))
         (revision (or (and (not stash-mode)
                            (opencode--bound-value 'magit-buffer-revision-hash))
                       section-revision buffer-revision))
         (full (and revision (fboundp 'magit-rev-parse)
                      (let ((default-directory root))
                        (ignore-errors (magit-rev-parse revision)))))
         (parent
          (and full
               (let ((default-directory root))
                 (ignore-errors (magit-rev-verify (concat full "^"))))))
         (range (opencode--bound-value 'magit-buffer-range))
         (typearg (opencode--bound-value 'magit-buffer-typearg))
         (args (opencode--bound-value 'magit-buffer-diff-args))
         (reversed (member "-R" args))
         (type (or (and (fboundp 'magit-diff-type)
                        (ignore-errors (magit-diff-type hunk)))
                   (opencode--bound-value 'magit-buffer-diff-type)))
         (pair
          (cond
           (stash-mode
            (if (equal section-revision buffer-revision)
                (cons (concat buffer-revision "^2") buffer-revision)
              (cons (concat buffer-revision "^") section-revision)))
           (full (cons (or parent (opencode--git-empty-tree-hash root)) full))
           ((equal typearg "--no-index")
            (user-error "OpenCode: --no-index diff context is not supported"))
           ((eq type 'staged) (cons (or range "HEAD") "{index}"))
           ((eq type 'unstaged) (cons "{index}" "{worktree}"))
           (range
            (or (and (fboundp 'magit-split-range)
                     (ignore-errors (magit-split-range range)))
                (cons range "{worktree}")))
           (t (cons "{index}" "{worktree}"))))
         (from (if reversed (cdr pair) (car pair)))
         (to (if reversed (car pair) (cdr pair)))
         (context
          (opencode--revision-pair
           'git root from to (list type range typearg args))))
    (if (and full (not reversed) (not stash-mode))
        (plist-put context :header
                   (format "Commit: %s [git]" (opencode--short-id full)))
      context)))

(defun opencode--jj-revision-expressions (range)
  "Return every revision expression selected by jj diff RANGE."
  (let ((rest range)
        expressions)
    (while rest
      (let ((arg (pop rest)))
        (cond
         ((equal arg "-r")
          (when rest (push (pop rest) expressions)))
         ((string-prefix-p "--revisions=" arg)
          (push (substring arg 12) expressions))
         ((and (string-prefix-p "-r" arg) (> (length arg) 2))
          (push (substring arg 2) expressions)))))
    (nreverse expressions)))

(defun opencode--jj-union-revset (expressions)
  "Return one union revset for EXPRESSIONS."
  (string-join (mapcar (lambda (expression)
                         (format "(%s)" expression))
                       expressions)
               " | "))

(defun opencode--jj-explicit-range (range)
  "Parse explicit jj --from/--to arguments in RANGE."
  (let (from to)
    (dolist (arg range)
      (cond
       ((string-prefix-p "--from=" arg) (setq from (substring arg 7)))
       ((string-prefix-p "--to=" arg) (setq to (substring arg 5)))))
    (when (or from to)
      (cons (or from "@-") (or to "@")))))

(defun opencode--majutsu-revision-context (root)
  "Return Majutsu revision display and complete identity data for ROOT."
  (let* ((range (opencode--bound-value 'majutsu-buffer-diff-range))
         (expressions (or (opencode--jj-revision-expressions range) '("@"))))
    (if (or (null range) (opencode--jj-revision-expressions range))
        (let* ((union (opencode--jj-union-revset expressions))
               (selected (opencode--revision-side 'jj root union))
               (from (format "roots(%s)-" union))
               (to (format "heads(%s)" union))
               (context
                (opencode--revision-pair
                 'jj root from to
                 (list range (plist-get selected :identity))))
               (entries (plist-get selected :entries)))
          (if (/= (length entries) 1)
              context
            (let ((change (cadar entries)))
              (plist-put
               context :header
               (format "Commit: %s [jj]%s"
                       (opencode--short-id (caar entries))
                       (if (and change (not (string-empty-p change)))
                           (format " (change %s)" (opencode--short-id change))
                         ""))))))
      (let* ((pair (or (opencode--jj-explicit-range range)
                       (and (fboundp 'majutsu-jj--parse-diff-range)
                            (ignore-errors
                              (majutsu-jj--parse-diff-range range)))
                       (user-error "OpenCode: cannot interpret jj diff range %S"
                                   range))))
        (opencode--revision-pair 'jj root (car pair) (cdr pair) range)))))

(defun opencode--diff-old-file (file-section majutsu)
  "Return FILE-SECTION's old path, accounting for MAJUTSU headers."
  (or (opencode--section-slot file-section 'source)
      (when majutsu
        (let ((header (opencode--section-slot file-section 'header)))
          (when (stringp header)
            (cond
             ((string-match "^rename from \\(.+\\)$" header)
              (match-string 1 header))
             ((string-match "^diff --git a/\\(.*\\) b/\\(.*\\)$" header)
              (let ((old (match-string 1 header))
                    (new (match-string 2 header)))
                (unless (equal old new) old)))))))))

(defun opencode--diff-line-records (hunk start end)
  "Return selected line records from HUNK between START and END."
  (when (opencode--section-slot hunk 'combined)
    (user-error "OpenCode: combined diff selections are not supported yet"))
  (when (and (eq (opencode--bound-value 'majutsu-diff-backend) 'color-words)
             (derived-mode-p 'majutsu-diff-mode))
    (user-error "OpenCode: select a --git Majutsu diff for exact line capture"))
  (let* ((content (opencode--section-slot hunk 'content))
         (hunk-end (opencode--section-slot hunk 'end))
         (from (opencode--section-slot hunk 'from-range))
         (to (opencode--section-slot hunk 'to-range))
         (ends-with-newline (and (> end start) (= (char-before end) ?\n))))
    (unless (and content hunk-end from to
                 (>= start content) (<= end hunk-end))
      (user-error "OpenCode: select diff body lines within one hunk"))
    (save-excursion
      (goto-char content)
      (let ((old (car from))
            (new (car to))
            records)
        (while (and (< (point) end) (< (point) hunk-end))
          (let* ((bol (line-beginning-position))
                 (eol (line-end-position))
                 (marker (char-after bol))
                 (kind (pcase marker
                         (?+ 'added)
                         (?- 'removed)
                         (?\s 'context)
                         (_ nil)))
                 (old-line (and (memq kind '(removed context)) old))
                 (new-line (and (memq kind '(added context)) new))
                 (segment-start (max start bol))
                 (segment-end (min end eol)))
            (when (and (not kind) (> end bol) (< start (1+ eol)))
              (user-error "OpenCode: select only ordinary diff body lines"))
            (when (and kind (<= segment-start segment-end)
                       (> end bol) (< start (1+ eol)))
              (let ((text (buffer-substring-no-properties
                           segment-start segment-end)))
                (when (= segment-start bol)
                  (setq text (substring text (min 1 (length text)))))
                (push (list :kind kind :old old-line :new new-line :text text)
                      records)))
            (when (memq kind '(removed context)) (setq old (1+ old)))
            (when (memq kind '(added context)) (setq new (1+ new))))
          (forward-line 1))
        (unless records
          (user-error "OpenCode: the selection contains no diff body lines"))
        (list :records (nreverse records)
              :ends-with-newline ends-with-newline)))))

(defun opencode--line-span (records key)
  "Return RECORDS' compact source line span for KEY."
  (let ((numbers (delq nil (mapcar (lambda (record) (plist-get record key))
                                    records))))
    (when numbers
      (let ((first (apply #'min numbers))
            (last (apply #'max numbers)))
        (if (= first last) (number-to-string first)
          (format "%d-%d" first last))))))

(defun opencode--diff-location (file old-file records)
  "Return a compact FILE source location for RECORDS."
  (let ((old (opencode--line-span records :old))
        (new (opencode--line-span records :new)))
    (cond
     ((and old new old-file (not (equal old-file file)))
      (format "%s:old %s -> %s:new %s" old-file old file new))
     ((and old new (equal old new)) (format "%s:%s" file new))
     ((and old new) (format "%s:old %s; new %s" file old new))
     (old (format "%s:old %s" (or old-file file) old))
     (new (format "%s:%s" file new))
     (t file))))

(defun opencode--render-diff-records (records &optional ends-with-newline)
  "Render RECORDS with gutters and preserve ENDS-WITH-NEWLINE."
  (let* ((old-width (max 1 (apply #'max
                                  (mapcar (lambda (record)
                                            (length (format "%s"
                                                            (or (plist-get record :old) ""))))
                                          records))))
         (new-width (max 1 (apply #'max
                                  (mapcar (lambda (record)
                                            (length (format "%s"
                                                            (or (plist-get record :new) ""))))
                                          records)))))
    (concat
     (mapconcat
      (lambda (record)
        (format (format "%%-%ds %%%ds %%s %%s" old-width new-width)
                (or (plist-get record :old) "")
                (or (plist-get record :new) "")
                (or (alist-get (plist-get record :kind)
                               opencode-diff-line-markers)
                    " ")
                (plist-get record :text)))
      records "\n")
     (if ends-with-newline "\n" ""))))

(defun opencode--semantic-diff-host ()
  "Return the supported semantic diff host at point, if any."
  (cond
   ((derived-mode-p 'majutsu-diff-mode) 'jj)
   ((derived-mode-p 'magit-diff-mode 'magit-status-mode 'magit-log-mode
                    'magit-stash-mode)
    'git)))

(defun opencode--semantic-diff-file-p (section majutsu)
  "Return non-nil when SECTION is a rendered diff file for MAJUTSU."
  (and section
       (or majutsu
           (opencode--section-slot section 'header)
           (opencode--section-slot section 'binary)
           (seq-some
            (lambda (child)
              (eq (opencode--section-slot child 'type) 'hunk))
            (opencode--section-slot section 'children)))))

(defun opencode--region-sections (start end)
  "Return every Magit section intersecting the region START to END."
  (let ((position start)
        sections)
    (while (< position end)
      (when-let ((section (magit-section-at position)))
        (cl-pushnew section sections :test #'eq))
      (setq position
            (or (next-single-property-change
                 position 'magit-section nil end)
                end)))
    (when (> end start)
      (when-let ((section (magit-section-at (1- end))))
        (cl-pushnew section sections :test #'eq)))
    sections))

(defun opencode--semantic-diff-context ()
  "Return semantic context for an active Magit or Majutsu diff region."
  (when (and (use-region-p) (fboundp 'magit-section-at)
             (opencode--semantic-diff-host))
    (let* ((vcs (opencode--semantic-diff-host))
           (majutsu (eq vcs 'jj))
           (hunk-types (if majutsu '(jj-hunk) '(hunk)))
           (file-types (if majutsu '(jj-file) '(file)))
           (start (region-beginning))
           (end (region-end))
           (sections (opencode--region-sections start end))
           (hunks
            (delete-dups
             (delq nil
                   (mapcar (lambda (section)
                             (opencode--section-ancestor section hunk-types))
                           sections))))
           (files
            (delete-dups
             (delq nil
                   (mapcar (lambda (section)
                             (opencode--section-ancestor section file-types))
                           sections))))
           (diff-files
            (seq-filter
             (lambda (file-section)
               (opencode--semantic-diff-file-p file-section majutsu))
             files))
           (hunk (and (= (length hunks) 1) (car hunks)))
           (touches-diff (or hunks diff-files)))
      (when touches-diff
        (unless hunk
          (user-error "OpenCode: select diff body lines within one hunk"))
        (let* ((file-section (opencode--section-ancestor hunk file-types))
               (file (opencode--section-slot file-section 'value))
               (old-file (opencode--diff-old-file file-section majutsu))
               (selection (opencode--diff-line-records hunk start end))
               (records (plist-get selection :records))
               (root (opencode--diff-project-root vcs))
               (revision (if majutsu
                             (opencode--majutsu-revision-context root)
                            (opencode--magit-revision-context root hunk))))
          (list :diff t
                :project-key root
                :project-header
                (format "Project: %s" (abbreviate-file-name root))
                :revision-key (plist-get revision :key)
                :revision-header (plist-get revision :header)
                :body
                (let* ((ends-with-newline
                        (plist-get selection :ends-with-newline))
                       (rendered
                        (opencode--render-diff-records
                         records ends-with-newline)))
                  (concat
                   (opencode--diff-location file old-file records)
                   "\n```diff\n"
                   rendered
                   (unless ends-with-newline "\n")
                   "```\n"
                   (unless ends-with-newline
                     "Selection boundary: before the final line break.\n")
                   "\n"))))))))

(defun opencode--capture-context (selection)
  "Capture structured context for SELECTION from the current buffer."
  (or (opencode--semantic-diff-context)
      (list :text (opencode--generic-context-preamble selection))))

(defun opencode--render-context (capture &optional previous)
  "Render CAPTURE, deduplicating headers already present in PREVIOUS."
  (if (not (plist-get capture :diff))
      (plist-get capture :text)
    (let* ((same-project
            (and previous
                 (equal (plist-get capture :project-key)
                        (plist-get previous :project-key))))
           (same-revision
            (and same-project
                 (equal (plist-get capture :revision-key)
                        (plist-get previous :revision-key))))
           (headers
            (delq nil
                  (list (unless same-project (plist-get capture :project-header))
                        (unless same-revision (plist-get capture :revision-header))))))
      (concat (when headers (concat (string-join headers "\n") "\n\n"))
              (plist-get capture :body)))))

(defun opencode--context-preamble (selection)
  "Build the complete context preamble for a one-shot message."
  (opencode--render-context (opencode--capture-context selection)))

(defun opencode--selection-target-key (selection)
  "Return SELECTION's stable daemon-local agent identity."
  (list (plist-get selection :server-url)
        (plist-get selection :project-id)
        (plist-get selection :workspace-id)
        (plist-get selection :log-id)
        (plist-get selection :created)))

(defun opencode--validate-selection-target (selection)
  "Revalidate SELECTION against its configured instance before a send.
Numeric log IDs can be reused after a daemon data reset, so the selected
creation timestamp and project/workspace association are part of identity."
  (unless (opencode--selection-valid-p selection)
    (user-error
     "OpenCode: this selection is stale or its instance is no longer configured; select the agent again"))
  (let* ((server-url (plist-get selection :server-url))
         (response
          (opencode--fetch-logs server-url (plist-get selection :directory)))
         (project (alist-get 'project response))
         (matching-workspaces
          (seq-filter
           (lambda (workspace)
             (opencode--workspace-matches-response-p workspace response))
           (opencode--fetch-workspaces server-url)))
         (workspace (and (null (cdr matching-workspaces))
                         (car matching-workspaces)))
         (log
          (seq-find
           (lambda (candidate)
             (and (equal (alist-get 'logID candidate)
                         (plist-get selection :log-id))
                  (equal (alist-get 'created candidate)
                         (plist-get selection :created))))
           (append (opencode--logs-vector response) nil))))
    (unless (and project workspace log
                 (equal (alist-get 'workspaceID workspace)
                        (plist-get selection :workspace-id))
                 (equal (alist-get 'projectID project)
                        (plist-get selection :project-id))
                 (equal (alist-get 'directory project)
                        (plist-get selection :directory))
                 (equal (alist-get 'worktree project)
                        (plist-get selection :worktree)))
      (user-error
       "OpenCode: the selected agent identity changed on %s; select it again"
       server-url))
    selection))

(defun opencode--send (selection text)
  "POST TEXT as a user message to the agent named by SELECTION.
Signals `opencode-error' on failure, so a caller that owns durable text
(the compose buffer) can keep it when the send did not land."
  (opencode--validate-selection-target selection)
  (let ((server-url (plist-get selection :server-url))
        (log-id (plist-get selection :log-id))
        (payload `((parts . [((type . "text") (text . ,text))]))))
    (opencode--post-json
     server-url (format "/log-id/%s/message" log-id) payload)))

(defun opencode--sent-message (selection)
  "Echo a \"sent to ...\" confirmation for SELECTION."
  (message "OpenCode: sent to %s (logID %s @ %s) — watch the reply in the web UI (%s)"
           (plist-get selection :label)
           (plist-get selection :log-id)
           (plist-get selection :instance-label)
           (plist-get selection :server-url)))

;;;###autoload
(defun opencode-message (message)
  "Send MESSAGE plus the current editor context to the selected agent.
Interactively, saves the current buffer first (so the agent reads your
latest on disk), captures context (active region with line numbers, or
the file and line at point), and prompts for MESSAGE.  Watch the reply
in the OpenCode web UI.  In Magit and Majutsu diff buffers, an active
region is captured with project, revision, file, and old/new source lines.

This is the one-shot tempo: author a single line and send it now.  To
accumulate several takes across a module before sending, use
`opencode-compose' instead.

If no agent is selected for this buffer, prompts to pick one."
  (interactive
   (progn
     (when (and buffer-file-name (buffer-modified-p))
       (save-buffer))
     (list (read-string "Message OpenCode agent: "))))
  (when (string-empty-p (string-trim message))
    (user-error "OpenCode: empty message"))
  (let* ((selection (opencode--agent-selection t))
         (text (concat (opencode--context-preamble selection) message)))
    (opencode--send selection text)
    (opencode--sent-message selection)))

;;; ---------------------------------------------------------------------------
;;; Compose: accumulate several takes in a buffer, send on C-c C-c
;;;
;;; The one-shot `opencode-message' and `opencode-compose' share one spine
;;; (`opencode--context-preamble' to capture, `opencode--send' to deliver);
;;; they differ only in WHERE you author (minibuffer vs a persistent buffer)
;;; and WHEN you send (now vs on demand).  A compose buffer IS the pending
;;; message to one agent: its text is exactly the payload, so what you see is
;;; what gets sent.

(defvar-local opencode--compose-selection nil
  "The selection plist this compose buffer sends to.
Captured when the buffer is created, so it keeps targeting that agent even
if the project root's selection changes elsewhere.")

(defvar-local opencode--compose-last-context nil
  "Most recent structured context appended to this compose buffer.")

(defun opencode--compose-header-line ()
  "Header-line string for a compose buffer (agent name + key hints).
Kept out of the buffer body so it can never leak into the sent message."
  (format " OpenCode → %s @ %s    C-c C-c send · C-c C-k discard"
          (if opencode--compose-selection
              (plist-get opencode--compose-selection :label)
            "?")
          (if opencode--compose-selection
              (plist-get opencode--compose-selection :instance-label)
            "?")))

(defvar opencode-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'opencode-compose-send)
    (define-key map (kbd "C-c C-k") #'opencode-compose-discard)
    map)
  "Keymap for `opencode-compose-mode'.")

(define-derived-mode opencode-compose-mode text-mode "OC-Compose"
  "Major mode for composing a multi-part OpenCode message.
The buffer content is exactly what will be sent to the agent named in the
header line.  \\<opencode-compose-mode-map>\\[opencode-compose-send] sends \
the whole buffer and kills it; \\[opencode-compose-discard] discards it."
  (setq header-line-format '(:eval (opencode--compose-header-line))))

(defun opencode--compose-buffer (selection)
  "Return the compose buffer for SELECTION's agent, creating it if needed.
Reuses an existing compose buffer targeting the same stable agent identity —
so repeated `opencode-compose' calls from anywhere accumulate into one draft —
else creates a fresh one bound to SELECTION."
  (let ((target-key (opencode--selection-target-key selection)))
    (or (seq-find
         (lambda (buf)
           (with-current-buffer buf
              (and (derived-mode-p 'opencode-compose-mode)
                   opencode--compose-selection
                   (equal (opencode--selection-target-key
                           opencode--compose-selection)
                          target-key))))
         (buffer-list))
        (let ((buf (generate-new-buffer
                    (format "*opencode compose: %s @ %s*"
                            (plist-get selection :label)
                            (plist-get selection :instance-label)))))
          (with-current-buffer buf
            (opencode-compose-mode)
            (setq opencode--compose-selection selection))
          buf))))

(defun opencode--compose-append (preamble)
  "Append PREAMBLE at the end of the current buffer, on its own blank line.
Normalizes any trailing whitespace of the previous take to a single blank
line, so accumulated takes stay evenly separated.  Leaves point at the end
(ready for the comment)."
  (goto-char (point-max))
  (when (> (buffer-size) 0)
    (skip-chars-backward " \t\n")
    (delete-region (point) (point-max))
    (insert "\n\n"))
  (insert preamble)
  (goto-char (point-max)))

;;;###autoload
(defun opencode-compose (&optional cross-project)
  "Append the current editor context to a compose buffer for the selected agent.
Saves the current buffer, captures context (active region with line
numbers, or the file and line at point), and appends it to a persistent
compose buffer for the agent this buffer messages — creating it, or reusing
the one already open for that agent so takes accumulate.  Pops to the buffer
at end, where you type your comment.  Repeated Magit/Majutsu diff captures
emit project and revision headers only when those facts change.

With a prefix argument (\\[universal-argument]), CROSS-PROJECT is non-nil:
first pick a running configured instance, a project, and the target agent;
then start (or resume) its compose buffer.
Without a prefix argument, uses the agent already selected for this buffer,
prompting to pick one if none is.

The buffer content is exactly what will be sent.  In the compose buffer,
\\<opencode-compose-mode-map>\\[opencode-compose-send] sends the whole \
buffer (then kills it) and \\[opencode-compose-discard] discards it."
  (interactive "P")
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer))
  (let* ((selection (if cross-project
                        (opencode--choose-agent t)
                      (opencode--agent-selection t)))
         ;; Capture in the SOURCE buffer before switching to the draft.
         (capture (opencode--capture-context selection))
         (buf (opencode--compose-buffer selection)))
    (with-current-buffer buf
      (opencode--compose-append
       (opencode--render-context capture opencode--compose-last-context))
      (setq opencode--compose-last-context capture))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(defun opencode-compose-send ()
  "Send the whole compose buffer to its agent, then kill the buffer.
On send failure the buffer survives with its content intact, so nothing
you accumulated is lost."
  (interactive)
  (unless (derived-mode-p 'opencode-compose-mode)
    (user-error "OpenCode: not a compose buffer"))
  (let ((selection opencode--compose-selection)
        (text (string-trim (buffer-string))))
    (when (string-empty-p text)
      (user-error "OpenCode: compose buffer is empty — nothing to send"))
    ;; Signals on failure → we never reach kill-buffer, buffer is preserved.
    (opencode--send selection text)
    (opencode--sent-message selection)
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))

(defun opencode-compose-discard ()
  "Discard this compose buffer without sending (confirm when non-empty)."
  (interactive)
  (unless (derived-mode-p 'opencode-compose-mode)
    (user-error "OpenCode: not a compose buffer"))
  (when (or (= (buffer-size) 0)
            (yes-or-no-p "Discard this OpenCode message? "))
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))

(provide 'opencode)

;;; opencode.el ends here
