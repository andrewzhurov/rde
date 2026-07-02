;;; opencode.el --- Pair with an OpenCode agent from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrew Zhurov

;; Author: Andrew Zhurov
;; Maintainer: Andrew Zhurov
;; URL: https://github.com/sst/opencode
;; Version: 0.1.0
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
;; Two commands:
;;
;;   `opencode-select-agent'  Pick which running agent the current
;;                            project's buffers talk to.  Lists the
;;                            agents (conversations) of the workspace
;;                            the current directory belongs to.  Create
;;                            and manage agents in the web UI; this only
;;                            selects among the ones already there.
;;
;;   `opencode-message'       The gesture.  Saves the current buffer (so
;;                            the agent reads your latest), captures
;;                            editor context (active region with line
;;                            numbers, else point's line + file), prompts
;;                            for a message, and sends it to the selected
;;                            agent.  Watch the reply in the web UI.
;;
;; The agent's file changes flow back into Emacs via
;; `global-auto-revert-mode' (enable it).  A buffer with unsaved edits to
;; a file the agent also rewrote is a genuine conflict: auto-revert
;; refuses to clobber it and warns — resolve it yourself.
;;
;; Selection is keyed on the project root (git worktree top level), so
;; every buffer under a root shares one selected agent.  The mapping is
;; an editor-local preference; it is not stored on the server.
;;
;; Talks to a running daemon (e.g. `make -C dev daily-web' on
;; http://localhost:4098).  Point `opencode-server-url' elsewhere to
;; pair with a different instance.

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)
(require 'subr-x)
(require 'project)
(require 'vc)

(defgroup opencode nil
  "Pair with an OpenCode agent from Emacs."
  :group 'tools
  :prefix "opencode-")

(defcustom opencode-server-url "http://localhost:4098"
  "Base URL of the OpenCode server to pair with.
The daily-driver dev instance serves its API on port 4098; the
isolated instance on 4097.  No trailing slash."
  :type 'string
  :group 'opencode)

(defcustom opencode-request-timeout 10
  "Seconds to wait for an OpenCode HTTP request before giving up."
  :type 'integer
  :group 'opencode)

(defcustom opencode-include-line-numbers t
  "When non-nil, prefix quoted region/line context with line numbers."
  :type 'boolean
  :group 'opencode)

(defvar opencode-agent-alist nil
  "Alist mapping a project root (string) to a selected agent logID (number).
Editor-local: which agent each project's buffers currently message.
Populated by `opencode-select-agent'.")

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

(defun opencode--url (path)
  "Join PATH onto `opencode-server-url'."
  (concat (string-remove-suffix "/" opencode-server-url) path))

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

(defun opencode--get (path)
  "GET PATH from the OpenCode server and return the parsed JSON body.
Synchronous.  Signals `opencode-error' on transport failure, timeout,
or a non-2xx status."
  (let ((url-request-method "GET")
        (buf (url-retrieve-synchronously
              (opencode--url path) t nil opencode-request-timeout)))
    (unless buf
      (signal 'opencode-error
              (list (format "no response from %s (is the server running?)"
                            opencode-server-url))))
    (unwind-protect
        (with-current-buffer buf
          (let ((status (opencode--http-status)))
            (when (and status (>= status 400))
              (signal 'opencode-error
                      (list (format "GET %s → HTTP %d" path status))))
            (opencode--parse-json-buffer)))
      (kill-buffer buf))))

(defun opencode--post-json (path payload)
  "POST PAYLOAD (a Lisp object, JSON-encoded) to PATH.
Synchronous.  Returns the parsed JSON body when present, otherwise t.
Signals `opencode-error' on transport failure, timeout, or non-2xx."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (buf (url-retrieve-synchronously
               (opencode--url path) t nil opencode-request-timeout)))
    (unless buf
      (signal 'opencode-error
              (list (format "no response from %s (is the server running?)"
                            opencode-server-url))))
    (unwind-protect
        (with-current-buffer buf
          (let ((status (opencode--http-status)))
            (when (and status (>= status 400))
              (signal 'opencode-error
                      (list (format "POST %s → HTTP %d" path status))))
            ;; Body may be empty ({ ok: true } or 204) — tolerate both.
            (goto-char (point-min))
            (if (re-search-forward "\n\n" nil t)
                (if (looking-at-p "[ \t\n]*\\'") t (opencode--parse-json-buffer))
              t)))
      (kill-buffer buf))))

;;; ---------------------------------------------------------------------------
;;; Agent selection

(defun opencode--fetch-logs (root)
  "Fetch the agent logs for ROOT's workspace.
Returns the parsed { project, logs } object."
  (opencode--get (concat "/agent/logs?directory=" (url-hexify-string root))))

(defun opencode--logs-vector (response)
  "Extract the logs vector from a /agent/logs RESPONSE."
  (or (alist-get 'logs response) []))

(defun opencode--project-null-p (response)
  "Non-nil when RESPONSE reports the directory is not an opened project."
  (let ((project (assq 'project response)))
    ;; project key present and its value is JSON null (nil in alist parse).
    (and project (null (cdr project)))))

;;;###autoload
(defun opencode-select-agent ()
  "Select which OpenCode agent the current project's buffers message.
Lists the conversations of the workspace the current directory belongs
to and stores the chosen agent against the project root.  Refetches
each call, so agents created in the web UI appear immediately.

When the directory is not an opened project, says so and points you at
the web UI — open it there first."
  (interactive)
  (let* ((root (opencode--root))
         (response (opencode--fetch-logs root)))
    (when (opencode--project-null-p response)
      (user-error "OpenCode: %s is not an open project — open it in the web UI (%s) first"
                  root opencode-server-url))
    (let* ((logs (append (opencode--logs-vector response) nil))
           (choices (mapcar
                     (lambda (log)
                       (cons (format "%s" (alist-get 'title log "(untitled)"))
                             (alist-get 'logID log)))
                     logs)))
      (when (null choices)
        (user-error "OpenCode: no agents in this project yet — create one in the web UI (%s)"
                    opencode-server-url))
      (let* ((pick (completing-read
                    (format "Agent for %s: " (abbreviate-file-name root))
                    choices nil t))
             (log-id (cdr (assoc pick choices))))
        (setf (alist-get root opencode-agent-alist nil nil #'string=) log-id)
        (message "OpenCode: %s → %s (logID %s)"
                 (abbreviate-file-name root) pick log-id)
        log-id))))

(defun opencode--agent-for-root (&optional prompt-if-missing)
  "Return the selected agent logID for the current project root.
When none is selected and PROMPT-IF-MISSING is non-nil, run
`opencode-select-agent' to pick one."
  (let* ((root (opencode--root))
         (log-id (alist-get root opencode-agent-alist nil nil #'string=)))
    (or log-id
        (when prompt-if-missing (opencode-select-agent))
        (user-error "OpenCode: no agent selected for %s (M-x opencode-select-agent)"
                    (abbreviate-file-name root)))))

;;; ---------------------------------------------------------------------------
;;; Context capture + message

(defun opencode--relative-name ()
  "Return the current buffer's file path relative to the project root."
  (if buffer-file-name
      (file-relative-name (file-truename buffer-file-name) (opencode--root))
    (buffer-name)))

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

(defun opencode--context-preamble ()
  "Build a markdown preamble describing the current editor context.
Active region → the selected lines with a line range; otherwise the
file and the line at point.  Returns a string ending in a blank line."
  (let ((file (opencode--relative-name)))
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

;;;###autoload
(defun opencode-message (message)
  "Send MESSAGE plus the current editor context to the selected agent.
Interactively, saves the current buffer first (so the agent reads your
latest on disk), captures context (active region with line numbers, or
the file and line at point), and prompts for MESSAGE.  Watch the reply
in the OpenCode web UI.

If no agent is selected for this project, prompts to pick one."
  (interactive
   (progn
     (when (and buffer-file-name (buffer-modified-p))
       (save-buffer))
     (list (read-string "Message OpenCode agent: "))))
  (when (string-empty-p (string-trim message))
    (user-error "OpenCode: empty message"))
  (let* ((log-id (opencode--agent-for-root t))
         (text (concat (opencode--context-preamble) message))
         (payload `((parts . [((type . "text") (text . ,text))]))))
    (opencode--post-json (format "/log-id/%s/message" log-id) payload)
    (message "OpenCode: sent to logID %s — watch the reply in the web UI (%s)"
             log-id opencode-server-url)))

(provide 'opencode)

;;; opencode.el ends here
