;;; opencode.el --- Pair with an OpenCode agent from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrew Zhurov

;; Author: Andrew Zhurov
;; Maintainer: Andrew Zhurov
;; URL: https://github.com/sst/opencode
;; Version: 0.2.0
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
;;                            workspace the daemon knows (across every
;;                            project), then pick one of its agents — so
;;                            you can drive, say, the `guild' project's
;;                            agent from a buffer in another repo entirely.
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
;;                            With a prefix arg (C-u), first pick the target
;;                            agent — any workspace, cross-project — as
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
;; server.  Talks to a running daemon (e.g. `make -C dev daily-web' on
;; http://localhost:4098).  Point `opencode-server-url' elsewhere to pair
;; with a different instance.

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

;;; A selection is a plist describing a chosen agent:
;;;
;;;   (:log-id    NUMBER   agent logID — the message target)
;;;    :label     STRING   "<project> / <agent title>" for echoes/menus)
;;;    :directory STRING)  the agent's project directory, or nil — used to
;;;                        decide whether a buffer's file is same-project
;;;                        (pretty relative path) or cross-project (absolute).

(defvar opencode-agent-alist nil
  "Alist mapping a project root (string) to a selection plist.
Layer A of the selection model: which agent each project's buffers
currently message.  Populated by `opencode-select-agent'.")

(defvar opencode-agent-current nil
  "The most recently selected agent, as a selection plist, or nil.
Layer B of the selection model: the global sticky default, used as the
fallback when the current buffer's project root has no selection of its
own.  Populated by `opencode-select-agent'.")

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
;;; Server reads

(defun opencode--fetch-logs (directory)
  "Fetch the agent logs for the workspace DIRECTORY belongs to.
Returns the parsed { project, logs } object."
  (opencode--get (concat "/agent/logs?directory=" (url-hexify-string directory))))

(defun opencode--fetch-workspaces ()
  "Fetch the daemon's full workspace catalog as a list of alists.
Newest-first.  Each entry has keys like `projectID', `workspaceID',
`directory', `worktree', `name', `projectName', `branch'."
  (append (opencode--get "/project/workspaces") nil))

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

;;; ---------------------------------------------------------------------------
;;; Selection: pick an agent, remember it (layers A + B)

(defun opencode--project-label (project-directory)
  "A short human label for PROJECT-DIRECTORY (its basename), or \"?\"."
  (if (and project-directory (not (string-empty-p project-directory)))
      (file-name-nondirectory (directory-file-name project-directory))
    "?"))

(defun opencode--pick-agent (query-directory &optional project-label)
  "Pick an agent of the workspace QUERY-DIRECTORY belongs to.
Fetches the workspace's agents, prompts for one, and returns a selection
plist (:log-id :label :directory).  PROJECT-LABEL, when given, names the
project in menu/echo text; otherwise it is derived from the resolved
project directory.  Signals `user-error' when the directory is not an
open project or has no agents."
  (let ((response (opencode--fetch-logs query-directory)))
    (when (opencode--project-null-p response)
      (user-error
       "OpenCode: %s is not an open project — open it in the web UI (%s) first"
       query-directory opencode-server-url))
    (let* ((project-directory (opencode--project-directory response))
           (label (or project-label (opencode--project-label project-directory)))
           (logs (append (opencode--logs-vector response) nil))
           (choices
            (mapcar
             (lambda (log)
               (cons (format "%s (#%s)"
                             (alist-get 'title log "(untitled)")
                             (alist-get 'logID log))
                     log))
             logs)))
      (when (null choices)
        (user-error
         "OpenCode: no agents in %s yet — create one in the web UI (%s)"
         label opencode-server-url))
      (let* ((pick (completing-read
                    (format "Agent in %s: " label) choices nil t))
             (log (cdr (assoc pick choices)))
             (log-id (alist-get 'logID log))
             (title (alist-get 'title log "(untitled)")))
        (list :log-id log-id
              :label (format "%s / %s" label title)
              :directory project-directory)))))

(defun opencode--pick-workspace ()
  "Prompt for one of the daemon's known workspaces.
Returns the chosen workspace alist.  Only workspaces with a usable
directory are offered.  Signals `user-error' when none qualify."
  (let* ((workspaces
          (seq-filter (lambda (ws)
                        (let ((dir (alist-get 'directory ws)))
                          (and dir (not (string-empty-p dir)))))
                      (opencode--fetch-workspaces)))
         (choices
          (mapcar
           (lambda (ws)
             (let* ((dir (alist-get 'directory ws))
                    (name (or (alist-get 'projectName ws)
                              (alist-get 'name ws)
                              (opencode--project-label dir))))
               (cons (format "%s — %s" name (abbreviate-file-name dir)) ws)))
           workspaces)))
    (when (null choices)
      (user-error "OpenCode: the daemon reports no open workspaces (%s)"
                  opencode-server-url))
    (cdr (assoc (completing-read "Workspace: " choices nil t) choices))))

(defun opencode--remember (root selection)
  "Store SELECTION against project ROOT (layer A) and as sticky default (layer B)."
  (setf (alist-get root opencode-agent-alist nil nil #'string=) selection)
  (setq opencode-agent-current selection)
  selection)

(defun opencode--choose-agent (cross-project)
  "Interactively choose an agent and remember it (layers A + B).
With CROSS-PROJECT non-nil, first pick ANY workspace the daemon knows —
across every project — then one of its agents; otherwise pick among the
current project's agents.  Refetches each call, so agents created in the
web UI appear immediately.  Returns the selection plist."
  (let* ((root (opencode--root))
         (selection
          (if cross-project
              (let ((ws (opencode--pick-workspace)))
                (opencode--pick-agent
                 (alist-get 'directory ws)
                 (or (alist-get 'projectName ws)
                     (alist-get 'name ws)
                     (opencode--project-label (alist-get 'directory ws)))))
            (opencode--pick-agent root))))
    (opencode--remember root selection)
    selection))

;;;###autoload
(defun opencode-select-agent (&optional cross-project)
  "Select which OpenCode agent this buffer's messages go to.
Without a prefix argument, lists the agents of the workspace the current
directory belongs to.  With a prefix argument (\\[universal-argument]),
CROSS-PROJECT is non-nil: first pick ANY workspace the daemon knows —
across every project — then pick one of its agents.  Use that to message,
for example, the `guild' project's agent from a buffer in another repo.

The pick is remembered against this project root AND as the global sticky
default (used from buffers whose root has no selection of its own)."
  (interactive "P")
  (let ((selection (opencode--choose-agent cross-project)))
    (message "OpenCode: %s → %s (logID %s)"
             (abbreviate-file-name (opencode--root))
             (plist-get selection :label)
             (plist-get selection :log-id))
    selection))

(defun opencode--selection-for-root ()
  "Return the selection plist in effect for the current root, or nil.
Layer A (this root's own pick) wins; otherwise layer B (the global
sticky default)."
  (let ((root (opencode--root)))
    (or (alist-get root opencode-agent-alist nil nil #'string=)
        opencode-agent-current)))

(defun opencode--agent-selection (&optional prompt-if-missing)
  "Return the selection plist in effect for the current buffer.
When none is selected and PROMPT-IF-MISSING is non-nil, run
`opencode-select-agent' to pick one; otherwise signal `user-error'."
  (or (opencode--selection-for-root)
      (when prompt-if-missing (opencode-select-agent))
      (user-error "OpenCode: no agent selected (M-x opencode-select-agent)")))

;;;###autoload
(defun opencode-which-agent ()
  "Report which OpenCode agent this buffer currently messages."
  (interactive)
  (let* ((root (opencode--root))
         (own (alist-get root opencode-agent-alist nil nil #'string=))
         (selection (or own opencode-agent-current)))
    (if (not selection)
        (message "OpenCode: no agent selected for %s (M-x opencode-select-agent)"
                 (abbreviate-file-name root))
      (message "OpenCode: %s → %s (logID %s)%s"
               (abbreviate-file-name root)
               (plist-get selection :label)
               (plist-get selection :log-id)
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

(defun opencode--context-preamble (selection)
  "Build a markdown preamble describing the current editor context.
The file is named for SELECTION (relative for a same-project agent,
absolute for a cross-project one).  Active region → the selected lines
with a line range; otherwise the file and the line at point.  Returns a
string ending in a blank line."
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

(defun opencode--send (selection text)
  "POST TEXT as a user message to the agent named by SELECTION.
Signals `opencode-error' on failure, so a caller that owns durable text
(the compose buffer) can keep it when the send did not land."
  (let ((log-id (plist-get selection :log-id))
        (payload `((parts . [((type . "text") (text . ,text))]))))
    (opencode--post-json (format "/log-id/%s/message" log-id) payload)))

(defun opencode--sent-message (selection)
  "Echo a \"sent to ...\" confirmation for SELECTION."
  (message "OpenCode: sent to %s (logID %s) — watch the reply in the web UI (%s)"
           (plist-get selection :label)
           (plist-get selection :log-id)
           opencode-server-url))

;;;###autoload
(defun opencode-message (message)
  "Send MESSAGE plus the current editor context to the selected agent.
Interactively, saves the current buffer first (so the agent reads your
latest on disk), captures context (active region with line numbers, or
the file and line at point), and prompts for MESSAGE.  Watch the reply
in the OpenCode web UI.

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

(defun opencode--compose-header-line ()
  "Header-line string for a compose buffer (agent name + key hints).
Kept out of the buffer body so it can never leak into the sent message."
  (format " OpenCode → %s    C-c C-c send · C-c C-k discard"
          (if opencode--compose-selection
              (plist-get opencode--compose-selection :label)
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
Reuses an existing compose buffer targeting the same logID — so repeated
`opencode-compose' calls from anywhere accumulate into one draft — else
creates a fresh one bound to SELECTION."
  (let ((log-id (plist-get selection :log-id)))
    (or (seq-find
         (lambda (buf)
           (with-current-buffer buf
             (and (derived-mode-p 'opencode-compose-mode)
                  opencode--compose-selection
                  (equal (plist-get opencode--compose-selection :log-id) log-id))))
         (buffer-list))
        (let ((buf (generate-new-buffer
                    (format "*opencode compose: %s*"
                            (plist-get selection :label)))))
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
at end, where you type your comment.

With a prefix argument (\\[universal-argument]), CROSS-PROJECT is non-nil:
first pick the target agent — ANY workspace the daemon knows, across every
project — then start (or resume) its compose buffer.  Use that to draft a
message to, say, the `guild' project's agent from a buffer in another repo.
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
         ;; Compute the preamble in the SOURCE buffer (region/point/file).
         (preamble (opencode--context-preamble selection))
         (buf (opencode--compose-buffer selection)))
    (with-current-buffer buf
      (opencode--compose-append preamble))
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
