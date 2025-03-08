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
