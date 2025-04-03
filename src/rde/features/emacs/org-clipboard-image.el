(defvar my-project-rel-store-path "store/sha256/")

(defun make-relative-path (file-storage-path current-path)
  "Return the relative path from current-path to file-storage-path.
   Both paths should share a common prefix."
  (let* ((abs-file (expand-file-name file-storage-path))
         (abs-current (expand-file-name current-path))
         (relative (file-relative-name abs-file (file-name-directory abs-current))))
    (concat "./"
            (if (string= "./" (substring relative 0 2))
                (substring relative 2)
              relative))))

(defun my-org-insert-from-clipboard ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  ;; TODO: assert for Wayland
  (let* ((hash (string-trim-right (shell-command-to-string "wl-paste | sha256sum | sed 's/[- ]//g'"))) ;; TODO: get values from guix store
         (project-root-path (project-root (project-current)))
         (project-store-path (concat project-root-path my-project-rel-store-path))
         (store-rel-path (make-relative-path project-store-path (buffer-file-name)))
         (store-item-rel-path (concat store-rel-path hash ".png")))
    (shell-command (concat "mkdir -p " project-store-path))
    (shell-command (concat "wl-paste > " store-item-rel-path))
    (insert (concat "[[" store-item-rel-path "]]")))
  ;; (org-display-inline-images)
  )
