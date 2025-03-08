(define-module (rde features org-paste-inline-image)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)

  #:use-module (rde packages)

  #:use-module (gnu packages base)
  #:use-module (gnu packages xdisorg) ;; wl-clipboard
  #:use-module (gnu packages base)    ;; coreutils, sed

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module (guix gexp)

  #:export (feature-org-paste-inline-image
            feature-org-copy-inline-image))

;; Emacs has yank-media, which may be a better way to paste.
;; I'm not confident with it, so a dumb hard-coded solution it is.

(define* (feature-org-paste-inline-image
          #:key
          (store-path "~/notes/org-roam/store/sha256"))

  (define (get-home-services config)
    (define emacs-f-name 'org-paste-inline-image)

    (list
     (simple-service
      'org-paste-inline-image-add-packages
      home-profile-service-type
      (list
       wl-clipboard
       coreutils
       sed))

     (rde-elisp-configuration-service
      emacs-f-name
      config
      (let ((wl-paste  (file-append wl-clipboard "/bin/wl-paste"))
            (mkdir     (file-append coreutils    "/bin/mkdir"))
            (sha256sum (file-append coreutils    "/bin/sha256sum"))
            (sed       (file-append sed          "/bin/sed"))
            )

        ;; (format #t "~a" wl-paste)
        `((defun org-paste-inline-image ()
           "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
           (interactive)
           ;; TODO: assert for Wayland

           (let* ((item-hash (string-trim-right (shell-command-to-string (concat ,wl-paste " | " ,sha256sum " | " ,sed " 's/[- ]//g'"))))
                  ;; TODO set proper file extension or make emacs display images with no extensions
                  ;; wl-copy -l to get a hint of mime type
                  ;; or `identify  -format '%m\n' -quiet' of imagemagick
                  (store-item-path (concat ,store-path "/" item-hash ".png")))
             (shell-command (concat ,mkdir " -p " ,store-path))
             (shell-command (concat ,wl-paste " > " store-item-path))
             (insert (concat "[[" store-item-path "]]")))
           ;; (org-display-inline-images)
           )))

      #:summary ""
      #:commentary ""
      #:keywords '(convenience org))
     ))

  (feature
   (name 'org-paste-inline-image)
   (values `((org-paste-inline-image . #t)))
   (home-services-getter get-home-services)))

(define* (feature-org-copy-inline-image)

  (define (get-home-services config)
    (define emacs-f-name 'org-copy-inline-image)

    (list
     (simple-service
      'org-copy-inline-image-add-packages
      home-profile-service-type
      (list
       wl-clipboard))

     (rde-elisp-configuration-service
      emacs-f-name
      config
      (let ((wl-copy  (file-append wl-clipboard "/bin/wl-copy")))

        `((defun org-copy-inline-image ()
           "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
           (interactive)
           (let* ((file-relative-url (ffap-next-guess)))
             (if (not (s-starts-with? "file:" file-relative-url))
                 (message "Did not find file:... at point.")
               (let* ((file-relative-path (string-remove-prefix "file:" file-relative-url))
                      (base-path default-directory)
                      (file-path (concat base-path file-relative-path)))
                 (shell-command (concat ,wl-copy " < " file-path))
                 (message (concat "Copied to clipboard: " file-path))))))))

      #:summary ""
      #:commentary ""
      #:keywords '(convenience org))))

  (feature
   (name 'org-copy-inline-image)
   (values `((org-copy-inline-image . #t)))
   (home-services-getter get-home-services)))
