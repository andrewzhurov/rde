(define-module (andrewzhurov users keeper)
  #:use-module (contrib features javascript)
  #:use-module (contrib packages node)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu packages)
  #:use-module (gnu packages node)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (guix channels)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (rde features base)
  #:use-module (rde features clojure)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features gnupg)
  #:use-module (rde features irc)
  #:use-module (rde features keyboard)
  #:use-module (rde features mail)
  #:use-module (rde features networking)
  #:use-module (rde features password-utils)
  #:use-module (rde features security-token)
  #:use-module (rde features system)
  #:use-module (rde features xdg)
  #:use-module (rde features markup)
  #:use-module (rde features docker)
  #:use-module (rde features virtualization)
  #:use-module (rde features ocaml)
  #:use-module (rde features presets)
  #:use-module (rde features video)
  #:use-module (rde features version-control)
  #:use-module (rde features terminals)
  #:use-module (rde features rust)
  #:use-module (rde features arei)
  #:use-module (rde features web-browsers)
  #:use-module (rde features bittorrent)
  #:use-module (rde features)
  #:use-module (rde home services emacs)
  #:use-module (rde home services i2p)
  #:use-module (rde home services wm)
  #:use-module (rde home services video)
  #:use-module (rde packages aspell)
  #:use-module (rde packages package-management)
  #:use-module (rde packages)

  #:use-module (rde features gitwatch)
  #:use-module (rde features org-paste-inline-image)
  #:use-module (rde features wm)
  #:use-module (rde packages clj-kondo)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)


  #:use-module (srfi srfi-1))


;;; Helpers

(define* (mail-acc id user #:optional (type 'gmail))
  "Make a simple mail-account with gmail type by default."
  (mail-account
   (id   id)
   (fqda user)
   (type type)))

(define* (mail-lst id fqda urls)
  "Make a simple mailing-list."
  (mailing-list
   (id   id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))


;;; Service extensions

(define emacs-extra-packages-service
  (simple-service
   'emacs-extra-packages
   home-emacs-service-type
   (home-emacs-extension
    (init-el
     `((with-eval-after-load 'piem
         (setq piem-inboxes
               '(("guix-devel"
                  :url "https://yhetil.org/guile-devel/"
                  :address "guile-devel@gnu.org"
                  :coderepo "~/work/gnu/guile/")
                 ("guix-patches"
                  :url "https://yhetil.org/guix-patches/"
                  :address "guix-patches@gnu.org"
                  :coderepo "~/work/gnu/guix/")
                 ("rde-devel"
                  :url "https://lists.sr.ht/~abcdw/rde-devel"
                  :address "~abcdw/rde-devel@lists.sr.ht"
                  :coderepo "~/work/abcdw/rde/"))))
       (setq vterm-max-scrollback 100000)
       (setq org-media-note-screenshot-extension ".png") ;; default is jpeg, excalidraw does not import it
       ;; (setq load-prefer-newer t)
       ;; (setq load-no-native t) ;; otherwise emacs-jsonrpc breaks for eglot
       ;; (setq native-comp-deferred-compilation-deny-list '("jsonrpc"))
       (global-auto-revert-mode)
       (with-eval-after-load 'org
         (setq org-use-speed-commands t)
         (define-key org-mode-map (kbd "M-o")
           (lambda ()
             (interactive)
             (org-end-of-meta-data t))))
       (with-eval-after-load 'geiser-mode
         (setq geiser-mode-auto-p nil)
         (defun abcdw-geiser-connect ()
           (interactive)
           (geiser-connect 'guile "localhost" "37146"))

         (define-key geiser-mode-map (kbd "C-c aM-j") 'abcdw-geiser-connect))
       (with-eval-after-load 'simple
         (setq-default display-fill-column-indicator-column 80)
         (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))
       (setq copyright-names-regexp
             (format "%s <%s>" user-full-name user-mail-address))
       (add-hook 'after-save-hook (lambda () (copyright-update nil nil)))

       (with-eval-after-load 'crux
         (global-set-key [remap rename-file] #'crux-rename-file-and-buffer)
         (global-set-key [remap delete-file] #'crux-delete-file-and-buffer))
       ))
    (elisp-packages
     (append
      (list
       (@ (rde packages emacs-xyz) emacs-clojure-ts-mode)
       ;; (@ (rde packages emacs-xyz) emacs-jsonrpc-1.0.25)
       ;; (@ (rde packages emacs-xyz) emacs-eglot-1.17)
       ;; (@ (rde packages emacs-xyz) emacs-combobulate)
       )
      (strings->packages
       ;; "emacs-dirvish"
       "emacs-piem"
       ;; "emacs-company"
       "emacs-ox-haunt"
       "emacs-haskell-mode"
       "emacs-rainbow-mode"
       "emacs-hl-todo"
       "emacs-yasnippet"
       ;; "emacs-company"
       ;; "emacs-consult-dir"
       ;; "emacs-all-the-icons-completion" "emacs-all-the-icons-dired"
       "emacs-kind-icon"
       "emacs-nginx-mode" "emacs-yaml-mode"
       ;; "emacs-lispy"
       "emacs-ytdl"
       "emacs-multitran"
       "emacs-minimap"
       "emacs-ement"
       "emacs-restart-emacs"
       "emacs-org-present"

       ;; andrewzhurov's stuff below
       "emacs-minimap"
       "emacs-paredit"
       "emacs-dired-hacks"
       "emacs-aggressive-indent"
       "emacs-typescript-mode"
       ;; "emacs-tide"
       "emacs-crux"
       ))))))


(define home-extra-packages-service
  (simple-service
   'home-profile-extra-packages
   home-profile-service-type
   (append
    (list
     (@ (gnu packages tree-sitter) tree-sitter-clojure)
     (@ (gnu packages tree-sitter) tree-sitter-html)
     (@ (rde packages clj-kondo) clj-kondo)
     ;; guix-from-channels-lock ;; perhaps this will spare guix deps on `guix gc`, may override `guix` binary, so guix pull will be in vain
     ;; realtek-firmware ;; for wifi, I suppose
     ;; atheros-firmware ;; mayb for wifi
     ;; iwlwifi-firmware ;; mayb for wifi
     )
    (strings->packages
     "calibre"
     "icecat"

     "imagemagick"
     "obs" "obs-wlrobs"
     "binutils" "make"

     "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra"
     "papirus-icon-theme" ; 85k files
     "arc-theme"
     "thunar" "fd"

     "libreoffice"
     ;; "ffmpeg"
     "ripgrep" "curl"

     ;; Andrew Zhurov's stuff below
     ;; clojure-related stuff
     ;; "clojure" ;; already in feature
     "rlwrap" ;; mayb required by clojure cli
     ;; "node" ;; mayb required for shadow-cljs
     ;; "supercollider" ;; does it work?
     ;; "leiningen" ;; does it work?
     ;; already in feature
     ;; "clojure-tools" ;; that may provide `clj` command line util, not tested. required by cider-jack-in Emacs command
     ;; "clojure-tools-cli" ;; does it work?
     ;; "openjdk" ;; breaks cider for daylight
     ;; "icedtea" ;; breaks shadow-cljs for rent

     ;; "clj-kondo"
     ;; "emacs-clojure-mode" ;; does it work?
     ;; "emacs-cider" ;; mayb required by clojure - emacs integration ;; in feature
     ;; "emacs-lsp-mode"
     ;; "emacs-lsp-ui"
     ;; "emacs-company-lsp" ;; for better completion
     ;; "emacs-eglot" ;; for automatic LSP launch for current file, mode-based
     ;; "emacs-lsp-java" ;; seems to add Java support for LSP

     ;; "emacs-org-tanglesync" ;; for literate programming, I think I recall

     ;; "sqlite" ;; for unisonweb

     ;; "qbittorrent" ;; we've got built-in transmission

     ;; "fastboot" "android-udev-rules" "adb" ;; for android USB OS install, some of these packages may not be needed though. I ended up having my own version of fastboot built
     ;; (@ (rde packages scrcpy) scrcpy) ;; error
     ;; scrcpy ;; error
     ;; "scrcpy" ;; package not found

     ;; "nmtui" ;; is not a package, seems to come included with guix profile; or mayb comes from 'network-manager'; for network (wifi incl.) management

     ;; "awscli"
     "p7zip"

     ;; "libopenshot" ;; build error
     ;; "pitivi" ;; build error
     ;; "gstreamer" ;; some error
     ;; "blender" ;; display error
     "audacity"

     "nix"
     "emacs-arei" "guile-next" "guile-ares-rs"
     ))))


(define mpv-add-user-settings-service
  (simple-service
   'mpv-add-user-settings-irc
   home-mpv-service-type
   (home-mpv-extension
    (mpv-conf
     `((global
        ((keep-open . yes)
         (ytdl-format . "bestvideo[height<=?720][fps<=?30][vcodec!=?vp9]+bestaudio/best")
         (save-position-on-quit . yes)
         (speed . 1))))))))

(define (feature-additional-services)
  (feature-custom-services
   #:feature-name-prefix 'keeper
   #:home-services
   (list
    emacs-extra-packages-service
    ;; home-extra-packages-service
    mpv-add-user-settings-service)))

;;; User-specific features with personal preferences

;; Initial user's password hash will be available in store, so use this
;; feature with care (display (crypt "hi" "$6$abc"))

(define dev-features
  (list
   (feature-markdown)))

(define virtualization-features
  (list
   (feature-docker)
   ;; (feature-qemu)
   ))

(define general-features
  (append
   ;; rde-base
   ;; rde-desktop
   ;; rde-mail
   rde-cli
   rde-emacs))

(define keyboard-features
  (list
   (feature-swaykbdd)))

(define %all-features
  (append
   ;; keyboard-features
   ;; virtualization-features
   dev-features
   general-features))

(define-public %keeper-features
  (append
   (remove (lambda (f)
             (member
              (feature-name f)
              '(base-services
                kernel
                swaylock
                xdg
                git)))
           %all-features)
   (list
    (feature-mpv)
    ;; (feature-librewolf)
    (feature-ungoogled-chromium
     #:default-browser? #t
     #:default-startup-flags '( ;; taken from default value, may get outdated
                               "--user-data-dir=$XDG_DATA_HOME/chromium"
                               ;; required by chromium-web-store
                               ;; https://github.com/NeverDecaf/chromium-web-store?tab=readme-ov-file#installation
                               "--extension-mime-request-handling=always-prompt-for-install"
                               ;; May prevent freezing of PDF viewer, https://issues.chromium.org/issues/41496556
                               "--disable-renderer-accessibility"))
    (feature-transmission #:auto-start? #f)
    (feature-git)
    (feature-additional-services)
    (feature-user-info
     #:user-name "keeper"
     #:full-name "Andrew Zhurov"
     #:email "zhurov.andrew@gmail.com"
     ;; (crypt "bob" "$6$abc")
     ;; #:timezone  "Asia/Minsk"
     #:emacs-advanced-user? #t)

    ;; (feature-gnupg
    ;;  #:gpg-primary-key "E90D83EC1ED2270386E31FD9FB17D084738D8F62"
    ;;  ;; #:ssh-keys '(("58AAE5966479124A357F7D6B9D710EBA1C24E10E"))
    ;;  )

    ;; (feature-security-token)
    (feature-password-store
     #:password-store-directory "/home/keeper/password-store"
     ;; #:remote-password-store-url "ssh://abcdw@olorin.lan/~/state/password-store"
     )

    (feature-xdg
     #:xdg-user-directories-configuration
     (home-xdg-user-directories-configuration
      (music "$HOME/music")
      (videos "$HOME/vids")
      (pictures "$HOME/pics")
      (documents "$HOME/docs")
      (download "$HOME/dl")
      (desktop "$HOME")
      (publicshare "$HOME")
      (templates "$HOME")))

    (feature-foot)

    (feature-emacs-keycast #:turn-on? #f)
    (feature-emacs-time)
    (feature-emacs-spelling
     #:spelling-program (@ (gnu packages hunspell) hunspell)
     #:spelling-dictionaries
     (list
      (@ (gnu packages hunspell) hunspell-dict-en)
      (@ (rde packages aspell) hunspell-dict-ru)))
    (feature-emacs-git
     #:project-directory "gits")
    ;; https://plaindrops.de/blog/2020/GTDorgmode/
    ;; https://www.labri.fr/perso/nrougier/GTD/index.html#org2d62325
    (feature-emacs-org
     #:org-directory "/home/keeper/notes/org-roam"
     #:org-indent? #t
     #:org-capture-templates
     ;; https://libreddit.tiekoetter.com/r/orgmode/comments/gc76l3/org_capture_inside_notmuch/
     `(("r" "Reply" entry (file+headline "" "Tasks")
        "* TODO %:subject %?\nSCHEDULED: %t\n%U\n%a\n"
        :immediate-finish t)
       ("t" "Todo" entry (file+headline "" "Tasks") ;; org-default-notes-file
        "* TODO %?\nSCHEDULED: %t\n%a\n")
       ("p" "PhD Todo" entry
        (file+headline "~/work/abcdw/private/phd.org" "Tasks")
        "* TODO %?\nSCHEDULED: %t\n%a\n")))
    (feature-emacs-org-roam
     #:org-roam-directory "/home/keeper/notes/org-roam")
    (feature-emacs-org-agenda
     #:org-agenda-files '("/home/keeper/notes/org-roam"
                          "/home/keeper/notes/org-roam/daily"))

    ;; (feature-emacs-elfeed
    ;;  #:elfeed-org-files '("/data/abcdw/work/abcdw/private/rss.org"))

    ;; (feature-emacs-arei)
    (feature-clojure)
    (feature-javascript)
    (feature-emacs-flymake)
    (feature-rust)
    ;; (feature-emacs-guix
    ;;  #:guix-directory "~/gits/guix")

    (feature-yt-dlp)
    ;; (feature-ocaml #:opam? #t)
    (feature-gitwatch)
    (feature-org-paste-inline-image)
    (feature-org-copy-inline-image)
    (feature-emacs-org-media-note)
    ;; (feature-emacs-org-roam-ui)

    (feature-keyboard
     ;; To get all available options, layouts and variants run:
     ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
     #:keyboard-layout
     (keyboard-layout
      "us,ru" "dvorak,"
      #:options '("grp:shifts_toggle" "ctrl:nocaps"))))))
