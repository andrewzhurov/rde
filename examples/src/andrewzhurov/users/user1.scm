(define-module (andrewzhurov users user1)
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
     realtek-firmware ;; for wifi, I suppose
     atheros-firmware ;; mayb for wifi
     iwlwifi-firmware ;; mayb for wifi
     )
    (strings->packages
     "figlet" ;; TODO: Move to emacs-artist-mode
     "calibre"
     "icecat" ;; "nyxt"

     "hut"
     "utox" "qtox"
     ;; "jami"

     "alsa-utils" ;; "cozy" ;; commented on 2025-02-07
     "pavucontrol" "wev"
     "imagemagick"
     "obs" "obs-wlrobs"
     "recutils" "binutils" "make" "gdb"

     "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra"
     "papirus-icon-theme" ; 85k files
     "arc-theme"
     "thunar" "fd"
     ;; "glib:bin"

     "libreoffice" ;; perhaps use the feature instead
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

(define (wallpaper url hash)
  (origin
    (method url-fetch)
    (uri url)
    (file-name "wallpaper.png")
    (sha256 (base32 hash))))

(define wallpaper-ai-art
  (wallpaper "https://w.wallhaven.cc/full/j3/wallhaven-j3m8y5.png"
             "0qqx6cfx0krlp0pxrrw0kvwg6x40qq9jic90ln8k4yvwk8fl1nyw"))

(define wallpaper-dark-rider
  (wallpaper "https://w.wallhaven.cc/full/lm/wallhaven-lmlzwl.jpg"
             "01j5z3al8zvzqpig8ygvf7pxihsj2grsazg9yjiqyjgsmp00hpaf"))


(define sway-extra-config-service
  (simple-service
   'sway-extra-config
   home-sway-service-type
   `((output DP-2 scale 2)
     ;; (output * bg ,wallpaper-ai-art center)
     ;; (output eDP-1 disable)
     ,@(map (lambda (x) `(workspace ,x output DP-2)) (iota 8 1))

     ;; (workspace 9 output DP-2)
     ;; (workspace 10 output DP-2)

     ;; (bindswitch --reload --locked lid:on exec /run/setuid-programs/swaylock)

     (bindsym
      $mod+a
      exec "chromium --app=http://localhost:8000")

     (bindsym
      $mod+Shift+a
      exec "chromium --app=http://localhost:3001")

     (bindsym
      --locked $mod+Shift+t exec
      ,(file-append (@ (gnu packages music) playerctl) "/bin/playerctl")
      play-pause)

     (bindsym
      --locked $mod+Shift+n exec
      ,(file-append (@ (gnu packages music) playerctl) "/bin/playerctl")
      next)

     (bindsym $mod+Shift+o move workspace to output left)
     (bindsym $mod+Ctrl+o focus output left)
     (input type:touchpad
            ;; TODO: Move it to feature-sway or feature-mouse?
            (;; (natural_scroll enabled)
             (tap enabled)))

     ;; (xwayland disable)
     (bindsym $mod+Shift+Return exec emacs)

     ;; Andrew Zhurov's config below
     (output DP-2)
     ;; This worked fine for TV in Minsk
     ;; (output DP-2 mode --custom 1920x1082) ;; tried both this
     ;; (output DP-2 pos 0 0 scale 1) ;; + this

     ;; (output DP-2 mode --custom 2133x1200) ;; tried both this
     (output DP-2 pos 0 0 scale 1) ;; + this

     ;; (output eDP-1 pos 0 1080 res 1600x900 scale 1)
     ;; (output DP-2 pos 0 0 res 1920x1080 scale 1) ;; and that
     (output DP-2 mode --custom 1600x900)
     ;; 2133x1200 close to max width-wide 16:9 I could get working
     ;; 1920x1082 behaves fine
     (workspace 9 output DP-2)
     (workspace 10 output DP-2))))

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
   #:feature-name-prefix 'user1
   #:home-services
   (list
    emacs-extra-packages-service
    home-extra-packages-service
    sway-extra-config-service
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
   rde-base
   rde-desktop
   ;; rde-mail
   rde-cli
   rde-emacs))

(define keyboard-features
  (list
   (feature-swaykbdd)))

(define %all-features
  (append
   keyboard-features
   virtualization-features
   dev-features
   general-features))

(define-public %user1-features
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
    (feature-git)
    (feature-additional-services)
    (feature-user-info
     #:user-name "user1"
     #:full-name "Andrew Zhurov"
     #:email "zhurov.andrew@gmail.com"
     #:user-initial-password-hash "$6$abc$K.ndnqzH77koKCx7UOhpgavtAONRhv5fdNjuvdD43nST2lOTPtNatrzEdiT8L85IybqFwYmkAYiaL.1MoIBBq1"
     ;; (crypt "bob" "$6$abc")
     ;; #:timezone  "Asia/Minsk"
     #:emacs-advanced-user? #t)

    (feature-gnupg
     #:gpg-primary-key "E90D83EC1ED2270386E31FD9FB17D084738D8F62"
     ;; #:ssh-keys '(("58AAE5966479124A357F7D6B9D710EBA1C24E10E"))
     )

    ;; (feature-security-token)
    (feature-password-store
     #:password-store-directory "/home/user1/password-store"
     ;; #:remote-password-store-url "ssh://abcdw@olorin.lan/~/state/password-store"
     )

    (feature-swaylock
     ;; swaylock-effects often crashes with red screen
     ;; #:swaylock (@ (gnu packages wm) swaylock-effects)
     ;; The blur on lock screen is not privacy-friendly.
     ;; #:extra-config '((screenshots)
     ;;                  (effect-blur . 7x5)
     ;;                  (clock))
     )

    ;; (feature-mail-settings
    ;;  #:mail-directory-fn (const "/home/user1/mail")
    ;;  #:mail-accounts (list
    ;;                   (mail-account
    ;;                    (id 'personal)
    ;;                    (type 'gmail)
    ;;                    (fqda "zhurov.andrew@gmail.com")
    ;;                    (aliases '())
    ;;                    (pass-cmd "pass show mail/personal")))
    ;;  #:mailing-lists (list (mail-lst 'guile-devel "guile-devel@gnu.org"
    ;;                                  '("https://yhetil.org/guile-devel/0"))
    ;;                        (mail-lst 'guix-devel "guix-devel@gnu.org"
    ;;                                  '("https://yhetil.org/guix-devel/0"))
    ;;                        (mail-lst 'guix-bugs "guix-bugs@gnu.org"
    ;;                                  '("https://yhetil.org/guix-bugs/0"))
    ;;                        (mail-lst 'guix-patches "guix-patches@gnu.org"
    ;;                                  '("https://yhetil.org/guix-patches/1"))))


    ;; (feature-notmuch
;;      #:extra-tag-updates-post
;;      '("notmuch tag +guix-home +inbox -- 'thread:\"\
;; {((subject:guix and subject:home) or (subject:service and subject:home) or \
;; subject:/home:/) and tag:new}\"'"
;;        "notmuch tag +rde +list -- 'to:.*rde.*@lists.sr.ht and tag:new}'")
;;      #:notmuch-saved-searches
;;      (append
;;       ;; TODO: [Andrew Tropin, 2024-01-07] Archive replied emails automatically
;;       '((:name "To Process"
;;          :query "tag:todo or (tag:inbox and not tag:unread and not tag:replied)"
;;          :key "t")
;;         (:name "Drafts" :query "tag:draft" :key "d")
;;         (:name "Watching" :query "thread:{tag:watch} and tag:unread" :key "w")
;;         (:name "Work Inbox"
;;          :query "tag:work and tag:inbox and tag:unread"
;;          :key "W")
;;         (:name "Personal Inbox"
;;          :query "tag:personal and tag:inbox and tag:unread"
;;          :key "P")
;;         (:name "Guix Home Inbox" :key "H" :query "tag:guix-home and tag:unread"))
;;       ;; %rde-notmuch-saved-searches
;;       '()))

    ;; (feature-irc-settings
    ;;  #:irc-accounts (list
    ;;                  (irc-account
    ;;                   (id 'srht)
    ;;                   (network "chat.sr.ht")
    ;;                   (bouncer? #t)
    ;;                   (nick "andrew.zhurov"))
    ;;                  (irc-account
    ;;                   (id 'libera)
    ;;                   (network "irc.libera.chat")
    ;;                   (nick "andrew.zhurov"))
    ;;                  (irc-account
    ;;                   (id 'oftc)
    ;;                   (network "irc.oftc.net")
    ;;                   (nick "andrew.zhurov"))))

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
    ;; (feature-emacs-tempel
    ;;  #:default-templates? #t
    ;;  #:templates
    ;;  `(fundamental-mode
    ;;    ,#~""
    ;;    (t (format-time-string "%Y-%m-%d"))
    ;;    (todo
    ;;     (if (derived-mode-p 'lisp-data-mode 'clojure-mode 'scheme-mode)
    ;;         ";;"
    ;;         comment-start)
    ;;     (if (string-suffix-p " " comment-start) "" " ")
    ;;     "TODO"  ": [" user-full-name ", "
    ;;     (format-time-string "%Y-%m-%d") "] ")))
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
     #:org-directory "/home/user1/notes/org-roam"
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
     #:org-roam-directory "/home/user1/notes/org-roam")
    (feature-emacs-org-agenda
     #:org-agenda-files '("/home/user1/notes/org-roam"
                          "/home/user1/notes/org-roam/daily"))

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
    (feature-sway-screenrecord)
    ;; (feature-emacs-org-roam-ui)

    (feature-keyboard
     ;; To get all available options, layouts and variants run:
     ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
     #:keyboard-layout
     (keyboard-layout
      "us,ru" "dvorak,"
      #:options '("grp:shifts_toggle" "ctrl:nocaps"))))))
