(define-module (andrewzhurov configs)
  #:use-module (abcdw feature-lists)
  #:use-module (andrewzhurov hosts t450)

  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features security-token)
  #:use-module (rde features keyboard)
  #:use-module (rde features system)
  #:use-module (rde features xdg)
  #:use-module (rde features password-utils)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features mail)
  #:use-module (rde features irc)
  #:use-module (rde features networking)
  #:use-module (rde features clojure)
  #:use-module (rde features gitwatch)
  #:use-module (rde features wm)
  #:use-module (contrib features javascript)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (rde home services i2p)
  #:use-module (rde home services emacs)
  #:use-module (rde home services wm)

  #:use-module (gnu home-services ssh)

  #:use-module (gnu packages)
  #:use-module (rde packages)

  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (ice-9 match))



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
    (elisp-packages
     (append
      (strings->packages
       ;; "emacs-dirvish"
       "emacs-hl-todo"
       "emacs-yasnippet"
       ;; "emacs-company"
       "emacs-consult-dir"
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
       "emacs-tide"
       "emacs-rustic"
       "emacs-rust-mode"
       "emacs-flycheck" ;; for clj-kondo for Clojure
       ))))))

(define home-extra-packages-service
  (simple-service
   'home-profile-extra-packages
   home-profile-service-type
   (append
    (strings->packages
     "figlet" ;; TODO: Move to emacs-artist-mode
     "calibre"
     "icecat" "nyxt"
     "ungoogled-chromium-wayland" "ublock-origin-chromium"

     "utox" "qtox" "jami"

     "alsa-utils" "youtube-dl" "imv" "cozy"
     "pavucontrol" "wev"
     "imagemagick"
     "obs" "obs-wlrobs"
     "recutils" "binutils" "make"
     ;; "fheroes2"

     "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra"
     "papirus-icon-theme" "arc-theme"
     "thunar" "fd"
     ;; "glib:bin"

     "libreoffice"
     "ffmpeg"
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
     "icedtea" ;; breaks shadow-cljs for rent

     "clj-kondo"
     ;; "emacs-clojure-mode" ;; does it work?
     ;; "emacs-cider" ;; mayb required by clojure - emacs integration ;; in feature
     ;; "emacs-lsp-mode"
     ;; "emacs-lsp-ui"
     ;; "emacs-company-lsp" ;; for better completion
     ;; "emacs-eglot" ;; for automatic LSP launch for current file, mode-based
     ;; "emacs-lsp-java" ;; seems to add Java support for LSP

     "emacs-org-tanglesync" ;; for literate programming, I think I recall

     "sqlite" ;; for unisonweb

     "qbittorrent"

     ;; "fastboot" "android-udev-rules" "adb" ;; for android USB OS install, some of these packages may not be needed though. I ended up having my own version of fastboot built
     ;; (@ (rde packages scrcpy) scrcpy) ;; error
     ;; scrcpy ;; error
     ;; "scrcpy" ;; package not found

     ;; "nmtui" ;; is not a package, seems to come included with guix profile; or mayb comes from 'network-manager'; for network (wifi incl.) management

     "realtek-firmware" ;; for wifi, I suppose
     "atheros-firmware" ;; mayb for wifi
     "iwlwifi-firmware" ;; mayb for wifi

     "mplayer" ;; seems to be for mpv

     "awscli"
     "p7zip"

     ;; "libopenshot" ;; build error
     ;; "pitivi" ;; build error
     ;; "gstreamer" ;; some error
     ;; "blender" ;; display error

     ))))

(define sway-extra-config-service
  (simple-service
   'sway-extra-config
   home-sway-service-type
   `((bindsym
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
     (workspace 10 output DP-2)
     )))


;;; User-specific features with personal preferences

;; Initial user's password hash will be available in store, so use this
;; feature with care (display (crypt "hi" "$6$abc"))

(define %andrewzhurov-features
  (list
   (feature-user-info
    #:user-name "user1"
    #:full-name "Andrew Zhurov"
    #:email "zhurov.andrew@gmail.com"
    ;; #:user-initial-password-hash
    ;; "$6$abc$3SAZZQGdvQgAscM2gupP1tC.SqnsaLSPoAnEOb2k6jXMhzQqS1kCSplAJ/vUy2rrnpHtt6frW2Ap5l/tIvDsz."
    #:emacs-advanced-user? #f)
   (feature-gnupg
    #:gpg-primary-key "AE2DD20B5BCB36A3")

   (feature-mail-settings
    #:mail-accounts (list (mail-acc 'personal       "zhurov.andrew@gmail.com")
			  )

    ;; #:mailing-lists (list)
    ;; #:mailing-lists (list (mail-lst 'guix-devel "guix-devel@gnu.org"
    ;;                                 '("https://yhetil.org/guix-devel/0"))
    ;;                       (mail-lst 'guix-bugs "guix-bugs@gnu.org"
    ;;                                 '("https://yhetil.org/guix-bugs/0"))
    ;;                       (mail-lst 'guix-patches "guix-patches@gnu.org"
    ;;                                 '("https://yhetil.org/guix-patches/1")))
    )

   (feature-custom-services
    #:feature-name-prefix 'abcdw
    #:home-services
    (list
     emacs-extra-packages-service
     home-extra-packages-service
     sway-extra-config-service))

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

   (feature-emacs-keycast #:turn-on? #f)
   ;; (feature-emacs-spelling
   ;;  #:spelling-program (@ (gnu packages libreoffice) hunspell)
   ;;  #:spelling-dictionaries (strings->packages
   ;;                           "hunspell-dict-en"
   ;;                           "hunspell-dict-ru"))
   (feature-emacs-git
    #:project-directory "~/work")
   (feature-emacs-org
    #:org-directory "~/notes/org-roam"
    #:org-indent? #f
    #:org-capture-templates
    `(("t" "Todo" entry (file+headline "" "Tasks") ;; org-default-notes-file
       "* TODO %?\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t)))
   (feature-emacs-org-roam
    #:org-roam-directory "~/notes/org-roam")
   (feature-emacs-org-agenda
    #:org-agenda-files '("~/notes/org-roam"
                         "~/notes/org-roam/daily"))

   (feature-javascript
    #:node (@ (gnu packages node) node-lts))

   (feature-keyboard
    ;; To get all available options, layouts and variants run:
    ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
    #:keyboard-layout
    (keyboard-layout
     "us,ru" "dvorak,"
     #:options '("grp:shifts_toggle" "ctrl:nocaps")))

   (feature-gitwatch)
   (feature-sway-screenrecord)
   ;; (feature-emacs-rust)
   ;; (feature-ucm)
   (feature-emacs-org-roam-ui)
   ;; (feature-markdown)
   (feature-clojure)
   ;; (feature-emacs-eglot) ;; comes from somewhere else already
   ))


;;; t450

(define-public t450-config
  (rde-config
   (features
    (append
     %andrewzhurov-features
     %all-features
     %t450-features))))

(define-public t450-os
  (rde-config-operating-system t450-config))

(define-public t450-he
  (rde-config-home-environment t450-config))


;;; Dispatcher, which helps to return various values based on environment
;;; variable value.

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("t450-home" t450-he)
      ("t450-system" t450-os)
      (_ t450-he))))

(dispatcher)
