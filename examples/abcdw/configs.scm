(define-module (abcdw configs)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (rde features security-token)
  #:use-module (rde features keyboard)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (rde features xdisorg)
  #:use-module (rde features xdg)
  #:use-module (rde features password-utils)
  #:use-module (rde features version-control)
  #:use-module (rde features fontutils)
  #:use-module (rde features terminals)
  #:use-module (rde features tmux)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features linux)
  #:use-module (rde features bittorrent)
  #:use-module (rde features mail)
  #:use-module (rde features docker)
  #:use-module (rde features virtualization)
  #:use-module (rde features video)
  #:use-module (rde features finance)
  #:use-module (rde features markup)
  #:use-module (rde features networking)
  #:use-module (gnu services)
  #:use-module (rde home services i2p)

  ;; #:use-module (gnu services nix)
  #:use-module (gnu system keyboard)
  #:use-module (rde features gitwatch)
  #:use-module (contrib features javascript)
  #:use-module (rde packages emacs-xyz)
  #:use-module (rde packages scrcpy)
  #:use-module (rde packages jet)
  #:use-module (rde packages bb)
  #:use-module (rde features clojure)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu packages)
  #:use-module (gnu packages libreoffice)
  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (ice-9 match))


;;; User-specific features

;; Initial user's password hash will be available in store, so it's
;; use this feature with care
;; (display (crypt "hi" "$6$abc"))

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

(define %abcdw-features
  (list
   (feature-user-info
    ;; #:user-initial-password-hash
    ;; "$6$abc$3SAZZQGdvQgAscM2gupP1tC.SqnsaLSPoAnEOb2k6jXMhzQqS1kCSplAJ/vUy2rrnpHtt6frW2Ap5l/tIvDsz."
    ;; (crypt "bob" "$6$abc")

    ;; WARNING: This option can reduce the explorability by hiding
    ;; some helpful messages and parts of the interface for the sake
    ;; of minimalistic, less distractive and clean look.  Generally
    ;; it's not recommended to use it.
    #:emacs-advanced-user? #f ;;#t
    #:user-name "user1"
    #:full-name "Andrew Zhurov"
    #:email "zhurov.andrew@gmail.com")
   (feature-gnupg
    #:gpg-primary-key "AE2DD20B5BCB36A3"
    ;; #:gpg-primariy-key "74830A276C328EC2"
    ;; #:gpg-smart-card? #t
    )
   (feature-security-token)
   ;; (feature-password-store
   ;;  #:remote-password-store-url "ssh://abcdw@olorin.lan/~/state/password-store")

   (feature-mail-settings
    #:mail-accounts (list (mail-acc 'personal       "zhurov.andrew@gmail.com")
			  )

    ;; #:mailing-lists (list)
    #:mailing-lists (list (mail-lst 'guix-devel "guix-devel@gnu.org"
                                    '("https://yhetil.org/guix-devel/0"))
                          (mail-lst 'guix-bugs "guix-bugs@gnu.org"
                                    '("https://yhetil.org/guix-bugs/0"))
                          (mail-lst 'guix-patches "guix-patches@gnu.org"
                                    '("https://yhetil.org/guix-patches/1")))
    )

   (feature-keyboard
    ;; To get all available options, layouts and variants run:
    ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
    #:keyboard-layout
    (keyboard-layout
     "us,ru" "dvorak,"
     #:options '("grp:shifts_toggle" "ctrl:nocaps")))))

;;; TODO: feature-wallpapers https://wallhaven.cc/
;;; TODO: feature-icecat
;; PipeWire/iwd:
;; https://github.com/J-Lentz/iwgtk
;; https://github.com/krevedkokun/guix-config/blob/master/system/yggdrasil.scm


;;; Generic features should be applicable for various hosts/users/etc


;;; WARNING: The order can be important for features extending
;;; services of other features.  Be careful changing it.
(define %main-features
  (list
   (feature-yggdrasil)
   ;; (feature-ssh-proxy #:host "pinky-ygg" #:auto-start? #f)
   ;; (feature-ssh-proxy #:host "pinky-ygg" #:name "hundredrps"
   ;;                    #:proxy-string "50080:localhost:8080"
   ;;                    #:reverse? #t
   ;;                    #:auto-start? #f)
   (feature-i2pd
    #:outproxy 'http://acetone.i2p:8888
    ;; 'purokishi.i2p
    #:less-anonymous? #t)
   (feature-custom-services
    #:feature-name-prefix 'ixy
    #:system-services
    (list
     ;; (service nix-service-type)
     )
    #:home-services
    ;; TODO: move to feature-irc-settings
    (list
     (simple-service
      'i2pd-add-ilita-irc
      home-i2pd-service-type
      (home-i2pd-extension
       (tunnels-conf
        `((IRC-ILITA ((type . client)
                      (address . 127.0.0.1)
                      (port . 6669)
                      (destination . irc.ilita.i2p)
                      (destinationport . 6667)
                      (keys . ilita-keys.dat)))))))))

   (feature-base-services
    #:guix-substitute-urls '("http://ci.guix.trop.in"
                             "https://bordeaux.guix.gnu.org"))
   (feature-desktop-services)
   (feature-docker)
   (feature-qemu)

   (feature-pipewire)
   (feature-backlight #:step 10)
   (feature-networking)

   (feature-fonts
    #:font-monospace (font "Iosevka" #:size 11 #:weight 'regular)
    ;; #:font-monospace (font "Fira Mono" #:size 14 #:weight 'semi-light)
    ;; #:font-packages (list font-fira-mono)
    #:default-font-size 11)

   (feature-alacritty
    #:config-file (local-file "./config/alacritty/alacritty.yml")
    #:default-terminal? #f
    #:backup-terminal? #t
    #:software-rendering? #f)
   (feature-vterm)
   (feature-tmux
    #:tmux-conf (local-file "./config/tmux/tmux.conf"))
   (feature-zsh
    #:enable-zsh-autosuggestions? #t)
   (feature-bash)
   (feature-direnv)
   (feature-git #:sign-commits? #f)
   (feature-ssh
    #:ssh-configuration
    (home-ssh-configuration
     (extra-config
      (append
       ;; TODO: Move it feature-qemu?
       (map (lambda (id)
              (ssh-host
               (host (format #f "qemu~a" id))
               (options
                `((host-name . "localhost")
                  (port . ,(+ 10020 id))))))
              (iota 4))
       (list
        (ssh-host
         (host "pinky-ygg")
         (options
          '((host-name . "200:554d:3eb1:5bc5:6d7b:42f4:8792:efb8")
            (port . 50621)
            (control-master . "auto")
            (control-path . "~/.ssh/master-%r@%h:%p")
            (compression . #t))))
        (ssh-host
         (host "pinky")
         (options
          '((host-name . "23.137.249.202")
            (port . 50621)
            (compression . #t)))))))
     (toplevel-options
      '((host-key-algorithms . "+ssh-rsa")
        (pubkey-accepted-key-types . "+ssh-rsa")))))

   ;; https://sr.ht/~tsdh/swayr/
   ;; https://github.com/ErikReider/SwayNotificationCenter
   ;; https://github.com/swaywm/sway/wiki/i3-Migration-Guide

   ;; https://github.com/natpen/awesome-wayland
   (feature-sway
    ;; #:xwayland? #t
    #:extra-config
    ;; `((output DP-2 scale 2)
    ;;   (output eDP-1 disable)
    ;;   ,@(map (lambda (x) `(workspace ,x output eDP-1)) (iota 8 1))

    `((output DP-2)
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

      ;; (bindswitch --reload --locked lid:on exec /run/setuid-programs/swaylock)

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
      (bindsym $mod+Shift+Return exec emacs)))
   (feature-sway-run-on-tty
    #:sway-tty-number 2)
   (feature-sway-screenshot)
   ;; (feature-sway-statusbar
   ;;  #:use-global-fonts? #f)
   (feature-waybar)
   (feature-swayidle)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    ;; The blur on lock screen is not privacy-friendly.
    #:extra-config '((screenshots)
                     (effect-blur . 7x5)
                     (clock)))
   (feature-kanshi
    #:extra-config
    `((profile laptop ((output eDP-1 enable)))
      (profile docked ((output eDP-1 enable)
                       (output DP-2 scale 2)))))
   ;; (feature-rofi)

   ;; TODO: Add an app for saving and reading articles and web pages
   ;; https://github.com/wallabag/wallabag
   ;; https://github.com/chenyanming/wallabag.el

   ;; (feature-emacs-portable)
   (feature-emacs
    #:default-application-launcher? #f
    #:extra-init-el `((load ,(local-file "./tmp.el")))
    #:additional-elisp-packages
    (append
     (strings->packages
      ;; "emacs-dirvish"
      "emacs-elfeed" "emacs-hl-todo"
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
      )))

   (feature-emacs-appearance
    #:extra-elisp
    `((setq modus-themes-syntax '(faint))
      ;; (setq modus-themes-region '(bg-only))
      ;; (setq modus-themes-paren-match '(underline))
      (setq modus-themes-org-blocks 'tinted-background)))
   (feature-emacs-faces)
   (feature-emacs-tramp)
   (feature-emacs-completion
    #:mini-frame? #f
    #:marginalia-align 'right)

   (feature-emacs-corfu
    #:corfu-auto #f
    #:corfu-doc-auto #f)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-keycast #:turn-on? #t)
   ;; (feature-emacs-spelling
   ;;  #:spelling-program (@ (gnu packages libreoffice) hunspell)
   ;;  #:spelling-dictionaries (strings->packages
   ;;                           "hunspell-dict-en"
   ;;                           "hunspell-dict-ru"))

   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-emacs-monocle)
   (feature-emacs-message)


   (feature-emacs-elpher)
   (feature-emacs-telega)
   (feature-emacs-pdf-tools)
   (feature-emacs-nov-el)

   ;; TODO: Revisit <https://en.wikipedia.org/wiki/Git-annex>
   (feature-emacs-git
    #:project-directory "~/work")
   ;; TODO: <https://www.labri.fr/perso/nrougier/GTD/index.html#table-of-contents>
   (feature-emacs-org
    #:org-directory "~/notes/org"
    #:org-indent? #t
    #:org-capture-templates
    `(("t" "Todo" entry (file+headline "" "Tasks") ;; org-default-notes-file
       "* TODO %?\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t)))
   (feature-emacs-org-roam
    ;; TODO: Rewrite to states
    #:org-roam-directory "~/notes/org-roam/")
   (feature-emacs-org-agenda
    #:org-agenda-files '("~/notes/org-roam"
                         "~/notes/org-roam/daily"
                         ;; "~/notes/org-roam/TODO"
                         ))
   (feature-emacs-org-protocol)
   (feature-emacs-elfeed
    #:elfeed-org-files '("~/work/abcdw/private/rss.org"))
   (feature-emacs-citar)
   (feature-emacs-smartparens
    #:show-smartparens? #t)
   (feature-emacs-geiser)
   (feature-emacs-guix)
   (feature-emacs-tempel
    #:default-templates? #t
    #:templates
    `(fundamental-mode
      ,#~""
      (t (format-time-string "%Y-%m-%d"))
      ;; TODO: Move to feature-guix
      ,((@ (rde gexp) slurp-file-like)
        (file-append ((@ (guix packages) package-source)
                      (@ (gnu packages package-management) guix))
                     "/etc/snippets/tempel/text-mode"))))


   (feature-ledger)
   (feature-markdown)

   (feature-mpv
    ;; #:extra-mpv-conf '((speed . 1.61))
    )

   (feature-isync #:isync-verbose #t)
   (feature-l2md)
   (feature-msmtp)
   (feature-notmuch
    ;; TODO: Add integration with mail-lists
    ;; `notmuch-show-stash-mlarchive-link-alist'
    #:extra-tag-updates-post
    '("notmuch tag +guix-home -- 'thread:\"\
{((subject:guix and subject:home) or (subject:service and subject:home) or \
subject:/home:/) and tag:new}\"'")
    #:notmuch-saved-searches
    (cons*
     ;; TODO: Add tag:unread to all inboxes.  Revisit archive workflow.
     '(:name "Work Inbox" :query "tag:work and tag:inbox and tag:unread" :key "W")
     '(:name "Personal Inbox" :query "tag:personal and tag:inbox" :key "P")
     '(:name "Guix Home Inbox" :key "H" :query "tag:guix-home and tag:unread")
     '(:name "RDE Inbox"       :key "R"
             :query "(to:/rde/ or cc:/rde/) and tag:unread")

     ;; '(:name "Watching" :query "thread:{tag:watch} and tag:unread" :key "tw")
     %rde-notmuch-saved-searches))

   (feature-transmission #:auto-start? #f)

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
   (feature-base-packages
    #:home-packages
    (append
     ;; (list scrcpy)
     (list bb jet)
     (strings->packages
      "figlet" ;; TODO: Move to emacs-artist-mode
      "calibre"
      ;; "icecat" "nyxt"
      "ungoogled-chromium-wayland" "ublock-origin-chromium"

      ;; "utox" "qtox" "jami"

      "alsa-utils" "youtube-dl" "imv" "cozy"
      "pavucontrol" "wev"
      "imagemagick"
      "obs" "obs-wlrobs"
      "recutils" "binutils" "make"
      "fheroes2"
      "recutils" "binutils" "make"
      ;; "fheroes2"
      ;; TODO: Enable pipewire support to chromium by default
      ;; chrome://flags/#enable-webrtc-pipewire-capturer
      "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra"
      "papirus-icon-theme" "arc-theme"
      ;; "thunar" "fd"
      ;; "glib:bin"

      "libreoffice"
      ;; TODO: Fix telega package!
      "ffmpeg"
      "ripgrep" "curl"

       ;; clojure-related stuff
      ;; "clojure" ;; already in feature
      "rlwrap" ;; mayb required by clojure cli
      "node" ;; mayb required for shadow-cljs
      "supercollider" ;; does it work?
      "leiningen" ;; does it work?
      ;; already in feature
      ;; "clojure-tools" ;; that may provide `clj` command line util, not tested. required by cider-jack-in Emacs command
      ;; "clojure-tools-cli" ;; does it work?
      ;; "openjdk" ;; breaks cider for daylight
      "icedtea" ;; breaks shadow-cljs for rent

      "clj-kondo"
      "emacs-clojure-mode" ;; does it work?
      "emacs-cider" ;; mayb required by clojure - emacs integration ;; in feature
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
      )))))

(define %laptop-features
  (list ))


;;; Hardware/host specifis features

;; TODO: Switch from UUIDs to partition labels For better
;; reproducibilty and easier setup.  Grub doesn't support luks2 yet.

(define ixy-mapped-devices
  (list (mapped-device
         (source (uuid "0e51ee1e-49ef-45c6-b0c3-6307e9980fa9"))
         (target "enc")
         (type luks-device-mapping))))

(define t450-file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "8F1E-C5A1" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device
           (uuid "cbbacbe6-2d31-411b-b94f-c8663738161c"
                 'ext4))
          (type "ext4"))
         ;; %base-file-systems
	 (list)))

(define ixy-file-systems
  (append
   (map (match-lambda
	  ((subvol . mount-point)
	   (file-system
	     (type "btrfs")
	     (device "/dev/mapper/enc")
	     (mount-point mount-point)
	     (options (format #f "subvol=~a" subvol))
	     (dependencies ixy-mapped-devices))))
	'((root . "/")
	  (boot . "/boot")
	  (gnu  . "/gnu")
	  (home . "/home")
	  (data . "/data")
	  (log  . "/var/log")))
   (list
    (file-system
      (mount-point "/boot/efi")
      (type "vfat")
      (device (uuid "8C99-0704" 'fat32))))))

(define %ixy-features
  (list
   (feature-host-info
    #:host-name "ixy"
    ;; ls `guix build tzdata`/share/zoneinfo
    #:timezone  "Asia/Tbilisi")
   ;;; Allows to declare specific bootloader configuration,
   ;;; grub-efi-bootloader used by default
   ;; (feature-bootloader)
   (feature-file-systems
    #:mapped-devices ixy-mapped-devices
    #:file-systems   ixy-file-systems)
   (feature-hidpi)))

(define %t450-features
  (list
   (feature-host-info
    #:host-name "t450"
    #:timezone "Asia/Tbilisi")
   (feature-file-systems
    #:file-systems t450-file-systems)
   ;;(feature-hidpi)
   (feature-gitwatch)
   (feature-sway-screenrecord)
   ;; (feature-emacs-rust)
   ;; (feature-ucm)
   (feature-emacs-org-roam-ui)
   ;; (feature-markdown)
   (feature-clojure)
   (feature-javascript)
   (feature-emacs-eglot)
   ))


;;; rde-config and helpers for generating home-environment and
;;; operating-system records.

(define-public ixy-config
  (rde-config
   (features
    (append
     %abcdw-features
     %main-features
     %ixy-features))))

(define-public t450-config
  (rde-config
   (features
    (append
     %abcdw-features
     %main-features
     %t450-features))))

;; TODISCUSS: Make rde-config-os/he to be a feature instead of getter?
(define-public ixy-os
  (rde-config-operating-system ixy-config))

(define-public ixy-he
  (rde-config-home-environment ixy-config))



(define t450-os
  (rde-config-operating-system t450-config))
(define t450-he
  (rde-config-home-environment t450-config))

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (display rde-target)
    (match rde-target
      ("ixy-home" ixy-he)
      ("ixy-system" ixy-os)
      ("t450-home" t450-he)
      ("t450-system" t450-os)
      (_ ixy-he))))

;; (pretty-print-rde-config ixy-config)
;; (use-modules (gnu services)
;; 	     (gnu services base))
;; (display
;;  (filter (lambda (x)
;; 	   (eq? (service-kind x) console-font-service-type))
;; 	 (rde-config-system-services ixy-config)))

;; (use-modules (rde features))
;; ((@ (ice-9 pretty-print) pretty-print)
;;  (map feature-name (rde-config-features ixy-config)))

(dispatcher)
