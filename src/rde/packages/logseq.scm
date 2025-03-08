(define-module (rde packages logseq)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages base)
  #:use-module (gnu packages node)
  #:use-module (gnu build chromium-extension)

  #:use-module (srfi srfi-1)

  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system clojure)
  #:use-module (guix build-system copy)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages compression)

  #:export (logseq)
  )

;; 0.8.7 requires following deps
;; source - https://github.com/logseq/logseq/blob/b150e1aa09a4dd14409c16ccb12d87da8a05e208/package.json#L71
;; Not all of those are required for the electron build
;; {
;;  "@capacitor/android": "3.2.2",
;;  "@capacitor/app": "1.0.6",
;;  "@capacitor/camera": "1.2.1",
;;  "@capacitor/clipboard": "^1.0.8",
;;  "@capacitor/core": "3.2.2",
;;  "@capacitor/filesystem": "1.0.6",
;;  "@capacitor/haptics": "^1.1.4",
;;  "@capacitor/ios": "3.2.2",
;;  "@capacitor/keyboard": "^1.2.0",
;;  "@capacitor/share": "^1.1.2",
;;  "@capacitor/splash-screen": "1.1.3",
;;  "@capacitor/status-bar": "1.0.6",
;;  "@excalidraw/excalidraw": "0.10.0",
;;  "@kanru/rage-wasm": "0.2.1",
;;  "@logseq/react-tweet-embed": "1.3.1-1",
;;  "@sentry/react": "^6.18.2",
;;  "@sentry/tracing": "^6.18.2",
;;  "@tabler/icons": "1.54.0",
;;  "@tippyjs/react": "4.2.5",
;;  "aes-js": "3.1.2",
;;  "bignumber.js": "^9.0.2",
;;  "capacitor-voice-recorder": "2.1.0",
;;  "check-password-strength": "2.0.7",
;;  "chokidar": "3.5.1", ;; n
;;  "chrono-node": "2.2.4", ;; n
;;  "codemirror": "5.58.1", ;; n
;;  "d3-force": "3.0.0", ;; n ;; just d3, although it may contain d3-force
;;  "diff": "5.0.0", ;; n
;;  "electron": "19.0.12", ;; n
;;  "electron-dl": "3.3.0", ;; n
;;  "fs": "0.0.1-security", ;; n
;;  "fs-extra": "9.1.0", ;; n
;;  "fuse.js": "6.4.6", ;; n
;;  "grapheme-splitter": "1.0.4", ;; n
;;  "graphology": "0.20.0", ;; n
;;  "gulp-cached": "1.1.1", ;; n
;;  "highlight.js": "10.4.1", ;; y
;;  "ignore": "5.1.8",
;;  "is-svg": "4.3.0",
;;  "jszip": "3.7.0",
;;  "mldoc": "1.4.0",
;;  "path": "0.12.7",
;;  "pixi-graph-fork": "0.2.0",
;;  "pixi.js": "6.2.0",
;;  "posthog-js": "1.10.2",
;;  "react": "17.0.2",
;;  "react-dom": "17.0.2",
;;  "react-grid-layout": "0.16.6",
;;  "react-icon-base": "^2.1.2",
;;  "react-icons": "2.2.7",
;;  "react-intersection-observer": "^9.3.5",
;;  "react-resize-context": "3.0.0",
;;  "react-textarea-autosize": "8.3.3",
;;  "react-tippy": "1.4.0",
;;  "react-transition-group": "4.3.0",
;;  "reakit": "1.3.11",
;;  "remove-accents": "0.4.2",
;;  "send-intent": "3.0.11",
;;  "threads": "1.6.5",
;;  "url": "^0.11.0",
;;  "yargs-parser": "20.2.4"
;;  }

;; This will take _A TON_ of effort to pack.
;; As there are two dozen js deps to pack and a dozen of clojure deps to pack.
;; Perhaps easier will be to pack an already built version.

(define-public logseq
  (let* ((revision "1")
         (commit "b150e1aa09a4dd14409c16ccb12d87da8a05e208"))
    (package
      (name "logseq")
      (version (git-version "0.8.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/logseq/logseq")
               (commit commit)))
         (sha256
          (base32 "01pwh1whyvcgb30rwz8775z1a715357iavshv1knws6yqhnky391"))
         (file-name (git-file-name name version))))
      (build-system clojure-build-system)
      (native-inputs (list node))
      (arguments
       (list
        ;; #:install-plan
        ;; '(("logseq" "bin/"))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'build
              (lambda* (#:key inputs #:allow-other-keys)
                (format #t "~s" inputs)
                ;; #$(file-append coreutils "/bin/ls")
                ;; (invoke #$(file-append node "/bin/npm") "install")
                (invoke "npm")
                (invoke "npm" "install")
                ;; (invoke "npx" "yarn release")
                )))
        ))
      (home-page "")
      (synopsis "")
      (description "")
      (license license:agpl3))))


(define-public logseq-bin
  (package
    (name "logseq-bin")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/logseq/logseq/releases/download/0.8.7/Logseq-linux-x64-0.8.7.zip")
       (sha256 (base32 "0bkxdcxabb33x59x0r4fj79328ih781rmr5dxj49hms6cmwyh1nl"))))
    (build-system binary-build-system)
    (native-inputs (list unzip))
    (arguments
     `(
       #:install-plan
       `(("Logseq" "bin/"))
       ;; #:phases
       ;; #~(modify-phases %standard-phases
       ;;     (add-before 'install 'install-info
       ;;       (lambda* (#:key outputs #:allow-other-keys)
       ;;         (let* ((out  (assoc-ref outputs "out"))
       ;;                (info (string-append out "/share/info")))
       ;;           (invoke #$(file-append
       ;;                      coreutils
       ;;                      "/bin/ls")
       ;;                   )
       ;;           (install-file "doc/rde.info" info)))))
       #:validate-runpath? #f))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:agpl3)))

logseq
logseq-bin
