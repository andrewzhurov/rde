(define-module (rde packages ucm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools) ;for meson-0.55
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(use-modules
   ((guix licenses) #:prefix license:)
   (guix utils)
   (guix build utils)
   (guix packages)
   (guix git-download)
   (guix build-system trivial)
   (guix build-system copy)
   (gnu packages ssh)
   (gnu packages version-control)
   (gnu packages linux)
   (gnu packages web)
   (gnu packages pkg-config)
   (gnu packages python)
   (gnu packages compression)
   (gnu packages tls)
   )

(use-modules (guix build-system haskell))
(use-modules (gnu packages less))
(use-modules (gnu packages terminals))
(use-modules (gnu packages ncurses))
(use-modules (gnu packages multiprecision))
(define ucm-inputs
  `(;; ("git" ,git) ;; commented inputs are not required for patching binaries
    ;; ("less" ,less)
    ;; ("fzf" ,fzf)
    ("ncurses" ,ncurses) ;; closest to ncurse5 I found in a brief search
    ("zlib" ,zlib)
    ("gmp" ,gmp)))

(define-public ucm
  (let ((commit-sha "b7481ce9fe9e59b88d16b4561b2b378efb0ee686"))
    (package
     (name "ucm")
     (version "bullshit")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unisonweb/unison")
             (commit commit-sha)))
       ;; (file-name (git-file-name name version)) ;; what is it for? TODO correct or remove
       (sha256
        (base32
         "18639c18gbzi3xw554lgkv71k17d63grchkv086mrfx0rcmfwsx9"))
       ))

     (build-system haskell-build-system)
     (inputs ;; TODO correct inputs
      `(("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
        ("ghc-quickcheck"            ,ghc-quickcheck)
        ("ghc-semigroups"            ,ghc-semigroups)
        ("ghc-setlocale"             ,ghc-setlocale)
        ("ghc-utf8-string"           ,ghc-utf8-string)
        ("ghc-x11"                   ,ghc-x11)))
     (arguments
     `(;; #:cabal-revision
       ;; ("2" "1yfsjx7dqikg3hvld7i91xfsg5lawmr5980lvfd794sybmgxsf17")
       ))
     ;; (arguments ;; TODO correct arguments
      ;; '(;; #:configure-flags (list "--configure-option=project-file=contrib/cabal.project")
        ;; #:cabal-revision '("4" "0xbwyvwl6f2zylk60f2akwgq03qv49113xil7b1z1s3vlwbn5aj1")
        ;; #:phases
        ;; (modify-phases %standard-phases
        ;;   (add-before 'configure 'setup-cabal-config-file ;; cabal project file is in 'contrib' dir and can't be found. Let's copy it to the root dir.
        ;;     (lambda* (#:key outputs source #:allow-other-keys)
        ;;       (let ((cabal-project-file (string-append source "/contrib/cabal.project"))
        ;;             (out (assoc-ref outputs "out")))
        ;;         (use-modules (ice-9 pretty-print))
        ;;         (pretty-print out)
        ;;         (install-file cabal-project-file out)
        ;;         #t)))
                            ;; (add-after
                            ;;  'install 'install-xsession
                            ;;  (lambda _
                            ;;    (let* ((xsessions (string-append %output "/share/xsessions")))
                            ;;      (mkdir-p xsessions)
                            ;;      (call-with-output-file
                            ;;          (string-append xsessions "/xmonad.desktop")
                            ;;        (lambda (port)
                            ;;          (format port "~
                            ;;            [Desktop Entry]~@
                            ;;            Name=~a~@
                            ;;            Comment=~a~@
                            ;;            Exec=~a/bin/xmonad~@
                            ;;            Type=Application~%" ,name ,synopsis %output))))))
       ;; )
     ;;  )
     ;; )

     (synopsis "A friendly programming language from the future")
     (description "Unison is a new programming language, currently under active development. It's a modern, statically-typed purely functional language, similar to Haskell, but with the ability to describe entire distributed systems with a single program.")
     (home-page "https://unisonweb.org/")
     (license license:gpl2) ;; it's not GPL2
     )))

ucm
