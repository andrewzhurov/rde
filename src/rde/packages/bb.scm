(define-module (rde packages bb)
  #:export (bb))

;; based on build inputs of ucm for nix, found here: https://github.com/ceedubs/unison-nix/blob/trunk/nix/ucm.nix#L72
;; inputs being: buildInputs = [ git less fzf ncurses5 zlib ] ++ lib.optionals (!stdenv.isDarwin) [ gmp ];
;; git, less, fzf are not required for patching the binary
(use-modules (gnu packages version-control)) ;; for git
(use-modules (gnu packages less))
(use-modules (gnu packages terminals)) ;; for fzf
(use-modules (gnu packages ncurses))
(use-modules (gnu packages compression)) ;; for zlib
(use-modules (gnu packages multiprecision)) ;; for gmp

(define bb-inputs
  `(("git", git)
    ("less", less)
    ("fzf", fzf)
    ("ncurses" ,ncurses) ;; closest to ncurse5 I found in a brief search. works
    ("zlib" ,zlib)
    ("gmp" ,gmp)))

;; UI may need to be set up https://github.com/ceedubs/unison-nix/blob/trunk/nix/ucm.nix#L82

;; Following installation instructions from https://app.slack.com/docs/TLL09QC85/FMT7TDDDY?origin_team=TLL09QC85
;; with an added phase of patching binaries.
;; Installation instructions are:
;; mkdir unisonlanguage
;; curl -L https://github.com/unisonweb/unison/releases/download/release%2FM2k/ucm-linux.tar.gz --output unisonlanguage/ucm.tar.gz
;; tar -xzf unisonlanguage/ucm.tar.gz -C unisonlanguage
;; ./unisonlanguage/ucm
(use-modules (gnu packages compression)) ;; for gzip
(use-modules (gnu packages base)) ;; for tar
(define bb-native-inputs
  `(("gzip" ,gzip)
    ("tar" ,tar)))

(use-modules (guix packages)) ;; for package
(use-modules (guix download)) ;; for url-download
(use-modules (nonguix build-system binary)) ;; for binary-build-system
(use-modules ((guix licenses) #:prefix license:)) ;; for license
(define bb
  (package
    (name "bb")
    (version
     "M2"
     ;; "M2j" ;; buggy
     ;; "M2k" ;; buggy
     ;; "latest" ;; buggy
     )
    (source (origin
              (method url-fetch)
              (uri
               ;; "https://github.com/unisonweb/unison/releases/download/latest/ucm-linux.tar.gz" ;; buggy
               "https://github.com/babashka/babashka/releases/download/v0.9.162/babashka-0.9.162-linux-amd64-static.tar.gz"
               )
              (sha256
               (base32
                "07dgpg1j202ayggvwi8d1g85q33p7fydwa1db7499m95svllfh6l"))))

    (build-system binary-build-system)
    (arguments
     `(#:validate-runpath? #f ;; Build breaks if turned on, works fine if turned off
       #:strip-binaries? #f ;; For some reason it breaks the program
       ;; #:patchelf-plan ;; patching bb binary
       ;; `(("bb"
       ;;    ("ncurses" "zlib" "gmp")))
       #:install-plan
       `(("bb" "/bin/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "-xvzf" (assoc-ref inputs "source")))))))

    (native-inputs bb-native-inputs)
    (inputs bb-inputs)

    (synopsis "A friendly programming language from the future")
    (description "Unison is a new programming language, currently under active development. It's a modern, statically-typed purely functional language, similar to Haskell, but with the ability to describe entire distributed systems with a single program.")
    (home-page "https://unisonweb.org/")
    (license license:gpl2) ;; it's not GPL2
    ))

bb
