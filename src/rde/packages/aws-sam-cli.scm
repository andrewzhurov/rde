(define-module (rde packages aws-sam-cli)
  #:use-module (ice-9 pretty-print)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression) ;; for zlib
  #:use-module (gnu packages gcc) ;; for gcc
  #:use-module (gnu packages base) ;; for glibc
  #:export (aws-sam-cli))

;; based on build inputs of ucm for nix, found here: https://github.com/ceedubs/unison-nix/blob/trunk/nix/ucm.nix#L72
;; inputs being: buildInputs = [ git less fzf ncurses5 zlib ] ++ lib.optionals (!stdenv.isDarwin) [ gmp ];
;; git, less, fzf are not required for patching the binary
(use-modules (gnu packages version-control)) ;; for git
(use-modules (gnu packages less))
(use-modules (gnu packages terminals)) ;; for fzf
(use-modules (gnu packages ncurses))
(use-modules (gnu packages compression)) ;; for zlib
(use-modules (gnu packages multiprecision)) ;; for gmp

(use-modules (gnu packages compression)) ;; for unzip
(define aws-sam-cli-native-inputs
  `(("unzip" ,unzip)
    ))


(use-modules (guix packages)) ;; for package
(use-modules (guix download)) ;; for url-download
(use-modules (nonguix build-system binary)) ;; for binary-build-system
(use-modules ((guix licenses) #:prefix license:)) ;; for license
(define aws-sam-cli
  (package
    (name "aws-sam-cli")
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
               "https://github.com/aws/aws-sam-cli/releases/download/v1.56.0/aws-sam-cli-linux-x86_64.zip"
               ;; "https://github.com/aws/aws-sam-cli/releases/download/v1.56.0/aws-sam-cli-1.56.0.x86_64_linux.bottle.tar.gz"
               )
              (sha256
               (base32
                ;; "0q7dj15lgkfmzr2alq6b917hqvi2gpzi1nsphci6pa3pbnssci3q"
                "0n8ml89jah77iw2viqjk0ir29wnpjf9ssqq4mg0ha16d52cqgxnz"))))

    (build-system binary-build-system)
    (arguments
     `(#:validate-runpath? #t ;; Build breaks if turned on, works fine if turned off
       #:strip-binaries? #f ;; For some reason it breaks the program
       #:patchelf-plan ;; patching aws-sam-cli binary
       `(("./dist/sam"
          ("zlib" "glibc")))
       #:install-plan
       `(
         ;; ("./" "./")
         ;; ("bin/sam" "bin/sam")
         ("dist/sam" "bin/sam")
         )
       #:phases
       (modify-phases %standard-phases
                      (replace 'unpack
                               (lambda* (#:key inputs #:allow-other-keys)
                                 (invoke "unzip" (assoc-ref inputs "source"))))
                      (add-after 'patchelf 'my-install
                        (lambda* (#:key inputs outputs coreutils #:allow-other-keys)
                          (let* ((out  (assoc-ref outputs "out"))
                                 (bash (string-append (assoc-ref inputs "bash") "/bin/bash"))
                                 (coreutils (assoc-ref inputs "coreutils"))
                                 (cat (string-append coreutils "/bin/cat"))
                                 (ls (string-append coreutils "/bin/ls"))
                                 (pwd (string-append coreutils "/bin/pwd"))
                                 (libc (assoc-ref inputs "libc"))
                                 (ldd (string-append libc "/bin/ldd")))
                            ;; (substitute* "./install" (("/bin/sh") bash))
                            (format #t "bash:~a\n" bash)
                            (invoke ldd "./dist/sam")
                            (invoke ls "-l" "./")
                            (invoke ls "-l" "./dist/")
                            (invoke cat "./install")
                            ;; (invoke cat "./dist/sam")
                            ;; (invoke cat "./install")
                            (format #t "inputs: ~a\n" inputs)
                            (format #t "outputs: ~a\n" outputs)
                            ;; (invoke (format #t "~a/install --install-dir=~a" ))
                            (invoke "./install" "--install-dir" out)

                            ;; (invoke ls "-l" out)
                            ;; (invoke ls "-l" (string-append out "/current"))
                            (format #t "bin:\n")
                            (invoke ls "-l" (string-append out "/bin"))
                            (format #t "dist:\n")
                            (invoke ls "-l" (string-append out "/dist"))
                            (format #t "ldd:\n")
                            (invoke ldd (string-append out "/bin/sam"))
                            (invoke (string-append out "/bin/sam") "--version")
                            ))))))

    (native-inputs aws-sam-cli-native-inputs)
    (inputs
     `(("gcc:lib" ,gcc "lib")
       ("glibc" ,glibc)
       ("zlib" ,zlib)))

    (synopsis "A friendly programming language from the future")
    (description "Unison is a new programming language, currently under active development. It's a modern, statically-typed purely functional language, similar to Haskell, but with the ability to describe entire distributed systems with a single program.")
    (home-page "https://unisonweb.org/")
    (license license:gpl2) ;; it's not GPL2
    ))

aws-sam-cli
