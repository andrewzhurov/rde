(define-module (rde packages rust)
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
  #:use-module (gnu packages docker)

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
  #:use-module (guix build-system)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix build utils)
  #:use-module (gnu packages compression)
  ;; #:use-module (gnu packages linux)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages bootstrap)

  #:use-module (nonguix build-system binary)

  #:export (rust-lsp))

;; #!/usr/bin/env bash
;; # based on https://www-grepular-com.translate.goog/Sandbox_Rust_Development_with_Rust_Analyzer?_x_tr_sl=en&_x_tr_tl=ru&_x_tr_hl=en&_x_tr_pto=op,wapp
;; set -e

;; IMAGE_NAME="andrewzhurov/rust-analyzer"

;; # Build the image if it does not exist
;; if [[ $(docker images --filter "reference=$IMAGE_NAME" -q) == "" ]]; then
;;     docker build -q -t "$IMAGE_NAME" . -f-<<EOF
;;  FROM rustlang/rust:nightly

;;  RUN curl -L https://github.com/rust-lang/rust-analyzer/releases/download/2022-08-15/rust-analyzer-x86_64-unknown-linux-gnu.gz | gunzip -c - > /rust-analyzer
;;  RUN chmod +x /rust-analyzer

;;  ENTRYPOINT ["/rust-analyzer"]./Dockerfile
;; EOF

;; fi

;; docker run \
;;   -u $(stat -c '%u:%g' .) \
;;   -i --rm \
;;   -v "$PWD:$PWD:ro" \
;;   --workdir "$PWD" \
;;   "$IMAGE_NAME" "$@"

(define inputs (list docker))

(define-public rust-lsp
  (package
    (name "rust-lsp")
    (version "2022-08-15")
    (source #f)
    (build-system trivial-build-system)
    (arguments
       `(#:modules
         ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils)
                        (srfi srfi-1))
           (let* ((bin (string-append %output "/bin"))
                  (rust-lsp-path (string-append bin "/" "rust-lsp")))
             (mkdir-p bin)
             (copy-file wrapper )))))

      (native-inputs inputs)
      (propagated-inputs inputs)
      (inputs inputs)
      (home-page "")
      (synopsis "")
      (description "")
      (license license:epl1.0)))

(define-public wasm-pack
  (package
    (name "wasm-pack")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/rustwasm/wasm-pack/releases/download/v0.12.1/wasm-pack-v0.12.1-x86_64-unknown-linux-musl.tar.gz")
       (sha256 (base32 "07v2xkz4zavg4046q8jl182lh9lmpsxbkmnm9p05zgbnswxblfbk"))))

    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("./wasm-pack" "/bin/wasm-pack"))
      ;; (("wasm-pack" "bin/wasm-pack"))

      ;; #:phases
      ;; (with-imported-modules '((ice-9 pretty-print))
      ;;   (modify-phases %standard-phases
      ;;     (add-after 'install 'add-my-spell
      ;;       (lambda* (#:rest all ;; #:key inputs outputs #:allow-other-keys
      ;;                 )
      ;;         (pretty-print all #:display? #t)
      ;;         ;; (format #t "~a" inputs)
      ;;         ))))
      ;; (modify-phases %standard-phases
      ;;   (add-after 'unpack 'add-my-spell
      ;;     (lambda* (#:key outputs #:allow-other-keys ;; #:key inputs outputs #:allow-other-keys
      ;;               )
      ;;       (let* ((out (assoc-ref outputs "out"))
      ;;              (bin (string-append out "/bin/")))

      ;;         ;; (format #t "~a" out)
      ;;         ;; (format #t "~a" rest)
      ;;         (install-file "wasm-pack" bin)

      ;;         ;; (chmod bin #o755)
      ;;         )
      ;;       )))
      )
      )

    (home-page "")
    (synopsis "")
    (description "")
    (license license:epl1.0)))

;; (use-modules (rde api store))
;; (parameterize ((%daemon-socket-uri "file:///var/guix/daemon-socket/socket"))
;;   (build-with-store wasm-pack))

(define-public cargo-watch
  (package
    (name "cargo-watch")
    (version "v8.5.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/watchexec/cargo-watch/releases/download/v8.5.2/cargo-watch-v8.5.2-x86_64-unknown-linux-musl.tar.xz")
       (sha256 (base32 "0cajma063yi30iclk14jz5xc26219h7lqmnd4w7d26kmr91g5nyd"))))

    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("./cargo-watch" "/bin/cargo-watch"))))

    (home-page "")
    (synopsis "")
    (description "")
    (license license:epl1.0)))

(define-public evcxr
  (package
    (name "evcxr")
    (version "v0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/evcxr/evcxr/releases/download/v0.17.0/evcxr-v0.17.0-x86_64-unknown-linux-gnu.tar.gz")
       (sha256 (base32 "0x12kaw03mnxfavbc5hz2hsa90c0w8l7d4pbpwpmd5c3na81ijzw"))))

    (build-system binary-build-system)
    (arguments
     (list
      #:patchelf-plan `'(("evcxr" ("gcc" ;;"ld"
                                   )))
      #:install-plan `'(("evcxr" "/bin/"))
      #:phases (gexp
                (modify-phases %standard-phases
                  (add-before 'patchelf 'patch-ld
                    (lambda* (#:key inputs outputs  #:allow-other-keys)
                      (let* ((ld-so (search-input-file inputs "/lib/ld-linux-x86-64.so.2" ;; (ungexp (glibc-dynamic-linker))
                                                       ))
                             (out   (assoc-ref outputs "out"))
                             (evcxr (assoc-ref out "evcxr"))
                             (evcxr2 (string-append out "/evcxr"))
                             )
                        (format #t "~y" outputs)
                        (format #t "~y" out)
                        ;; (format #t "~y" inputs)
                        ;; (format #t "~y" rest)
                        (format #t "all files ~y" (find-files "."))
                        ;; (format #t "ld-so ~y" ld-so)
                        ;; (format #t "~y" (find-files output))
                        (format #t "evcxr ~y" evcxr)
                        (format #t "evcxr2 ~y" evcxr2)
                        (invoke "patchelf" "--add-rpath"
                                "/lib/ld-linux-x86-64.so.2"
                                ;; ld-so
                                "./evcxr"
                                ;; evcxr2
                                )
                        )
                      ))))))

    (native-inputs
     (list unzip))
    (inputs
     (list `(,gcc "lib")
           ;; `((glibc-dynamic-linker) "ld")
           ))

    (home-page "")
    (synopsis "")
    (description "")
    (license license:epl1.0)))

;; (use-modules (guix gexp))
;; (use-modules (rde api store))
;; (glibc-dynamic-linker)
;; (parameterize ((%daemon-socket-uri "file:///var/guix/daemon-socket/socket"))
;;   (build-with-store evcxr))


;; ((@ (rde api store) build-with-store) wasm-pack)
;; (use-modules (guix scripts)
;;              (guix ui)
;;              (guix packages)
;;              (guix derivations)
;;              (guix store)
;;              (guix scripts build)
;;              (guix tests)
;;              (web uri))
;; (uri-reference?)
;; (guix-build)
;; (build wasm-pack)
;; (with-status-verbosity (assoc-ref opts 'verbosity)
;;   (with-store store
;;     (build-derivations store
;;                        (package->derivation wasm-pack))))
;; (%daemon-socket-uri)
;; (let ((store (open-connection "file:///var/guix/daemon-socket/socket")))
;;   (lowered-gexp-inputs store (primitive-eval (lower-object wasm-pack))))
;; (build-package wasm-pack
;;                #:print-build-trace #t
;;                #:print-extended-build-trace? #t
;;                #:verbosity 100)
;; (let ((store (open-connection "file:///var/guix/daemon-socket/socket")))
;;   (build-derivations store
;;                      wasm-pack ))
;; (open-connection "file:///var/guix/daemon-socket/socket")
;; (open-connection-for-tests)
;; (with-store store (+ 1 1))
;; (;;show-derivation-outputs
;;  show-what-to-build*
;;  (build-package wasm-pack))
