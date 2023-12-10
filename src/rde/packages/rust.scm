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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)

  #:export ( rust-lsp)
  )

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

rust-lsp
