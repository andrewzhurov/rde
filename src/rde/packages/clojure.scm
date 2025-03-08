(define-module (rde packages clojure)
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
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression)

  #:export (clojure-lsp)
  )

;; (define-public clojure-lsp
;;   (package
;;     (name "clojure-lsp")
;;     (version "2022.04.18-00.59.32")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (string-append "https://github.com/clojure-lsp/clojure-lsp/releases/download/"
;;                            version
;;                            "/clojure-lsp-native-static-linux-amd64.zip"))
;;        (sha256 (base32 "04czrar5adh7b7kz9n47kl88lylffzcv7ym6n5ylnrkbzvzp1f5r"))))
;;     (build-system copy-build-system)
;;     (native-inputs (list unzip))
;;     (arguments
;;      `(#:install-plan
;;        '(("clojure-lsp" "bin/"))))
;;     (home-page "https://clojure.org/releases/tools")
;;     (synopsis "CLI tools for the Clojure programming language")
;;     (description "The Clojure command line tools can be used to start a
;; Clojure repl, use Clojure and Java libraries, and start Clojure programs.")
;;     (license license:epl1.0)))

(define-public clojure-lsp
  (package
   (name "clojure-lsp")
   (version "2024.03.01-11.37.51"
            ;; "2022.12.09-15.51.10"
            ;; "2022.07.24-18.25.43"
            ;; "2022.04.18-00.59.32"
            )
   (source
    (origin
      (method url-fetch)
      (uri (string-append "https://github.com/clojure-lsp/clojure-lsp/releases/download/"
                           version
                           "/clojure-lsp-native-static-linux-amd64.zip"))
      (sha256 (base32 "1sf3zkvv25f6v32m095fgdgw0jck3vp2m3srbp60mwaxnqjq7pmd"
                      ;; "1l5372031va7ij3b6j001q6wsiqmmqxn78ajhzv6qfq0bcl6dziy"
                      ;; "1l5372031va7ij3b6j001q6wsiqmmqxn78ajhzv6qfq0bcl6dziy"
                      ;; "04czrar5adh7b7kz9n47kl88lylffzcv7ym6n5ylnrkbzvzp1f5r"
                      ))))
   (build-system copy-build-system)
   (native-inputs (list unzip))
   (arguments
    `(#:install-plan
      '(("clojure-lsp" "bin/"))))
   (synopsis "")
   (description "")
   (home-page "")
   (license license:epl1.0)))
