(define-module (rde packages hypothesis)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages node)
  #:use-module (gnu packages base)
  #:use-module (gnu build chromium-extension)

  #:use-module (srfi srfi-1)

  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system gnu)

  #:use-module ((guix licenses) #:prefix license:))

(define hypothesis
  (package
    (name "hypothesis")
    (version "v1.879.0-built")
    (home-page "https://github.com/andrewzhurov/browser-extension.git")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256 ;; TODO fix sha
               (base32
                "09m3fl9nlhiprqryyqwi9lrnnbphmypydz9300mkp43gv55fxzs2"))))
    (build-system gnu-build-system)
    (outputs '("chromium"))
    (arguments
     '(#:tests? #f                      ; TODO run tests
       #:allowed-references ()
       #:phases
       (modify-phases (map (lambda (phase)
                             (assq phase %standard-phases))
                           '(set-paths unpack patch-source-shebangs))
         ;; (add-after 'unpack 'build-chromium
         ;;   (lambda* (#:key inputs #:allow-other-keys)
         ;;     (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm"))
         ;;           (make (string-append (assoc-ref inputs "make") "/bin/make"))
         ;;           )
         ;;         ;; (invoke #$(file-append node "/bin/npm") "--version")
         ;;         ;; TODO ensure it uses package.lock
         ;;         ;;(invoke npm "extensions")
         ;;       ;;(invoke make "extension")
         ;;       (invoke make "extension")
         ;;         )))
         (add-after 'unpack 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((chromium (assoc-ref outputs "chromium")))
               (copy-recursively "build" chromium)
               #t))))))
    (native-inputs
     `(("node" ,node)
       ("make" ,gnu-make)
       ;;("automake" ,automake)
       ))
    (synopsis "Annotate the web.")
    (description
     "")
    (license license:gpl3+)))

(define-public hypothesis/chromium
  (make-chromium-extension hypothesis "chromium"))

hypothesis/chromium
