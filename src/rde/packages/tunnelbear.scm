(define-module (rde packages tunnelbear)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu build chromium-extension)
  #:use-module (gnu build icecat-extension)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages python))

(define tunnelbear
  (package
    (name "tunnelbear")
    (version "1.56.0")
    (home-page "https://github.com/gorhill/uBlock")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "150n3g42wx8bp1dgmf360gd87g8pk6zzgkyd8k698mj6r5anfwpa"))))
    (build-system gnu-build-system)
    (outputs '("xpi" "firefox" "chromium"))
    (properties '((addon-id . "uBlock0@raymondhill.net")))
    (arguments
     (list
      #:tests? #f                      ;no tests
      #:allowed-references '()
      #:phases
      #~(modify-phases (map (lambda (phase)
                              (assq phase %standard-phases))
                            '(set-paths unpack patch-source-shebangs))
          (add-after 'unpack 'do-not-depend-on-git
            (lambda _
              (mkdir-p "dist/build/uAssets/main")
              (copy-recursively #$ublock-main-assets "dist/build/uAssets/main")
              (mkdir-p "dist/build/uAssets/prod")
              (copy-recursively #$ublock-prod-assets "dist/build/uAssets/prod")))
          (add-after 'unpack 'make-files-writable
            (lambda _
              ;; The build system copies some files and later tries
              ;; modifying them.
              (for-each make-file-writable (find-files "."))))
          (add-after 'patch-source-shebangs 'build-xpi
            (lambda _
              (invoke "./tools/make-firefox.sh" "all")))
          (add-after 'build-xpi 'build-chromium
            (lambda _
              (invoke "./tools/make-chromium.sh")))
          (add-after 'build-chromium 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((addon-id #$(assq-ref properties 'addon-id))
                     (firefox (in-vicinity
                               (assoc-ref outputs "firefox") addon-id))
                     (xpi (assoc-ref outputs "xpi"))
                     (chromium (assoc-ref outputs "chromium")))
                (install-file "dist/build/uBlock0.firefox.xpi"
                              (string-append xpi "/lib/mozilla/extensions"))
                (copy-recursively "dist/build/uBlock0.firefox" firefox)
                (copy-recursively "dist/build/uBlock0.chromium" chromium)))))))
    (native-inputs
     (list python-wrapper zip))
    (synopsis "Block unwanted content from web sites")
    (description
     "uBlock Origin is a @dfn{wide spectrum blocker} for IceCat and
ungoogled-chromium.")
    (license license:gpl3+)))
