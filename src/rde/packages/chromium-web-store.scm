(define-module (rde packages chromium-web-store)
  #:use-module (guix build download)
  #:use-module (guix build-system)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu build chromium-extension)
  ;; from chromium-extension.scm
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix )
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages node-xyz)
  #:use-module (guix build-system trivial))

(define (make-signing-key seed)
  "Return a derivation for a deterministic PKCS #8 private key using SEED."
  (computed-file
   (string-append seed "-signing-key.pem")
   (with-extensions (list guile-gcrypt)
     #~(begin
         (use-modules (gcrypt base16) (gcrypt hash) (ice-9 iconv))
         (let* ((sha256sum (bytevector->base16-string
                            (sha256 (string->bytevector #$seed "UTF-8"))))
                ;; certtool.c wants a 56 byte seed for a 2048 bit key.
                (key-size 2048)
                (normalized-seed (string-take sha256sum 56)))

           (system* #$(file-append gnutls "/bin/certtool")
                    "--generate-privkey"
                    "--key-type=rsa"
                    "--pkcs8"
                    ;; Use the provable FIPS-PUB186-4 algorithm for
                    ;; deterministic results.
                    "--provable"
                    "--password="
                    "--no-text"
                    (string-append "--bits=" (number->string key-size))
                    (string-append "--seed=" normalized-seed)
                    "--outfile" #$output))))
   #:local-build? #t))

(define* (make-crx signing-key package #:optional (package-output "out"))
  "Create a signed \".crx\" file from the unpacked Chromium extension residing
in PACKAGE-OUTPUT of PACKAGE.  The extension will be signed with SIGNING-KEY."
  (define name (package-name package))
  (define version (package-version package))

  (computed-file
   (string-append name "-" version ".crx")
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let ((crx3 #+(file-append node-crx3 "/bin/crx3"))
               (packdir (string-append (getcwd) "/extension")))
           (mkdir packdir)
           (copy-recursively (ungexp package package-output) packdir
                             ;; Ensure consistent file modification times.
                             #:keep-mtime? #t)
           (invoke crx3 "--keyPath" #$signing-key packdir)
           (copy-file (string-append packdir ".crx") #$output))))
   #:local-build? #t))

(define (crx->chromium-json crx version)
  "Return a derivation that creates a Chromium JSON settings file for the
extension given as CRX.  VERSION is used to signify the CRX version, and
must match the version listed in the extension manifest.json."
  ;; See chrome/browser/extensions/external_provider_impl.cc and
  ;; extensions/common/extension.h for documentation on the JSON format.
  (computed-file "extension.json"
                 #~(call-with-output-file #$output
                     (lambda (port)
                       (format port "{
  \"external_crx\": \"~a\",
  \"external_version\": \"~a\"
}
"
                               #$crx #$version)))
                 #:local-build? #t))


(define (signing-key->public-der key)
  "Return a derivation for a file containing the public key of KEY in DER
format."
  (computed-file "der"
                 #~(system* #$(file-append gnutls "/bin/certtool")
                            "--load-privkey" #$key
                            "--pubkey-info"
                            "--outfile" #$output
                            "--outder")
                 #:local-build? #t))

(define (file-sha256sum file)
  (with-extensions (list guile-gcrypt)
    #~(begin
        (use-modules (gcrypt base16) (gcrypt hash))
        (bytevector->base16-string (file-sha256 #$file)))))

(define* (crx-pkg->chromium-extension crx-pkg #:optional (pkg-output "out"))
  (let* ((name (package-name crx-pkg))
         (version (package-version crx-pkg)))
    (package
      (inherit crx-pkg)
      (name (string-append name "-chromium"))
      (source #f)
      (native-inputs '())
      (inputs '())
      (propagated-inputs '())
      (outputs '("out"))
      (build-system trivial-build-system)
      (arguments
       (list #:modules '((guix build utils))
             #:builder
             (let*
                 ((private-key (make-signing-key name))
                  (public-key (signing-key->public-der private-key))
                  (checksum (file-sha256sum public-key))
                  (crx (make-crx private-key crx-pkg pkg-output))
                  (json (crx->chromium-json crx version)))
               #~(begin
                   (use-modules (guix build utils))
                   (define (base16-char->chromium-base16 char)
                     ;; Translate CHAR, a hexadecimal character, to a Chromium-style
                     ;; representation using the letters a-p (where a=0, p=15).
                     (string-ref "abcdefghijklmnop"
                                 (string-index "0123456789abcdef" char)))
                   (let ((file-name (string-map base16-char->chromium-base16
                                                (string-take #$checksum 32)))
                         (extension-directory
                          (string-append #$output
                                         "/share/chromium/extensions")))
                     (mkdir-p extension-directory)
                     (symlink #$json (string-append extension-directory "/"
                                                    file-name ".json"))))))))))

(define chromium-web-store-crx
  (package
    (name "chromium-web-store-crx")
    (version "1.5.4.2")
    (home-page "https://github.com/NeverDecaf/chromium-web-store")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/NeverDecaf/chromium-web-store/releases/download/v1.5.4.2/Chromium.Web.Store.crx")
       (sha256
        (base32
         "070pwb7brdcn1mfvplkd56vjc7lbz4iznzkqvfsakvgbv68k71ah"))
       ;; (sha256 %null-sha256)
       ))
    (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("." "out/"))))
    (description "")
    (synopsis "")
    (license license:gpl3+)))

chromium-web-store-crx
(crx-pkg->chromium-extension chromium-web-store-crx)

;; (define chromium-web-store
;;   (package
;;     (name "chromium-web-store")
;;     (version "1.5.4.2")
;;     (home-page "https://github.com/NeverDecaf/chromium-web-store")
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                     (url home-page)
;;                     (commit version)))
;;               (file-name (git-file-name name version))
;;               (sha256
;;                (base32
;;                 "150n3g42wx8bp1dgmf360gd87g8pk6zzgkyd8k698mj6r5anfwpa"))))
;;     (build-system gnu-build-system)
;;     (outputs '("chromium"))
;;     (properties '((addon-id . "chromium-web-store")))
;;     (arguments
;;      (list
;;       #:tests? #f                      ;no tests
;;       #:allowed-references '()
;;       #:phases
;;       #~(modify-phases (map (lambda (phase)
;;                               (assq phase %standard-phases))
;;                             '(set-paths unpack patch-source-shebangs))
;;           (add-after 'unpack 'do-not-depend-on-git
;;             ;; (lambda _
;;             ;;   (mkdir-p "dist/build/uAssets/main")
;;             ;;   (copy-recursively #$ublock-main-assets "dist/build/uAssets/main")
;;             ;;   (mkdir-p "dist/build/uAssets/prod")
;;             ;;   (copy-recursively #$ublock-prod-assets "dist/build/uAssets/prod"))
;;             )
;;           (add-after 'unpack 'make-files-writable
;;             (lambda _
;;               ;; The build system copies some files and later tries
;;               ;; modifying them.
;;               (for-each make-file-writable (find-files "."))))
;;           ;; (add-after 'patch-source-shebangs 'build-xpi
;;           ;;   (lambda _
;;           ;;     (invoke "./tools/make-firefox.sh" "all")))
;;           (add-after 'unpack 'build-chromium
;;             (lambda _
;;               (invoke "./tools/make-chromium.sh")))
;;           (add-after 'build-chromium 'install
;;             (lambda* (#:key outputs #:allow-other-keys)
;;               (let* ((addon-id #$(assq-ref properties 'addon-id))
;;                      (firefox (in-vicinity
;;                                (assoc-ref outputs "firefox") addon-id))
;;                      (xpi (assoc-ref outputs "xpi"))
;;                      (chromium (assoc-ref outputs "chromium")))
;;                 (install-file "dist/build/uBlock0.firefox.xpi"
;;                               (string-append xpi "/lib/mozilla/extensions"))
;;                 (copy-recursively "dist/build/uBlock0.firefox" firefox)
;;                 (copy-recursively "dist/build/uBlock0.chromium" chromium)))))))
;;     (native-inputs
;;      (list python-wrapper zip))
;;     (synopsis "Block unwanted content from web sites")
;;     (description
;;      "uBlock Origin is a @dfn{wide spectrum blocker} for IceCat and
;; ungoogled-chromium.")
;;     (license license:gpl3+)))
