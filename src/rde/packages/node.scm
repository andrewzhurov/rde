(define-module (rde packages node)
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

  #:use-module (gnu packages gcc)
  #:use-module (nonguix build-system binary)
  )

;; /gnu/store/j9pq3sjw12qk9pqsdqnkq28zj0vxny3x-node-lsp-2022.04.18-00.59.32/bin/node: error: depends on 'libstdc++.so.6', which cannot be found in RUNPATH ()
;; /gnu/store/j9pq3sjw12qk9pqsdqnkq28zj0vxny3x-node-lsp-2022.04.18-00.59.32/bin/node: error: depends on 'libgcc_s.so.1', which cannot be found in RUNPATH ()
;; /gnu/store/j9pq3sjw12qk9pqsdqnkq28zj0vxny3x-node-lsp-2022.04.18-00.59.32/bin/node: error: depends on 'ld-linux-x86-64.so.2', which

(define patchelf-libs `())
(define fastboot-inputs
  `(("gcc:lib" ,gcc "lib")))


(define-public node-bin
  (package
    (name "node-lsp")
    (version "2022.04.18-00.59.32")
    (source
     (origin
      (method url-fetch)
      (uri "https://nodejs.org/dist/v16.17.0/node-v16.17.0-linux-x64.tar.xz")
      (sha256 (base32 "16xl65mjkvs9cwa4f1j5fn7vbhi6j4zsrybxpdyxzl542xx7v1ph"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan ;; patching ucm binary
       `(("bin/node"
          ("gcc:lib")))
       #:validate-runpath? #f ;; Build breaks if turned on, works fine if turned off
       #:install-plan
       '(("bin/node" "bin/node"))
       #:phases (modify-phases %standard-phases ;; breaks without, does not work with
                  (delete 'make-dynamic-linker-cache))))
    (inputs `(("gcc:lib" ,gcc "lib")
              ))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:epl1.0)))

node-bin
