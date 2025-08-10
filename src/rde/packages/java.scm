(define-module (rde packages java)
  #:use-module (gnu packages base)
  #:use-module (gnu packages node)
  #:use-module (gnu packages node-xyz)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system node)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public cfr
  (package
    (name "cfr")
    (version "0.152")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/leibnitz27/cfr/releases/download/" version "/cfr-" version ".jar"))
       (sha256
        (base32
         "1zrrbl27252xm4lzvngqib9y6xsd1klh4dfmz3z5cc0vqpfc5zgz"))))
    (build-system copy-build-system)
    (arguments
     (arguments
      '(#:install-plan
        '(("cfr-0.152.jar" "out/cfr.jar")))))
    (home-page "https://github.com/leibnitz27")
    (synopsis "This is the public repository for the CFR Java decompiler")
    (description "This is the public repository for the CFR Java decompiler")
    (license license:expat)))
