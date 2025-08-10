(define-module (rde packages emacs-xyz)
  #:use-module (rde packages messaging)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages video)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))


(define-public emacs-jdecomp
  (let* ((commit "692866abc83deedce62be8d6040cf24dda7fb7a8"))
    (package
      (name "emacs-jdecomp")
      (version "0.2.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/xiongtx/jdecomp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1k68247p8sx6mzagbic0wn671ilax51hbra3p38g8vq5b4yx54bn"))))
      (build-system emacs-build-system))))

emacs-jdecomp
