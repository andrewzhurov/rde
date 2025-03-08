(define-module (rde packages ldtk)
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

(define-public node-bin
  (package
    (name "ldtk")
    (version "1.5.3")
    (source
     (origin
      (method url-fetch)
      (uri "https://github.com/deepnight/ldtk/archive/refs/tags/v1.5.3.tar.gz")
      (sha256 (base32 "16xl65mjkvs9cwa4f1j5fn7vbhi6j4zsrybxpdyxzl542xx7v1ph"))))
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (string-append (assoc-ref %outputs "out")
                                    "/share/guile/site/"
                                    (effective-version))))
           (mkdir-p out)
           (call-with-output-file (string-append out "/hg2g.scm")
             (lambda (port)
               (define defmod 'define-module) ;fool Geiser
               (write `(,defmod (hg2g)
                         #:export (the-answer))
                      port)
               (write '(define the-answer 42) port)))))))
    (inputs `(("gcc:lib" ,gcc "lib")
              ))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:epl1.0)))

node-bin
