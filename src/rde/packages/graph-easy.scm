(define-module (gnu packages graph-easy)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:))

(define-public graph-easy
  (package
    (name "graph-easy")
    (version "7.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.com/api/v4/projects/4207231"
                                  "/packages/generic/graphviz-releases/"
                                  version "/graphviz-" version ".tar.xz"))
              (sha256
               (base32
                "1b6x3g03j7q77lzyvdp34hkzld5sg1l1ippc6sh1qxnmm59xs3ly"))))
    (build-system gnu-build-system)
    (arguments
     ;; FIXME: rtest/rtest.sh is a ksh script (!).  Add ksh as an input.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'move-guile-bindings
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((lib (string-append #$output "/lib"))
                          (extdir (string-append lib "/guile/"
                                                 #$(version-major+minor
                                                    (package-version
                                                     (this-package-input "guile")))
                                                 "/extensions")))
                     (mkdir-p extdir)
                     (rename-file (string-append
                                   lib "/graphviz/guile/libgv_guile.so")
                                  (string-append extdir
                                                 "/libgv_guile.so"))))))))
    (inputs
     (list libxrender
           libx11
           gts
           gd
           guile-3.0                    ;Guile bindings
           pango
           fontconfig
           freetype
           libltdl
           libxaw
           expat
           libjpeg-turbo
           libpng))
    (native-inputs
     (list bison
           pkg-config
           swig))
    (outputs '("out" "doc"))            ;5 MiB of html + pdfs
    (home-page "")
    (synopsis "")
    (description "")
    (properties
     '((release-monitoring-url . "https://graphviz.org/download/source/")))
    (license license:epl1.0)))
