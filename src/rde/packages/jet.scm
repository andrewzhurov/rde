(define-module (rde packages jet)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:export (jet))


(define jet
  (package
    (name "jet")
    (version "0.2.18")
    (source (origin
              (method url-fetch)
              (uri
               "https://github.com/borkdude/jet/releases/download/v0.2.18/jet-0.2.18-linux-amd64.tar.gz")
              (sha256
               (base32
                "0qb708dcn4barln7zl9l40f5y9kisyhwg18an911hl7c2zahczij"))))

    (build-system binary-build-system)
    (arguments
     `(#:install-plan
       `(("jet" "/bin/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "-xvzf" (assoc-ref inputs "source")))))))

    (native-inputs `(("gzip" ,gzip)
                     ("tar" ,tar)))

    (synopsis "CLI to transform between JSON, EDN and Transit, powered with a minimal query language.")
    (description "This is a command line tool to transform between JSON, EDN and Transit, powered with a minimal query language. It runs as a GraalVM binary with fast startup time which makes it suited for shell scripting. It comes with a query language to do intermediate transformation. It may seem familiar to users of jq. Although in 2021, you may just want to use the --func option instead (who needs a DSL if you can use normal Clojure?)")
    (home-page "https://github.com/borkdude/jet")
    (license license:epl1.0)
    ))

jet
