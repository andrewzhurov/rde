(define-module (rde packages clj-kondo)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages readline)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:)
  )

(define-public clj-kondo
 (package
   (name "clj-kondo")
   (version "2024.05.24")
   (source (origin
             (method url-fetch/zipbomb)
             (uri (string-append
                   "https://github.com/clj-kondo/clj-kondo/releases/download/v"
                   version "/clj-kondo-" version "-linux-amd64.zip"))
             (sha256
              (base32
               "13a264kf2la4s93kijqgcppkp1zmxdhv8c959llmsbdblv9cn38w"))))
   (build-system binary-build-system)
   (arguments
    (list #:patchelf-plan `'(("clj-kondo" ("gcc" "zlib")))
          #:install-plan `'(("clj-kondo" "/bin/"))
          #:phases #~(modify-phases %standard-phases
                       (add-after 'unpack 'chmod
                         (lambda _
                           (chmod "clj-kondo" #o755))))))
   (native-inputs
    (list unzip))
   (inputs
    (list `(,gcc "lib")
          zlib))
   (supported-systems '("x86_64-linux"))
   (home-page "https://github.com/clj-kondo/clj-kondo")
   (synopsis  "Linter for Clojure code")
   (description "Clj-kondo performs static analysis on Clojure, ClojureScript
and EDN, without the need of a running REPL.")
   (license license:epl1.0)))
