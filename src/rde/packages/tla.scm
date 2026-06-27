(define-module (gnu packages tla)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages llvm-meta)
  #:use-module (gnu packages mingw)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix search-paths)
  #:use-module (guix download)
  #:use-module (guix memoization)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35))

(define tla-bin
  (let* ((commit-sha "91d5e51a4f29426b5273704628f755f8ee325900"))
    (package
      (name "tla-bin")
      (version commit-sha)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pmer/tla-bin")
                      (commit commit-sha)))
                (sha256
                 (base32
                  "1dac6kk9h9d1i0225d402h5dlgnl88lpz3g6h4chb0il2kraw8nl"))))
      (supported-systems '("x86_64-linux")) ;; could be more
      (native-inputs (list coreutils))
      (build-system trivial-build-system)
      (arguments
       `(#:guile ,%bootstrap-guile
         #:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out (string-append (assoc-ref %outputs "out")))
                  (bin (string-append out "/bin"))
                  (lib (string-append out "/lib")))
             (invoke "./install.sh")
             (mkdir-p bin)
             (mkdir-p lib)
             (copy-recursively "bin" bin)
             (copy-recursively "lib" lib)))))
      (home-page "https://github.com/pmer/tla-bin")
      (synopsis "Command line binaries for the TLA+ language")
      (description "tla-bin is a wrapper around https://github.com/tlaplus/tlaplus
 that provides command line binaries for pcal, tlc, tlatex and sany, making
 automation around TLA+ easy. Also provides a binary that starts the TLA+ REPL.")
      (license license:expat-0)))) ;; MIT actually, is it the same?

(use-modules (rde comment))
(comment
 (use-modules (guix scripts)
              (guix ui))
 (show-derivation-outputs
  (with-status-verbosity (assoc-ref opts 'verbosity)
    (with-store store
      (build-derivations store
                         (package->derivation tla-bin))))
  (build-package tla-bin
                 #:print-build-trace #t
                 #:print-extended-build-trace? #t
                 #:verbosity 100))
 (use-modules (rde api store))
 (build-with-store tla-bin)
 )
