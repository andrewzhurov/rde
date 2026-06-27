(use-modules (rde comment))

(comment
 (use-modules (guix gexp))
 (use-modules (rde api store))

 (glibc-dynamic-linker)
 (parameterize ((%daemon-socket-uri "file:///var/guix/daemon-socket/socket"))
   (build-with-store evcxr))


 ((@ (rde api store) build-with-store) wasm-pack)
 (use-modules (guix scripts)
              (guix ui)
              (guix packages)
              (guix derivations)
              (guix store)
              (guix scripts build)
              (guix tests)
              (web uri))
 (uri-reference?)
 (guix-build)
 (build wasm-pack)
 (with-status-verbosity (assoc-ref opts 'verbosity)
   (with-store store
     (build-derivations store
                        (package->derivation wasm-pack))))
 (%daemon-socket-uri)
 (let ((store (open-connection "file:///var/guix/daemon-socket/socket")))
   (lowered-gexp-inputs store (primitive-eval (lower-object wasm-pack))))
 (build-package wasm-pack
                #:print-build-trace #t
                #:print-extended-build-trace? #t
                #:verbosity 100)
 (let ((store (open-connection "file:///var/guix/daemon-socket/socket")))
   (build-derivations store
                      wasm-pack ))
 (open-connection "file:///var/guix/daemon-socket/socket")
 (open-connection-for-tests)
 (with-store store (+ 1 1))
 (;;show-derivation-outputs
  show-what-to-build*
  (build-package wasm-pack))

 (use-modules (gnu packages python))
 (use-modules (guix store))
 (use-modules (guix packages))
 (use-modules (guix utils))

 (define-public python-3.10-fixed
   (package
     (inherit python-3.10)
     (arguments
      (substitute-keyword-arguments (package-arguments python-3.10)
        ((#:make-flags flags)
         #~(list (string-append (car #$flags) " test_xml_etree" " test_xml_etree_c")))))))

 (parameterize ((%daemon-socket-uri "file:///var/guix/daemon-socket/socket"))
   (build-with-store python-3.10-fixed))

 )
