(define-module (rde packages fastboot))

(define fastboot-version "r33.0.0")

;; list libraries that the binary needs to be patched with
(use-modules (gnu packages gcc))
(define patchelf-libs `("gcc:lib"))
(define fastboot-inputs
  `(("gcc:lib" ,gcc "lib")))

(use-modules (gnu packages compression)) ;; for unzip
(define fastboot-native-inputs
  `(("unzip" ,unzip)
    ))

(use-modules (guix packages)) ;; for package
(use-modules (guix download)) ;; for url-download
(use-modules (nonguix build-system binary)) ;; for binary-build-system
(use-modules ((guix licenses) #:prefix license:)) ;; for license

(define fastboot
  (package
    (name "fastboot")
    (version fastboot-version)
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://dl.google.com/android/repository/platform-tools_" version "-linux.zip")) ;; url is taken from https://developer.android.com/studio/releases/platform-tools
              (sha256
               (base32
                "1m3lqhdzlakph7bfkk2vlifhmld52szhrv6bij26l7qwm9h6nx6a"
                ))))

    (build-system binary-build-system)
    (arguments
     `(#:validate-runpath? #f ;; Build breaks if turned on, works fine if turned off
       #:strip-binaries? #f ;; For some reason it breaks the program
       #:patchelf-plan ;; patching ucm binary
       `(("platform-tools/fastboot"
          ("gcc:lib")))
       #:install-plan
       `(("platform-tools/fastboot" "/bin/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
                  (lambda* (#:key inputs #:allow-other-keys)
                    (invoke "unzip" (assoc-ref inputs "source")))))))

    (native-inputs fastboot-native-inputs)
    (inputs fastboot-inputs)

    (synopsis "fastboot binary")
    (description "")
    (home-page "")
    (license license:gpl2) ;; it's not GPL2
    ))

fastboot
