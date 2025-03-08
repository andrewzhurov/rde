(define-module (rde packages scrcpy)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:)

  #:export (scrcpy))

(use-modules (gnu packages sdl)
             (gnu packages video)
             (gnu packages pkg-config)
             (gnu packages libusb)
             (gnu packages java)
             (gnu packages android))

(define-public scrcpy
  (package
    (name "scrcpy")
    (version "1.23")
    (home-page "https://github.com/Genymobile/scrcpy")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url "https://github.com/Genymobile/scrcpy")
                           (commit (string-append "v" version))))
       (sha256
        (base32
         "00v6lf65s99hrms942ppvmsy4fk9y3wpsr61j12x2z44bz0z87jr"))
       (file-name (string-append "scrcpy-" version "-checkout"))))
    (build-system meson-build-system)
       ;; (modify-phases %standard-phases
       ;;   (add-before 'build 'set-java-home
       ;;    (lambda* (#:key inputs #:allow-other-keys)
       ;;      (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
       ;;      #t)))
    (arguments
     (list
      #:configure-flags
      #~(list (format #f "-Dprebuilt_server=~a"
                      #$(local-file "./packages/scrcpy-server-v1.23")))))
    (inputs
     (list ffmpeg sdl2 pkg-config libusb))
    (propagated-inputs (list adb))
    (synopsis "Developers and power user friendly GNU/Linux distribution")
    (description "The GNU/Linux distribution, a set of tools for managing
development environments, home environments, and operating systems, a set of
predefined configurations, practices and workflows.")
    (license license:gpl3+)))
