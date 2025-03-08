(define-module (rde packages gitwatch))

(use-modules
   ((guix licenses) #:prefix license:)
   (guix utils)
   (guix build utils)
   (guix packages)
   (guix git-download)
   (guix build-system trivial)
   (guix build-system copy)
   (gnu packages ssh)
   (gnu packages version-control)
   (gnu packages linux)
   (gnu packages web)
   (gnu packages pkg-config)
   (gnu packages python)
   (gnu packages compression)
   (gnu packages tls))

(define-public gitwatch
  (let ((commit "9339ff6832c56eb30b6f2c7df0eaae48122b3571")
        (revision "1"))
    (package
      (name "gitwatch")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gitwatch/gitwatch")
                      (commit commit)))
                (file-name (git-file-name name version))

                (sha256
                 (base32
                  "1jrjvzi3lka05dv0g4jswajak2w267995fcvka4bnkfik71k8kmv"))))

      (build-system copy-build-system)
      (arguments
       `(#:install-plan
	 '(("gitwatch.sh" "bin/gitwatch"))))

      (propagated-inputs `(("git" ,git)
			   ("inotify-tools" ,inotify-tools)))
      (home-page "https://github.com/gitwatch/gitwatch")
      (synopsis "Watch a file or folder and automatically commit changes to a git repo easily.")
      (description "A bash script to watch a file or folder and commit change.")
      (license license:gpl3))))
