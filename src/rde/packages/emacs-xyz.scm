;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022, 2023, 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Samuel Culpepper <samuel@samuelculpepper.com>
;;; Copyright © 2024 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2024, 2025 Nicolas Graves <ngraves@ngraves.fr>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

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

(define-public emacs-arei-latest
  (let* ((commit "cec17d88f452f740ac007a07b10de403e76b0ccb")
         (revision "1"))
    (package
      (inherit emacs-arei)
      (name "emacs-arei")
      (version (git-version "0.9.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.sr.ht/~abcdw/emacs-arei")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1k68247p8sx6mzagbic0wn671ilax51hbra3p38g8vq5b4yx54bn"))))
      (build-system emacs-build-system))))

(define-public emacs-justify-kp
 (let ((commit "385e6b8b909ae0f570f30101cec3677e21c9e0a0"))
  (package
   (name "emacs-justify-kp")
   (version "20171119")
   (home-page "https://github.com/qzdl/justify-kp")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "13fylx4mvw7cgzd2mq060x43b1x7g5vdf16jm49c31f6b3jj1qi0"))))
   (build-system emacs-build-system)
   (inputs (list emacs-dash emacs-s))
   (synopsis "Paragraph justification for emacs using Knuth/Plass algorithm ")
   (description
    "Paragraph justification for emacs using Knuth/Plass algorithm ")
   (license license:gpl3+))))

(define-public emacs-eslint-fix
 (let ((commit "636bf8d8797bdd58f1b543c9d3f4910e3ce879ab"))
  (package
   (name "emacs-eslint-fix")
   (version "1.0")
   (home-page "https://github.com/codesuki/eslint-fix")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "02hjm685fl4f33s5fi8nc088wwfzhyy6abx5g4i93b2dx3hr2lyi"))))
   (build-system emacs-build-system)
   (synopsis "Fix current file using ESLint --fix")
   (description
    "This packages provides `eslint-fix', which fixes the current file using ESLint.")
   (license license:gpl3+))))

(define-public emacs-flymake-eslint
 (let ((commit "6ab909b85a8e97815db9831cdd5f283a7830177f"))
  (package
   (name "emacs-flymake-eslint")
   (version "1.7.0")
   (home-page "https://github.com/orzechowskid/flymake-eslint")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "19jyim5rrmv8bdskaw8gxb6fch3jj3irqhddi2aaxvgdxn321yxm"))))
   (build-system emacs-build-system)
   (synopsis "Flymake backend for Javascript using eslint")
   (description
    "A backend for Flymake which uses eslint.  Enable it with M-x
flymake-eslint-enable RET.")
   (license license:gpl3+))))

(define-public emacs-json-simple-flymake
 (let ((commit "f3dacf070d1e04d5805323b0a95d58c5b9b7f607"))
  (package
   (name "emacs-json-simple-flymake")
   (version "20230802")
   (home-page "https://github.com/mokrates/json-simple-flymake")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "052c7xivwd2grxwpspjnfj3hzla2lgc3r8yq24i5jbyasdnpghbc"))))
   (build-system emacs-build-system)
   (synopsis "Really simple but standalone json flymake utilizing the
builtin json parser")
   (description
    "Really simple but standalone json flymake utilizing the builtin json
parser.")
   (license license:gpl3+))))

(define-public emacs-hide-header-line
  (package
    (inherit emacs-hide-mode-line)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-it-update-header-line
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "hide-mode-line.el"
	       ((" mode-line-format")
                " header-line-format"))
             #t)))))))

(define-public emacs-header-minions
  (package
    (inherit emacs-minions)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-it-update-header-line
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "minions.el"
	       (("mode-line-format")
                "header-line-format"))
             #t)))))))

(define-public emacs-git-email-sans-mu4e
  (package
    (inherit emacs-git-email)
    (inputs (modify-inputs (package-inputs emacs-git-email)
              (delete "mu")))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-git-email)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'remove-mu4e
              (lambda _
                (delete-file "git-email-mu4e.el")))))))))

;; Note: There is also a channel with a development version
;; at https://codeberg.org/suhail/git-email
(define-public emacs-git-email-latest emacs-git-email-sans-mu4e)

(define-public emacs-transient-latest emacs-transient)

(define-public emacs-git-gutter-transient
  (package
   (name "emacs-git-gutter-transient")
   (version "0.1.0")
   (source
    (local-file "../features/emacs/git-gutter-transient" #:recursive? #t))
   (build-system emacs-build-system)
   (inputs
    `(("emacs-magit" ,emacs-magit)))
   (propagated-inputs
    `(("emacs-git-gutter" ,emacs-git-gutter)))
   (license license:gpl3+)
   (home-page "https://sr.ht/~abcdw/git-gutter-transient")
   (synopsis "Navigate, stage and revert hunks with ease")
   (description "This package provides transient interface for git-gutter function
to manipulate and navigate hunks.")))

(define-public emacs-gider
  (package
    (name "emacs-gider")
    (version "0.1.0")
    (source
     (local-file "../../../files/emacs/gider" #:recursive? #t))
    (arguments
     (list
      #:exclude #~(list "^\\.dir-locals\\.el$" "^test/")
      #:include #~(cons "^src/" %default-include)))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-geiser emacs-geiser-guile))
    (license license:gpl3+)
    (home-page "https://sr.ht/~abcdw/rde")
    (synopsis "Guile Interactive Development Enviroment")
    (description "Right now it's just a few helpers on top of geiser.")))

(define-public emacs-geiser-latest
  (let ((commit "bd12f2dc6c5949e260f094fb60737498cd0ae9a5")
        (revision "1"))
    (package
      (inherit emacs-geiser)
      (name "emacs-geiser")
      (version (git-version "0.28.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/emacs-geiser/geiser")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16qi3vk1yps4f5v98ipdl5kq0jq5qlnlpx8c598csj9yk86p1hsw")))))))

(define-public emacs-srht-latest
  (let ((commit "d9a8f6a43671d67a86622507136d4195c2dcd149")
        (revision "1"))
    (package
      (inherit emacs-srht)
      (name "emacs-srht")
      (version (git-version "0.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.sr.ht/~akagi/srht.el")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "08ysidwlz4z6baih2810fpr1679217lnsb0jhwyvvj05g25ysy5b")))))))

(define-public emacs-geiser-guile-latest
  ((package-input-rewriting/spec
    `(("emacs-geiser" . ,(const emacs-geiser-latest))))
   emacs-geiser-guile))

(define-public emacs-geiser-eros-latest
  ((package-input-rewriting/spec
    `(("emacs-geiser" . ,(const emacs-geiser-latest))))
   emacs-geiser-eros))

(define-public emacs-gider-latest
  ((package-input-rewriting/spec
    `(("emacs-geiser" . ,(const emacs-geiser-latest))
      ("emacs-geiser-guile" . ,(const emacs-geiser-guile-latest))))
   emacs-gider))

(define-public emacs-guix-latest
  ((package-input-rewriting/spec
    `(("emacs-geiser" . ,(const emacs-geiser-latest))))
   emacs-guix))

(define-public emacs-telega-server-latest emacs-telega-server)

(define-public emacs-telega-latest emacs-telega)

(define-public emacs-telega-contrib-latest emacs-telega-contrib)

(define-public emacs-consult-eglot-sans-eglot
  (package
    (inherit emacs-consult-eglot)
    (inputs
     (modify-inputs (package-inputs emacs-consult-eglot)
       (append emacs-eglot)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs emacs-consult-eglot)
       (delete "emacs-eglot")))))

(define-public emacs-dirvish-latest emacs-dirvish)

(define-public emacs-docker-latest
  (let ((commit "cc0046e6a557dce0ccc4108dd22e04f21ba8b0dc")
        (revision "0"))
    (package
      (inherit emacs-docker)
      (name "emacs-docker")
      (version (git-version "2.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Silex/docker.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11l8jpqj6m04ndhnfz41nhph1rqjvqbfd5vw334mph776aq1baln"))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs emacs-docker)
         (delete "emacs-docker-tramp"))))))

(define-public emacs-clojure-ts-mode
  (package
    (name "emacs-clojure-ts-mode")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure-emacs/clojure-ts-mode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "101c1xwrmkb9rq713jij105117y2d0ffiplxsnb3z1h2pgcil0p8"))))
    (build-system emacs-build-system)
    (license license:gpl3+)
    (home-page "https://github.com/clojure-emacs/clojure-ts-mode.git")
    (synopsis "Major mode for Clojure code backed up by Tree-sitter")
    (description "\
clojure-ts-mode is an Emacs major mode that provides font-lock (syntax
highlighting), indentation, and navigation support for the Clojure(Script)
programming language, powered by the tree-sitter-clojure tree-sitter grammar.")))

(define-public emacs-zotra
  (let ((commit "c63e274950b5975c7d74f5d0df4b1a1e07f9b5f5")
        (revision "0"))
    (package
      (name "emacs-zotra")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mpedramfar/zotra")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1jy4a8fhfdvk2i9ac18mka3b2hk3x3r8kmvxkhip1ks50s2m71cg"))))
      (build-system emacs-build-system)
      (license license:gpl3)
      (home-page "https://github.com/mpedramfar/zotra")
      (synopsis "Get bibliographic information from a url")
      (description
       "This emacs library provides functions to get bibliographic information
 from a url and save it into a bibtex file. It also provides a way to obtain a
 list of attachments (e.g. PDF files) associated with a url. This is done
 using Zotero translators, but without using the Zotero client."))))

;; Andrew Zhurov's sandbox
(use-modules (guix download))

(define-public emacs-flycheck-clj-kondo-ff7bed
  (let* ((commit "ff7bed2315755cfe02ef471edf522e27b78cd5ca")
         (revision "0"))
    (package
      (name "emacs-flycheck-clj-kondo")
      (version (git-version "0.0.3" revision commit)) ;; latest as of 2022-08-23
      (source
       (origin
         (method git-fetch)
         (uri  (git-reference
                (url "https://github.com/borkdude/flycheck-clj-kondo")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0h0cbiifzjfm5ymwf98h6nlkaqavdlxvccdsb1h0yf4246scf251"))))
      (inputs
       (list emacs-flycheck))
      (build-system emacs-build-system)
      (home-page "https://github.com/borkdude/flycheck-clj-kondo")
      (synopsis
       "Emacs integration for clj-kondo via flycheck")
      (description
       "This package integrates clj-kondo with Emacs via flycheck.")
      (license license:epl1.0) ;; presumably the same as for clj-kondo, but it's not set explicitly in the project repo
      )))

(define-public emacs-flycheck-clj-kondo-e38c67
  (let* ((commit "e38c67ba9db1ea1cbe1b61ab39b506c05efdcdbf")
         (revision "0"))
    (package
      (name "emacs-flycheck-clj-kondo")
      (version (git-version "2024-02-19" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri  (git-reference
                (url "https://github.com/borkdude/flycheck-clj-kondo")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1pxlb8axgmc8cw4id40z576kd041qb1irq8rkjn6xbda585ix58f"))))
      (inputs
       (list emacs-flycheck))
      (build-system emacs-build-system)
      (home-page "https://github.com/borkdude/flycheck-clj-kondo")
      (synopsis
       "Emacs integration for clj-kondo via flycheck")
      (description
       "This package integrates clj-kondo with Emacs via flycheck.")
      (license license:epl1.0) ;; presumably the same as for clj-kondo, but it's not set explicitly in the project repo
      )))

(define-public emacs-flycheck-clj-kondo emacs-flycheck-clj-kondo-e38c67)
;; (define-public emacs-flycheck-clj-kondo emacs-flycheck-clj-kondo-ff7bed)

(define-public emacs-org-roam-ui
  (package
    (name "emacs-org-roam-ui")
    (version "9ed0c5705a302a91fab2b8bcc777a12dcf9b3682")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/org-roam/org-roam-ui")
             (commit "9ed0c5705a302a91fab2b8bcc777a12dcf9b3682")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1am11vnzklv0cbivsw5r8x8fx457166mvfgyv7cdhrz88s8iqm23"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases %standard-phases
       #:include (cons "out" %default-include))) ;; "out" contains .js code, as mentioned here: https://github.com/org-roam/org-roam-ui/issues/77#issuecomment-907664589
    (propagated-inputs
     `(("emacs-org-roam" ,emacs-org-roam)
       ("emacs-websocket" ,emacs-websocket)
       ("emacs-simple-httpd" ,emacs-simple-httpd)
       ("emacs-f" ,emacs-f)
       ))
    (home-page "https://github.com/org-roam/org-roam-ui")
    (synopsis "A graphical frontend for exploring your org-roam Zettelkasten")
    (description "Org-Roam-UI is a frontend for exploring and interacting with your org-roam notes.

Org-Roam-UI is meant a successor of org-roam-server that extends functionality of org-roam with a Web app that runs side-by-side with Emacs.")
       (license license:gpl3+))) ;; actually it's GPL3.0

(define-public emacs-flymake-clippy
  (package
    (name "emacs-flymake-clippy")
    (version "9ed0c5705a302a91fab2b8bcc777a12dcf9b3682")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacsmirror/flymake-clippy")
             (commit "713b7e873d6b30dc0ded75d5d890d6847f2ea093")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "097yha74kabxzyf6zqdi94wxjs7zdsg38nxwz1w4w86wxlrq0ymg"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases %standard-phases
       #:include (cons "out" %default-include)))
    (home-page "https://github.com/emacsmirror/flymake-clippy")
    (synopsis "")
    (description "")
    (license license:gpl3+))) ;; GPL 3.0

(define-public emacs-org-clipboard-image
  (package
   (name "emacs-org-clipboard-image")
   (version "0.1.0")
   (source
    (local-file "../features/emacs/org-clipboard-image" #:recursive? #t))
   (build-system emacs-build-system)
   (propagated-inputs
    `(("wl-clipboard" ,(@ (gnu packages xdisorg) wl-clipboard)) ;; for wl-paste
      ("coreutils"    ,(@ (gnu packages base)    coreutils))    ;; for sha256sum
      ("sed"          ,(@ (gnu packages base)    sed))          ;; for sed
      ))
   (license license:gpl3+)
   (home-page "")
   (synopsis "Save image from clipboard under a hash name and paste it as org image link.")
   (description "")))

(define-public emacs-org-media-note
  (package
    (name "emacs-org-media-note")
    (version "2024-06-27")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yuchen-lea/org-media-note")
             (commit "1f5cc8d1bbbc1b2fc99d27ac753ad64a5c98c8c9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mlqajlpnl5cn8ypc7ga4b8yq83fmsz5nvyynx1bfhp64p2f975y"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-mpv" ,(@ (gnu packages emacs-xyz) emacs-mpv))
       ("emacs-org-ref" ,(@ (gnu packages emacs-xyz) emacs-org-ref))
       ("emacs-pretty-hydra" ,(@ (gnu packages emacs-xyz) emacs-pretty-hydra))
       ("mpv" ,(@ (gnu packages video) mpv))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'debug
           (lambda* (#:key outputs #:allow-other-keys)
             (format #t "~a" outputs)))
         (delete 'build))
       #:include %default-include))
    (home-page "https://github.com/yuchen-lea/org-media-note")
    (synopsis "")
    (description "Taking interactive notes when watching videos or listening to audios in org-mode.")
    (license license:gpl3+))) ;; GPL 3.0

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
           "00l6mc643na97jrb0k595kwmfg8wc7m5iqjd9l9vvf3dal6389b8"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/xiongtx/jdecomp")
      (synopsis "Emacs interface to Java decompilers")
      (description "")
      (license license:gpl3+)))) ;; no license, actually

(use-modules (rde api store))
(build-with-store emacs-org-media-note)


(use-modules (ice-9 pretty-print)
             (ice-9 match)
             (guix records)
             (srfi srfi-197))

'(package
   emacs-org-media-note
   (struct-layout (struct-vtable emacs-org-media-note))
   (struct-vtable (struct-vtable emacs-org-media-note))
   (record-type-fields (struct-vtable emacs-org-media-note))
   (record-accessor (struct-vtable emacs-org-media-note) 'name)


   (pretty-print '(1 2 (3 4 5) 6 7 8 9 0 11 12 13 14 15 16 17 18 19 20) (current-output-port))
   (chain 1 (+ 2 _) (+ 3 _)))

(define* (record->field+accessor rec)
  (let* ((fields (record-type-fields (struct-vtable rec)))
         (fields+accessors (map (lambda (field)
                                   `(,field . ,(record-accessor (struct-vtable rec) field)))
                                 fields)))
    fields+accessors))

(define* (record->alist rec)
  (let* ((field+accessor (record->field+accessor rec))
         (record-alist (map (match-lambda ((field . accessor)
                                    `(,field . ,(accessor rec))))
                     field+accessor)))
    ;; (call-with-output-string (lambda (p) (object->fields rec field+accessor p)))
    record-alist))

(define* (pprint-record rec)
  (chain rec
         (record->alist _)
         (pretty-print _)))

'((pprint-record emacs-org-media-note)
  (assoc-ref (record->alist emacs-org-media-note) 'name)


  (list '(1 . 2))
  (package-name emacs-org-media-note)
  (object->string emacs-org-media-note))

emacs-jdecomp
