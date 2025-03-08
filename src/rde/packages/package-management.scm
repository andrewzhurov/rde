;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (rde packages package-management)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages guile)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:export (guix-from-channels-lock)
  ;; #:export (nonguix-from-channels-lock)
  )

(define-public guix-from-channels-lock
  (let ((commit "59b2a60d0041882d732e1766e28f0df5a1ef1ac1"))
    (package
      (inherit guix)
      (version (string-append "1.4.0-" (string-take commit 7)))
      (source
       (git-checkout
        (url "https://git.savannah.gnu.org/git/guix.git")
        (commit commit)))
      (arguments
       (substitute-keyword-arguments (package-arguments guix)
         ((#:tests? _)
          #f)
         ((#:phases phases)
          #~(modify-phases #$phases (delete 'check)))
         ((#:configure-flags flags #~'())
          #~(append
             #$flags
             (list
              ;; "--disable-daemon"
              ;; "--localstatedir=/var"
              #$(string-append "--with-channel-commit=" commit))))))

      (inputs (modify-inputs (package-inputs guix)
                (replace "guile" guile-next))))))

;; (define-public nonguix-from-channels-lock
;;   (let ((commit "b6d05dbefd2664aa6706d13ec4f46526a814369f"))
;;     (package
;;       (inherit nonguix)
;;       (version (string-append "1.4.0-" (string-take commit 7)))
;;       (source (origin
;;                 (method git-fetch)
;;                 (uri (git-reference
;;                       (url "https://gitlab.com/nonguix/nonguix"
;;                        ;; (channel-url pinned-channel-guix)
;;                        )
;;                       (commit commit)))
;;                 (sha256
;;                  (base32
;;                   "03z38k828ismsbfwvpwiqr557vlhdg0vg7dpvyqbvyhfy8l6c27w"))
;;                 (file-name (string-append "nonguix-" version "-checkout"))))
;;       (arguments (list #:tests? #f))
;;       (inputs (package-inputs nonguix)))))
