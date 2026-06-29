;;; rde --- Reproducible development environment.
;;;
;;; SPDX-FileCopyrightText: 2024, 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rde env guix channels)
  #:use-module (guix channels)
  #:export (core-channels))

(define core-channels
  (list (channel
         (name 'guix)
         ;; Use codeberg directly: git.guix.gnu.org now 302-redirects to
         ;; codeberg, and guile-git's libgit2 refuses cross-host redirects.
         (url "https://codeberg.org/guix/guix.git")
         (branch "master")
         (commit
          ;; rde-tested guix (came in via abcdw/master 2026-05-04 merge).
          "0ce84291efb22a0f7b73a9099b4d2bfde267e352" ;; abcdw/master 2026-05-04
          ;; ----- older pins kept for reference -----
          ;; "92c63391ee25205be3b8525d5d1fe5b9f345f37f" ;; original,                CommitDate: Mon Sep 8  20:32:51 2025 +0800
          ;; "df3d4db1f76c03855d92f7de96bac5f36e338e1c" ;; updated tdlib,           CommitDate: Tue Dec 2 18:04:29 2025 +0100
          ;; "a2590694ae0350f9d7400f6f6f41fdbac2fa5340" ;; added python-3.11/fixed, CommitDate: Wed Oct 15 22:03:05 2025 +0200
          )
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

core-channels
