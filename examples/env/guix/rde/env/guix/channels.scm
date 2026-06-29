;;; This is an automatic copy of RDE's channels.scm
;;; Do not edit it manually

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
         (url "https://git.guix.gnu.org/guix.git")
         (branch "master")
         (commit
          ;; "df3d4db1f76c03855d92f7de96bac5f36e338e1c"    ;; updated tdlib,           CommitDate: Tue Dec 2 18:04:29 2025 +0100
          ;; "02114e6e983751ce6156a4d09beb50142109a030"    ;; moved tdlb,              CommitDate: Wed Oct 22 19:37:56 2025 +0200
          ;; "2f12efaf3994edc23be816bb49dd2a349ca99ac9" ;; before moved tdlib,      CommitDate: Wed Oct 22 19:37:48 2025 +0200
          ;; "0c3462568b718bb0ec6df9562adc5ebc8d49e885" ;; added 7zip,              CommitDate: Wed Oct 22 16:43:05 2025 +0200
          ;; "a2590694ae0350f9d7400f6f6f41fdbac2fa5340" ;; added python-3.11/fixed, CommitDate: Wed Oct 15 22:03:05 2025 +0200
          "92c63391ee25205be3b8525d5d1fe5b9f345f37f" ;; original,                CommitDate: Mon Sep 8  20:32:51 2025 +0800
          )
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

core-channels
