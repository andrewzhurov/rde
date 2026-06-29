;;; rde --- Reproducible development environment.
;;;
;;; SPDX-FileCopyrightText: 2024, 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rde-configs env guix channels)
  #:use-module ((rde env guix channels) #:prefix rde:)
  #:use-module (guix channels)
  #:export (core-channels))

(define core-base-channels
  (cons
   (channel
    (name 'rde)
    (url "https://git.sr.ht/~abcdw/rde")
    (branch "master")
    (commit
     "f801b5f6a3df2aac8099c991f29277044317db68")
    (introduction
     (make-channel-introduction
      "257cebd587b66e4d865b3537a9a88cccd7107c95"
      (openpgp-fingerprint
       "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
   rde:core-channels))

(define core-channels
  (cons
   (channel
    (name 'nonguix)
    (url "https://gitlab.com/nonguix/nonguix")
    (commit
     ;; "3c61d24b8818de45738ffe358e8b40d705cb3a6d" ;; before adding linux-libre-6.17
     ;; "705333bf29dd5ed80044362d060fabd539271dc7" ;; python-3.11/fixed removed, CommitDate: Wed Dec 17 14:44:42 2025 -0500
     ;; "3f4a1907cae9b0def09d1549c491bb4e307b9097" ;; before adding python-3.11/fixed, (linux-libre-6.16), CommitDate: Wed Oct 8 10:39:25 2025 +0200
     "25d7a8091c2c9678a8694f073d846a7001165169" ;; before ffmpeg-6, CommitDate: Tue Sep 23 22:45:08 2025 +0200
     ;;"d741f41eca0a04694775118eedaa332ffa1a26f8"   ;; 5 Dec 2025
     )
    ;; Enable signature verification:
    (introduction
     (make-channel-introduction
      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
      (openpgp-fingerprint
       "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
   core-base-channels))

core-channels
