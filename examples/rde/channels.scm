(use-modules (guix ci)
             (guix channels))

(list
 %default-guix-channel
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (branch "master")
  (commit
   "ac1f7b074ea47e8a330e3475732c2e905928df35" ;; latest as of 2025-03-13
   ;; "7c1a45d8e71580fe45f75b4d7eba0a52eb2ed9b0" ;; latest as of 2025-02-07
   ;; "877ed8cc4eee26ddde3d7d200c19c370c6bf7cb1" ;; latest as of 2024-06-26
   ;; "b6d05dbefd2664aa6706d13ec4f46526a814369f" ;; latest as of 2024-02-27
   ;; "71a53faf2e1925a309b480f17e5b836740ce54bc" ;; latest as of 2023-12-23
   ;; "f5fa2efb0c5b0efdf0d1e446ee97a30a32f3f4bd"
   )
  (introduction
   (make-channel-introduction
    "ee2826e22be82ebd624b4daeadc6de97eaa69d02"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (introduction
   (make-channel-introduction
    "257cebd587b66e4d865b3537a9a88cccd7107c95"
    (openpgp-fingerprint
     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))))
