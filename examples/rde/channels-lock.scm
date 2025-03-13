(use-modules (guix channels))

(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix-mirror")
        (branch "master")
        (commit
         "e124661486ec722b5c09a94b416f0104b9dde5a4")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
         "ac1f7b074ea47e8a330e3475732c2e905928df35")
        (introduction
          (make-channel-introduction
            "ee2826e22be82ebd624b4daeadc6de97eaa69d02"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'rde)
        (url "https://git.sr.ht/~abcdw/rde")
        (branch "master")
        (commit
          "079e3e4ee1072b06c2a22485aa0b821019975f19")
        (introduction
          (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))))
