
(list (channel
        (name 'rde)
        (url "https://github.com/abcdw/rde"
             ;; "https://github.com/abcdw/rde.git" ;; don't use, will cache version with "Permission denied"
             ;; "https://git.sr.ht/~abcdw/rde" ;; sr.ht is unavailable atm
             )
        (branch "master")
        (commit
          "b57387f29ed1ef835b500ca8beca312b2233117d")
        (introduction
          (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "bd6d76b8a44bb14dedaed070b7056f2f56c2e161")
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
         "e662a42388490a2736276c2051aaf5cd3693a1cc"
          ;; "ccb2e98821def4c45af84e904ddde908a8a87748"
          )
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      )

;; from nonrde lock file, does not work
;; (list (channel
;;         (name 'guix)
;;         (url "https://git.savannah.gnu.org/git/guix.git")
;;         (branch "master")
;;         (commit
;;           "2e8b4f9bfa00489fd3acff305837a79af236e183")
;;         (introduction
;;           (make-channel-introduction
;;             "9edb3f66fd807b096b48283debdcddccfea34bad"
;;             (openpgp-fingerprint
;;               "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
;;       (channel
;;         (name 'nonguix)
;;         (url "https://gitlab.com/nonguix/nonguix")
;;         (branch "master")
;;         (commit
;;           "ccb2e98821def4c45af84e904ddde908a8a87748")
;;         (introduction
;;           (make-channel-introduction
;;             "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
;;             (openpgp-fingerprint
;;               "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

;; broken magit
;; (list (channel
;;         (name 'guix)
;;         (url "https://git.savannah.gnu.org/git/guix.git")
;;         (branch "master")
;;         (commit
;;           "8918e42de06722f296296db8a693f14211d5bdc2")
;;         (introduction
;;           (make-channel-introduction
;;             "9edb3f66fd807b096b48283debdcddccfea34bad"
;;             (openpgp-fingerprint
;;               "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
;;       (channel
;;         (name 'nonguix)
;;         (url "https://gitlab.com/nonguix/nonguix")
;;         (branch "master")
;;         (commit
;;           "dfc2b256ebe208605c7da1b35ab0bfd8304ae675")
;;         (introduction
;;           (make-channel-introduction
;;             "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
;;             (openpgp-fingerprint
;;               "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
