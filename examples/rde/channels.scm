(use-modules (guix ci)
             (guix channels))

(list
 ;; %default-guix-channel
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (branch "master")
  (commit "f5fa2efb0c5b0efdf0d1e446ee97a30a32f3f4bd")
  (introduction
   (make-channel-introduction
    "ee2826e22be82ebd624b4daeadc6de97eaa69d02"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

 (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (commit "bb580995a3ace2ecf4e3ef33d200d5b8faad751e")
  (introduction
   (make-channel-introduction
    "bb580995a3ace2ecf4e3ef33d200d5b8faad751e"
    (openpgp-fingerprint
     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))

 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (branch "master")
  (commit
   "8e883dc8210d4a7c3f09961994685ed54942fd73"
   ;; "2c757e8fb4385f889ec91f02b77acdf27143c316" ;; somewhat latest, somewhat working
   ;; "d16edd03cfa84f6d5fed979fd7283966cd3e4934" ;; latest as of 2023-10-07 14:00
   )
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
