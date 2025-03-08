(define-module (andrewzhurov kernels unfree)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features system)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (gnu packages linux)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:export (unfree-kernel))

(define nonguix-pub (local-file "../files/keys/nonguix-key.pub"))

(define (unfree-kernel config)
  (define cleaned-features
    (remove (lambda (f)
              (member (feature-name f) (list 'base-services 'kernel)))
            (rde-config-features config)))
  (rde-config
   (inherit config)
   (features
    (append
     (list
      (feature-kernel
       #:kernel linux
       #:kernel-arguments '("snd_hda_intel.dmic_detect=0")
       #:firmware (list linux-firmware))
      (feature-base-services
       ;; TODO: Use substitute-urls directly for guix commands?
       #:default-substitute-urls (list "https://bordeaux.guix.gnu.org"
                                       "https://ci.guix.trop.in"
                                       "https://gitlab.com/nonguix/nonguix")
       #:guix-substitute-urls (list "https://substitutes.nonguix.org")
       #:guix-authorized-keys (list nonguix-pub)))
     cleaned-features))))
