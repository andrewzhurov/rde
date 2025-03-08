(define-module (andrewzhurov hosts t450)
  #:use-module (rde features base)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (rde features gitwatch)
  #:use-module (rde packages bb)

  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu packages linux)

  #:use-module (guix gexp)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (ice-9 match))

(define t450-original-mapped-devices
  (list (mapped-device
         (source (uuid "0e51ee1e-49ef-45c6-b0c3-6307e9980fa9"))
         (target "enc")
         (type luks-device-mapping))))

(define t450-file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "8F1E-C5A1" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device
           (uuid "cbbacbe6-2d31-411b-b94f-c8663738161c"
                 'ext4))
          (type "ext4"))
         ;; %base-file-systems
	 (list)))

(define nonguix-pub (local-file "../files/keys/nonguix-key.pub"))

(define-public %t450-features
  (list
   (feature-host-info
    #:host-name "t450"
    ;; ls `guix build tzdata`/share/zoneinfo
    #:timezone  "Europe/Minsk")

   ;;; Allows to declare specific bootloader configuration,
   ;;; grub-efi-bootloader used by default
   ;; (feature-bootloader)

   (feature-file-systems
    #:file-systems t450-file-systems)

   (feature-kanshi
    #:extra-config
    `((profile laptop ((output eDP-1 enable)))
      (profile docked ((output eDP-1 enable)
                       (output DP-2 scale 2)))))
   (feature-hidpi
    #:scaling-factor 1)

   (feature-kernel
    #:kernel linux
    #:kernel-arguments '("snd_hda_intel.dmic_detect=0")
    #:firmware (list linux-firmware))

   (feature-base-services
    ;; TODO: Use substitute-urls directly for guix commands?
    #:default-substitute-urls (list "https://bordeaux.guix.gnu.org"
                                    "https://ci.guix.trop.in"
                                    "https://substitutes.nonguix.org")
    #:guix-substitute-urls (list "https://substitutes.nonguix.org")
    #:guix-authorized-keys (list nonguix-pub))
   ))
