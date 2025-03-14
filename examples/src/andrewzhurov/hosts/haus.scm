(define-module (andrewzhurov hosts haus)
  #:use-module (rde features base)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (rde features gitwatch)
  #:use-module (rde packages bb)

  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu packages linux)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module (guix gexp)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages nvidia)

  #:use-module (ice-9 match))

;; (define transform
;;   (options->transformation
;;    '((with-graft . "mesa=nvda"))))

(define haus-mapped-devices
  (list (mapped-device
         (source (uuid "0e51ee1e-49ef-45c6-b0c3-6307e9980fa9"))
         (target "my-root")
         (type luks-device-mapping))))

(define haus-file-systems
  (list (file-system
          (mount-point "/boot/efi")
          (device (uuid "EE27-28F8" 'fat32))
          (type "vfat"))
        (file-system
          (mount-point "/")
          (device "/dev/mapper/my-root")
          (type "ext4")
          (dependencies haus-mapped-devices))))

(define nonguix-pub (local-file "../files/keys/nonguix-key.pub"))

(define-public %haus-features
  (list
   (feature-host-info
    #:host-name "haus"
    ;; ls `guix build tzdata`/share/zoneinfo
    #:timezone  "Europe/Minsk")

   ;;; Allows to declare specific bootloader configuration,
   ;;; grub-efi-bootloader used by default
   ;; (feature-bootloader)
   (feature-bootloader
    #:bootloader-configuration
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets (list "/boot/efi"))
     (keyboard-layout (keyboard-layout "us" "dvorak"))))

   (feature-file-systems
    #:mapped-devices haus-mapped-devices
    #:file-systems haus-file-systems)

   (feature-kanshi)
   (feature-hidpi
    #:scaling-factor 2)

   (feature-kernel
    #:kernel linux
    #:kernel-arguments '("snd_hda_intel.dmic_detect=0"
                         ;; "modprobe.blacklist=nouveau" "rd.driver.blacklist=nouveau" "nvidia_drm.modeset=1" ;; "nvidia_drm.fbdev=1"
                         ;; "NVreg_EnableGpuFirmware=1"
                         )
    ;; #:kernel-loadable-modules (list nvidia-module) ;; https://gitlab.com/nonguix/nonguix/-/issues/227
    #:firmware (list linux-firmware))

   (feature-base-services
    ;; TODO: Use substitute-urls directly for guix commands?
    #:default-substitute-urls (list "https://bordeaux.guix.gnu.org"
                                    "https://ci.guix.trop.in"
                                    "https://substitutes.nonguix.org")
    #:guix-substitute-urls (list "https://substitutes.nonguix.org")
    #:guix-authorized-keys (list nonguix-pub))
   ))
