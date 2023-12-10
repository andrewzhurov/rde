(define-module (andrewzhurov hosts t450)
  #:use-module (rde features base)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (ice-9 match)

  #:use-module (rde features gitwatch)
  #:use-module (rde packages bb))

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

(define-public %t450-features
  (list
   (feature-host-info
    #:host-name "t450"
    ;; ls `guix build tzdata`/share/zoneinfo
    #:timezone  "Asia/Tbilisi")
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
   ;; (feature-hidpi)
   ))
