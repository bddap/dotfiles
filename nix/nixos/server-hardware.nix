# Hardware configuration for install-to-disk.
#
# Generic hardware support + partition-label-based fileSystems
# matching the disko layout.

{ lib, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  # ── Filesystems (match disko partition labels) ────────────────────────
  fileSystems."/" = {
    device = "/dev/disk/by-partlabel/disk-main-root";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-partlabel/disk-main-ESP";
    fsType = "vfat";
    options = [ "fmask=0077" "dmask=0077" ];
  };

  # ── Kernel modules for generic hardware ───────────────────────────────
  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "ahci"
    "usbhid"
    "usb_storage"
    "sd_mod"
    "sata_nv"
    "sata_sil"
    "sata_sis"
    "ehci_pci"
    "ohci_pci"
    "uhci_hcd"
    "virtio_pci"
    "virtio_blk"
    "virtio_scsi"
    "virtio_net"
    "thunderbolt"
  ];

  # Don't touch EFI variables during install-to-disk (would write to host's NVRAM).
  # The target machine boots via fallback path (EFI/BOOT/BOOTX64.EFI).
  boot.loader.efi.canTouchEfiVariables = false;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.nvidia.open = true;


}
