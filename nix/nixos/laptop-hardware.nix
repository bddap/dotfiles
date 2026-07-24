# Hardware configuration for laptops installed via ./install-to-disk.
#
# Instead of a per-machine nixos-generate-config output (fs UUIDs,
# detected modules), this uses partition labels matching the disko
# layout (nix/nixos/disko.nix) and a superset of storage/input kernel
# modules, so one file works on any x86_64 laptop. Machine-specific
# tuning (e.g. a nixos-hardware profile) goes in the host file.

{ lib, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  # ── Filesystems (match disko partition labels) ────────────────────────
  boot.initrd.luks.devices.cryptroot = {
    device = "/dev/disk/by-partlabel/disk-main-luks";
    allowDiscards = true;
  };
  fileSystems."/" = {
    device = "/dev/mapper/cryptroot";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-partlabel/disk-main-ESP";
    fsType = "vfat";
    options = [ "fmask=0077" "dmask=0077" ];
  };

  # ── Kernel modules: generic superset, not machine-detected ───────────
  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "thunderbolt"
    "ahci"
    "usbhid"
    "usb_storage"
    "uas"
    "sd_mod"
    "sdhci_pci"
    # virtio_* so the same image boots in qemu for testing
    "virtio_pci"
    "virtio_blk"
    "virtio_scsi"
  ];

  # Microcode: both vendors enabled; only the matching one is applied.
  hardware.enableRedistributableFirmware = true;
  hardware.cpu.amd.updateMicrocode = true;
  hardware.cpu.intel.updateMicrocode = true;

  # Don't touch EFI variables during install-to-disk (would write to the
  # *installing* machine's NVRAM). The target boots via the fallback path
  # (EFI/BOOT/BOOTX64.EFI), which bootctl installs by default.
  boot.loader.efi.canTouchEfiVariables = false;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.nvidia.open = true;
}
