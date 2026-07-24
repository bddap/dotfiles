# Generic hardware baseline for laptops installed via ./install-to-disk.
#
# Machine-specific hardware config (filesystem/LUKS UUIDs, detected
# modules) is generated per machine by nixos-generate-config and lives in
# nix/nixos/local/hardware-configuration.nix (gitignored). What stays here
# is the generic part: a superset of storage/input kernel modules and both
# microcode vendors, so a config generated on the *installing* machine
# still boots any x86_64 laptop.

{ lib, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

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
}
