# Declarative disk layout for install-to-disk.
#
# GPT: 1G EFI System Partition + LUKS2 (ext4 root inside).
# No swap partition (add zram/swapfile declaratively later if wanted).
#
# Partition labels are the contract between this file and
# laptop-hardware.nix (which mounts by /dev/disk/by-partlabel/*):
#   disk-main-ESP, disk-main-luks — keep them in sync.
#
# `disk` is the target device at format time only; the installed
# system never references it (mounts go by partlabel).
{ disk, passwordFile }: {
  disko.devices = {
    disk.main = {
      device = disk;
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            type = "EF00";
            size = "1G";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "fmask=0077" "dmask=0077" ];
            };
          };
          luks = {
            size = "100%";
            content = {
              type = "luks";
              name = "cryptroot";
              settings.allowDiscards = true;
              inherit passwordFile;
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
        };
      };
    };
  };
}
