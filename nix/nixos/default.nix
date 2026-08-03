# The system configuration (built by ./nixos-build).
{ lib, ... }:
let pkgs = import ../nix { };
in {
  # Host-specific config lives in nix/nixos/local/ (gitignored) — the
  # single home for everything machine-specific: networking.hostName, the
  # nixos-generate-config hardware-configuration.nix, any per-machine
  # tuning. Imported iff present. ./install-to-disk populates it on new
  # machines; it is never carried from one machine to another.
  imports =
    if builtins.pathExists ./local/default.nix
    then [ ./local/default.nix ]
    else [ ];

  # Placeholders so the tree evaluates on a fresh clone (no
  # nix/nixos/local/ yet). A real machine's local/ overrides these via
  # normal priority (mkDefault loses to any plain definition).
  fileSystems."/" = lib.mkDefault { device = "none"; fsType = "tmpfs"; };
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  virtualisation.libvirtd.enable = true;

  boot = {
    loader.systemd-boot.enable = true;

    # CVE-2026-31431 (algif_aead): default linux_6_12 has no backport yet.
    # Pin to 6.18 LTS, which carries the fix from 6.18.22 onward.
    kernelPackages = pkgs.linuxPackages_6_18;
  };

  hardware.bluetooth.enable = true;

  # steam wants this
  hardware.graphics.enable32Bit = true;

  hardware.keyboard.zsa.enable = true;

  hardware.nvidia-container-toolkit.enable = true;
  hardware.nvidia-container-toolkit.mount-nvidia-executables = true;

  # Open kernel modules (Turing+). mkDefault so a machine can override in
  # nix/nixos/local/; driver >= 560 requires the option to be set.
  hardware.nvidia.open = lib.mkDefault true;

  networking.networkmanager.enable = true;

  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      userServices = true;
    };
  };

  services.xserver = {
    enable = true;

    videoDrivers = [ "nvidia" "amdgpu" "intel" ];

    xkb.layout = "us";
    xkb.variant = "";
  };

  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  services.gnome.localsearch.enable = false;

  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "a";

  # maybe not needed for trackpad inpu since it's enabled by default in most desktopManager?
  services.libinput.enable = true;

  services.printing.enable = true;

  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  programs.fish.enable = true;
  programs.nix-ld.enable = true;
  programs.virt-manager.enable = true;

  users.users.a = {
    isNormalUser = true;
    description = "a";
    extraGroups = [ "networkmanager" "wheel" "libvirtd" "kvm" ];
    packages = [ pkgs.home-manager ];
    shell = pkgs.fish;
  };

  fonts.packages = [ pkgs.comic-mono ];

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = [ ]; # use home-manager instead

  # Tried this, too much effort so gnome is manually configured for now
  # [org/gnome/desktop/input-sources]
  # xkb-options=['terminate:ctrl_alt_bksp', 'ctrl:swap_lalt_lctl']
  # [org/gnome/desktop/wm/keybindings]
  # toggle-fullscreen=['<Control><Super>f']
  # [org/gnome/nautilus/preferences]
  # migrated-gtk-settings=true
  # [org/gtk/gtk4/settings/file-chooser]
  # show-hidden=true

  services.openssh.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

  services.udev.extraRules = ''
    # Framework Laptop 16 - LED Matrix
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="32ac", ATTRS{idProduct}=="0020", MODE="0660", TAG+="uaccess"

    # # Framework Laptop 16 - LED Matrix (CDC ACM serial)
    # SUBSYSTEM=="tty", KERNEL=="ttyACM*", ATTRS{idVendor}=="32ac", ATTRS{idProduct}=="0020", MODE="0660", TAG+="uaccess"

  '';

}
