{ ... }:
let pkgs = import ../nix { };
in {
  imports = [ ./hardware-configuration.nix ];

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    # how did this get here? Shouldn't this be in hardware-configuration.nix?
    initrd.luks.devices."luks-f78f0095-dcc0-4b51-b602-c0689384506f".device =
      "/dev/disk/by-uuid/f78f0095-dcc0-4b51-b602-c0689384506f";

    # disable tmpfs for /tmp, its limited size causes pain
    tmp.useTmpfs = false;
  };

  networking.hostName = "nixos"; # Define your hostname.
  hardware.bluetooth.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # steam wants this
  hardware.opengl.driSupport32Bit = true;

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
    nssmdns = true;
    publish = {
      enable = true;
      userServices = true;
    };
  };

  services.xserver = {
    enable = true;

    # does this belong in hardware-configuration.nix?
    videoDrivers = [ "nvidia" ];

    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;

    layout = "us";
    xkbVariant = "";

    # maybe not needed for trackpad inpu since it's enabled by default in most desktopManager?
    libinput.enable = true;

    displayManager.autoLogin.enable = true;
    displayManager.autoLogin.user = "a";
  };

  services.printing.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  programs.fish.enable = true;
  programs.nix-ld.enable = true;

  users.users.a = {
    isNormalUser = true;
    description = "a";
    extraGroups = [ "networkmanager" "wheel" ];
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

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
