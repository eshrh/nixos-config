{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ./caps_to_esc.nix
    ./ibus.nix
    ./fonts/fonts.nix
    ./razer/razer.nix
    ./nonfree.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices."luks-58e3c917-e5d1-4406-a6fa-acf832a930e0".device = "/dev/disk/by-uuid/58e3c917-e5d1-4406-a6fa-acf832a930e0";

  time.timeZone = "Asia/Tokyo";
  services.xserver.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.xkb = {
    layout = "us";
    variant = "dvorak";
  };
  console.keyMap = "dvorak";

  users.users.esrh = {
    isNormalUser = true;
    description = "Eshan Ramesh";
    extraGroups = ["networkmanager" "wheel" "audio" "openrazer" "cdrom"];
    shell = pkgs.fish;
  };

  environment.systemPackages = with pkgs; [
    emacs
    git
    wine
    winetricks
    winePackages.stableFull
    lutris
  ];

  programs = {
    fish.enable = true;
    steam.enable = true;
    # cdemu.enable = true;
    gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };
  };

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    audio.enable = true;
    pulse.enable = true;
  };
  services.openssh.enable = true;

  nix.settings.experimental-features = ["nix-command" "flakes"];
  system.stateVersion = "23.11";
}
